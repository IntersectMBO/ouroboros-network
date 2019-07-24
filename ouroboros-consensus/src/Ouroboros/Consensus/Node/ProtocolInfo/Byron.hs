{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.ProtocolInfo.Byron (
    protocolInfoByron
  , ByronConfig
  , ByronProtocolSetupError (..)
  , PBftSignatureThreshold(..)
    -- * Secrets
  , PBftLeaderCredentials
  , mkPBftLeaderCredentials
  ) where

import           Control.Exception (Exception, throw)
import           Control.Monad.Except
import           Data.Coerce
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Maybe

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Common as Cardano.Common
import qualified Cardano.Chain.Delegation as Cardano.Delegation
import qualified Cardano.Chain.Genesis as Cardano.Genesis
import qualified Cardano.Chain.Update as Cardano.Update
import qualified Cardano.Crypto as Cardano

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Ouroboros.Consensus.Ledger.Byron.Config

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  Conditions
-------------------------------------------------------------------------------}

data ByronProtocolSetupError =
      ByronInconsistentLeaderCredentials
        Cardano.VerificationKey
        Cardano.Delegation.Certificate
      deriving (Eq, Show)

instance Exception ByronProtocolSetupError

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data PBftLeaderCredentials = PBftLeaderCredentials {
      plcSignKey     :: Cardano.SigningKey
    , plcDlgCert     :: Cardano.Delegation.Certificate
    } deriving (Eq, Show)

mkPBftLeaderCredentials :: Cardano.SigningKey
                        -> Cardano.Delegation.Certificate
                        -> Maybe PBftLeaderCredentials
mkPBftLeaderCredentials (sk@(Cardano.toVerification -> vk)) cert =
  if Cardano.Delegation.delegateVK cert == vk
  then Just $ PBftLeaderCredentials sk cert
  else Nothing

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Signature threshold. This represents the proportion of blocks in a
--   pbftSignatureWindow-sized window which may be signed by any single key.
newtype PBftSignatureThreshold =
        PBftSignatureThreshold { unSignatureThreshold :: Double }

-- TODO This is currently configured for the demo. Parameterise so it can work
-- both for a real node and a demo node.
protocolInfoByron :: Cardano.Genesis.Config
                  -> Maybe PBftLeaderCredentials
                  -> Maybe PBftSignatureThreshold
                  -> Cardano.Update.ProtocolVersion
                  -> Cardano.Update.SoftwareVersion
                  -> ProtocolInfo (ByronBlockOrEBB ByronConfig)
protocolInfoByron gc mLeader mSigThresh pVer sVer =
    ProtocolInfo {
        pInfoConfig = WithEBBNodeConfig $ EncNodeConfig {
            encNodeConfigP   = PBftNodeConfig {
                pbftParams          =  PBftParams
                  { pbftSecurityParam      = SecurityParam (fromIntegral k)
                  , pbftNumNodes           =
                      fromIntegral . Set.size . Cardano.Genesis.unGenesisKeyHashes
                      $ gdGenesisKeyHashes
                  , pbftSignatureWindow    = fromIntegral k
                  , pbftSignatureThreshold = unSignatureThreshold $
                      fromMaybe mainnetPBftSignatureThreshold mSigThresh
                  }
              , pbftIsLeader        = proofOfCredentials <$> mLeader
            }
          , encNodeConfigExt = ByronConfig {
                pbftProtocolMagic   = Cardano.Genesis.configProtocolMagic gc
              , pbftProtocolVersion = pVer
              , pbftSoftwareVersion = sVer
              , pbftGenesisConfig   = gc
              , pbftGenesisHash     = coerce Cardano.Genesis.configGenesisHeaderHash gc
              , pbftEpochSlots      = Cardano.Genesis.configEpochSlots gc
              , pbftGenesisDlg      = Cardano.Genesis.configHeavyDelegation gc
              , pbftSecrets         = Dummy.dummyGeneratedSecrets
                --TODO: These "richmen" secrets ^^ are here to support demos
                -- where we need to elaborate from mock transactions to real
                -- ones. It should be removed when we can eliminate elaboration.
              }
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = ByronEBBLedgerState $ ByronLedgerState {
                blsCurrent   = initState
              , blsSnapshots = Seq.empty
              }
          , ouroborosChainState = Seq.empty
          }
      , pInfoInitState  = ()
      }
  where
    Cardano.Genesis.GenesisData
      { Cardano.Genesis.gdGenesisKeyHashes
      , Cardano.Genesis.gdK = Cardano.Common.BlockCount k
    } = Cardano.Genesis.configGenesisData gc

    proofOfCredentials :: PBftLeaderCredentials
                       -> PBftIsLeader PBftCardanoCrypto
    proofOfCredentials (PBftLeaderCredentials sk cert) =
      PBftIsLeader
      { pbftCoreNodeId =
        fromMaybe (throw $ ByronInconsistentLeaderCredentials vk cert)
        (genesisKeyCoreNodeId gc (VerKeyCardanoDSIGN issuerVK))
      , pbftSignKey    = SignKeyCardanoDSIGN sk
      , pbftVerKey     = VerKeyCardanoDSIGN vk
      , pbftGenVerKey  = VerKeyCardanoDSIGN issuerVK
      }
      where
        vk = Cardano.toVerification sk
        Cardano.Delegation.UnsafeACertificate
          { Cardano.Delegation.issuerVK = issuerVK } = cert

    initState :: Cardano.Block.ChainValidationState
    Right initState = runExcept $ Cardano.Block.initialChainValidationState gc

    -- | See chapter 4.1 of
    --   https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
    mainnetPBftSignatureThreshold :: PBftSignatureThreshold
    mainnetPBftSignatureThreshold = PBftSignatureThreshold 0.22

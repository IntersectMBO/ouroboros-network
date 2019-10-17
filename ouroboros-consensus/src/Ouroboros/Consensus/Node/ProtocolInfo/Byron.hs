{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.ProtocolInfo.Byron (
    protocolInfoByron
  , ByronConfig
  , PBftSignatureThreshold(..)
  , defaultPBftSignatureThreshold
    -- * Secrets
  , PBftLeaderCredentials
  , PBftLeaderCredentialsError
  , mkPBftLeaderCredentials
    -- * For testing
  , plcCoreNodeId
  ) where

import           Control.Exception (Exception)
import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set

import qualified Cardano.Chain.Block as Block
import           Cardano.Chain.Common (BlockCount (..))
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron hiding (genesisConfig)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Ouroboros.Consensus.Ledger.Byron.Config

import           Ouroboros.Network.Magic

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data PBftLeaderCredentials = PBftLeaderCredentials {
      plcSignKey    :: Crypto.SigningKey
    , plcDlgCert    :: Delegation.Certificate
    , plcCoreNodeId :: CoreNodeId
    } deriving Show

-- | Make the 'PBftLeaderCredentials', with a couple sanity checks:
--
-- * That the block signing key and the delegation certificate match.
-- * That the delegation certificate does correspond to one of the genesis
--   keys from the genesis file.
--
mkPBftLeaderCredentials :: Genesis.Config
                        -> Crypto.SigningKey
                        -> Delegation.Certificate
                        -> Either PBftLeaderCredentialsError
                                  PBftLeaderCredentials
mkPBftLeaderCredentials gc sk cert = do
    guard (Delegation.delegateVK cert == Crypto.toVerification sk)
      ?! NodeSigningKeyDoesNotMatchDelegationCertificate

    let vkGenesis = Delegation.issuerVK cert
    nid <- genesisKeyCoreNodeId gc (VerKeyCardanoDSIGN vkGenesis)
             ?! DelegationCertificateNotFromGenesisKey

    return PBftLeaderCredentials {
      plcSignKey     = sk
    , plcDlgCert     = cert
    , plcCoreNodeId  = nid
    }
  where
    (?!) :: Maybe a -> e -> Either e a
    Just x  ?! _ = Right x
    Nothing ?! e = Left  e

data PBftLeaderCredentialsError =
       NodeSigningKeyDoesNotMatchDelegationCertificate
     | DelegationCertificateNotFromGenesisKey
  deriving (Eq, Show)

instance Exception PBftLeaderCredentialsError


{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Signature threshold. This represents the proportion of blocks in a
--   pbftSignatureWindow-sized window which may be signed by any single key.
newtype PBftSignatureThreshold =
        PBftSignatureThreshold { unSignatureThreshold :: Double }

-- | See chapter 4.1 of
--   https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
defaultPBftSignatureThreshold :: PBftSignatureThreshold
defaultPBftSignatureThreshold = PBftSignatureThreshold 0.22

protocolInfoByron :: Genesis.Config
                  -> Maybe PBftSignatureThreshold
                  -> Update.ProtocolVersion
                  -> Update.SoftwareVersion
                  -> Maybe PBftLeaderCredentials
                  -> ProtocolInfo (ByronBlockOrEBB ByronConfig)
protocolInfoByron genesisConfig@Genesis.Config {
                    Genesis.configGenesisHash = genesisHash
                  , Genesis.configGenesisData =
                      Genesis.GenesisData {
                        Genesis.gdK                = BlockCount kParam
                      , Genesis.gdGenesisKeyHashes = genesisKeyHashes
                      , Genesis.gdProtocolMagicId  = protocolMagicId
                      }
                  }
                  mSigThresh pVer sVer mLeader =
    ProtocolInfo {
        pInfoConfig = WithEBBNodeConfig $ EncNodeConfig {
            encNodeConfigP   = PBftNodeConfig {
                pbftParams          =  PBftParams
                  { pbftSecurityParam      = SecurityParam (fromIntegral kParam)
                  , pbftNumNodes           = fromIntegral . Set.size
                                           . Genesis.unGenesisKeyHashes
                                           $ genesisKeyHashes
                  , pbftSignatureThreshold = unSignatureThreshold $
                      fromMaybe defaultPBftSignatureThreshold mSigThresh
                  , pbftNetworkMagic       = NetworkMagic $ Crypto.unProtocolMagicId protocolMagicId
                  }
              , pbftIsLeader =
                  case mLeader of
                    Nothing                                  -> PBftIsNotALeader
                    Just (PBftLeaderCredentials sk cert nid) ->
                      PBftIsALeader PBftIsLeader {
                        pbftCoreNodeId = nid
                      , pbftSignKey    = SignKeyCardanoDSIGN sk
                      , pbftDlgCert    = cert
                      }
            }
          , encNodeConfigExt = ByronConfig {
                pbftProtocolMagic   = Genesis.configProtocolMagic genesisConfig
              , pbftProtocolVersion = pVer
              , pbftSoftwareVersion = sVer
              , pbftGenesisConfig   = genesisConfig
              , pbftGenesisHash     = genesisHash
              , pbftEpochSlots      = Genesis.configEpochSlots genesisConfig
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
          , ouroborosChainState = initChainStateWithEBBs CS.empty
          }
      , pInfoInitState  = ()
      }
  where
    initState :: Block.ChainValidationState
    Right initState = runExcept $ Block.initialChainValidationState genesisConfig

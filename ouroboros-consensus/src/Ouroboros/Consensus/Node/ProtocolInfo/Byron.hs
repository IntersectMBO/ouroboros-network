{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.ProtocolInfo.Byron (
    protocolInfoByron
  , ByronConfig
  ) where

import           Control.Monad.Except
import           Data.Coerce
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Cardano.Genesis
import qualified Cardano.Chain.Update as Cardano.Update
import qualified Cardano.Crypto as Cardano
import qualified Cardano.Crypto.Signing as Cardano.KeyGen

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Ouroboros.Consensus.Ledger.Byron.Config

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- TODO This is currently configured for the demo. Parameterise so it can work
-- both for a real node and a demo node.
protocolInfoByron :: NumCoreNodes
                  -> CoreNodeId
                  -> PBftParams
                  -> Cardano.Genesis.Config
                  -> ProtocolInfo (ByronBlockOrEBB ByronConfig)
protocolInfoByron (NumCoreNodes numCoreNodes) (CoreNodeId nid) params gc =
    ProtocolInfo {
        pInfoConfig = WithEBBNodeConfig $ EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                  pbftParams  = params
                    { pbftNumNodes = fromIntegral numCoreNodes
                      -- Set the signature window to be short for the demo.
                    , pbftSignatureWindow = 7
                    }
                , pbftNodeId  = CoreId nid
                , pbftSignKey = SignKeyCardanoDSIGN (snd (lookupKey nid))
                , pbftVerKey  = VerKeyCardanoDSIGN  (fst (lookupKey nid))
                , pbftGenVerKey = VerKeyCardanoDSIGN (lookupGenKey nid)
                }
          , encNodeConfigExt = ByronConfig {
                pbftProtocolMagic   = Cardano.Genesis.configProtocolMagic gc
              , pbftProtocolVersion = Cardano.Update.ProtocolVersion 1 0 0
              , pbftSoftwareVersion = Cardano.Update.SoftwareVersion (Cardano.Update.ApplicationName "Cardano Demo") 1
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
    initState :: Cardano.Block.ChainValidationState
    Right initState = runExcept $ Cardano.Block.initialChainValidationState gc

    lookupKey :: Int -> (Cardano.VerificationKey, Cardano.SigningKey)
    lookupKey n = (\x -> (Cardano.KeyGen.toVerification x, x))
                . (!! n)
                . Cardano.Genesis.gsRichSecrets
                . fromJust
                $ Cardano.Genesis.configGeneratedSecrets gc

    lookupGenKey :: Int -> Cardano.VerificationKey
    lookupGenKey n = Cardano.KeyGen.toVerification
                   . (!! n)
                   . Cardano.Genesis.gsDlgIssuersSecrets
                   . fromJust
                   $ Cardano.Genesis.configGeneratedSecrets gc

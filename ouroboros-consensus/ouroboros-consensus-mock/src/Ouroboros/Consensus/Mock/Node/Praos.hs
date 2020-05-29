{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Praos (
    MockPraosBlock
  , protocolInfoPraos
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

type MockPraosBlock = SimplePraosBlock SimpleMockCrypto PraosMockCrypto

protocolInfoPraos :: Monad m
                  => NumCoreNodes
                  -> CoreNodeId
                  -> PraosParams
                  -> HardFork.EraParams
                  -> ProtocolInfo m MockPraosBlock
protocolInfoPraos numCoreNodes nid params eraParams =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = PraosConfig {
                praosParams       = params
              , praosSignKeyVRF   = signKeyVRF nid
              , praosInitialEta   = 0
              , praosInitialStake = genesisStakeDist addrDist
              , praosVerKeys      = verKeys
              }
          , configLedger = SimpleLedgerConfig addrDist eraParams
          , configBlock  = SimpleBlockConfig (praosSecurityParam params)
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = genesisSimpleLedgerState addrDist
          , headerState = genesisHeaderState []
          }
      , pInfoLeaderCreds = Just (
            nid
          , MaintainForgeState {
                initForgeState =
                  PraosKey $
                    SignKeyMockKES
                      (fst $ verKeys Map.! nid)   -- key ID
                      0                           -- KES initial slot
              , updateForgeState = evolveKey
              }
          )
      }
  where
    signKeyVRF :: CoreNodeId -> SignKeyVRF MockVRF
    signKeyVRF (CoreNodeId n) = SignKeyMockVRF n

    verKeyVRF :: CoreNodeId -> VerKeyVRF MockVRF
    verKeyVRF (CoreNodeId n) = VerKeyMockVRF n

    verKeyKES :: CoreNodeId -> VerKeyKES (MockKES t)
    verKeyKES (CoreNodeId n) = VerKeyMockKES n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES (MockKES t), VerKeyVRF MockVRF)
    verKeys = Map.fromList
      [ (nid', (kesKey, vrfKey))
      | nid' <- enumCoreNodes numCoreNodes
      , let !kesKey = verKeyKES nid'
            !vrfKey = verKeyVRF nid'
      ]

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Praos (
    MockPraosBlock
  , protocolInfoPraos
  ) where

import           Data.Bifunctor (second)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Numeric.Natural (Natural)

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool (txForgetValidated)
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Util.IOLike

type MockPraosBlock = SimplePraosBlock SimpleMockCrypto PraosMockCrypto

protocolInfoPraos :: IOLike m
                  => NumCoreNodes
                  -> CoreNodeId
                  -> PraosParams
                  -> HardFork.EraParams
                  -> Natural
                  -> PraosEvolvingStake
                  -> ProtocolInfo m MockPraosBlock
protocolInfoPraos numCoreNodes nid params eraParams eta0 evolvingStakeDist =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PraosConfig {
                praosParams        = params
              , praosSignKeyVRF    = signKeyVRF nid
              , praosInitialEta    = eta0
              , praosInitialStake  = genesisStakeDist addrDist
              , praosEvolvingStake = evolvingStakeDist
              , praosVerKeys       = verKeys
              }
          , topLevelConfigLedger  = SimpleLedgerConfig addrDist eraParams
          , topLevelConfigBlock   = SimpleBlockConfig
          , topLevelConfigCodec   = SimpleCodecConfig
          , topLevelConfigStorage = SimpleStorageConfig (praosSecurityParam params)
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = genesisSimpleLedgerState addrDist
          , headerState = genesisHeaderState (PraosChainDepState [])
          }
      , pInfoBlockForging = sequence [praosBlockForging nid initHotKey]
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

    initHotKey :: HotKey PraosMockCrypto
    initHotKey =
        HotKey
          0
          (SignKeyMockKES
            -- key ID
            (fst $ verKeys Map.! nid)
            -- KES initial slot
            0)

praosBlockForging ::
     IOLike m
  => CoreNodeId
  -> HotKey PraosMockCrypto
  -> m (BlockForging m MockPraosBlock)
praosBlockForging cid initHotKey = do
    varHotKey <- newMVar initHotKey
    return $ BlockForging {
        forgeLabel       = "praosBlockForging"
      , canBeLeader      = cid
      , updateForgeState = \_ sno _ -> updateMVar varHotKey $
                                 second forgeStateUpdateInfoFromUpdateInfo
                               . evolveKey sno
      , checkCanForge    = \_ _ _ _ _ -> return ()
      , forgeBlock       = \cfg bno sno tickedLedgerSt txs isLeader -> do
                               hotKey <- readMVar varHotKey
                               return $
                                 forgeSimple
                                   (forgePraosExt hotKey)
                                   cfg
                                   bno sno
                                   tickedLedgerSt
                                   (map txForgetValidated txs)
                                   isLeader
      }

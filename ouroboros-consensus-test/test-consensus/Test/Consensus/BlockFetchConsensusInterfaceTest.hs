{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Test.Consensus.BlockFetchConsensusInterfaceTest (tests) where

import           Cardano.Crypto.KES
import           Cardano.Slotting.Slot (origin)
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Mock.Ledger.Stake (StakeDist (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
-- * Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- * Anchor
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
-- * DummyBlock
import           Cardano.Crypto.VRF.Mock (MockVRF, SignKeyVRF (..),
                     VerKeyVRF (..))
import           Cardano.Slotting.Time (slotLengthFromSec)
import           Ouroboros.Consensus.Block.Abstract (Header)
import           Ouroboros.Consensus.Block.SupportsProtocol
                     (BlockSupportsProtocol (..))
import           Ouroboros.Consensus.BlockchainTime.API (BlockchainTime)
import           Ouroboros.Consensus.HardFork.History.EraParams
                     (defaultEraParams)
import           Ouroboros.Consensus.Mock.Ledger.Address (mkAddrDist)
import           Ouroboros.Consensus.Mock.Ledger.Block
                     (BlockConfig (SimpleBlockConfig),
                     CodecConfig (SimpleCodecConfig), SimpleBlock (..),
                     SimpleLedgerConfig (..), SimpleMockCrypto (..),
                     StorageConfig (SimpleStorageConfig))
import           Ouroboros.Consensus.Mock.Ledger.Block.Praos
import           Ouroboros.Consensus.Mock.Ledger.Block.PraosRule
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
-- * ChainDB
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB (..))
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (LedgerDB (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LedgerDB
-- * BlockFetchConsensusInterface
import           Ouroboros.Consensus.NodeKernel
                     (initBlockFetchConsensusInterface)
import           Ouroboros.Network.BlockFetch
                     (BlockFetchConsensusInterface (..))
-- * STM
import           Control.Monad.Class.MonadSTM (MonadSTM (..),
                     MonadSTMTx (modifyTVar, newTVar, readTVar))
import           Ouroboros.Consensus.Util.IOLike (IOLike)
-- * testing framework
import           Test.Tasty
import           Test.Tasty.HUnit
-- * instances
import           Ouroboros.Consensus.Byron.Ledger.PBFT
import           Ouroboros.Consensus.Byron.Node

tests :: TestTree
tests =
  testGroup "BlockFetchConsensusInterface"
    [testGroup "readCandidateChains"
      [ testCase
          "should return value from candidates STM that was passed during initialization"
          test_readCandidateChains
      ]
    ]

type DummyBlock = SimplePraosBlock SimpleMockCrypto PraosMockCrypto

test_readCandidateChains :: Assertion
test_readCandidateChains = do
    -- * given BlockFetchConsensusInterface was initialized with STM holding candidates
    let initCandidates  = Map.empty @String @(AnchoredFragment (Header DummyBlock))
    candidatesTVar      <- atomically $ newTVar initCandidates
    blockFetchInterface <- initInterface (readTVar candidatesTVar)
    -- * and given that afterwards content of those candidates was modified
    let modifiedCandidates = initCandidates
    atomically $ modifyTVar candidatesTVar (const modifiedCandidates)
    -- * when calling readCandidateChains on BlockFetchConsensusInterface
    candidates          <- atomically $ readCandidateChains blockFetchInterface
    -- * then the result is the modified content
    candidates @?= modifiedCandidates
    -- res @?= Right ()
  where
    res = Right ()

initInterface ::
     forall m peer . IOLike m
  => STM m (Map peer (AnchoredFragment (Header DummyBlock)))
  -> m (BlockFetchConsensusInterface peer (Header DummyBlock) DummyBlock m)
initInterface getCandidates = do
  chainDB <- initChainDB
  initBlockFetchConsensusInterface cfg chainDB getCandidates (const 32) getCurrentSlot
  where

    cfg :: TopLevelConfig DummyBlock
    cfg = TopLevelConfig {
              topLevelConfigProtocol = PraosConfig {
                  praosParams        = PraosParams 3.3 securityParam 100
                , praosInitialEta    = 0
                , praosInitialStake  = stakeDist
                , praosEvolvingStake = constantStakeDist
                , praosSignKeyVRF    = signKeyVRF $ head nodes
                , praosVerKeys       = verKeys
                }
            , topLevelConfigLedger   = SimpleLedgerConfig addrDist eraParams
            , topLevelConfigBlock    = SimpleBlockConfig
            , topLevelConfigCodec    = SimpleCodecConfig
            , topLevelConfigStorage  = SimpleStorageConfig securityParam
          }
      where
        securityParam = SecurityParam 2048
        numCoreNodes = 2
        addrDist = mkAddrDist (NumCoreNodes numCoreNodes)
        nodes = fmap (\id -> CoreNodeId id) [1..numCoreNodes]
        stakeDist = StakeDist $ Map.fromList $ fmap (\k -> (k, 1 / 2)) nodes
        constantStakeDist = PraosEvolvingStake $ Map.fromList $ fmap (\e -> (e, stakeDist)) [0..10]
        signKeyVRF (CoreNodeId n) = SignKeyMockVRF n
        verKeys :: Map CoreNodeId (VerKeyKES (MockKES t), VerKeyVRF MockVRF)
        verKeys = Map.fromList
          [ (nid', (kesKey, vrfKey))
          | nid' <- nodes
          , let !kesKey = verKeyKES nid'
                !vrfKey = verKeyVRF nid'
          ]
        verKeyVRF (CoreNodeId n) = VerKeyMockVRF n
        verKeyKES (CoreNodeId n) = VerKeyMockKES n
        eraParams = defaultEraParams securityParam defaultSlotLength
        defaultSlotLength = slotLengthFromSec 1


    initChainDB :: m (ChainDB m DummyBlock)
    initChainDB = do
      ledgerDBTVar <- atomically . newTVar $ initLedger
      pure $ ChainDB {
          getLedgerDB        = readTVar ledgerDBTVar

        , addBlockAsync      = undefined
        , getCurrentChain    = undefined
        , getTipBlock        = undefined
        , getTipHeader       = undefined
        , getTipPoint        = undefined
        , getBlockComponent  = undefined
        , getIsFetched       = undefined
        , getIsValid         = undefined
        , getMaxSlotNo       = undefined
        , stream             = undefined
        , newFollower        = undefined
        , getIsInvalidBlock  = undefined
        , closeDB            = undefined
        , isOpen             = undefined
    }

    initLedger :: LedgerDB (ExtLedgerState DummyBlock)
    initLedger =
      let ledgerState   = undefined
          headerState   = undefined
          extendedState = ExtLedgerState ledgerState headerState
      in  LedgerDB.ledgerDbWithAnchor extendedState

    getCurrentSlot :: BlockchainTime m
    getCurrentSlot = undefined

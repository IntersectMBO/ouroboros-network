{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests) where

import           Control.Tracer (nullTracer)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Crypto.DSIGN.Mock

import           Network.TypedProtocol.Proofs (connect)
import           Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
                     (localStateQueryClient)
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (DiskLedgerView (..), getTip)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (Query (..),
                     QueryWithSomeFootprintL (..))
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (StaticEither (..))
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as TECHDEBT

import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LgrDB,
                     LgrDbArgs (..), mkLgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.FS.API (HasFS, SomeHasFS (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..), defaultDiskPolicy)
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LgrDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as TECHDEBT

import           Test.QuickCheck hiding (Result)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "LocalStateQueryServer"
    [ testProperty "localStateQueryServer" prop_localStateQueryServer
    ]

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

-- | Plan:
-- * Preseed the LgrDB of the server with the preferred chain of the
--  'BlockTree'.
-- * Acquire for each block in the 'BlockTree', including the ones not on the
--   chain, a state and send the 'QueryLedgerTip'. Collect these results.
-- * Check that when acquiring failed, it rightfully failed. Otherwise, check
--   whether the returned tip matches the block.
prop_localStateQueryServer
  :: SecurityParam
  -> BlockTree
  -> Permutation
  -> Positive (Small Int)
  -> Property
prop_localStateQueryServer k bt p (Positive (Small n)) = checkOutcome k chain actualOutcome
  where
    chain :: Chain TestBlock
    chain = treePreferredChain bt

    points :: [Maybe (Point TestBlock)]
    points = permute p $
         replicate n Nothing
      ++ (Just . blockPoint <$> (treeToBlocks bt))

    actualOutcome = runSimOrThrow $ do
      let client = mkClient points
      server <- mkServer k chain
      (\(a, _, _) -> a) <$>
        connect
          (localStateQueryClientPeer client)
          (localStateQueryServerPeer server)

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

-- | Checks whether the given outcome is correct: in case of an
-- 'AcquireFailure', we check whether it was warranted. Otherwise we check
-- whether the results are correct.
--
-- NOTE: when we don't get an 'AcquireFailure', even though we expected it, we
-- accept it. This is because the LgrDB may contain snapshots for blocks on
-- the current chain older than @k@, but we do not want to imitate such
-- implementation details.
--
-- Additionally, this function labels the test results.
checkOutcome
  :: SecurityParam
  -> Chain TestBlock
  -> [(Maybe (Point TestBlock), Either AcquireFailure (Point TestBlock))]
  -> Property
checkOutcome k chain = conjoin . map (uncurry checkResult)
  where
    immutableSlot :: WithOrigin SlotNo
    immutableSlot = Chain.headSlot $
      Chain.drop (fromIntegral (maxRollbacks k)) chain

    checkResult
      :: Maybe (Point TestBlock)
      -> Either AcquireFailure (Point TestBlock)
      -> Property
    checkResult (Just pt) = \case
      Right result
        -> tabulate "Acquired" ["Success"] $ result === pt
      Left AcquireFailurePointNotOnChain
        | Chain.pointOnChain pt chain
        -> counterexample
           ("Point " <> show pt <>
            " on chain, but got AcquireFailurePointNotOnChain")
           (property False)
        | otherwise
        -> tabulate "Acquired" ["AcquireFailurePointNotOnChain"] $ property True
      Left AcquireFailurePointTooOld
        | pointSlot pt >= immutableSlot
        -> counterexample
           ("Point " <> show pt <>
            " newer than the immutable tip, but got AcquireFailurePointTooOld")
           (property False)
        | otherwise
        -> tabulate "Acquired" ["AcquireFailurePointTooOld"] $ property True
    checkResult Nothing = \case
      Right _result -> tabulate "Acquired" ["Success"] True
      Left  failure -> counterexample ("acuire tip point resulted in " ++ show failure) False

mkClient
  :: Monad m
  => [Maybe (Point TestBlock)]
  -> LocalStateQueryClient
       TestBlock
       (Point TestBlock)
       (Query TestBlock)
       m
       [(Maybe (Point TestBlock), Either AcquireFailure (Point TestBlock))]
mkClient points = localStateQueryClient [(pt, QueryWithSomeFootprintL (BlockQuery QueryLedgerTip)) | pt <- points]

mkServer
  :: IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LocalStateQueryServer TestBlock (Point TestBlock) (Query TestBlock) m ())
mkServer k chain = do
    lgrDB <- initLgrDB k chain
    return $
      localStateQueryServer
        cfg
        (\seP -> do
           ldb0 <- atomically $ LgrDB.getCurrent lgrDB
           pure $ case seP of
             StaticLeft ()  -> StaticLeft $ mkDLV ldb0
             StaticRight pt -> StaticRight $ case LgrDB.ledgerDbPrefix pt ldb0 of
               Nothing  -> Left  $ castPoint $ getTip $ LgrDB.ledgerDbAnchor ldb0
               Just ldb -> Right $ mkDLV ldb
        )
  where
    cfg = ExtLedgerCfg $ testCfg k

    mkDLV ldb =
      DiskLedgerView
        (LgrDB.ledgerDbCurrent ldb)
        (\(ExtLedgerStateTables NoTestLedgerTables) -> pure $ ExtLedgerStateTables NoTestLedgerTables)
        (\_rq -> pure $ ExtLedgerStateTables NoTestLedgerTables)
        (pure ())

-- | Initialise a 'LgrDB' with the given chain.
initLgrDB
  :: forall m. IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LgrDB m TestBlock)
initLgrDB k chain = do
    varDB          <- newTVarIO genesisLedgerDB
    varPrevApplied <- newTVarIO mempty
    let
      backingStoreInitialiser =
        TECHDEBT.newBackingStoreInitialiser
        mempty
        TECHDEBT.InMemoryBackingStore
    backingStore <- do
      v <- uncheckedNewTVarM Mock.empty
      TECHDEBT.newBackingStore
        backingStoreInitialiser
        (SomeHasFS (simHasFS v))
        (ExtLedgerStateTables NoTestLedgerTables)
    rawLock <- TECHDEBT.new ()
    let lgrDB = mkLgrDB varDB varPrevApplied backingStore rawLock resolve args
    LgrDB.validate lgrDB genesisLedgerDB BlockCache.empty 0 noopTrace
      (map getHeader (Chain.toOldestFirst chain)) >>= \case
        LgrDB.ValidateExceededRollBack _ ->
          error "impossible: rollback was 0"
        LgrDB.ValidateLedgerError _ ->
          error "impossible: there were no invalid blocks"
        LgrDB.ValidateSuccessful ledgerDB' -> do
          atomically $ LgrDB.setCurrent lgrDB ledgerDB'
          return lgrDB
  where
    resolve :: RealPoint TestBlock -> m TestBlock
    resolve = return . (blockMapping Map.!)

    blockMapping :: Map (RealPoint TestBlock) TestBlock
    blockMapping = Map.fromList
      [(blockRealPoint b, b) | b <- Chain.toOldestFirst chain]

    cfg = testCfg k

    genesisLedgerDB = LgrDB.ledgerDbWithAnchor (convertMapKind testInitExtLedger)

    noopTrace :: blk -> m ()
    noopTrace = const $ pure ()

    args = LgrDbArgs
      { lgrTopLevelConfig       = cfg
      , lgrHasFS                = SomeHasFS (error "lgrHasFS" :: HasFS m ())
      , lgrDiskPolicy           = defaultDiskPolicy k DefaultSnapshotInterval
      , lgrGenesis              = return testInitExtLedger
      , lgrTracer               = nullTracer
      , lgrTraceLedger          = nullTracer
      , lgrBackingStoreSelector = TECHDEBT.InMemoryBackingStore
      }

testCfg :: SecurityParam -> TopLevelConfig TestBlock
testCfg securityParam = TopLevelConfig {
      topLevelConfigProtocol = BftConfig {
          bftParams  = BftParams { bftSecurityParam = securityParam
                                 , bftNumNodes      = numCoreNodes
                                 }
        , bftSignKey = SignKeyMockDSIGN 0
        , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , topLevelConfigLedger  = eraParams
    , topLevelConfigBlock   = TestBlockConfig numCoreNodes
    , topLevelConfigCodec   = TestBlockCodecConfig
    , topLevelConfigStorage = TestBlockStorageConfig
    }
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 1

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams securityParam slotLength

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (1, 100)
  shrink (SecurityParam k) = [SecurityParam k' |  k' <- shrink k, k' > 0]

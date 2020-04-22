{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests) where

import           Control.Tracer (nullTracer)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Crypto.DSIGN.Mock

import           Network.TypedProtocol.Proofs (connect)
import           Ouroboros.Network.Block (Point (..), SlotNo, blockPoint,
                     pointSlot)
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
                     (localStateQueryClient)
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.IOLike

import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LedgerCursor as LedgerCursor
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB
                     (LedgerDbParams (..), LgrDB, LgrDbArgs (..), mkLgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.FS.API (HasFS)
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LgrDB
                     (ledgerDbFromGenesis)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty
import           Test.Tasty.QuickCheck

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
  -> Property
prop_localStateQueryServer k bt p = checkOutcome k chain actualOutcome
  where
    chain :: Chain TestBlock
    chain = treePreferredChain (testCfg k) bt

    points :: [Point TestBlock]
    points = blockPoint <$> permute p (treeToBlocks bt)

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
  -> [(Point TestBlock, Either AcquireFailure (Point TestBlock))]
  -> Property
checkOutcome k chain = conjoin . map (uncurry checkResult)
  where
    immutableSlot :: WithOrigin SlotNo
    immutableSlot = Chain.headSlot $
      Chain.drop (fromIntegral (maxRollbacks k)) chain

    checkResult
      :: Point TestBlock
      -> Either AcquireFailure (Point TestBlock)
      -> Property
    checkResult pt = \case
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

mkClient
  :: Monad m
  => [Point TestBlock]
  -> LocalStateQueryClient
       TestBlock
       (Query TestBlock)
       m
       [(Point TestBlock, Either AcquireFailure (Point TestBlock))]
mkClient points = localStateQueryClient [(pt, QueryLedgerTip) | pt <- points]

mkServer
  :: IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LocalStateQueryServer TestBlock (Query TestBlock) m ())
mkServer k chain = do
    lgrDB <- initLgrDB k chain
    return $ localStateQueryServer cfg $
      LedgerCursor.newLedgerCursor lgrDB getImmutablePoint
  where
    cfg = configLedger $ testCfg k
    getImmutablePoint = return $ Chain.headPoint $
      Chain.drop (fromIntegral (maxRollbacks k)) chain

-- | Initialise a 'LgrDB' with the given chain.
initLgrDB
  :: forall m. IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LgrDB m TestBlock)
initLgrDB k chain = do
    varDB          <- newTVarM genesisLedgerDB
    varPrevApplied <- newTVarM mempty
    let lgrDB = mkLgrDB varDB varPrevApplied resolve args
    LgrDB.validate lgrDB genesisLedgerDB BlockCache.empty 0
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

    params :: LedgerDbParams
    params = LedgerDbParams
      { ledgerDbSnapEvery     = maxRollbacks k
      , ledgerDbSecurityParam = k
      }

    cfg = testCfg k

    genesisLedgerDB = LgrDB.ledgerDbFromGenesis params testInitExtLedger

    args = LgrDbArgs
      { lgrTopLevelConfig       = cfg
      , lgrHasFS                = error "lgrHasFS" :: HasFS m ()
      , lgrDecodeLedger         = error "lgrDecodeLedger"
      , lgrDecodeConsensusState = error "lgrDecodeConsensusState"
      , lgrDecodeHash           = error "lgrDecodeHash"
      , lgrDecodeTipInfo        = error "lgrDecodeTipInfo"
      , lgrEncodeLedger         = error "lgrEncodeLedger"
      , lgrEncodeConsensusState = error "lgrEncodeConsensusState"
      , lgrEncodeHash           = error "lgrEncodeHash"
      , lgrEncodeTipInfo        = error "lgrEncodeTipInfo"
      , lgrParams               = params
      , lgrDiskPolicy           = error "lgrDiskPolicy"
      , lgrGenesis              = return testInitExtLedger
      , lgrTracer               = nullTracer
      , lgrTraceLedger          = nullTracer
      }

testCfg :: SecurityParam -> TopLevelConfig TestBlock
testCfg securityParam = TopLevelConfig {
      configConsensus = BftConfig
        { bftParams   = BftParams { bftSecurityParam = securityParam
                                  , bftNumNodes      = numCoreNodes
                                  }
        , bftNodeId   = CoreId (CoreNodeId 0)
        , bftSignKey  = SignKeyMockDSIGN 0
        , bftVerKeys  = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , configLedger = ()
    , configBlock  = TestBlockConfig slotLength eraParams numCoreNodes
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

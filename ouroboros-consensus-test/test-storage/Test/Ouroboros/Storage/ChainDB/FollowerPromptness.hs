{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | In the context of diffusion pipelining, it is important that tentative
-- followers promptly emit an instruction to roll forward after the tentative
-- header got set. We check this behavior by comparing the timestamps when a
-- tentative header got set and when a tentative follower emits an instruction
-- containing it.
--
-- This test is making use of io-sim to measure and check the *exact* timings of
-- various events. In particular, we can really rely on something occuring at a
-- specific point in time, compared to just a plausible range as would be
-- necessary with ordinary wall-clock time.
module Test.Ouroboros.Storage.ChainDB.FollowerPromptness (tests) where

import           Control.Monad (forever)
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (Tracer (..), contramapM, traceWith)
import           Data.Foldable (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Ouroboros.Network.Mock.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (miracle)
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as Punishment
import           Ouroboros.Consensus.Storage.ChainDB.Impl (ChainDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.Enclose
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Test.Util.ChainDB
import           Test.Util.ChainUpdates
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock
import           Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests = testGroup "FollowerPromptness"
    [ testProperty "followerPromptness" prop_followerPromptness
    ]

prop_followerPromptness :: FollowerPromptnessTestSetup -> Property
prop_followerPromptness fpts =
    label (bucket (length futureBlocks) (length allBlocks)) $
    counterexample ("Trace:\n" <> unlines (ppTrace <$> traceByTime)) $
    counterexample (condense fpts) $
    counterexample ("Instruction timings: " <> condense followerInstrTimings) $
           counterexample ("Failed to pipeline: " <> condense notPipelined)
           (null notPipelined)
      .&&. counterexample ("Not processed: " <> condense unprocessed)
           (null unprocessed)
      .&&. counterexample ("Future blocks pipelined: " <> condense futureBlocksPipelined)
           (null futureBlocksPipelined)
  where
    FollowerPromptnessOutcome{..} =
      runSimOrThrow $ runFollowerPromptnessTest fpts

    bucket x y =
      if | x == 0    -> "0%"
         | x == y    -> "100%"
         | otherwise -> "(0%, 100%)"

    allBlocks = getAllBlocks $ chainUpdates fpts

    futureBlocks = [ headerFieldHash hf
                   | hf <- allBlocks,
                     headerFieldSlot hf > staticNow fpts
                   ]

    -- Hashes of future blocks that were emitted as a follower
    -- instruction. This should be empty since the future check is static. If
    -- it weren't it might be the case that once-future blocks are pipelined
    -- when they are adopted as part of the chain.
    futureBlocksPipelined = futureBlocksFollowedUp followerInstrTimings

    -- Hashes of future blocks that were followed up on in the
    -- `followUpTimings` argument.
    futureBlocksFollowedUp :: Map Time (Set TestHash) -> [TestHash]
    futureBlocksFollowedUp followUpTimings =
      let followUps = Set.unions followUpTimings
      in filter (`Set.member` followUps) futureBlocks

    -- Hashes of tentative headers which were not immediately emitted as a
    -- follower instruction.
    notPipelined =
        tentativeHeadersWithoutFollowUp
          0
          followerInstrTimings

    -- Hashes of tentative header which were not processed (i.e. made obsolete
    -- due to adoption, or identified as a trap).
    unprocessed =
        tentativeHeadersWithoutFollowUp
          (artificialDelay fpts)
          tentativeHeaderUnsetTimings

    -- Given a collection of timestamped hashes (considered as follow-up events
    -- of a specific hash), return the timestamped tentative header hashes which
    -- are not contained therein after the given delay.
    tentativeHeadersWithoutFollowUp ::
         DiffTime
      -> Map Time (Set TestHash)
      -> [(Time, Set TestHash)]
    tentativeHeadersWithoutFollowUp delay followUpTimings =
        [ (time, notFollowedUp)
        | (time, tentativeHashes) <- Map.toAscList tentativeHeaderSetTimings
        , let followUpHashes =
                Map.findWithDefault mempty (addTime delay time) followUpTimings
              notFollowedUp = tentativeHashes Set.\\ followUpHashes
        , not $ Set.null notFollowedUp
        ]

    ppTrace (time, ev) = show time <> ": " <> ev

data FollowerPromptnessOutcome = FollowerPromptnessOutcome {
    -- | The set of tentative headers by timestamp when set. With the current
    -- implementation of ChainSel, all sets should contain exactly one element.
    tentativeHeaderSetTimings   :: Map Time (Set TestHash)
  , -- | The set of tentative headers by timestamp when unset.
    tentativeHeaderUnsetTimings :: Map Time (Set TestHash)
  , -- | The set of AddBlock instructions by a tentative follower by timestamp.
    followerInstrTimings        :: Map Time (Set TestHash)
    -- | Trace message, only used for debugging.
  , traceByTime                 :: [(Time, String)]
  }

runFollowerPromptnessTest ::
     forall m. IOLike m
  => FollowerPromptnessTestSetup
  -> m FollowerPromptnessOutcome
runFollowerPromptnessTest FollowerPromptnessTestSetup{..} = withRegistry \registry -> do
    varTentativeSetTimings   <- uncheckedNewTVarM Map.empty
    varTentativeUnsetTimings <- uncheckedNewTVarM Map.empty
    varFollowerInstrTimings  <- uncheckedNewTVarM Map.empty

    (withTime -> tracer, getTrace) <- recordingTracerTVar

    let chainDBTracer = Tracer \case
            ChainDBImpl.TraceAddBlockEvent ev -> do
              traceWith tracer $ "ChainDB: " <> show ev
              case ev of
                ChainDBImpl.PipeliningEvent pev -> case pev of
                  ChainDBImpl.SetTentativeHeader hdr FallingEdge -> do
                    addTiming varTentativeSetTimings (headerHash hdr)
                    -- Wait some non-zero duration to delay the further chain
                    -- selection logic (i.e. simulating expensive block body
                    -- validation).
                    threadDelay artificialDelay
                  ChainDBImpl.OutdatedTentativeHeader hdr ->
                    addTiming varTentativeUnsetTimings (headerHash hdr)
                  ChainDBImpl.TrapTentativeHeader hdr ->
                    addTiming varTentativeUnsetTimings (headerHash hdr)
                  _ -> pure ()
                _ -> pure ()
            _ -> pure ()
    chainDB <- openChainDB registry chainDBTracer

    -- Continually fetch instructions from a tentative follower.
    follower <-
      ChainDB.newFollower chainDB registry ChainDB.TentativeChain ChainDB.GetHash
    _ <- forkLinkedThread registry "Follower listener" $ forever $
      ChainDB.followerInstructionBlocking follower >>= \case
        Chain.AddBlock hdrHash -> addTiming varFollowerInstrTimings hdrHash
        Chain.RollBack _       -> pure ()

    -- Add all blocks to the ChainDB.
    let addBlock = ChainDB.addBlock_ chainDB Punishment.noPunishment
    for_ chainUpdates \case
      AddBlock blk      -> addBlock blk
      SwitchFork _ blks -> for_ blks addBlock

    tentativeHeaderSetTimings   <- readTVarIO varTentativeSetTimings
    tentativeHeaderUnsetTimings <- readTVarIO varTentativeUnsetTimings
    followerInstrTimings        <- readTVarIO varFollowerInstrTimings
    traceByTime                 <- getTrace
    pure FollowerPromptnessOutcome {..}
  where
    openChainDB ::
         ResourceRegistry m
      -> Tracer m (ChainDBImpl.TraceEvent TestBlock)
      -> m (ChainDB m TestBlock)
    openChainDB registry cdbTracer = do
        chainDbArgs <- do
          let mcdbTopLevelConfig = singleNodeTestConfigWithK securityParam
              mcdbChunkInfo      = mkTestChunkInfo mcdbTopLevelConfig
              mcdbInitLedger     = testInitExtLedger
              mcdbRegistry       = registry
          mcdbNodeDBs <- emptyNodeDBs
          let cdbArgs = fromMinimalChainDbArgs MinimalChainDbArgs{..}
          pure $ cdbArgs {
              cdbTracer = cdbTracer
            , cdbCheckInFuture = miracle (pure staticNow) 10
            }
        (_, (chainDB, ChainDBImpl.Internal{intAddBlockRunner})) <-
          allocate
            registry
            (\_ -> ChainDBImpl.openDBInternal chainDbArgs False)
            (ChainDB.closeDB . fst)
        _ <- forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
        pure chainDB

    withTime = contramapM \ev -> (,ev) <$> getMonotonicTime

    addTiming varTiming hash = do
        now <- getMonotonicTime
        atomically $ modifyTVar varTiming $
          Map.unionWith Set.union (Map.singleton now (Set.singleton hash))

data FollowerPromptnessTestSetup = FollowerPromptnessTestSetup {
    securityParam   :: SecurityParam
  , chainUpdates    :: [ChainUpdate]
  , artificialDelay :: DiffTime
  , staticNow       :: SlotNo
  }
  deriving stock (Show)

instance Condense FollowerPromptnessTestSetup where
  condense FollowerPromptnessTestSetup{..} =
    "Chain updates: " <> condense chainUpdates

instance Arbitrary FollowerPromptnessTestSetup where
  arbitrary = do
      securityParam   <- SecurityParam <$> chooseEnum (1, 5)
      -- Note that genChainUpdates does not guarantee that every update (i.e. a
      -- SwitchFork) will result in a new tentative header, but we don't rely on
      -- this here; rather, we only want to see a tentative candidate
      -- sufficiently often.
      chainUpdates    <- genChainUpdates TentativeChainBehavior securityParam 20
      artificialDelay <- secondsToDiffTime <$> chooseInteger (1, 10)
      staticNow <- elements (headerFieldSlot <$> getAllBlocks chainUpdates)
      pure FollowerPromptnessTestSetup {..}


  shrink FollowerPromptnessTestSetup{..} =
      [ FollowerPromptnessTestSetup {
            chainUpdates = init chainUpdates
          , staticNow = maximum (headerFieldSlot <$> getAllBlocks chainUpdates) - 1
          , ..
          }
      | not $ null chainUpdates
      ]

getAllBlocks :: [ChainUpdate] -> [HeaderFields TestBlock]
getAllBlocks = mapMaybe $ \case
  (AddBlock blk) -> Just $ getHeaderFields blk
  _              -> Nothing

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Test.Ouroboros.Storage.ChainDB.Scratch (
    API.getCurrentChain
  , Handles (..)
  , module TestBlock
  , addBlock
  , atomically
  , run
  ) where


import           Codec.Serialise (Serialise)
import           Control.Monad (void)
import           Control.Monad.Class.MonadSay
import           Data.Void (Void)
import           NoThunks.Class (AllowThunk (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as Internal
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args as Args
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB hiding (openDBInternal)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.API
                     (Follower (followerInstruction))
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Test.Ouroboros.Storage.TestBlock as TestBlock
import           Test.Util.ChainDB

import           Control.Monad.Class.MonadAsync (mapConcurrently)
import           Debug.Trace as Debug

type BlockConstraints blk =
  ( ConsensusProtocol  (BlockProtocol blk)
  , LedgerSupportsProtocol            blk
  , InspectLedger                     blk
  , Eq (ChainDepState  (BlockProtocol blk))
  , Eq (LedgerState                   blk)
  , Eq                                blk
  , Show                              blk
  , HasHeader                         blk
  , StandardHash                      blk
  , Serialise                         blk
  , Eq                       (Header  blk)
  , Show                     (Header  blk)
  , ConvertRawHash                    blk
  , HasHardForkHistory                blk
  , SerialiseDiskConstraints          blk
  )

makeChunkInfo :: ChunkInfo
makeChunkInfo = UniformChunkSize $ ChunkSize
  { chunkCanContainEBB = True
  , numRegularBlocks = 10
  }

makeTopLevelConfig :: ChunkInfo -> TopLevelConfig TestBlock
makeTopLevelConfig (UniformChunkSize chunkSize) =
  mkTestConfig (SecurityParam 2) chunkSize

makeInitExtLedgerState :: ExtLedgerState TestBlock
makeInitExtLedgerState = testInitExtLedger

makeMinimalChainDbArgs :: IOLike m => m (MinimalChainDbArgs m TestBlock)
makeMinimalChainDbArgs =
  let chunkInfo = makeChunkInfo
  in do
    nodeDBs <- emptyNodeDBs
    registry <- unsafeNewRegistry
    pure $ MinimalChainDbArgs
     { mcdbTopLevelConfig = makeTopLevelConfig chunkInfo
     , mcdbChunkInfo = chunkInfo
     , mcdbInitLedger = makeInitExtLedgerState
     , mcdbRegistry = registry
     , mcdbNodeDBs = nodeDBs
     }

alwaysInFuture :: (HasHeader (Header blk), IOLike m) => CheckInFuture m blk
alwaysInFuture = miracle (pure 5) 5

data Handles m blk = Handles
    { hChainDb       :: API.ChainDB m blk
    , hInternal      :: Internal.Internal m blk
    , hAddBlockAsync :: Async m Void
    }
  deriving NoThunks via AllowThunk (Handles m blk)

makeHandles :: (BlockConstraints blk, IOLike m)
            => MinimalChainDbArgs m blk -> m (Handles m blk)
makeHandles minimalArgs = do
  (chainDB, internal) <- Internal.openDBInternal makeArgs False
  background          <- async (Internal.intAddBlockRunner internal)
  link background
  return Handles
    { hChainDb = chainDB
    , hInternal = internal
    , hAddBlockAsync = background
    }
  where
    makeArgs =
      let args = fromMinimalChainDbArgs minimalArgs
      in args { Args.cdbCheckInFuture = alwaysInFuture }

addBlock :: IOLike m => Handles m blk -> blk -> m (Point blk)
addBlock Handles { hChainDb } = API.addBlock hChainDb InvalidBlockPunishment.noPunishment

run :: (MonadSay m, IOLike m) => m (Handles m TestBlock)
run = do
  registry <- unsafeNewRegistry
  minimalArgs <- makeMinimalChainDbArgs
  handles <- makeHandles minimalArgs

  say "\n"

  x <- async $ runNewFollower "1: " handles registry API.TentativeChain
  y <- async $ runNewFollower "2: " handles registry API.SelectedChain
  z <- async $ addBlocks handles


  say "\n"
  wait x
  wait y
  wait z
  pure handles


addBlocks :: IOLike m => Handles m TestBlock -> m ()
addBlocks handles = do

  let a = firstBlock 0 (TestBody 0 True)
  void $ addBlock handles a

  let b = mkNextBlock a 2 (TestBody 0 True)
  void $ addBlock handles b

  let c = mkNextBlock b 3 (TestBody 0 True)
  void $ addBlock handles c

  let d = mkNextBlock c 4 (TestBody 0 True)
  void $ addBlock handles d

  let e = mkNextBlock c 4 (TestBody 1 True)
  void $ addBlock handles e

  let f = mkNextBlock e 5 (TestBody 1 True)
  void $ addBlock handles f


runNewFollower
  :: (Show blk, StandardHash blk, IOLike m, MonadSay m)
  => String -> Handles m blk -> ResourceRegistry m -> API.ChainType -> m ()
runNewFollower tag handles registry chainType = do
  threadDelay 1
  selectedFollower <- API.newFollower (hChainDb handles) registry chainType GetBlock
  untilM (followerInstruction selectedFollower) (say . (\p -> tag <> show p))


untilM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
untilM get action = go
  where go = do
          x' <- get
          case x' of
            Just x -> do
              action x
              go
            Nothing ->
              pure ()



{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs(..)
  , defaultArgs
  , openDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad (unless)
import           Control.Monad.Except
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash, Point (..), StandardHash, blockPoint)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)

import           Ouroboros.Storage.ChainDB.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.VolDB as VolDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs m blk hdr = forall h1 h2 h3. ChainDbArgs {

      -- Decoders

      cdbDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , cdbDecodeBlock      :: forall s. Decoder s blk
    , cdbDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , cdbDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))

      -- Encoders

    , cdbEncodeHash       :: HeaderHash blk -> Encoding

      -- Error handling

    , cdbErrImmDb         :: ErrorHandling ImmDB.ImmutableDBError m
    , cdbErrVolDb         :: ErrorHandling (VolDB.VolatileDBError (HeaderHash blk)) m
    , cdbErrVolDbSTM      :: ThrowCantCatch (VolDB.VolatileDBError (HeaderHash blk)) (STM m)

      -- HasFS instances

    , cdbHasFSImmDb       :: HasFS m h1
    , cdbHasFSVolDb       :: HasFS m h2
    , cdbHasFSLgrDB       :: HasFS m h3

      -- Policy

    , cdbValidation       :: ImmDB.ValidationPolicy
    , cdbBlocksPerFile    :: Int
    , cdbMemPolicy        :: LgrDB.MemPolicy

      -- Integration

    , cdbNodeConfig       :: NodeConfig (BlockProtocol blk)
    , cdbEpochSize        :: EpochNo -> m EpochSize
    , cdbIsEBB            :: blk -> Maybe (HeaderHash blk)
    , cdbGetHeader        :: blk -> hdr
    , cdbGenesis          :: m (ExtLedgerState blk)
    }

-- | Default arguments for use within IO
--
-- See 'ImmDB.defaultArgs' and 'VolDB.defaultArgs' for a list of which fields
-- are not given a default and must therefore be set explicitly.
defaultArgs :: StandardHash blk => FilePath -> ChainDbArgs IO blk hdr
defaultArgs fp = toChainDbArgs ( ImmDB.defaultArgs fp
                               , VolDB.defaultArgs fp
                               , LgrDB.defaultArgs fp
                               )

-- | Internal: split chain DB args into imm DB and vol DB args
fromChainDbArgs :: ChainDbArgs m blk hdr
                -> ( ImmDB.ImmDbArgs m blk
                   , VolDB.VolDbArgs m blk hdr
                   , LgrDB.LgrDbArgs m blk
                   )
fromChainDbArgs ChainDbArgs{..} = (
      ImmDB.ImmDbArgs {
          immDecodeHash       = cdbDecodeHash
        , immDecodeBlock      = cdbDecodeBlock
        , immEncodeHash       = cdbEncodeHash
        , immErr              = cdbErrImmDb
        , immEpochSize        = cdbEpochSize
        , immValidation       = cdbValidation
        , immIsEBB            = cdbIsEBB
        , immHasFS            = cdbHasFSImmDb
        }
    , VolDB.VolDbArgs {
          volHasFS            = cdbHasFSVolDb
        , volErr              = cdbErrVolDb
        , volErrSTM           = cdbErrVolDbSTM
        , volBlocksPerFile    = cdbBlocksPerFile
        , volDecodeBlock      = cdbDecodeBlock
        , volGetHeader        = cdbGetHeader
        }
    , LgrDB.LgrDbArgs {
          lgrNodeConfig       = cdbNodeConfig
        , lgrHasFS            = cdbHasFSLgrDB
        , lgrDecodeLedger     = cdbDecodeLedger
        , lgrDecodeChainState = cdbDecodeChainState
        , lgrDecodeHash       = cdbDecodeHash
        , lgrMemPolicy        = cdbMemPolicy
        , lgrGenesis          = cdbGenesis
        }
    )

-- | Internal: construct chain DB args from imm DB and vol DB
--
-- Useful in 'defaultArgs'
toChainDbArgs :: ( ImmDB.ImmDbArgs m blk
                 , VolDB.VolDbArgs m blk hdr
                 , LgrDB.LgrDbArgs m blk
                 )
              -> ChainDbArgs m blk hdr
toChainDbArgs ( ImmDB.ImmDbArgs{..}
              , VolDB.VolDbArgs{..}
              , LgrDB.LgrDbArgs{..}
              ) = ChainDbArgs{
      -- Decoders
      cdbDecodeHash       = immDecodeHash
    , cdbDecodeBlock      = immDecodeBlock
    , cdbDecodeLedger     = lgrDecodeLedger
    , cdbDecodeChainState = lgrDecodeChainState
      -- Encoders
    , cdbEncodeHash       = immEncodeHash
      -- Error handling
    , cdbErrImmDb         = immErr
    , cdbErrVolDb         = volErr
    , cdbErrVolDbSTM      = volErrSTM
      -- HasFS instances
    , cdbHasFSImmDb       = immHasFS
    , cdbHasFSVolDb       = volHasFS
    , cdbHasFSLgrDB       = lgrHasFS
      -- Policy
    , cdbValidation       = immValidation
    , cdbBlocksPerFile    = volBlocksPerFile
    , cdbMemPolicy        = lgrMemPolicy
      -- Integration
    , cdbNodeConfig       = lgrNodeConfig
    , cdbEpochSize        = immEpochSize
    , cdbIsEBB            = immIsEBB
    , cdbGetHeader        = volGetHeader
    , cdbGenesis          = lgrGenesis
    }

{-------------------------------------------------------------------------------
  Internal environment
-------------------------------------------------------------------------------}

data ChainDbEnv m blk hdr = CDB {
      cdbImmDB          :: ImmDB m blk
    , cdbVolDB          :: VolDB m blk hdr
    , cdbLgrDB          :: LgrDB m blk
    , cdbChain          :: TVar m (AnchoredFragment hdr)
    , cdbInvalid        :: TVar m (Set (Point blk))
    , cdbHeader         :: blk -> hdr
    , cdbNextIteratorId :: TVar m IteratorId
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

openDB :: forall m blk hdr.
          ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , MonadThrow (STM m)
          , HasHeader hdr
          , HeaderHash hdr ~ HeaderHash blk
          , ProtocolLedgerView blk
          , LedgerConfigView   blk
          )
       => ChainDbArgs m blk hdr -> m (ChainDB m blk hdr)
openDB args = do
    immDB   <- ImmDB.openDB argsImmDb
    volDB   <- VolDB.openDB argsVolDb
    lgrDB   <- LgrDB.openDB argsLgrDb
                            immDB
                            (getAnyKnownBlock immDB volDB)
    -- TODO initialise from the ImmutableDB & the VolatileDB
    chain          <- atomically $ newTVar $ Empty genesisPoint
    invalid        <- atomically $ newTVar Set.empty
    nextIteratorId <- atomically $ newTVar $ IteratorId 0
    let env = CDB { cdbImmDB          = immDB
                  , cdbVolDB          = volDB
                  , cdbLgrDB          = lgrDB
                  , cdbChain          = chain
                  , cdbInvalid        = invalid
                  , cdbHeader         = cdbGetHeader args
                  , cdbNextIteratorId = nextIteratorId
                  }
    return ChainDB {
        addBlock           = undefined
      , getCurrentChain    = cdbGetCurrentChain    env
      , getCurrentLedger   = undefined
      , getTipBlock        = cdbGetTipBlock        env
      , getTipHeader       = cdbGetTipHeader       env
      , getTipPoint        = cdbGetTipPoint        env
      , getBlock           = cdbGetBlock           env
      , getIsFetched       = cdbGetIsFetched       env
      , streamBlocks       = cdbStreamBlocks       env
      , readBlocks         = undefined
      , readHeaders        = undefined
      , knownInvalidBlocks = cdbKnownInvalidBlocks env
      , closeDB            = cdbCloseDB            env
      }
  where
    (argsImmDb, argsVolDb, argsLgrDb) = fromChainDbArgs args

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

cdbCloseDB :: (MonadCatch m, HasHeader blk) => ChainDbEnv m blk hdr -> m ()
cdbCloseDB CDB{..} = do
    -- TODO
    ImmDB.closeDB cdbImmDB
    VolDB.closeDB cdbVolDB

cdbGetIsFetched :: forall m blk hdr. MonadSTM m
                => ChainDbEnv m blk hdr
                -> STM m (Point blk -> Bool)
cdbGetIsFetched CDB{..} = basedOnHash <$> VolDB.getIsMember cdbVolDB
  where
    -- The volatile DB indexes by hash only, not by points. However, it should
    -- not be possible to have two points with the same hash but different
    -- slot numbers.
    basedOnHash :: (HeaderHash blk -> Bool) -> Point blk -> Bool
    basedOnHash f p =
        case Block.pointHash p of
          BlockHash hash -> f hash
          GenesisHash    -> False

cdbGetCurrentChain :: MonadSTM m
                   => ChainDbEnv m blk hdr
                   -> STM m (AnchoredFragment hdr)
cdbGetCurrentChain CDB{..} = readTVar cdbChain

cdbGetTipPoint :: ( MonadSTM m
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> STM m (Point blk)
cdbGetTipPoint = fmap (Block.castPoint . Fragment.headPoint)
               . cdbGetCurrentChain

cdbGetTipBlock :: ( MonadCatch m
                  , MonadSTM m
                  , HasHeader blk
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> m (Maybe blk)
cdbGetTipBlock cdb@CDB{..} = do
    tipPoint <- atomically $ cdbGetTipPoint cdb
    if tipPoint == genesisPoint
      then return Nothing
      else Just <$> getAnyKnownBlock cdbImmDB cdbVolDB tipPoint

cdbGetTipHeader :: ( MonadSTM m
                   , HasHeader hdr
                   )
                => ChainDbEnv m blk hdr
                -> m (Maybe hdr)
cdbGetTipHeader CDB{..} =
    eitherToMaybe . Fragment.head <$> atomically (readTVar cdbChain)
  where
    eitherToMaybe = either (const Nothing) Just

cdbKnownInvalidBlocks :: MonadSTM m
                      => ChainDbEnv m blk hdr
                      -> STM m (Set (Point blk))
cdbKnownInvalidBlocks CDB{..} = readTVar cdbInvalid

cdbGetBlock :: ( MonadCatch m
               , HasHeader blk
               )
            => ChainDbEnv m blk hdr
            -> Point blk -> m (Maybe blk)
cdbGetBlock CDB{..} = getAnyBlock cdbImmDB cdbVolDB

cdbStreamBlocks :: ( MonadCatch m
                   , MonadSTM   m
                   , MonadThrow (STM m)
                   , HasHeader blk
                   )
                => ChainDbEnv m blk hdr
                -> StreamFrom blk -> StreamTo blk
                -> m (Either (UnknownRange blk) (Iterator m blk))
cdbStreamBlocks CDB{..} = implStreamBlocks cdbImmDB cdbVolDB makeNewIteratorId
  where
    makeNewIteratorId = atomically $ do
      newIteratorId <- readTVar cdbNextIteratorId
      modifyTVar' cdbNextIteratorId succ
      return newIteratorId

{-------------------------------------------------------------------------------
  Lower level functionality

  These are functions that don't require all parts of the ChainDB to have
  been initialized
-------------------------------------------------------------------------------}

-- | Stream blocks
--
-- TODO handle corruption and recovery
-- TODO test that EBBs are handled correctly
-- TODO the ImmDB.Iterator isn't guaranteed to be closed in case of an
-- exception, we would need something like ResourceT for that.
--
-- = Start & end point
--
-- The start point can either be in the ImmutableDB (on our chain) or in the
-- VolatileDB (on our chain or on a recent fork). We first check whether it is
-- in the VolatileDB, if not, we check if it is in the ImmutableDB (see
-- \"Garbage collection\" for why this order is important). Similarly for the
-- end point.
--
-- If a bound can't be found in the ChainDB, an 'UnknownRange' error is
-- returned.
--
-- When the bounds are nonsensical, e.g.,
-- > StreamFromExclusive (Point { pointSlot = SlotNo 3 , .. }
-- > StreamToInclusive   (Point { pointSlot = SlotNo 3 , .. }
-- An 'InvalidIteratorRange' exception is thrown.
--
-- = Paths of blocks
--
-- To stream blocks from the ImmutableDB we can simply use the iterators
-- offered by the ImmutableDB.
--
-- To stream blocks from the VolatileDB we have to construct a path of block
-- hashes backwards through the VolatileDB, starting from the end point using
-- 'getPredecessor' until we get to the start point, genesis, or we get to a
-- block that is not in the VolatileDB. Then, for each hash in the path, we
-- can ask the VolatileDB for the corresponding block.
--
-- If the path through the VolatileDB is incomplete, we will first have to
-- stream blocks from the ImmutableDB and then switch to the path through the
-- VolatileDB. We only allow the tip of the ImmutableDB to be the switchover
-- point between the two DBs. In other words, the incomplete path through the
-- VolatileDB must fit onto the tip of the ImmutableDB. This must be true at
-- the time of initialising the iterator, but does not have to hold during the
-- whole lifetime of the iterator. If it doesn't fit on it, it means the path
-- forked off more than @k@ blocks in the past and blocks belonging to it are
-- more likely to go missing because of garbage-collection (see the next
-- paragraph). In that case, we return 'ForkTooOld'.
--
-- = Garbage collection
--
-- We have to be careful about the following: as our chain grows, blocks from
-- our chain will be copied to the ImmutableDB in the background. After a
-- while, old blocks will be garbage-collected from the VolatileDB. Blocks
-- that were part of the current chain will be in the ImmutableDB, but blocks
-- that only lived on forks will be gone forever.
--
-- This means that blocks that were part of the VolatileDB when the iterator
-- was initialised might no longer be part of the VolatileDB when we come to
-- the point that the iterator will try to read them. When this is noticed, we
-- will try to open an iterator from the ImmutableDB to obtain the blocks that
-- have moved over. However, this will only work if they were and are part of
-- the current chain, otherwise they will have been deleted from the
-- VolatileDB without being copied to the ImmutableDB.
--
-- This iterator is opened with an open upper bound and will be used to stream
-- blocks until the path has been fully streamed, the iterator is exhausted,
-- or a block doesn't match the expected hash. In the latter two cases, we
-- switch back to the VolatileDB. If the block is missing from the VolatileDB,
-- we will switch back to streaming from the ImmutableDB. If that fails, we
-- switch back to the VolatileDB. To avoid eternally switching between the two
-- DBs, we only switch back to the VolatileDB if the stream from the
-- ImmutableDB has made progress, i.e. streamed at least one block with the
-- expected hash. If no block was streamed from the ImmutableDB, not even the
-- first one, we know for sure that that block isn't part of the VolatileDB
-- (the reason we switch to the ImmutableDB) and isn't part of the ImmutableDB
-- (no block was streamed). In that case, we return 'IteratorBlockGCed' and
-- stop the stream.
--
-- Note that the open upper bound doesn't allow us to include blocks in the
-- stream that are copied to the ImmutableDB after opening this iterator, as
-- the bound of the iterator is fixed upon initialisation. These newly added
-- blocks will be included in the stream because we will repeatedly open new
-- ImmutableDB iterators (as long as we make progress).
--
-- = Bounds checking
--
-- The ImmutableDB is slot-based instead of point-based, which means that
-- before we know whether a block in the ImmutableDB matches a given point, we
-- must first read that block to obtain its hash, after which we can then
-- verify whether it matches the hash of the point. This is important for the
-- start and end bounds (both points) of a stream: we must first read the
-- blocks corresponding to the bounds to be sure the range is valid. Note that
-- these reads happen before the first call to 'iteratorNext' (which will
-- trigger a second read of the first block).
--
-- Note that when streaming to an /exclusive/ bound, the block corresponding
-- to that bound ('Point') must exist in the ChainDB.
--
-- = Costs
--
-- Opening an iterator has some costs:
--
-- * When blocks have to be streamed from the ImmutableDB: as discussed in
--   \"Bounds checking\", the blocks corresponding to the bounds have to be
--   read from disk.
--
-- * When blocks have to be streamed both from the ImmutableDB and the
--   VolatileDB, the blocks corresponding to the two bound will have to be
--   read upfront, as described in the previous bullet point. Since the tip of
--   the ImmutableDB must be the switchover point between the two, it will be
--   the upper bound.
--
-- In summary:
--
-- * Only streaming from the VolatileDB: 0 blocks read upfront.
-- * Only streaming from the ImmutableDB: 2 blocks read upfront.
-- * Streaming from both the ImmutableDB and the VolatileDB: 2 blocks read
--   upfront.
--
-- Additionally, when we notice during streaming that a block is no longer in
-- the VolatileDB, we try to see whether it can be streamed from the
-- ImmutableDB instead. Opening such an iterator (with an exclusive bound) has
-- the cost of reading (but not parsing) one extra block from disk, in
-- addition to the block(s) we are actually interested in. This can happen
-- multiple times. See #548.
implStreamBlocks :: forall m blk hdr.
                    ( MonadCatch m
                    , MonadSTM   m
                    , MonadThrow (STM m)
                    , HasHeader blk
                    )
                 => ImmDB m blk
                 -> VolDB m blk hdr
                 -> m IteratorId   -- ^ How to make a new 'IteratorId'
                 -> StreamFrom blk -> StreamTo blk
                 -> m (Either (UnknownRange blk) (Iterator m blk))
implStreamBlocks immDB volDB makeNewIteratorId from to = do
    unless (validBounds from to) $
      throwM $ InvalidIteratorRange from to
    runExceptT start
  where
    start :: ExceptT (UnknownRange blk) m (Iterator m blk)
    start = do
      path <- lift $ atomically $ VolDB.computePathSTM volDB from to
      case path of
        VolDB.NotInVolDB        _hash           -> streamFromImmDB
        VolDB.PartiallyInVolDB  predHash hashes -> streamFromBoth predHash hashes
        VolDB.CompletelyInVolDB hashes          -> case NE.nonEmpty hashes of
          Just hashes' -> lift $ streamFromVolDB hashes'
          Nothing      -> lift $ emptyIterator

    streamFromVolDB :: NonEmpty (HeaderHash blk) -> m (Iterator m blk)
    streamFromVolDB = createIterator . InVolDB from

    streamFromImmDB :: ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromImmDB = streamFromImmDBHelper True

    streamFromImmDBHelper :: Bool -- ^ Check the hash of the upper bound
                          -> ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromImmDBHelper checkUpperBound = do
        -- First check whether the block in the ImmDB at the end bound has the
        -- correct hash.
        when checkUpperBound $
          lift (ImmDB.getBlockWithPoint immDB endPoint) >>= \case
            Just _  -> return ()
            Nothing -> throwError $ MissingBlock endPoint
        -- 'ImmDB.streamBlocksFrom' will check the hash of the block at the
        -- start bound.
        immIt <- ExceptT $ ImmDB.streamBlocksFrom immDB from
        lift $ createIterator $ InImmDB from immIt (StreamTo to)
      where
        endPoint = case to of
          StreamToInclusive pt -> pt
          StreamToExclusive pt -> pt

    -- | If we have to stream from both the ImmutableDB and the VolatileDB, we
    -- only allow the (current) tip of the ImmutableDB to be the switchover
    -- point between the two DBs. If not, this would mean we have to stream a
    -- fork that forks off more than @k@ blocks in the past, in which case the
    -- risk of blocks going missing due to GC increases. So we refuse such a
    -- stream.
    streamFromBoth :: HeaderHash blk
                   -> [HeaderHash blk]
                   -> ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromBoth predHash hashes = lift (ImmDB.getBlockAtTip immDB) >>= \case
        -- The ImmutableDB is empty
        Nothing -> throwError $ ForkTooOld from
        -- The incomplete path fits onto the tip of the ImmutableDB.
        Just blk | blockHash blk == predHash -> case NE.nonEmpty hashes of
          Just hashes' -> stream (blockPoint blk) hashes'
          -- The path is actually empty, but the exclusive upper bound was in
          -- the VolatileDB. Just stream from the ImmutableDB without checking
          -- the upper bound (which might not be in the ImmutableDB)
          Nothing      -> streamFromImmDBHelper False
        -- The incomplete path doesn't fit onto the tip of the ImmutableDB.
        -- Note that since we have constructed the incomplete path through the
        -- VolatileDB, blocks might have moved from the VolatileDB to the
        -- ImmutableDB so that the tip of the ImmutableDB has changed. Either
        -- the path used to fit onto the tip but the tip has changed, or the
        -- path simply never fitted onto the tip.
        Just blk -> case dropWhile (/= blockHash blk) hashes of
          -- The current tip is not in the path, this means that the path
          -- never fitted onto the tip of the ImmutableDB. We refuse this
          -- stream.
          []                    -> throwError $ ForkTooOld from
          -- The current tip is in the path, with some hashes after it, this
          -- means that some blocks in our path have moved from the VolatileDB
          -- to the ImmutableDB. We can shift the switchover point to the
          -- current tip.
          _tipHash:hash:hashes' -> stream (blockPoint blk) (hash NE.:| hashes')
          -- The current tip is the end of the path, this means we can
          -- actually stream everything from just the ImmutableDB. No need to
          -- check the hash at the upper bound again.
          [_tipHash]            -> streamFromImmDBHelper False

      where
        stream pt hashes' = do
          let immEnd = SwitchToVolDBFrom (StreamToInclusive pt) hashes'
          immIt <- ExceptT $ ImmDB.streamBlocksFrom immDB from
          lift $ createIterator $ InImmDB from immIt immEnd

    emptyIterator :: m (Iterator m blk)
    emptyIterator = createIterator Closed

    createIterator :: IteratorState m blk -> m (Iterator m blk)
    createIterator itState = do
      iteratorId <- makeNewIteratorId
      varItState <- newTVarM itState
      return Iterator {
          iteratorNext  = implIteratorNext  varItState immDB volDB
        , iteratorClose = implIteratorClose varItState
        , iteratorId    = iteratorId
        }

    implIteratorClose :: TVar m (IteratorState m blk) -> m ()
    implIteratorClose varItState = atomically (readTVar varItState) >>= \case
      Closed                 -> return ()
      InImmDB _ immIt _      -> do
        ImmDB.iteratorClose immIt
        atomically $ writeTVar varItState Closed
      InImmDBRetry _ immIt _ -> do
        ImmDB.iteratorClose immIt
        atomically $ writeTVar varItState Closed
      InVolDB {}             ->
        atomically $ writeTVar varItState Closed

-- | Possible states of an iterator.
--
-- When streaming solely from the ImmutableDB ('InImmDB' where 'InImmDBEnd' is
-- /not/ 'SwitchToVolDBFrom'): we will remain in this state until we are done,
-- and end up in 'Closed'.
--
-- When streaming solely from the VolatileDB ('InVolDB'): when
-- 'VolDB.getBlock' returns 'Nothing', i.e. the block is missing from the
-- VolatileDB and might have moved to the ImmutableDB: we switch to the
-- 'InImmDBRetry' state, unless we just come from that state, in that case,
-- return 'IteratorBlockGCed' and close the iterator.
--
-- When streaming from the ImmutableDB with a planned switchover to the
-- VolatileDB ('InImmDB' where 'InImmDBEnd' is 'SwitchToVolDBFrom') and we
-- have reached the end of the ImmutableDB iterator (exhausted or upper bound
-- is reached): we switch to the 'InVolDB' state.
--
-- In the 'InImmDBRetry' state, we distinguish two cases:
--
-- 1. We have just switched to it because a block was missing from the
--    VolatileDB. We have an iterator that could stream this block from the
--    ImmutableDB (if it was indeed moved to the ImmutableDB). If the streamed
--    block matches the expected hash, we continue. If not, or if the iterator
--    is immediately exhausted, then the block is missing and we return
--    'IteratorBlockGCed' and close the iterator.
--
-- 2. We have successfully streamed one or more blocks from the ImmutableDB
--    that were previously part of the VolatileDB. When we now encounter a
--    block of which the hash does not match the expected hash or when the
--    iterator is exhausted, we switch back to the 'InVolDB' state.
--
data IteratorState m blk
  = InImmDB
      (StreamFrom blk)
      (ImmDB.Iterator (HeaderHash blk) m blk)
      (InImmDBEnd blk)
    -- ^ Streaming from the ImmutableDB.
    --
    -- Invariant: an ImmutableDB iterator opened using the 'StreamFrom'
    -- parameter as lower bound will yield the same next block as the iterator
    -- stored as parameter. There is one difference, which is exactly the
    -- reason for keeping track of this 'StreamFrom': if the latter iterator
    -- (the parameter) is exhausted and blocks have been appended to the end
    -- of the ImmutableDB since it was originally opened, the new iterator can
    -- include them in its stream.
    --
    -- Invariant: the iterator is not exhausted.
  | InVolDB
      (StreamFrom blk)
      (NonEmpty (HeaderHash blk))
    -- ^ Streaming from the VolatileDB.
    --
    -- The (non-empty) list of hashes is the path to follow through the
    -- VolatileDB.
    --
    -- Invariant: if the blocks corresponding to the hashes have been moved to
    -- the ImmutableDB, it should be possible to stream these blocks from the
    -- ImmutableDB by starting an iterator using the 'StreamFrom' parameter.
    -- Note that the hashes of these blocks still have to be checked against
    -- the hashes in the path, because the blocks might not have been part of
    -- the current chain, in which case they will not be in the ImmutableDB.
  | InImmDBRetry
      (StreamFrom blk)
      (ImmDB.Iterator (HeaderHash blk) m blk)
      (NonEmpty (HeaderHash blk))
    -- ^ When streaming blocks (a list of hashes) from the VolatileDB, we
    -- noticed a block was missing from the VolatileDB. It may have moved to
    -- the ImmutableDB since we initialised the iterator (and built the path),
    -- so we'll try if we can stream it from the ImmutableDB.
    --
    -- Invariants: invariants of 'InImmDB' + invariant of 'InVolDB'.

  | Closed

-- | Determines if/when to stop streaming from the ImmutableDB and what to do
-- afterwards.
data InImmDBEnd blk
  = StreamAll
    -- ^ Don't stop streaming until the iterator is exhausted.
  | StreamTo          (StreamTo blk)
    -- ^ Stream to the upper bound.
  | SwitchToVolDBFrom (StreamTo blk)  (NonEmpty (HeaderHash blk))
    -- ^ Stream to the upper bound. Afterwards, start streaming the path (the
    -- second parameter) from the VolatileDB.

implIteratorNext :: forall m blk hdr.
                    ( MonadCatch m
                    , MonadSTM   m
                    , HasHeader blk
                    )
                 => TVar m (IteratorState m blk)
                 -> ImmDB m blk
                 -> VolDB m blk hdr
                 -> m (IteratorResult blk)
implIteratorNext varItState immDB volDB =
    atomically (readTVar varItState) >>= \case
      Closed ->
        return IteratorExhausted
      InImmDB continueAfter immIt immEnd ->
        nextInImmDB continueAfter immIt immEnd
      InImmDBRetry continueAfter immIt immHashes ->
        nextInImmDBRetry (Just continueAfter) immIt immHashes
      InVolDB continueAfter volHashes ->
        nextInVolDB continueAfter volHashes
  where
    -- | Read the next block while in the 'InVolDB' state.
    nextInVolDB :: StreamFrom blk
                   -- ^ In case the block corresponding to the first hash in
                   -- the path is missing from the VolatileDB, we can use this
                   -- lower bound to try to stream it from the ImmutableDB (if
                   -- the block indeed has been moved there).
                -> NonEmpty (HeaderHash blk)
                -> m (IteratorResult blk)
    nextInVolDB continueFrom (hash NE.:| hashes) =
      VolDB.getBlock volDB hash >>= \case
        -- Block is missing
        Nothing -> do
            -- Try if we can stream a block from the ImmutableDB that was
            -- previously in the VolatileDB. This will only work if the block
            -- was part of the current chain, otherwise it will not have been
            -- copied to the ImmutableDB.
            --
            -- This call cannot throw a 'ReadFutureSlotError' or a
            -- 'ReadFutureEBBError' because if the block is missing, it /must/
            -- have been garbage-collected, which means that its slot was
            -- older than the slot of the tip of the ImmutableDB.
            immIt <- ImmDB.streamBlocksFromUnchecked immDB continueFrom
            nextInImmDBRetry Nothing immIt (hash NE.:| hashes)

        -- Block is there
        Just blk | Just hashes' <- NE.nonEmpty hashes -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes')
          return $ IteratorResult blk
        -- No more hashes, so we can stop
        Just blk -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult blk

    -- | Read the next block while in the 'InImmDB' state.
    nextInImmDB :: StreamFrom blk
                -> ImmDB.Iterator (HeaderHash blk) m blk
                -> InImmDBEnd blk
                -> m (IteratorResult blk)
    nextInImmDB continueFrom immIt immEnd = do
      immRes <- selectResult immEnd <$> ImmDB.iteratorNext    immIt
                                    <*> ImmDB.iteratorHasNext immIt
      case immRes of
        NotDone blk -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InImmDB continueFrom' immIt immEnd)
          return $ IteratorResult blk
        -- True indicates that this is the last element in the stream
        DoneAfter blk | SwitchToVolDBFrom _ hashes <- immEnd -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes)
          return $ IteratorResult blk
        DoneAfter blk -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult blk
        Done | SwitchToVolDBFrom _ hashes <- immEnd ->
          nextInVolDB continueFrom hashes
        Done -> do
          -- No need to switch to the VolatileDB, so we can stop
          atomically $ writeTVar varItState Closed
          return IteratorExhausted

    -- | Read the next block while in the 'InImmDBRetry' state.
    --
    -- We try to stream blocks that we suspect are now in the ImmutableDB
    -- because they are no longer in the VolatileDB. We don't know this for
    -- sure, so we must check whether they match the expected hashes.
    nextInImmDBRetry :: Maybe (StreamFrom blk)
                        -- ^ 'Nothing' iff the iterator was just opened and
                        -- nothing has been streamed from it yet. This is used
                        -- to avoid switching right back to the VolatileDB if
                        -- we came from there.
                     -> ImmDB.Iterator (HeaderHash blk) m blk
                     -> NonEmpty (HeaderHash blk)
                     -> m (IteratorResult blk)
    nextInImmDBRetry mbContinueFrom immIt (hash NE.:| hashes) =
      selectResult StreamAll <$> ImmDB.iteratorNext    immIt
                             <*> ImmDB.iteratorHasNext immIt >>= \case
        NotDone blk | blockHash blk == hash -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState $ case NE.nonEmpty hashes of
            Nothing      -> Closed
            Just hashes' -> InImmDBRetry continueFrom' immIt hashes'
          return $ IteratorResult blk

        DoneAfter blk | blockHash blk == hash -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState $ case NE.nonEmpty hashes of
            Nothing      -> Closed
            Just hashes' -> InVolDB continueFrom' hashes'
          return $ IteratorResult blk

        -- Hash mismatch or 'Done'
        _ -> case mbContinueFrom of
          -- We just switched to this state and the iterator was just opened.
          -- The block must be GC'ed, since we opened the iterator because it
          -- was missing from the VolatileDB and now it is not in the
          -- ImmutableDB either.
          Nothing -> do
            atomically $ writeTVar varItState Closed
            return $ IteratorBlockGCed hash

          -- We have already streamed something from the iterator. We can try
          -- looking in the VolatileDB again. If we hadn't streamed something
          -- yet, switching to the VolatileDB would be pointless as we just
          -- came from there.
          Just continueFrom -> nextInVolDB continueFrom (hash NE.:| hashes)

    -- | Return a 'Done' based on the 'InImmDBEnd'. See the documentation of
    -- 'Done'. The 'Bool' argument should be the result of
    -- 'ImmDB.iteratorHasNext' and indicates whether the iterator is able to
    -- stream more blocks ('True') or whether it is exhausted ('False') after
    -- returning the last result.
    --
    -- We're doing this because we're streaming from the ImmutableDB with an
    -- open upper bound, because the ImmutableDB doesn't support streaming to
    -- an exclusive upper bound.
    selectResult :: InImmDBEnd blk
                 -> ImmDB.IteratorResult (HeaderHash blk) blk
                 -> Bool  -- ^ has the iterator a next element
                 -> Done blk
    selectResult immEnd itRes hasNext = case itRes of
        ImmDB.IteratorResult _ blk -> select blk
        ImmDB.IteratorEBB  _ _ blk -> select blk
        ImmDB.IteratorExhausted    -> Done
      where
        select blk = case immEnd of
          StreamAll
            | hasNext             -> NotDone   blk
            | otherwise           -> DoneAfter blk
          StreamTo          to'   -> checkUpperBound blk to'
          SwitchToVolDBFrom to' _ -> checkUpperBound blk to'
        checkUpperBound blk = \case
          StreamToExclusive pt
            | pt == blockPoint blk -> Done
            | hasNext              -> NotDone   blk
            | otherwise            -> DoneAfter blk
          StreamToInclusive pt
            | pt == blockPoint blk -> DoneAfter blk
            | hasNext              -> NotDone   blk
            | otherwise            -> DoneAfter blk

-- | Auxiliary data type used for 'selectResult' in 'implIteratorNext'.
data Done blk
  = Done
    -- ^ We're done with the iterator, either it is exhausted or we reached
    -- its upper bound.
  | DoneAfter blk
    -- ^ We're done with the iterator, but have to return this last block. We
    -- must have reached its upper /inclusive/ bound.
  | NotDone     blk
    -- ^ We're not done yet with the iterator and have to return this block.
    -- We know the iterator is not exhausted, but this does not mean that the
    -- next block returned by it will be included in the stream as it might
    -- correspond to the exclusive upper bound.

{-------------------------------------------------------------------------------
  Unifying interface over the immutable DB and volatile DB, but independent
  of the ledger DB. These functions therefore do not require the entire
  Chain DB to have been initialized.
-------------------------------------------------------------------------------}

-- | Wrapper around 'getAnyBlock' for blocks we know should exist
--
-- If the block does not exist, this indicates disk failure.
getAnyKnownBlock :: forall m blk hdr. (MonadCatch m, HasHeader blk)
                 => ImmDB m blk
                 -> VolDB m blk hdr
                 -> Point blk
                 -> m blk
getAnyKnownBlock immDB volDB p = do
    mBlock <- mustExist p <$> getAnyBlock immDB volDB p
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get a block from either the immutable DB or volatile DB
--
-- Returns 'Nothing' if the block is unknown.
-- Throws 'NoGenesisBlockException' if the 'Point' refers to the genesis block.
getAnyBlock :: forall m blk hdr. (MonadCatch m, HasHeader blk)
            => ImmDB m blk
            -> VolDB m blk hdr
            -> Point blk
            -> m (Maybe blk)
getAnyBlock immDB volDB p = case pointHash p of
    GenesisHash    -> throwM $ NoGenesisBlock @blk
    BlockHash hash -> do
      -- Note: to determine whether a block is in the ImmutableDB, we can look
      -- at the slot of its tip, which we'll call @immTipSlot@. If the slot of
      -- the requested point > @immTipSlot@, then the block will not be in the
      -- ImmutableDB but in the VolatileDB. However, there is a race condition
      -- here: if between the time we got @immTipSlot@ and the time we look up
      -- the block in the VolatileDB the block was moved from the VolatileDB
      -- to the ImmutableDB, and it was deleted from the VolatileDB, we won't
      -- find the block, even though it is in the ChainDB.
      --
      -- Therefore, we first query the VolatileDB and if the block is not in
      -- it, then we can get @immTipSlot@ and compare it to the slot of the
      -- requested point. If the slot <= @immTipSlot@ it /must/ be in the
      -- ImmutableDB (no race condition here).
      mbVolBlock <- VolDB.getBlock volDB hash
      case mbVolBlock of
        Just block -> return $ Just block
        Nothing    -> do
          -- ImmDB will throw an exception if we ask for a block past the tip
          immTipSlot <- ImmDB.getSlotNoAtTip immDB
          if pointSlot p > immTipSlot
            -- It's not supposed to be in the ImmutableDB and the VolatileDB
            -- didn't contain it, so return 'Nothing'.
            then return Nothing
            else ImmDB.getBlockWithPoint immDB p

mustExist :: Point blk
          -> Maybe blk
          -> Either (ChainDbFailure blk) blk
mustExist p Nothing  = Left  $ ChainDbMissingBlock p
mustExist _ (Just b) = Right $ b

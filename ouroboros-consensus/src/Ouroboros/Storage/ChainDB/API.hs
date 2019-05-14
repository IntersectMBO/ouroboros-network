{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Storage.ChainDB.API (
    -- * Main ChainDB API
    ChainDB(..)
    -- * Support for tests
  , toChain
  , fromChain
    -- * Iterator API
  , StreamFrom(..)
  , StreamTo(..)
  , Iterator(..)
  , IteratorId(..)
  , IteratorResult(..)
  , UnknownRange(..)
    -- * Readers
  , ChainUpdate(..)
  , fromNetworkChainUpdate
  , toNetworkChainUpdate
  , Reader(..)
    -- * Recovery
  , ChainDbFailure(..)
    -- * Exceptions
  , ChainDbError(..)
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Exception (Exception)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function (on)
import           Data.Set (Set)
import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, SlotNo,
                     StandardHash)
import           Ouroboros.Network.Chain (Chain (..), Point (..), genesisPoint)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ReaderId)

import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Storage.Common
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.VolatileDB as VolDB

-- | The chain database
--
-- The chain database provides a unified interface on top of
--
-- * the immutable DB, storing the part of the chain that can't roll back
-- * the volatile DB, storing the blocks near the tip of the chain,
--   possibly in multiple competing forks
-- * the ledger DB, storing snapshots of the ledger state for blocks in the
--   immutable DB (and in-memory snapshots for the rest)
--
-- In addition to providing a unifying interface on top of these disparate
-- components, the main responsibilities that the Chain DB itself has are
--
-- * Chain selection (on initialization and whenever a block is added)
-- * Trigger full recovery whenever we detect disk failure in any component
-- * Provide iterators across fixed fragments of the current chain
-- * Provide readers that track the status of the current chain
--
-- The Chain DB instantiates all the various type parameters of these databases
-- to conform to the unified interface we provide here. One minor nuisance here
-- is that we require @HeaderHash blk ~ HeaderHash hdr@ and hence the following
-- types are all equal
--
-- > Point        blk  ;  Point        hdr
-- > ChainHash    blk  ;  ChainHash    hdr
-- > ChainUpdate  blk  ;  ChainUpdate  hdr
--
-- In the API we offer here we will consistently use the @blk@ versions of
-- these types and cast only where needed.
data ChainDB m blk hdr =
    ( HasHeader blk
    , HasHeader hdr
    , HeaderHash blk ~ HeaderHash hdr
    ) => ChainDB {
      -- | Add a block to the heap of blocks
      --
      -- We do /not/ assume that the block is valid (under the legder rules);
      -- it is the responsibility of the Chain DB itself to only select chains
      -- that are valid.
      --
      -- Conversely, the caller cannot assume that the new block will be added
      -- to the current chain; even if the block is valid, it will not become
      -- part of the chain if there are other chains available that are
      -- preferred by the consensus algorithm (typically, longer chains).
      addBlock           :: blk -> m ()

      -- | Get the current chain fragment
      --
      -- Suppose the current chain is
      --
      -- > a -> b -> c -> d -> e -> ff
      --
      -- and suppose @k = 2@; this means that the most distant fork we can
      -- switch to is something like
      --
      -- > a -> b -> c -> d -> e' -> f'
      --
      -- The fragment we return will be @[e, f]@, anchored at @d@. In other
      -- words, the length of the fragment will under normal circumstances
      -- be exactly @k@ blocks long. It may be shorter if
      --
      -- * We are near genesis
      --   The anchor will be the genesis point
      --   (which does not correspond to an actual block)
      --
      -- * The volatile DB suffered some data loss
      --   Typically (but not necessarily) the immutable DB will not be empty
      --   and the anchor will be pointing to the tip of the immutable DB.
      --
      -- POSTCONDITION: The Chain DB will be able to switch to any fork starting
      -- from the anchor of the returned fragment or any subsequent block
      -- (provided the new fork is at least of the same length as the old).
      --
      -- NOTE: A direct consequence of this guarantee is that the anchor of the
      -- fragment will move as the chain grows.
    , getCurrentChain    :: STM m (AnchoredFragment hdr)

      -- | Get current ledger
    , getCurrentLedger   :: STM m (ExtLedgerState blk)

      -- | Get block at the tip of the chain, if one exists
      --
      -- Returns 'Nothing' if the database is empty.
    , getTipBlock        :: m (Maybe blk)

      -- | Get header at the tip of the chain
      --
      -- NOTE: Calling 'getTipHeader' is cheaper than 'getTipBlock' and then
      -- extracting the header: most of the time the header at the tip is
      -- actually in memory, whereas the block never is.
      --
      -- Returns 'Nothing' if the database is empty.
    , getTipHeader       :: m (Maybe hdr)

      -- | Get point of the tip of the chain
      --
      -- Will return 'genesisPoint' if the database is empty; if the
      -- current chain fragment is empty due to data loss in the volatile DB,
      -- 'getTipPoint' will return the tip of the immutable DB.
    , getTipPoint        :: STM m (Point blk)

      -- | Get block at the specified point (if it exists)
    , getBlock           :: Point blk -> m (Maybe blk)

      -- | Return membership check function for recent blocks
      --
      -- This check is only reliable for blocks up to @k@ away from the tip.
      -- For blocks older than that the results should be regarded as
      -- non-deterministic.
    , getIsFetched       :: STM m (Point blk -> Bool)

      -- | Stream blocks
      --
      -- Streaming is not restricted to the current fork, but there must be
      -- an unbroken path from the starting point to the end point /at the time
      -- of initialization/ of the iterator. Once the iterator has been
      -- initialized, it will not be affected by subsequent calls to 'addBlock'.
      --
      -- The iterator /does/ have a limited lifetime, however. The chain DB
      -- internally partitions the chain into an " immutable " part and a
      -- " volatile " part, moving blocks from the volatile DB to the immutable
      -- DB when they become more than @k@ deep into the chain. When a block
      -- with slot number @n@ is added to the immutble DB, a time delay @t@
      -- kicks in; after that time delay expires, all blocks older than @n@ may
      -- be removed from the volatile DB, /including any blocks that happen to
      -- live on other forks/ (since those forks must now, by definition, be too
      -- distant). This time delay @t@ also provides a worst-case bound for the
      -- lifetime of the iterator: if the iterator traverses a chain that
      -- forks off from our current chain at the tip of the immutable DB,
      -- then the first block on that fork will become unavailable as soon as
      -- another block is pushed to the current chain and the subsequent
      -- time delay expires.
      --
      -- TODO: How should iterators respond to corruption? (Readers can roll
      -- back, but iterators cannot.)
    , streamBlocks       :: StreamFrom blk -> StreamTo blk
                         -> m (Either (UnknownRange blk) (Iterator m blk))

      -- | Chain reader
      --
      -- A chain reader is an iterator that tracks the state of the /current/
      -- chain: calling @next@ on the iterator will either give you the next
      -- block, or (if we have switched to a fork) the instruction to rollback.
      --
      -- The tracking iterator starts at genesis (see also 'trackForward').
    , readBlocks         :: m (Reader m blk blk)

      -- | Like 'readBlocks', but return headers
    , readHeaders        :: m (Reader m blk hdr)

      -- | Known to be invalid blocks
    , knownInvalidBlocks :: STM m (Set (Point blk))

      -- | TODO Close the ChainDB
    , closeDB            :: m ()
    }

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

toChain :: forall m blk hdr.
           (MonadThrow m, HasHeader blk, MonadSTM m)
        => ChainDB m blk hdr -> m (Chain blk)
toChain chainDB = bracket streamAll iteratorClose (go Genesis)
  where
    streamAll = do
      tip   <- atomically $ getTipPoint chainDB
      errIt <- streamBlocks chainDB (StreamFromExclusive genesisPoint)
                                    (StreamToInclusive tip)
      case errIt of
        Left  e  -> error (show e)
        Right it -> return it

    go :: Chain blk -> Iterator m blk -> m (Chain blk)
    go chain it = do
      next <- iteratorNext it
      case next of
        IteratorExhausted   -> return chain
        IteratorBlockGCed _ -> error "block was garbage-collected"
        IteratorResult blk  -> go (Chain.addBlock blk chain) it

fromChain :: forall m blk hdr. Monad m
          => m (ChainDB m blk hdr)
          -> Chain blk
          -> m (ChainDB m blk hdr)
fromChain openDB chain = do
    chainDB <- openDB
    mapM_ (addBlock chainDB) $ Chain.toOldestFirst chain
    return chainDB

{-------------------------------------------------------------------------------
  Iterator API
-------------------------------------------------------------------------------}

-- | The lower bound for a ChainDB iterator.
--
-- Hint: use @'StreamFromExclusive' 'genesisPoint'@ to start streaming from
-- Genesis.
data StreamFrom blk =
    StreamFromInclusive (Point blk)
  | StreamFromExclusive (Point blk)
  deriving (Show, Eq)

data StreamTo blk =
    StreamToInclusive (Point blk)
  | StreamToExclusive (Point blk)
  deriving (Show, Eq)

data Iterator m blk = Iterator {
      iteratorNext  :: m (IteratorResult blk)
    , iteratorClose :: m ()
    , iteratorId    :: IteratorId
    }

-- | Equality instance for iterators
--
-- This relies on the iterator IDs assigned by the database.
--
-- NOTE: Iterators created by /different instances of the DB/ may end up with
-- the same ID. This should not matter in practice since there should not /be/
-- more than one DB, but it should nonetheless be noted.
instance Eq (Iterator m blk) where
  (==) = (==) `on` iteratorId

newtype IteratorId = IteratorId Int
  deriving (Show, Eq, Ord, Enum)

data IteratorResult blk =
    IteratorExhausted
  | IteratorResult blk
  | IteratorBlockGCed (HeaderHash blk)
    -- ^ The block that was supposed to be streamed was garbage-collected from
    -- the VolatileDB, but not added to the ImmutableDB.
    --
    -- This will only happen when streaming very old forks.

data UnknownRange blk =
    -- | The block at the given point was not found in the ChainDB.
    MissingBlock (Point blk)
    -- | The requested range forks off too far in the past, i.e. it doesn't
    -- fit on the tip of the ImmutableDB.
  | ForkTooOld (StreamFrom blk)
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Chain updates

  This is a generalization of 'ChainUpdate' used in the network layer to
  support a 'Functor' instance.
-------------------------------------------------------------------------------}

-- | Chain updates returned by readers
data ChainUpdate blk a =
    AddBlock a
  | RollBack (Point blk)
  deriving (Show, Functor)

fromNetworkChainUpdate :: Chain.ChainUpdate blk -> ChainUpdate blk blk
fromNetworkChainUpdate = go
  where
    go (Chain.AddBlock blk)   = AddBlock blk
    go (Chain.RollBack point) = RollBack point

toNetworkChainUpdate :: ChainUpdate blk blk -> Chain.ChainUpdate blk
toNetworkChainUpdate = go
  where
    go (AddBlock blk)   = Chain.AddBlock blk
    go (RollBack point) = Chain.RollBack point

{-------------------------------------------------------------------------------
  Readers
-------------------------------------------------------------------------------}

-- | Reader
--
-- See 'newReader' for more info.
data Reader m blk a = Reader {
      -- | The next chain update (if one exists)
      --
      -- Not in @STM@ because might have to read the headers from disk.
      --
      -- We may roll back more than @k@, but only in case of data loss.
      readerInstruction         :: m (Maybe (ChainUpdate blk a))

      -- | Blocking version of 'readerInstruction'
    , readerInstructionBlocking :: m (ChainUpdate blk a)

      -- | Move the iterator forward
      --
      -- Must be given a list of points in order of preference; the iterator
      -- will move forward to the first point on the list that is on the current
      -- chain. Returns 'Nothing' if the iterator did not move, or the new point
      -- otherwise.
      --
      -- When successful, the first call to 'readerInstruction' after
      -- 'readerForward' will be a 'RollBack' to the point returned by
      -- 'readerForward'.
      --
      -- Cannot live in @STM@ because the points specified might live in the
      -- immutable DB.
    , readerForward             :: [Point blk] -> m (Maybe (Point blk))

      -- | Per-database reader ID
      --
      -- Two readers with the same ID are guaranteed to be the same reader,
      -- provided that they are constructed by the same database. (We don't
      -- expect to have more than one instance of the 'ChainDB', however.)
    , readerId                  :: ReaderId
    }
  deriving (Functor)

instance Eq (Reader m blk a) where
  (==) = (==) `on` readerId

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Database failure
--
-- This exception wraps any kind of unexpected problem with the on-disk storage
-- of the chain. It is primarily for monitoring purposes:
--
-- * It should never be thrown outside the scope of the Chain DB; it is the
--   responsibility of the Chain DB itself to catch these exceptions and
--   trigger data recovery. The exception type is part of the public API because
--   these exceptions will be logged.
-- * The various constructors only serve to give more detailed information about
--   what went wrong, in case sysadmins want to investigate the disk failure.
--   The Chain DB itself does not differentiate; all disk failures are treated
--   equal and all trigger the same recovery procedure.
data ChainDbFailure blk =
    -- | A block in the immutable DB failed to parse
    ImmDbParseFailure (Either EpochNo SlotNo) CBOR.DeserialiseFailure

    -- | When parsing a block from the immutable DB we got some trailing data
  | ImmDbTrailingData (Either EpochNo SlotNo) Lazy.ByteString

    -- | Block missing from the immutable DB
    --
    -- This exception gets thrown when a block that we /know/ should exist in
    -- the DB (for example, because we have its successor) nonetheless was
    -- not found
  | ImmDbMissingBlock (Either EpochNo SlotNo)

    -- | A block at a certain slot in the immutable DB had an unexpected hash.
    --
    -- The immutable DB only knows of slots and not of hashes. This exception
    -- gets thrown when the block at the given slot (1st argument) in the
    -- immutable DB had another (3rd argument) hash than the expected one (2nd
    -- argument).
  | ImmDbHashMismatch (Point blk) (HeaderHash blk) (HeaderHash blk)

    -- | We requested an iterator that was immediately exhausted
    --
    -- When we ask the immutable DB for an iterator with a particular start
    -- position but no stop position, the resulting iterator cannot be
    -- exhausted immediately: the start position is inclusive, the DB would
    -- throw an exception if the slot number is beyond the last written block,
    -- and the DB does not contain any trailing empty slots.
  | ImmDbUnexpectedIteratorExhausted (Point blk)

    -- | The immutable DB threw an "unexpected error"
    --
    -- These are errors indicative of a disk failure (as opposed to API misuse)
  | ImmDbFailure ImmDB.UnexpectedError

    -- | A block in the volatile DB failed to parse
  | VolDbParseFailure (HeaderHash blk) CBOR.DeserialiseFailure

    -- | When parsing a block from the volatile DB, we got some trailing data
  | VolDbTrailingData (HeaderHash blk) Lazy.ByteString

    -- | Block missing from the volatile DB
    --
    -- This exception gets thrown when a block that we /know/ should exist
    -- in the DB (for example, because its hash exists in the volatile DB's
    -- successor index) nonetheless was not found
  | VolDbMissingBlock (HeaderHash blk)

    -- | The volatile DB throw an "unexpected error"
    --
    -- These are errors indicative of a disk failure (as opposed to API misuse)
  | VolDbFailure (VolDB.UnexpectedError (HeaderHash blk))

    -- | Block missing from the chain DB
    --
    -- Thrown when we are not sure in which DB the block /should/ have been.
  | ChainDbMissingBlock (Point blk)

deriving instance StandardHash blk => Show (ChainDbFailure blk)

instance (StandardHash blk, Typeable blk) => Exception (ChainDbFailure blk)


{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Database error.
--
-- Thrown upon incorrect use: invalid input.
data ChainDbError blk =
    -- | Thrown when requesting the genesis block from the database
    --
    -- Although the genesis block has a hash and a point associated with it,
    -- it does not actually exist other than as a concept; we cannot read and
    -- return it.
    NoGenesisBlock

    -- | When there is no chain/fork that satisfies the bounds passed to
    -- 'streamBlocks'.
    --
    -- * The lower and upper bound are not on the same chain.
    -- * The bounds don't make sense, e.g., the lower bound starts after the
    --   upper bound, or the lower bound starts from genesis, /inclusive/.
  | InvalidIteratorRange (StreamFrom blk) (StreamTo blk)

deriving instance StandardHash blk => Show (ChainDbError blk)

instance (StandardHash blk, Typeable blk) => Exception (ChainDbError blk)

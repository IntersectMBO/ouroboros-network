{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

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
    -- * Readers
  , Reader(..)
    -- * Recovery
  , ChainDbFailure(..)
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
import           Ouroboros.Network.Block (ChainUpdate (..), HasHeader (..),
                     HeaderHash, SlotNo, StandardHash)
import           Ouroboros.Network.Chain (Chain (..), Point (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ReaderId)

import           Ouroboros.Consensus.Ledger.Extended

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API.Types (FsError)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB

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
    , streamBlocks       :: StreamFrom blk -> StreamTo blk -> m (Iterator m blk)

      -- | Chain reader
      --
      -- A chain reader is an iterator that tracks the state of the /current/
      -- chain: calling @next@ on the iterator will either give you the next
      -- block header, or (if we have switched to a fork) the instruction to
      -- rollback.
      --
      -- The tracking iterator starts at genesis (see also 'trackForward').
      --
      -- This is intended for use by chain consumers to /reliably/ follow a
      -- chain, desipite the chain being volatile.
      --
      -- Examples of users include the server side of the chain sync
      -- mini-protocol for the node-to-node protocol.
      --
    , newHeaderReader    :: m (Reader m hdr)

      -- | This is the same as the reader 'newHeaderReader' but it provides a
      -- reader for /whole blocks/ rather than headers.
      --
      -- Examples of users include the server side of the chain sync
      -- mini-protocol for the node-to-client protocol.
      --
    , newBlockReader     :: m (Reader m blk)

      -- | Known to be invalid blocks
    , knownInvalidBlocks :: STM m (Set (Point blk))

      -- | Check if the specified point is on the current chain
      --
      -- This lives in @m@, not @STM m@, because if the point is not on the
      -- current chain fragment, it might have to query the immutable DB.
    , pointOnChain       :: Point blk -> m Bool
    }

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

toChain :: forall m blk hdr.
           (MonadThrow m, HasHeader blk)
        => ChainDB m blk hdr -> m (Chain blk)
toChain chainDB = bracket
    (streamBlocks chainDB StreamFromGenesis StreamToEnd)
    iteratorClose
    (go Genesis)
  where
    go :: Chain blk -> Iterator m blk -> m (Chain blk)
    go chain it = do
      next <- iteratorNext it
      case next of
        IteratorExhausted  -> return chain
        IteratorResult blk -> go (Chain.addBlock blk chain) it

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

data StreamFrom blk =
    StreamFromInclusive (Point blk)
  | StreamFromExclusive (Point blk)
  | StreamFromGenesis

data StreamTo blk =
    StreamToInclusive (Point blk)
  | StreamToExclusive (Point blk)
  | StreamToEnd

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
  deriving (Show, Eq, Ord)

data IteratorResult blk =
    IteratorExhausted
  | IteratorResult blk

{-------------------------------------------------------------------------------
  Readers
-------------------------------------------------------------------------------}

-- | Reader
--
-- See 'newReader' for more info.
data Reader m hdr = Reader {
      -- | The next chain update (if one exists)
      --
      -- > data ChainUpdate hdr = AddBlock hdr
      -- >                      | RollBack (Point hdr)
      --
      -- Does not live in @STM@ because might have to read the headers from disk.
      --
      -- We may roll back more than @k@ only in case of data loss.
      readerInstruction         :: m (Maybe (ChainUpdate hdr))

      -- | Blocking version of 'readerInstruction'
    , readerInstructionBlocking :: m (ChainUpdate hdr)

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
    , readerForward             :: [Point hdr] -> m (Maybe (Point hdr))

      -- | Per-database reader ID
      --
      -- Two readers with the same ID are guaranteed to be the same reader,
      -- provided that they are constructed by the same database. (We don't
      -- expect to have more than one instance of the 'ChainDB', however.)
    , readerId                  :: ReaderId
    }

instance Eq (Reader m hdr) where
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

    -- | File system error whilst accessing the volatile DB
  | VolDbFileSystemError FsError

deriving instance StandardHash blk => Show (ChainDbFailure blk)

instance (StandardHash blk, Typeable blk) => Exception (ChainDbFailure blk)

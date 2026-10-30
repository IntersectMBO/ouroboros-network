{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node-wide mux counters: how the muxes of one set have failed, counted
-- once per mux as it dies, and the bytes they sent and read, by
-- mini-protocol and direction; a remote set and a local one, traced
-- periodically.
--
module Network.Mux.Counters
  ( -- * Counters
    MuxCounters
  , withMuxCounters
  , readMuxCounters
  , MuxCounts (..)
  , MuxFailures (..)
  , noFailures
  , noCounts
  , ByteCounts
    -- * Tracing
  , CountersTrace (..)
  , countersInterval
    -- * Internal, for the mux and the tests
  , newMuxCounters
  , countersLoop
  , ByteSlots
  , withByteCells
  , newByteSlots
  , readSlots
  , countMuxerFailure
  , countDemuxerFailure
  , countMiniProtocolFailure
  , snapshotMuxCountersWith
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException, SomeException, fromException)
import Control.Monad (forM_, forever, unless)
import Control.Monad.Class.MonadAsync (MonadAsync, race)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadThrow (MonadMask, bracket)
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Void (absurd)
import Data.Word (Word64)
import Foreign.C.Error (Errno (..), eCONNRESET, ePIPE)
import GHC.IO.Exception (IOErrorType (..), IOException (..))

import Network.Mux.Trace (Error (..))
import Network.Mux.Types (IngressQueue, IngressQueueState (..), MiniProtocolDir,
           MiniProtocolNum, RuntimeError)

-- | How the muxes have failed, each failure counted once, as the mux died
-- of it.
data MuxFailures = MuxFailures {
    -- The muxer.
    mfWriteTimeouts  :: !Word64,
    -- ^ the peer took nothing within the SDU timeout
    mfWriteErrors    :: !Word64,
    -- ^ the bearer's write failed otherwise: a timed-out connection, a
    -- host unreachable

    -- The demuxer.
    mfReadTimeouts   :: !Word64,
    -- ^ an SDU header arrived, the rest did not in time
    mfOverruns       :: !Word64,
    -- ^ the peer exceeded a mini-protocol's ingress limit
    mfProtocolErrors :: !Word64,
    -- ^ undecodable SDUs, unknown mini-protocols, data an initiator-only
    -- mux cannot take
    mfReadErrors     :: !Word64,
    -- ^ the bearer's read failed otherwise: a connection that timed out, a
    -- host unreachable

    -- Either bearer side.
    mfPeerClosed     :: !Word64,
    -- ^ the peer closed the connection and the mux failed of it: read as
    -- its end while a mini-protocol was running, or met as a reset or a
    -- broken pipe by a read or a write
    mfOtherErrors    :: !Word64,
    -- ^ the muxer or the demuxer died with an exception of a kind not listed,
    -- asynchronous ones aside: an encoding that would not force, a bug

    -- The mini-protocols.
    mfMiniProtocols  :: !(Map (MiniProtocolNum, MiniProtocolDir) Word64)
    -- ^ a mini-protocol's thread died with an exception of its own, by
    -- protocol and our side's direction: a violation it detected, its time
    -- limit
  }
  deriving (Eq, Show)

-- | No failure counted.
noFailures :: MuxFailures
noFailures = MuxFailures 0 0 0 0 0 0 0 0 Map.empty

-- | One side's counters at an instant.
data MuxCounts = MuxCounts {
    mcFailures :: !MuxFailures,
    mcSent     :: !ByteCounts,  -- ^ bytes written, by mini-protocol and direction
    mcReceived :: !ByteCounts   -- ^ bytes read, likewise
  }
  deriving (Eq, Show)

-- | Nothing counted.
noCounts :: MuxCounts
noCounts = MuxCounts noFailures Map.empty Map.empty

-- | Bytes on the wire, SDU headers included, by mini-protocol number and the
-- direction of our side of it: what our initiators and our responders sent,
-- or what was read for them.
type ByteCounts = Map (MiniProtocolNum, MiniProtocolDir) Word64

-- | One mux's byte counters, one per mini-protocol and direction it runs,
-- written only by its own muxer or demuxer, so no two connections share a
-- variable on the data path; made by 'newByteSlots'.
type ByteSlots m = Map (MiniProtocolNum, MiniProtocolDir) (StrictTVar m Word64)

-- | What one mux sent and read: a slot per mini-protocol and direction the
-- muxer writes into, and the mini-protocols' ingress queues themselves,
-- each carrying what the demuxer read for its mini-protocol, counted in
-- the write that queued it so that a read costs nothing beyond that write.
-- The queues are only ever read here, for that count, never written.
data MuxByteCells m = MuxByteCells {
    mbcSent :: !(ByteSlots m),
    -- ^ the muxer's slots
    mbcRecv :: !(Map (MiniProtocolNum, MiniProtocolDir) (IngressQueue m))
    -- ^ the ingress queues, read for their 'iqReceived'
  }

-- | The counts in the slots, leaving out the ones still at zero: a protocol
-- appears once it has carried something.
readSlots :: MonadSTM m => ByteSlots m -> STM m ByteCounts
readSlots = fmap nonZero . traverse readTVar

-- | The counts that are not zero.
nonZero :: ByteCounts -> ByteCounts
nonZero = Map.filter (/= 0)

-- | Counters shared by every mux of one set: the failures, and the byte
-- cells of the live muxes beside the totals of the ones that have ended, so
-- a total never goes down when a connection closes.
data MuxCounters m = MuxCounters {
    countersFailures :: !(StrictTVar m MuxFailures),
    -- ^ how the muxes have failed, bumped by each as it dies
    countersLive     :: !(StrictTVar m (IntMap (MuxByteCells m))),
    -- ^ the byte cells of the muxes running, each under the key it
    -- registered with
    countersNext     :: !(StrictTVar m Int),
    -- ^ the key the next mux registers under, counted up by each
    countersRetired  :: !(StrictTVar m Retired)
    -- ^ sent and read by the muxes that have ended, summed in as each
    -- retired
  }

-- | What the muxes that have ended sent and read.
data Retired = Retired !ByteCounts !ByteCounts

-- | A fresh set, nothing counted.
newMuxCounters :: MonadSTM m => m (MuxCounters m)
newMuxCounters =
  MuxCounters <$> newTVarIO noFailures
              <*> newTVarIO IntMap.empty
              <*> newTVarIO 0
              <*> newTVarIO (Retired Map.empty Map.empty)

-- | The failures with the bytes of every mux, ended or live, summed in, in
-- one transaction: for tests and small sets, as a transaction over every
-- live mux's counters is restarted by any muxer or demuxer writing one it
-- has read, and reads them all over again; a node reads with the loop's
-- snapshot, a transaction per counter.
readMuxCounters :: MonadSTM m => MuxCounters m -> STM m MuxCounts
readMuxCounters = snapshotMuxCountersWith id

-- | The same, with each read run by @runTx@: in a transaction for the totals
-- and one per counter of each live mux, so no transaction spans more than
-- one variable a muxer or a demuxer writes, and a busy connection makes it
-- rerun at most the read of one. The totals and the live set go together,
-- before the cells: a mux that ends in between is in the live set and not
-- yet in the totals, and its cells, which ending leaves as they were, count
-- it once; a sum never goes down across snapshots.
snapshotMuxCountersWith :: (MonadSTM m, Monad n)
                        => (forall a. STM m a -> n a)
                        -> MuxCounters m
                        -> n MuxCounts
snapshotMuxCountersWith runTx MuxCounters { countersFailures, countersLive, countersRetired } = do
  (failures, Retired sent received, live) <- runTx $
    (,,) <$> readTVar countersFailures
         <*> readTVar countersRetired
         <*> (IntMap.elems <$> readTVar countersLive)
  cells <- mapM (readMuxCellsWith runTx) live
  return MuxCounts {
      mcFailures = failures,
      mcSent     = Map.unionsWith (+) (sent : map fst cells),
      mcReceived = Map.unionsWith (+) (received : map snd cells)
    }

-- | One mux's bytes sent and read, each counter read in a transaction of
-- its own by @runTx@: the slots, and the bytes read for each queue's
-- mini-protocol, leaving out the ones still at zero.
readMuxCellsWith :: (MonadSTM m, Monad n)
                 => (forall a. STM m a -> n a)
                 -> MuxByteCells m
                 -> n (ByteCounts, ByteCounts)
readMuxCellsWith runTx MuxByteCells { mbcSent, mbcRecv } =
  (,) <$> (nonZero <$> traverse (runTx . readTVar) mbcSent)
      <*> (nonZero <$> traverse (runTx . fmap iqReceived . readTVar) mbcRecv)

-- | Byte cells for a mux for the duration of @k@, which is given the muxer's
-- slots: one per mini-protocol and direction, beside the ingress queues
-- given, which count what the demuxer reads whether or not anyone looks.
-- With counters the slots are made and the cells registered as the mux
-- starts, and on the way out their counts move into the retired totals in
-- the same transaction that unregisters them; without, there are no slots,
-- so the muxer counts nothing.
withByteCells :: forall m a. (MonadSTM m, MonadMask m)
              => Maybe (MuxCounters m)
              -> Map (MiniProtocolNum, MiniProtocolDir) (IngressQueue m)
              -> (ByteSlots m -> m a)
              -> m a
withByteCells counters queues k = case counters of
    Nothing -> k Map.empty
    Just c  -> do
      slots <- atomically $ newByteSlots (Map.keys queues)
      let cells = MuxByteCells { mbcSent = slots, mbcRecv = queues }
      bracket (register c cells) (retire c cells) (const (k slots))
  where
    -- The cells among the live muxes' under the next key, which is returned.
    register :: MuxCounters m -> MuxByteCells m -> m Int
    register MuxCounters { countersLive, countersNext } cells = atomically $ do
      i <- stateTVar countersNext (\n -> (n, n + 1))
      modifyTVar countersLive (IntMap.insert i cells)
      return i

    -- The cells' counts into the retired totals and the cells out of the live
    -- set, in one transaction. The cells themselves are left as they were: a
    -- snapshot that read the live set before the mux ended reads them after,
    -- and counts the mux once. So the queues keep what the run read, and a
    -- 'Network.Mux.Mux' is run once, as its status has it.
    retire :: MuxCounters m -> MuxByteCells m -> Int -> m ()
    retire MuxCounters { countersLive, countersRetired } cells i = atomically $ do
      (sent, received) <- readMuxCellsWith id cells
      modifyTVar countersRetired $ \(Retired s r) ->
        Retired (Map.unionWith (+) s sent) (Map.unionWith (+) r received)
      modifyTVar countersLive (IntMap.delete i)

-- | A slot per mini-protocol and direction given, at zero. A mux makes its
-- muxer's for every mini-protocol it runs; an SDU for a key not among them
-- is not counted.
newByteSlots :: MonadSTM m => [(MiniProtocolNum, MiniProtocolDir)] -> STM m (ByteSlots m)
newByteSlots keys = Map.fromList <$> sequence [ (,) key <$> newTVar 0 | key <- keys ]

-- | What a muxer's death counts as, if anything: the peer closing, which a
-- write meets as a reset or a broken pipe; an asynchronous exception is the
-- mux ending and counts nothing; a kind not listed counts as an error of
-- its own. The mux fails of every one but the asynchronous, so the counts
-- sum to the muxes dropped.
muxerFailure :: SomeException -> Maybe (MuxFailures -> MuxFailures)
muxerFailure e
  | isPeerClose e = Just peerClosed
  | isIOError e   = Just (\f -> f { mfWriteErrors = mfWriteErrors f + 1 })
  | isAsync e     = Nothing
  | otherwise     = Just $ case fromException e of
      Just SDUWriteTimeout -> \f -> f { mfWriteTimeouts = mfWriteTimeouts f + 1 }
      _                    -> otherError

-- | What a demuxer's death counts as, if anything: the peer closing, read as
-- the end of the stream or met as a reset; the rest as for the muxer. The
-- mux asks only when it fails of it: a close read after every
-- mini-protocol has finished ends it stopped, and counts nothing.
demuxerFailure :: SomeException -> Maybe (MuxFailures -> MuxFailures)
demuxerFailure e
  | isPeerClose e = Just peerClosed
  | isIOError e   = Just (\f -> f { mfReadErrors = mfReadErrors f + 1 })
  | isAsync e     = Nothing
  | otherwise     = Just $ case fromException e of
      Just SDUReadTimeout         -> \f -> f { mfReadTimeouts = mfReadTimeouts f + 1 }
      Just IngressQueueOverRun {} -> \f -> f { mfOverruns = mfOverruns f + 1 }
      Just UnknownMiniProtocol {} -> protocolError
      Just InitiatorOnly {}       -> protocolError
      Just SDUDecodeError {}      -> protocolError
      _                           -> otherError
  where
    protocolError :: MuxFailures -> MuxFailures
    protocolError f = f { mfProtocolErrors = mfProtocolErrors f + 1 }

-- | One more peer close.
peerClosed :: MuxFailures -> MuxFailures
peerClosed f = f { mfPeerClosed = mfPeerClosed f + 1 }

-- | One more death of a kind not listed.
otherError :: MuxFailures -> MuxFailures
otherError f = f { mfOtherErrors = mfOtherErrors f + 1 }

-- | A bearer's I/O error, wrapped in 'Error' as the socket bearer throws it,
-- or raw.
ioErrorOf :: SomeException -> Maybe IOException
ioErrorOf e = case fromException e of
  Just (IOException ioe _) -> Just ioe
  _                        -> fromException e

-- | An I/O error, wrapped or raw.
isIOError :: SomeException -> Bool
isIOError = isJust . ioErrorOf

-- | The peer closed the connection: the bearer read its end, or an
-- operation found the connection reset or the pipe broken, which is how
-- a close arrives from a peer that closes without lingering, as the node
-- does. A socket's error is judged by its errno; one without, of the
-- vanished kind, is how the simulation's channel reports its end. On
-- Windows the socket library reports neither the errno nor the kind, so a
-- reset there is an I/O error like any other.
isPeerClose :: SomeException -> Bool
isPeerClose e = case fromException e of
  Just BearerClosed {} -> True
  _                    -> maybe False peerGone (ioErrorOf e)
  where
    peerGone :: IOException -> Bool
    peerGone ioe = case ioe_errno ioe of
      Just errno -> Errno errno `elem` [eCONNRESET, ePIPE]
      Nothing    -> ioe_type ioe == ResourceVanished

-- | An asynchronous exception: the mux ending, or the thread cancelled.
isAsync :: SomeException -> Bool
isAsync e = isJust (fromException e :: Maybe SomeAsyncException)

-- | The mux's own errors, counted where they arose.
muxOwn :: SomeException -> Bool
muxOwn e = isJust (fromException e :: Maybe Error)
        || isJust (fromException e :: Maybe RuntimeError)

-- | Count the exception a mux's muxer died with.
countMuxerFailure :: MonadSTM m => Maybe (MuxCounters m) -> SomeException -> STM m ()
countMuxerFailure counters = countFailure counters . muxerFailure

-- | Count the exception a mux's demuxer died with, the mux failing of it.
countDemuxerFailure :: MonadSTM m => Maybe (MuxCounters m) -> SomeException -> STM m ()
countDemuxerFailure counters = countFailure counters . demuxerFailure

-- | Count a failure, if there is one to count and a set to count it in.
countFailure :: MonadSTM m
             => Maybe (MuxCounters m)
             -> Maybe (MuxFailures -> MuxFailures)
             -> STM m ()
countFailure counters failure =
  forM_ counters $ \MuxCounters { countersFailures } ->
    forM_ failure (modifyTVar countersFailures)

-- | Count the exception a mini-protocol's thread died with, by its protocol
-- and our side's direction, unless it is the mux's own or asynchronous,
-- which is the mux ending.
countMiniProtocolFailure :: MonadSTM m
                         => Maybe (MuxCounters m)
                         -> MiniProtocolNum
                         -> MiniProtocolDir
                         -> SomeException
                         -> STM m ()
countMiniProtocolFailure counters num dir e =
  forM_ counters $ \MuxCounters { countersFailures } ->
    unless (muxOwn e || isAsync e) $ modifyTVar countersFailures $ \f ->
      f { mfMiniProtocols = Map.insertWith (+) (num, dir) 1 (mfMiniProtocols f) }

-- | A snapshot of one set's counters.
data CountersTrace =
    TraceRemoteCounters MuxCounts  -- ^ the remote muxes
  | TraceLocalCounters  MuxCounts  -- ^ the local muxes
  deriving (Eq, Show)

-- | A prime, so that snapshots do not keep step with other periodic work such
-- as keep-alive, and shorter than a 10 s scrape interval.
countersInterval :: DiffTime
countersInterval = 7

-- | Every @interval@, a second at least, snapshot the remote and the local
-- set, each if there is one, and trace each. 'withMuxCounters' runs it for
-- the sets it makes.
countersLoop :: (MonadDelay m, MonadSTM m)
             => Maybe (MuxCounters m)    -- ^ the remote set, if any
             -> Maybe (MuxCounters m)    -- ^ the local set, if any
             -> DiffTime
             -> Tracer m CountersTrace
             -> m void
countersLoop remote local interval tracer = forever $ do
  threadDelay delay
  -- Small transactions, one for the totals and one per counter: the counters
  -- need no consistency across them, and one over everything the muxes write
  -- would be restarted by every batch.
  forM_ remote $ \c ->
    snapshotMuxCountersWith atomically c >>= traceWith tracer . TraceRemoteCounters
  forM_ local  $ \c ->
    snapshotMuxCountersWith atomically c >>= traceWith tracer . TraceLocalCounters
  where
    -- A second at least: a zero or a negative interval would make the loop
    -- spin.
    delay :: DiffTime
    delay = max 1 interval

-- | Counter sets for as long as @k@ runs: one for the remote muxes and, if
-- asked, one for the local muxes, each traced every @interval@, a second at
-- least, by 'countersLoop' raced with @k@, so an exception in either ends
-- both. An interval of zero, or less, makes no set: the muxes count nothing
-- and nothing is traced. The only way to a set, so none is kept without
-- the loop that reads it.
withMuxCounters :: (MonadAsync m, MonadDelay m)
                => DiffTime
                -> Bool                 -- ^ a local set too
                -> Tracer m CountersTrace
                -> (Maybe (MuxCounters m) -> Maybe (MuxCounters m) -> m a)
                -> m a
withMuxCounters interval localToo tracer k
  | interval <= 0 = k Nothing Nothing
  | otherwise = do
      remote <- newMuxCounters
      local  <- if localToo then Just <$> newMuxCounters else return Nothing
      either absurd id <$>
        race (labelThisThread "mux-counters" >> countersLoop (Just remote) local interval tracer)
             (k (Just remote) local)

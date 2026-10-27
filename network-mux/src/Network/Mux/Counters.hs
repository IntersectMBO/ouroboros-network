{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node-wide mux counters: how muxes on one side, node-to-node or
-- node-to-client, have failed, counted once per mux as it dies; traced
-- periodically.
--
module Network.Mux.Counters
  ( -- * Counters
    MuxCounters
  , newMuxCounters
  , readMuxCounters
  , MuxCounts (..)
  , MuxFailures (..)
  , noFailures
  , noCounts
    -- * Tracing
  , CountersTrace (..)
  , countersLoop
  , countersInterval
    -- * Internal, for the mux and the tests
  , countMuxerFailure
  , countDemuxerFailure
  , countMiniProtocolFailure
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException, SomeException, fromException)
import Control.Monad (forM_, forever, unless)
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.C.Error (Errno (..), eCONNRESET, ePIPE)
import GHC.IO.Exception (IOErrorType (..), IOException (..))

import Network.Mux.Trace (Error (..))
import Network.Mux.Types (MiniProtocolDir, MiniProtocolNum, RuntimeError)

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
newtype MuxCounts = MuxCounts {
    mcFailures :: MuxFailures
  }
  deriving (Eq, Show)

-- | Nothing counted.
noCounts :: MuxCounts
noCounts = MuxCounts noFailures

-- | Counters shared by every mux on one side.
newtype MuxCounters m = MuxCounters {
    countersFailures :: StrictTVar m MuxFailures
  }

-- | A fresh set, nothing counted.
newMuxCounters :: MonadSTM m => m (MuxCounters m)
newMuxCounters = MuxCounters <$> newTVarIO noFailures

-- | The failures counted so far.
readMuxCounters :: MonadSTM m => MuxCounters m -> STM m MuxCounts
readMuxCounters MuxCounters { countersFailures } = MuxCounts <$> readTVar countersFailures

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

-- | The mux's own exceptions: asynchronous ones, which are the mux ending,
-- and its errors, counted where they arose.
muxOwn :: SomeException -> Bool
muxOwn e = isAsync e
        || isJust (fromException e :: Maybe Error)
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
-- and our side's direction, unless it is the mux's own.
countMiniProtocolFailure :: MonadSTM m
                         => Maybe (MuxCounters m)
                         -> MiniProtocolNum
                         -> MiniProtocolDir
                         -> SomeException
                         -> STM m ()
countMiniProtocolFailure counters num dir e =
  forM_ counters $ \MuxCounters { countersFailures } ->
    unless (muxOwn e) $ modifyTVar countersFailures $ \f ->
      f { mfMiniProtocols = Map.insertWith (+) (num, dir) 1 (mfMiniProtocols f) }

-- | A snapshot of one side's counters.
data CountersTrace =
    TraceRemoteCounters MuxCounts  -- ^ the node-to-node muxes
  | TraceLocalCounters  MuxCounts  -- ^ the node-to-client muxes
  deriving (Eq, Show)

-- | A prime, so that snapshots do not keep step with other periodic work such
-- as keep-alive, and shorter than a 10 s scrape interval.
countersInterval :: DiffTime
countersInterval = 7

-- | Every @interval@, a second at least, snapshot the node-to-node and the
-- node-to-client counters, each when the node keeps them, and trace each.
countersLoop :: (MonadDelay m, MonadSTM m)
             => Maybe (MuxCounters m)    -- ^ node-to-node, when kept
             -> Maybe (MuxCounters m)    -- ^ node-to-client, when kept
             -> DiffTime
             -> Tracer m CountersTrace
             -> m void
countersLoop remote local interval tracer = forever $ do
  threadDelay delay
  forM_ remote $ \c -> atomically (readMuxCounters c) >>= traceWith tracer . TraceRemoteCounters
  forM_ local  $ \c -> atomically (readMuxCounters c) >>= traceWith tracer . TraceLocalCounters
  where
    -- A second at least: a zero or a negative interval would make the loop
    -- spin.
    delay :: DiffTime
    delay = max 1 interval

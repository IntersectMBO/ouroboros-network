{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Per-connection RTT tracking driven by the SDU-header cookie
-- scheme.
--
-- Each outgoing SDU carries a random 'Cookie' in 'mhSendCookie'.
-- The peer, on receipt, remembers the freshest 'mhSendCookie' it has
-- observed and echoes it in the 'mhEchoCookie' field of its own
-- outgoing SDUs. When we see an 'mhEchoCookie' we sent earlier, we
-- look up the local send time in an outstanding-cookies map and
-- compute the RTT.
--
-- The map is a priority-search queue keyed by 'Cookie' and
-- prioritised by send 'Time'. That gives us:
--
--   * O(log N) lookup on echo receipt.
--   * O(1) access to the oldest outstanding entry for time-based
--     age-out.
--   * O((k+1) log N) pruning of everything at or before a given
--     time (used both for age-out and for the "prune-on-match"
--     enforcement of the monotone-echo invariant).
--
-- Cookies are drawn from an 'StdGen' PRNG per mux; a hostile peer
-- cannot echo a cookie value we have not previously sent, so RTT
-- cannot be forged low. The peer can inflate observed RTT only by
-- delaying its own outbound SDUs — which is a real RTT signal, not
-- an attack.
--
module Network.Mux.RTT
  ( -- * Peer-facing reader
    PeerRTT (..)
  , noPeerRTT
    -- * Internal sampling state
  , RTTState
  , newRTTState
  , newSendCookie
  , peerRTT
    -- * Configuration
  , defaultBucketDur
  , defaultRetention
  , defaultHoldDuration
  , defaultMaxOutstanding
  , defaultMintInterval
  , RTTComp
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as PSQ
import Data.Window.DigestTimeBatched qualified as DTB
import System.Random (StdGen, uniformR)

import Network.Mux.Types (Cookie (..), SDUHeader (..), noCookie)


-- | STM accessor for the local muxer's rolling RTT distribution to
-- this peer.
--
-- Callers supply a quantile at read time (@0.5@ for median, @0.99@
-- for a tail-guard, etc.). 'Nothing' is returned when no RTT samples
-- have been observed yet.
newtype PeerRTT m = PeerRTT {
    readPeerRTTQuantile :: Double -> STM m (Maybe DiffTime)
  }

-- | A 'PeerRTT' handle that always yields 'Nothing'. Placeholder for
-- test/demo sites that don't run over a real mux.
noPeerRTT :: MonadSTM m => PeerRTT m
noPeerRTT = PeerRTT (\_ -> pure Nothing)


-- | t-digest compression parameter for the RTT window.
type RTTComp = 100

-- | Default per-bucket duration.
defaultBucketDur :: DiffTime
defaultBucketDur = 1

-- | Default retention in buckets.
defaultRetention :: Int
defaultRetention = 60

-- | Default outstanding-cookie hold: comfortably above worst-case
-- peer dwell (KeepAlive interval + reply timeout + a multi-second
-- peer GC pause).
defaultHoldDuration :: DiffTime
defaultHoldDuration = 30

-- | Cap on the outstanding-cookie map size. At Word16, the birthday
-- collision rate is ~N/2¹⁶, so 4096 gives ~6% per-insert re-roll —
-- comfortable.
defaultMaxOutstanding :: Int
defaultMaxOutstanding = 4096

-- | Minimum interval between minting fresh cookies. Outgoing SDUs
-- within this window of the previous mint reuse the same cookie —
-- avoids per-SDU PRNG work and OrdPSQ inserts on high-throughput
-- connections. RTT samples for reused cookies are computed against
-- the mint time, so the observed RTT can over-estimate by up to
-- 'defaultMintInterval' for the later SDUs in a batch. 1 ms is fine
-- for RTTs of a few ms upward.
defaultMintInterval :: DiffTime
defaultMintInterval = 1e-3


-- | Internal per-mux RTT state.
data RTTState m = RTTState {
    -- | Freshest peer send-cookie observed (goes into next echo field).
    rttLastPeerCookie :: !(StrictTVar m Cookie)
    -- | Outstanding cookies we've sent, keyed by cookie and
    -- prioritised by send 'Time'.
    --
    -- On a successful echo match, prune-on-match deletes the matched
    -- entry and every older one; that implicitly enforces the
    -- monotone-echo invariant (later echoes cannot re-match a
    -- send time earlier than the last one we accepted), so no
    -- separate 'lastAccepted' guard is needed.
  , rttOutstanding    :: !(StrictTVar m (OrdPSQ Cookie Time ()))
    -- | Most recently minted (cookie, mint-time) — reused by
    -- 'newSendCookie' for SDUs sent within 'rttMintInterval' of the
    -- previous mint. 'Nothing' before the first mint.
  , rttLastMinted     :: !(StrictTVar m (Maybe (Cookie, Time)))
    -- | PRNG for cookie generation.
  , rttPRNG           :: !(StrictTVar m StdGen)
    -- | Rolling t-digest window of RTT samples.
  , rttWindow         :: !(StrictTVar m (DTB.TimedDigestWindow Time RTTComp))
    -- | How long we retain an unmatched cookie before evicting.
  , rttHoldDuration   :: !DiffTime
    -- | Cap on outstanding cookies (drop-oldest on overflow).
  , rttMaxOutstanding :: !Int
    -- | Cookie-reuse window; see 'defaultMintInterval'.
  , rttMintInterval   :: !DiffTime
  }


-- | Construct fresh RTT state. Caller supplies the 'StdGen' — pass
-- an entropy-seeded 'newStdGen' from IO in production, or a fixed
-- 'mkStdGen' from a test.
newRTTState
    :: MonadLabelledSTM m
    => StdGen
    -> m (RTTState m)
newRTTState g = do
    rttLastPeerCookie <- newTVarIO noCookie
    rttOutstanding    <- newTVarIO PSQ.empty
    rttLastMinted     <- newTVarIO Nothing
    rttPRNG           <- newTVarIO g
    rttWindow         <- newTVarIO (DTB.empty defaultBucketDur defaultRetention)
    labelTVarIO rttLastPeerCookie "RTT.lastPeerCookie"
    labelTVarIO rttOutstanding    "RTT.outstanding"
    labelTVarIO rttLastMinted     "RTT.lastMinted"
    labelTVarIO rttLastEcho       "RTT.lastEcho"
    labelTVarIO rttPRNG           "RTT.prng"
    labelTVarIO rttWindow         "RTT.window"
    return RTTState {
        rttLastPeerCookie,
        rttOutstanding,
        rttLastMinted,
        rttLastEcho,
        rttPRNG,
        rttWindow,
        rttHoldDuration   = defaultHoldDuration,
        rttMaxOutstanding = defaultMaxOutstanding,
        rttMintInterval   = defaultMintInterval,
      }


-- | Return a (sendCookie, echoCookie) pair for an outgoing SDU.
--
-- If the previous mint is within 'rttMintInterval' of @now@, the
-- previous cookie is reused — no PRNG draw, no 'OrdPSQ' insert. RTT
-- samples for reused cookies are computed against the *mint* time,
-- so late SDUs in a batch will see an RTT over-estimated by up to
-- 'rttMintInterval'.
--
-- Otherwise we draw a fresh non-collision cookie, insert it against
-- the given send time, and record it as the new last-mint. Overflow
-- behaviour: if outstanding is at capacity we drop the oldest entry.
-- Collision behaviour: re-roll if the drawn cookie already appears
-- in outstanding (bounded to a small constant number of re-rolls at
-- reasonable N).
newSendCookie
    :: MonadSTM m
    => RTTState m
    -> Time
    -> STM m (Cookie, Cookie)
newSendCookie RTTState { rttLastPeerCookie
                       , rttOutstanding
                       , rttLastMinted
                       , rttPRNG
                       , rttMaxOutstanding
                       , rttMintInterval
                       } !now = do
    mLast <- readTVar rttLastMinted
    echo  <- readTVar rttLastPeerCookie
    case mLast of
      Just (c, t) | now `diffTime` t < rttMintInterval ->
        pure (c, echo)
      _otherwise -> do
        outs <- readTVar rttOutstanding
        g0   <- readTVar rttPRNG
        let (!cookie, g1) = drawCookie outs g0
            outs'         = trimToCap rttMaxOutstanding
                                      (PSQ.insert cookie now () outs)
        writeTVar rttOutstanding outs'
        writeTVar rttPRNG        g1
        writeTVar rttLastMinted  (Just (cookie, now))
        pure (cookie, echo)
  where
    -- Draw a non-'noCookie' value that doesn't collide with outstanding.
    drawCookie outs g =
      let (w, g') = uniformR (1, maxBound) g
          c       = Cookie w
      in maybe (c, g') (const $ drawCookie outs g') $ PSQ.lookup c outs

    trimToCap cap psq
      | PSQ.size psq <= cap = psq
      | otherwise           = case PSQ.minView psq of
          Just (_k, _p, _v, rest) -> trimToCap cap rest
          Nothing                 -> psq


-- | Reader handle for consumers.
peerRTT :: MonadSTM m => RTTState m -> PeerRTT m
peerRTT RTTState { rttWindow } = PeerRTT $ \q -> do
  w <- readTVar rttWindow
  return $! realToFrac <$> DTB.windowQuantile q w

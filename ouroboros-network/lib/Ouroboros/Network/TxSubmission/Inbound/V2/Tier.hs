{-# LANGUAGE NamedFieldPuns #-}

-- | The inbound tx-submission fetch tiers.
--
-- A relay polls every inbound peer for txids at about one exchange per round
-- trip, and under load that polling is most of its CPU while nearly every
-- txid it carries is a duplicate.  The tiers bound the set of peers polled
-- at full speed without judging peers on what they deliver: local roots are
-- always fast; a peer that is itself a tx source ('isSource') is fast while
-- it produces; the first 'txFastPeers' of the other peers in a salted order
-- ('orderFast') are fast until the coin draws the next salt; everyone else
-- waits 'slowWait' between txid requests, blocking ones included.  The
-- order is a function of the salt and the peers in it, as the peer-sharing
-- responder's answer is: a departure moves the next peer up at once and an
-- arrival takes its place.  A peer enters the order once its server's
-- initial delay has passed, before its first decision, so a peer not
-- polling yet holds no fast slot.  Nothing a peer sends can move another
-- peer down.
--
-- The pure core lives here; "Ouroboros.Network.TxSubmission.Inbound.V2.State"
-- applies the gate and "Ouroboros.Network.TxSubmission.Inbound.V2.Registry"
-- runs the coin.
module Ouroboros.Network.TxSubmission.Inbound.V2.Tier
  ( -- * The effective tier
    effectiveTier
  , isSource
  , sourceUntil
  , recordNovel
    -- * The slow tier's wait
  , slowWait
  , nextJitter
    -- * The salted order
  , orderFast
    -- * The census
  , standingOf
  , tierStanding
  , censusOf
  ) where

import Control.Monad.Class.MonadTime.SI
import Data.List as List (foldl', sortOn)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Word (Word64)
import System.Random.SplitMix (mkSMGen, nextDouble, nextWord64, unseedSMGen)

import Ouroboros.Network.Hashable (Hashable, Salt, hashWithSalt)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Types


-- | The tier a peer is served on now: fast by standing or by the coin
-- ('peerTier', which the registry writes), or fast as a source.
effectiveTier :: TxDecisionPolicy -> Time -> PeerTxLocalState tx -> PeerTier
effectiveTier policy now PeerTxLocalState { peerTier, peerNovelAt }
  | peerTier == FastTier             = FastTier
  | isSource policy now peerNovelAt  = FastTier
  | otherwise                        = SlowTier

-- | A peer is a source while the accepts recorded inside the window reach
-- 'txSourceCount'; a count below one is the rule off.
isSource :: TxDecisionPolicy -> Time -> StrictSeq (Time, Int) -> Bool
isSource policy now novelAt = n >= 1 && inWindow >= n
  where
    n = txSourceCount policy
    inWindow = List.foldl' (\acc (t, c) -> if addTime (txSourceWindow policy) t >= now then acc + c else acc)
                           0 novelAt

-- | Until when a peer is a source by its record at @now@: 'recordNovel'
-- keeps the oldest entry only while the rest fall short of 'txSourceCount',
-- so a source stops being one once that entry leaves the window; one that
-- is not stays so without further accepts.
sourceUntil :: TxDecisionPolicy -> Time -> StrictSeq (Time, Int) -> Maybe Time
sourceUntil policy now novelAt
  | isSource policy now novelAt
  , Just (oldest, _) <- StrictSeq.lookup 0 novelAt
  = Just $! addTime (txSourceWindow policy) oldest
  | otherwise = Nothing

-- | Record a submission's @count@ novel accepted txs at @now@: one more
-- entry, or the last one's count raised when it bears the same time.
-- Then the entries outside the window at @now@ go, since the clock only
-- advances, and the oldest go while the rest still reach 'txSourceCount':
-- enough to answer 'isSource' and no more, at most one entry per
-- accepting submission inside the window.
recordNovel :: TxDecisionPolicy -> Time -> Int -> StrictSeq (Time, Int) -> StrictSeq (Time, Int)
recordNovel policy now count novelAt
  | n <= 0 || count <= 0 = novelAt
  | otherwise            = trim (StrictSeq.dropWhileL stale appended)
  where
    n = txSourceCount policy

    appended =
      case StrictSeq.lookup (StrictSeq.length novelAt - 1) novelAt of
        Just (t, c) | t == now -> StrictSeq.take (StrictSeq.length novelAt - 1) novelAt StrictSeq.|> (now, c + count)
        _                      -> novelAt StrictSeq.|> (now, count)

    stale (t, _) = addTime (txSourceWindow policy) t < now

    trim entries = go entries (List.foldl' (\acc (_, c) -> acc + c) 0 entries)
      where
        go s total =
          case StrictSeq.lookup 0 s of
            Just (_, c) | total - c >= n -> go (StrictSeq.drop 1 s) (total - c)
            _                            -> s

-- | The slow tier's wait before its next txid request:
-- 'txSlowCadence' jittered by 'txSlowJitter', so slow peers do not fire
-- together.  The jitter is symmetric: the mean is the cadence, the least
-- wait @cadence * (1 - jitter)@.
slowWait :: TxDecisionPolicy
         -> Double  -- ^ a uniform draw in @[0, 1)@
         -> DiffTime
slowWait TxDecisionPolicy { txSlowCadence, txSlowJitter } u =
    txSlowCadence * realToFrac (1 + txSlowJitter * (2 * u - 1))

-- | One step of a SplitMix generator: a uniform draw in @[0, 1)@ and the
-- next seed.  Kept as a 'Word64' in the peer's local state so the draw
-- touches no shared variable; the generator is rebuilt from the seed each
-- time.
nextJitter :: Word64 -> (Double, Word64)
nextJitter seed = (u, fst (unseedSMGen g'))
  where
    (u, g') = nextDouble (mkSMGen seed)

-- | A peer's rank in the salted order: its address hashed with the salt
-- and passed through a SplitMix step, so the order is uniform whatever the
-- address type's hash makes of small salts, then its registration number
-- for ties.
tierRank :: Hashable addr => Salt -> (addr, Word64) -> (Word64, Word64)
tierRank salt (addr, n) =
    (fst (nextWord64 (mkSMGen (fromIntegral (hashWithSalt salt addr)))), n)

-- | The fast tier from the salted order: the first @k@ keys by 'tierRank',
-- all of them when there are no more than @k@.
orderFast :: Hashable addr => Int -> Salt -> [(addr, Word64)] -> [(addr, Word64)]
orderFast k salt
  | k <= 0    = const []
  | otherwise = take k . sortOn (tierRank salt)

-- | A peer's standing from whether it is a local root, the coin's tier for
-- it and its effective tier: root first, coin before source.
standingOf :: Bool -> PeerTier -> PeerTier -> TierStanding
standingOf root coin effective
  | root                  = StandingRoot
  | coin == FastTier      = StandingCoin
  | effective == FastTier = StandingSource
  | otherwise             = StandingSlow

-- | The standing of a peer's place in the tiers at a time, as the
-- registry keeps it: a source until its deadline.
tierStanding :: Time -> PeerTierState -> TierStanding
tierStanding now PeerTierState { ptsCoin, ptsRoot, ptsSourceUntil } =
    standingOf ptsRoot ptsCoin (if sourceNow then FastTier else ptsCoin)
  where
    sourceNow = maybe False (>= now) ptsSourceUntil

-- | The census over the live peers' standings.
censusOf :: [TierStanding] -> TierCensus
censusOf = foldr step (TierCensus 0 0 0 0)
  where
    step StandingRoot   c = c { censusFastByRoot   = censusFastByRoot   c + 1 }
    step StandingCoin   c = c { censusFastByCoin   = censusFastByCoin   c + 1 }
    step StandingSource c = c { censusFastBySource = censusFastBySource c + 1 }
    step StandingSlow   c = c { censusSlow         = censusSlow         c + 1 }

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
-- arrival takes its place.  A peer enters the order at its first decision,
-- after the server's initial delay, so a peer not polling yet holds no
-- fast slot.  Nothing a peer sends can move another peer down.
--
-- The pure core lives here; "Ouroboros.Network.TxSubmission.Inbound.V2.State"
-- applies the gate and "Ouroboros.Network.TxSubmission.Inbound.V2.Registry"
-- runs the coin.
module Ouroboros.Network.TxSubmission.Inbound.V2.Tier
  ( -- * The effective tier
    effectiveTier
  , isSource
  , sourceCount
  , recordNovel
    -- * The slow tier's wait
  , slowWait
  , nextJitter
    -- * The salted order
  , orderFast
  , tierRank
    -- * The census
  , standingOf
  , censusOf
  ) where

import Control.Monad.Class.MonadTime.SI
import Data.List (sortOn)
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

-- | How many novel accepted txs inside 'txSourceWindow' make a source:
-- 'txSourceRate' sustained over the window, rounded up.  Zero when the
-- rule is off.
sourceCount :: TxDecisionPolicy -> Int
sourceCount TxDecisionPolicy { txSourceRate, txSourceWindow }
  | txSourceRate <= 0 = 0
  | otherwise         = ceiling (txSourceRate * realToFrac txSourceWindow)

-- | A peer is a source while it has delivered 'sourceCount' novel accepted
-- txs and the oldest of those is still inside the window.
isSource :: TxDecisionPolicy -> Time -> StrictSeq Time -> Bool
isSource policy now novelAt =
    n >= 1
 && StrictSeq.length novelAt >= n
 && case StrictSeq.lookup (StrictSeq.length novelAt - n) novelAt of
         Just oldest -> oldest `addTime'` txSourceWindow policy >= now
         Nothing     -> False
  where
    n = sourceCount policy
    addTime' t d = addTime d t

-- | Record @count@ novel accepted txs at @now@, keeping the newest
-- 'sourceCount' of them: enough to answer 'isSource' and no more.
recordNovel :: TxDecisionPolicy -> Time -> Int -> StrictSeq Time -> StrictSeq Time
recordNovel policy now count novelAt
  | n <= 0 || count <= 0 = novelAt
  | otherwise =
      let novelAt' = novelAt <> StrictSeq.fromList (replicate (min n count) now)
      in StrictSeq.drop (StrictSeq.length novelAt' - n) novelAt'
  where
    n = sourceCount policy

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

-- | The census over the live peers' standings.
censusOf :: [TierStanding] -> TierCensus
censusOf = foldr step (TierCensus 0 0 0 0)
  where
    step StandingRoot   c = c { censusFastByRoot   = censusFastByRoot   c + 1 }
    step StandingCoin   c = c { censusFastByCoin   = censusFastByCoin   c + 1 }
    step StandingSource c = c { censusFastBySource = censusFastBySource c + 1 }
    step StandingSlow   c = c { censusSlow         = censusSlow         c + 1 }

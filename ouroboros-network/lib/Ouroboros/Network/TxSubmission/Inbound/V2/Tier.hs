{-# LANGUAGE NamedFieldPuns #-}

-- | The inbound tx-submission fetch tiers.
--
-- A relay polls every inbound peer for txids at about one exchange per round
-- trip, and under load that polling is most of its CPU while nearly every
-- txid it carries is a duplicate.  The tiers bound the set of peers polled
-- at full speed without judging peers on what they deliver: local roots are
-- always fast; a peer that is itself a tx source ('isSource') is fast while
-- it produces; 'txFastPeers' inbound peers drawn at random ('drawFastTier')
-- are fast until the next draw; everyone else waits 'slowWait' between
-- txid requests, blocking ones included.  Promotion is upward only:
-- nothing a peer sends can move another peer down.
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
    -- * The coin
  , drawFastTier
  ) where

import Control.Monad.Class.MonadTime.SI
import Data.Sequence qualified as Seq
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Word (Word64)
import System.Random (RandomGen, uniformR)
import System.Random.SplitMix (mkSMGen, nextDouble, unseedSMGen)

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

-- | The coin: @k@ of the keys, uniformly without replacement, all of them
-- when there are no more than @k@.
drawFastTier :: RandomGen g => Int -> g -> [k] -> ([k], g)
drawFastTier k g0 keys
  | k >= n    = (keys, g0)
  | k <= 0    = ([], g0)
  | otherwise = go 0 g0 (Seq.fromList keys)
  where
    n = length keys
    -- partial Fisher–Yates: positions [0, k) are the draw
    go i g s
      | i >= k    = (foldr (:) [] (Seq.take k s), g)
      | otherwise =
          let (j, g') = uniformR (i, n - 1) g
              si = Seq.index s i
              sj = Seq.index s j
              s' = Seq.update i sj (Seq.update j si s)
          in go (i + 1) g' s'

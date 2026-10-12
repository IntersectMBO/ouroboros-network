{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The burst detector against a replay of its events: grants, changes of
-- the credited queue and sweeps, at strictly increasing instants, folded
-- through the detector and through an oracle written from the rules.
--
module Test.Mux.Burst (tests) where

import Control.Monad.Class.MonadTime.SI

import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)

import Network.Mux.Egress.Burst

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Network.Mux.Egress.Burst"
    [ testProperty "the records are the replay's"               prop_replay
    , testProperty "every credited grant is in exactly one record" prop_partition
    , testProperty "no record is longer than the cap"            prop_cap
    , testProperty "without counting grants there is no burst"   prop_no_credited
    ]

-- | One thing the bucket tells the detector, at an offset from the start.
data Event = Event {
    evAt   :: !DiffTime,
    evWhat :: !What
  }
  deriving (Show, Eq)

data What =
    EGrant !GrantEv
  | EQueued !Int      -- ^ the credited bearers queued, after the change
  | ESweep
  deriving (Show, Eq)

data GrantEv = GrantEv {
    gvTier         :: !Word8,
    gvBearer       :: !Word64,
    gvBytes        :: !Int,
    gvWaitTokens   :: !DiffTime,
    gvWaitWritable :: !DiffTime
  }
  deriving (Show, Eq)

-- | Events at strictly increasing offsets from the start, and the gap.
data Script = Script {
    scGap    :: !DiffTime,
    scEvents :: ![Event]
  }
  deriving Show

-- | Spacings that straddle the gap, land on it exactly, and now and then
-- pass the cap; each but the exact one a microsecond off a round number,
-- so no two events coincide and the exact one is the only equality.
genSpacing :: DiffTime -> Gen DiffTime
genSpacing gap = frequency
  [ (6, off . ms <$> choose (1, 50))
  , (3, (\d -> off (gap + ms d)) <$> choose (-30, 30))
  , (2, pure gap)
  , (2, off . ms <$> choose (200, 5000))
  , (1, off . ms <$> choose (30_000, 70_000)) ]
  where
    ms :: Int -> DiffTime
    ms n = fromIntegral n / 1000
    off d = d + 0.000_001

genEvent :: DiffTime -> Gen Event
genEvent t = Event t <$> frequency
  [ (6, EGrant <$> (GrantEv <$> frequency [ (4, choose (0, 3)), (2, pure 4), (1, pure 255), (1, pure 7) ]
                            <*> choose (0, 5)
                            <*> frequency [ (3, choose (1, burstGrantFloor - 1))
                                          , (1, pure burstGrantFloor)
                                          , (6, choose (burstGrantFloor, 65536)) ]
                            <*> (realToFrac <$> choose (0, 2 :: Double))
                            <*> (realToFrac <$> choose (0, 0.5 :: Double))))
  , (2, EQueued <$> choose (0, 3))
  , (1, pure ESweep) ]

instance Arbitrary Script where
  arbitrary = do
    gap <- elements [0.1, 0.2, 0.5]
    n   <- choose (0, 60)
    spacings <- vectorOf n (genSpacing gap)
    Script gap <$> mapM genEvent (scanl1 (+) spacings)
  shrink Script { scGap, scEvents } =
       [ Script scGap es | es <- shrinkList shrinkEvent scEvents ]
    where
      shrinkEvent e@Event { evWhat = EGrant g@GrantEv { gvTier, gvBytes } }
        | gvTier /= 4                = [ e { evWhat = EGrant g { gvTier = 4 } } ]
        | gvBytes /= burstGrantFloor = [ e { evWhat = EGrant g { gvBytes = burstGrantFloor } } ]
      shrinkEvent _ = []

-- | The detector run over the script, closed by a sweep well after the end.
runDetector :: Script -> [Burst]
runDetector Script { scGap, scEvents } =
    fst (takeBursts (burstSweep scGap (at (far scEvents)) (foldl' step emptyDetector scEvents)))
  where
    step d Event { evAt, evWhat } = case evWhat of
      EGrant GrantEv { gvTier, gvBearer, gvBytes, gvWaitTokens, gvWaitWritable } ->
        burstGrant scGap (at evAt) gvTier gvBearer gvBytes gvWaitTokens gvWaitWritable d
      EQueued n -> burstQueued scGap (at evAt) n d
      ESweep    -> burstSweep scGap (at evAt) d

at :: DiffTime -> Time
at d = d `addTime` Time 0

-- | An instant after the last event by more than the cap and the gap, with
-- nothing queued by then: whatever is open is over.
far :: [Event] -> DiffTime
far es = maximum (0 : map evAt es) + burstCap + 1

-- | The oracle: the counting grants, credited and at least the floor, cut
-- into runs. A run ends at the first event (the final sweep included) that
-- comes more than the gap after the run's last grant while nothing credited
-- is queued, or more than the cap after its first; the next counting grant
-- starts the next run. Filler is what was granted without counting while a
-- run was open, and the peak is the largest credited queue told to the
-- detector while it was.
oracle :: Script -> [Burst]
oracle Script { scGap, scEvents } = go 0 Nothing (scEvents ++ [Event (far scEvents) ESweep])
  where
    go _ _ [] = []
    go queued open (Event { evAt = t, evWhat } : es) =
      let -- is the open run over at this event?
          (closed, open') = case open of
            Just r | t - rLast r > scGap && queued == 0 -> ([finish False r], Nothing)
                   | t - rStart r > burstCap            -> ([finish True r], Nothing)
            _ -> ([], open)
      in closed ++ case evWhat of
           EGrant GrantEv { gvTier, gvBearer, gvBytes, gvWaitTokens, gvWaitWritable }
             | creditedTier gvTier && gvBytes >= burstGrantFloor ->
                 let n = fromIntegral gvBytes
                     r = case open' of
                       Nothing -> Run t t n 1 (Map.singleton gvTier (TierGrants n 1)) 0
                                      (Set.singleton gvBearer) queued gvWaitTokens gvWaitWritable
                       Just r0 -> r0 { rLast = t, rBytes = rBytes r0 + n, rBatches = rBatches r0 + 1
                                     , rTiers = Map.insertWith (<>) gvTier (TierGrants n 1) (rTiers r0)
                                     , rBearers = Set.insert gvBearer (rBearers r0)
                                     , rWaitTokens = max (rWaitTokens r0) gvWaitTokens
                                     , rWaitWritable = rWaitWritable r0 + gvWaitWritable }
                 in go queued (Just r) es
             | otherwise ->
                 go queued (fmap (\r -> r { rFiller = rFiller r + fromIntegral gvBytes }) open') es
           EQueued n ->
             go n (fmap (\r -> r { rPeak = max (rPeak r) n }) open') es
           ESweep -> go queued open' es

    finish capped Run { rStart, rLast, rBytes, rBatches, rTiers, rFiller, rBearers, rPeak
                      , rWaitTokens, rWaitWritable } =
      Burst { bStart = at rStart, bEnd = at rLast, bCapped = capped, bBytes = rBytes
            , bBatches = rBatches, bTiers = rTiers, bFiller = rFiller
            , bWidth = Set.size rBearers, bPeakQueued = rPeak
            , bWaitTokens = rWaitTokens, bWaitWritable = rWaitWritable }

data Run = Run {
    rStart, rLast :: !DiffTime,
    rBytes        :: !Word64,
    rBatches      :: !Word64,
    rTiers        :: !(Map.Map Word8 TierGrants),
    rFiller       :: !Word64,
    rBearers      :: !(Set.Set Word64),
    rPeak         :: !Int,
    rWaitTokens   :: !DiffTime,
    rWaitWritable :: !DiffTime
  }

-- | The grants that open or extend a burst.
countingGrants :: Script -> [Event]
countingGrants Script { scEvents } =
  [ e | e@Event { evWhat = EGrant GrantEv { gvTier, gvBytes } } <- scEvents
      , creditedTier gvTier, gvBytes >= burstGrantFloor ]

prop_replay :: Script -> Property
prop_replay sc =
    classify (length got > 1) "more than one record" $
    classify (any bCapped got) "a record closed by the cap" $
    classify (any ((> 0) . bFiller) got) "filler counted" $
    classify (any (\Event { evWhat } -> case evWhat of
                      EGrant GrantEv { gvTier, gvBytes } -> creditedTier gvTier && gvBytes < burstGrantFloor
                      _ -> False) (scEvents sc)) "a credited grant under the floor" $
    classify (any ((> 1) . bWidth) got) "more than one bearer in a record" $
    classify (any ((> 0) . bPeakQueued) got) "a credited queue inside a record" $
    counterexample (show got) $
      got === oracle sc
  where
    got = runDetector sc

-- | The records are disjoint, in order, start and end at counting grants,
-- and between them hold every counting grant once.
prop_partition :: Script -> Property
prop_partition sc =
    counterexample (show got) $
         counterexample "ordered" (and (zipWith (\a b -> bEnd a < bStart b) got (drop 1 got)))
    .&&. counterexample "bounds are grants"
           (all (\b -> bStart b `elem` instants && bEnd b `elem` instants) got)
    .&&. counterexample "each grant in one record"
           (all (\t -> length [ () | b <- got, bStart b <= t, t <= bEnd b ] == 1) instants)
    .&&. counterexample "batches add up"
           (sum (map bBatches got) === fromIntegral (length instants))
  where
    got = runDetector sc
    instants = map (at . evAt) (countingGrants sc)

prop_cap :: Script -> Property
prop_cap sc =
    counterexample (show got) $
      all (\b -> bEnd b `diffTime` bStart b <= burstCap) got
  where
    got = runDetector sc

-- | A peer catching up at rest rank, however long, or a credited one that
-- only ever sends small things: no burst.
prop_no_credited :: Script -> Bool -> Property
prop_no_credited sc@Script { scEvents } small =
    classify small "small grants instead of rest rank" $
    counterexample (show got) $ got === []
  where
    quiet e@Event { evWhat } = case evWhat of
      EGrant g@GrantEv { gvTier, gvBytes }
        | creditedTier gvTier ->
            e { evWhat = EGrant (if small then g { gvBytes = min gvBytes (burstGrantFloor - 1) }
                                          else g { gvTier = 4 }) }
      _ -> e
    got = runDetector sc { scEvents = map quiet scEvents }

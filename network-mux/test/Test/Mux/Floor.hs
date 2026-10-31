{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The floor's pure core against an independent start-time fair queue.
--
module Test.Mux.Floor (tests) where

import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import Network.Mux.Egress.Floor

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Network.Mux.Egress.Floor"
    [ testProperty "tiers 1 to 3 map onto the classes in order, no other tier has one" prop_floorClass_exhaustive
    , testProperty "the pick is the earliest start, lowest class on a tie, that class's head" prop_pick
    , testProperty "a grant starts at the start tag and moves the clock forward" prop_grant_clock
    , testProperty "grant sequence matches the reference" prop_matches_reference
    , testProperty "bytes across classes are fair"        prop_bytes_fair
    , testProperty "an idle class banks nothing"          prop_idle_banks_nothing
    , testProperty "the class heads are the first of each class" prop_class_heads
    ]

--
-- Generators: a scale for batch sizes, and per class a backlog of batches in
-- arrival order; a schedule interleaves arrivals with services so the
-- reference and the floor see the same dynamics, idle gaps included.
--

newtype Scale = Scale Int deriving Show

instance Arbitrary Scale where
  arbitrary = Scale <$> frequency [ (3, choose (1, 200_000)), (1, choose (1, 10)) ]
  shrink (Scale q) = [ Scale q' | q' <- shrink q, q' >= 1 ]

-- | A batch: a single SDU of control, a chunk, or bulk, relative to a scale.
genNeed :: Int -> Gen Int
genNeed q = frequency [ (4, choose (1, max 1 (q `div` 16)))
                      , (2, choose (1, max 1 q))
                      , (1, choose (1, 2 * q)) ]

data Op = Arrive Word8 Int   -- ^ a bearer of the class joins with a batch of this many bytes
        | Serve              -- ^ the floor grants one batch
  deriving (Eq, Show)

data Schedule = Schedule Int [Op] deriving Show

instance Arbitrary Schedule where
  arbitrary = do
    Scale q <- arbitrary
    n <- choose (0, 60)
    ops <- vectorOf n $ frequency
             [ (2, Arrive <$> choose (0, 2) <*> genNeed q)
             , (3, pure Serve) ]
    return (Schedule q ops)
  shrink (Schedule q ops) =
       [ Schedule q' ops | Scale q' <- shrink (Scale q) ]
    ++ [ Schedule q ops' | ops' <- shrinkList shrinkOp ops ]
    where
      shrinkOp (Arrive c need) = [ Arrive c need' | need' <- shrink need, need' >= 1 ]
      shrinkOp Serve           = []

--
-- The reference: textbook start-time fair queuing over lists. Written
-- without the module under test: its own queues, its own tags, its own
-- virtual time.
--

data Ref = Ref {
    rVirtual :: Integer,
    rFinish  :: Map Int Integer,
    rQueues  :: Map Int [Int]           -- ^ per class, needs in arrival order
  }

refNew :: Ref
refNew = Ref 0 Map.empty Map.empty

refArrive :: Int -> Int -> Ref -> Ref
refArrive c need r = r { rQueues = Map.insertWith (flip (++)) c [need] (rQueues r) }

-- | One grant, or Nothing when every queue is empty.
refServe :: Ref -> Maybe ((Int, Int), Ref)
refServe r =
  case [ (max (rVirtual r) (Map.findWithDefault 0 c (rFinish r)), c, need)
       | (c, need : _) <- Map.toAscList (rQueues r) ] of
       [] -> Nothing
       candidates ->
         let (s, c, need) = minimum candidates      -- earliest start, then lowest class
         in Just ((c, need), r { rVirtual = s
                                , rFinish  = Map.insert c (s + fromIntegral need) (rFinish r)
                                , rQueues  = Map.adjust tail c (rQueues r) })

refRun :: Schedule -> [(Int, Int)]
refRun (Schedule _ ops) = go refNew ops
  where
    go _ [] = []
    go r (Arrive c need : rest) = go (refArrive (fromIntegral c) need r) rest
    go r (Serve : rest) =
      case refServe r of
           Nothing      -> go r rest
           Just (g, r') -> g : go r' rest

--
-- Driving the floor: queues of (ticket, need) per class, heads to floorPick,
-- floorGranted after each grant.
--

type Queues = Map FloorClass [(Int, Int)]   -- ^ (ticket, need), arrival order

heads :: Queues -> Map FloorClass (Int, Int)
heads = Map.mapMaybe (\q -> case q of { (h : _) -> Just h; [] -> Nothing })

floorRun :: Schedule -> [(Int, Int)]
floorRun (Schedule _ ops) = go newFloorState Map.empty (0 :: Int) ops
  where
    go _ _ _ [] = []
    go st qs t (Arrive c need : rest) =
      go st (Map.insertWith (flip (++)) (FloorClass c) [(t, need)] qs) (t + 1) rest
    go st qs t (Serve : rest) =
      case floorPick st (heads qs) of
           Nothing -> go st qs t rest
           Just (c@(FloorClass ci), _) ->
             case fromMaybe [] (Map.lookup c qs) of
                  [] -> error "floorRun: picked an empty class"
                  (_, need) : others ->
                    (fromIntegral ci, need) : go (floorGranted c need st) (Map.insert c others qs) t rest

--
-- Properties
--

-- | 'floorClass' on every tier at once: the tiers with a class are exactly
-- 1 to 3, partners, pool relays and strangers with credit, and their classes
-- are 'floorClasses' in tier order. One statement over all 256 inputs, so
-- credited-and-no-other, distinctness, order and coverage of the classes
-- are checked together and nothing is drawn at random.
-- | A floor queue: waiters keyed by class and ticket, tickets unique.
newtype FloorQueue = FloorQueue (Map (FloorClass, Int) Int)
  deriving Show

instance Arbitrary FloorQueue where
    arbitrary = do
      n <- choose (0, 12)
      tickets <- nub <$> vectorOf n (choose (0, 40))
      FloorQueue . Map.fromList
        <$> mapM (\t -> (\c need -> ((FloorClass c, t), need)) <$> choose (0, 2) <*> choose (1, 1000)) tickets
    shrink (FloorQueue q) = [ FloorQueue (Map.fromList kvs) | kvs <- shrinkList (const []) (Map.toList q) ]

-- | 'classHeads' finds what a walk over the whole queue finds: for each class
-- with a waiter, its lowest ticket and that waiter's bytes; nothing for a
-- class without one.
prop_class_heads :: FloorQueue -> Property
prop_class_heads (FloorQueue q) =
    classify (Map.size walk == 3) "every class waiting" $
    classify (Map.size walk == 1) "one class waiting" $
    classify (Map.null q)         "nobody waiting" $
      classHeads q === walk
  where
    walk = Map.fromListWith (\_new old -> old) [ (c, (t, need)) | ((c, t), need) <- Map.toAscList q ]

prop_floorClass_exhaustive :: Property
prop_floorClass_exhaustive = once $
  let classed = [ (t, c) | t <- [minBound .. maxBound :: Word8], Just c <- [floorClass t] ]
  in map fst classed === [1, 2, 3]
     .&&. map snd classed === floorClasses
     .&&. property (and (zipWith (<) floorClasses (drop 1 floorClasses)))

-- | The pick is the definition of the floor's order, stated directly: with
-- no heads there is none; otherwise it names a class whose start tag is no
-- later than any other head's, the lower class on a tie, and that class's
-- head. The generator is held to producing the case where the choice
-- matters.
prop_pick :: Schedule -> Property
prop_pick sched =
  let (st, qs) = stateAfter sched
      hs = heads qs
      tags = [ startTag st c' | c' <- Map.keys hs ]
  in cover 50 (not (Map.null hs)) "a head to pick" $
     cover 20 (Map.size hs >= 2) "a choice between classes" $
     cover 3 (length (nub tags) < length tags) "a tie between classes" $
     checkCoverage $
     case floorPick st hs of
          Nothing -> property (Map.null hs)
          Just (c, t) ->
            counterexample ("picked " ++ show c ++ " with start " ++ show (startTag st c)
                            ++ " among " ++ show [ (c', startTag st c') | c' <- Map.keys hs ])
            $ conjoin [ (startTag st c, c) <= (startTag st c', c') | c' <- Map.keys hs ]
              .&&. fmap fst (Map.lookup c hs) === Just t

-- | The floor's grant sequence is the textbook's, arrivals interleaved.
prop_matches_reference :: Schedule -> Property
prop_matches_reference sched = floorRun sched === refRun sched

-- | With every class backlogged, after any number of grants no class is more
-- than a batch of its own and a batch of the other's behind another.
prop_bytes_fair :: Scale -> Positive Int -> Property
prop_bytes_fair (Scale q) (Positive n) =
  forAllShrink (vectorOf 3 (listOf1 (genNeed q))) (filter (all (not . null)) . shrink) $ \backlogs ->
    let maxNeed = maximum (concat backlogs)
        sched  = Schedule q ([ Arrive c need | (c, bl) <- zip [0 ..] backlogs, need <- cycleTo n bl ]
                             ++ replicate n Serve)
        grants = floorRun sched
        totals = scanl (\m (c, need) -> Map.insertWith (+) c need m) Map.empty grants
        spread m = if Map.size m < 3 then 0 else maximum (Map.elems m) - minimum (Map.elems m)
    in counterexample (show (map spread totals))
     $ all (\m -> spread m <= 2 * maxNeed) totals
  where
    cycleTo k xs = take k (cycle xs)

-- | A class that returns after sitting out some grants is served as if it
-- had just arrived: within its own batch and one of each other class.
prop_idle_banks_nothing :: Scale -> Positive Int -> Property
prop_idle_banks_nothing (Scale q) (Positive k) =
  forAll ((,) <$> genNeed q <*> genNeed q) $ \(a, b) ->
    let busy   = [ Arrive 0 a, Arrive 1 b ]
        -- class 2 arrives, is served once, then sits out k rounds of the others
        sched  = Schedule q (busy ++ [Arrive 2 a, Serve, Serve, Serve]
                             ++ concat (replicate k (busy ++ [Serve, Serve]))
                             ++ [Arrive 2 a] ++ busy ++ [Serve, Serve, Serve])
        grants = floorRun sched
        -- the last three grants: class 2 gets exactly one of them
        lastThree = drop (length grants - 3) grants
    in counterexample (show lastThree)
     $ length (filter ((== 2) . fst) lastThree) === 1

-- | The floor state and queues after a schedule, for the pick laws.
stateAfter :: Schedule -> (FloorState, Queues)
stateAfter (Schedule _ ops) = foldl' step (newFloorState, Map.empty) (zip [0 ..] ops)
  where
    step (st, qs) (t, Arrive c need) = (st, Map.insertWith (flip (++)) (FloorClass c) [(t, need)] qs)
    step (st, qs) (_, Serve) =
      case floorPick st (heads qs) of
           Nothing -> (st, qs)
           Just (c, _) ->
             case fromMaybe [] (Map.lookup c qs) of
                  [] -> error "stateAfter: picked an empty class"
                  (_, need) : others -> (floorGranted c need st, Map.insert c others qs)

-- | The transition the pick is measured against. Granting the pick @n@ bytes
-- makes the floor's virtual time the class's start tag, moves that class's
-- next start exactly @n@ bytes past it, and never moves any other class's
-- start backwards. This is what lets a wrong start tag fail here rather than
-- only in the grant sequence.
prop_grant_clock :: Schedule -> Property
prop_grant_clock (Schedule q ops0) =
  -- end on an arrival, so there is always a head to grant and nothing to
  -- discard
  forAll ((,) <$> choose (0, 2) <*> genNeed q) $ \(cl, need0) ->
  let sched = Schedule q (ops0 ++ [Arrive cl need0])
      (st, qs) = stateAfter sched
      hs = heads qs
  in case floorPick st hs of
          Nothing -> counterexample "no head after an arrival" False
          Just (c, _) ->
            let need = maybe 0 snd (Map.lookup c hs)
                st'  = floorGranted c need st
                s0   = startTag st c
            in counterexample (show (st, c, need, st'))
             $ conjoin
                 [ fsVirtual st' === s0
                 , startTag st' c === s0 + fromIntegral need
                 , property (fsVirtual st' >= fsVirtual st)
                 , conjoin [ property (startTag st' c' >= startTag st c') | c' <- floorClasses ]
                 ]

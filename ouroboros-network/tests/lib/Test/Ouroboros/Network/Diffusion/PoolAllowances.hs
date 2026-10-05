{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ouroboros.Network.Diffusion.PoolAllowances (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Monad.IOSim (runSimOrThrow)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Ouroboros.Network.Diffusion.PoolAllowances

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.PoolAllowances"
    [ testProperty "agrees with the token bucket after every step" prop_oracle
    , testProperty "credit within bounds and monotone in fresh"     prop_bounds
    , testProperty "nothing charged at zero credit"                  prop_zero
    , testProperty "a reset bucket reads the capacity"               prop_reset
    , testProperty "the clock reading starts at zero and grows"      prop_clock
    , testProperty "behind the lock it reads zero and takes no charge" prop_locked
    , testProperty "opened later, never more credit"                 prop_later_never_more
    , testProperty "a connection's rule follows every reindex"        prop_rule_follows_reindex
    ]

-- | What happens to a bucket: fresh bytes are announced, or bytes are served.
data Op = Advance Int | Charge Int
  deriving Show

-- | How a bucket starts: full, or empty behind a lock of so many fresh bytes.
data Start = Full | Locked Int
  deriving Show

data Run = Run {
    rAllowance :: Allowance,
    rStart     :: Start,
    rOps       :: [Op]
  }
  deriving Show

instance Arbitrary Run where
  arbitrary = do
    fmax  <- choose (1_000, 20_000_000)
    start <- frequency [ (1, pure Full), (2, Locked <$> choose (0, 3 * fmax)) ]
    n     <- choose (0, 200)
    ops   <- vectorOf n $ frequency
               [ (3, Advance <$> choose (1, fmax))        -- up to one object at a time
               , (3, Charge  <$> choose (1, 2 * fmax)) ]  -- up to a whole capacity at a time
    return Run { rAllowance = Allowance fmax, rStart = start, rOps = ops }
  shrink Run { rAllowance = Allowance fmax, rStart, rOps } =
       [ Run (Allowance fmax) rStart ops' | ops' <- shrinkList shrinkOp rOps ]
    ++ [ Run (Allowance f) rStart rOps    | f <- shrink fmax, f >= 1 ]
    ++ [ Run (Allowance fmax) s rOps      | s <- shrinkStart rStart ]
    where
      shrinkOp (Advance d) = [ Advance d' | d' <- shrink d, d' >= 1 ]
      shrinkOp (Charge n)  = [ Charge n'  | n' <- shrink n, n' >= 1 ]
      shrinkStart Full       = []
      shrinkStart (Locked l) = Full : [ Locked l' | l' <- shrink l, l' >= 0 ]

-- | The oracle: an explicit token bucket. Tokens start full, or κ of the lock
-- in debt, an advance adds κ of the bytes capped at the capacity, a charge
-- subtracts while tokens are positive and may run them into debt, and the
-- credit shown is the tokens floored at zero. Refills follow κ of the
-- cumulative reading so the rounding is the implementation's.
oracle :: Allowance -> Start -> [Op] -> [Int]
oracle al start = go (tokens0 start) (Fresh 0)
  where
    tokens0 Full       = capacity al
    tokens0 (Locked l) = negate (kappa (Fresh l))
    go _ _ [] = []
    go tokens fresh (op : ops) =
      case op of
           Advance d ->
             let fresh'  = Fresh (unFresh fresh + d)
                 tokens' = min (capacity al) (tokens + kappa fresh' - kappa fresh)
             in max 0 tokens' : go tokens' fresh' ops
           Charge n ->
             let tokens' = if tokens > 0 then tokens - n else tokens
             in max 0 tokens' : go tokens' fresh ops

-- | The implementation, stepped the same way.
impl :: Allowance -> Start -> [Op] -> [Int]
impl al start = go (Fresh 0) (charged0 start)
  where
    charged0 Full       = resetCharged al (Fresh 0)
    charged0 (Locked l) = lockedCharged (Fresh 0) (Fresh l)
    go _ _ [] = []
    go fresh ch (op : ops) =
      case op of
           Advance d ->
             let fresh' = Fresh (unFresh fresh + d)
             in credit al fresh' ch : go fresh' ch ops
           Charge n ->
             let ch' = charge al fresh n ch
             in credit al fresh ch' : go fresh ch' ops

prop_oracle :: Run -> Property
prop_oracle Run { rAllowance, rStart, rOps } =
  let got = impl rAllowance rStart rOps
      zeros = length (filter (== 0) got)
      locked = case rStart of { Locked _ -> True; Full -> False }
  in classify (zeros > 0) "runs dry at some point"
   . classify (zeros > length got `div` 2) "dry more than half the time"
   . classify (any (== capacity rAllowance) got) "reads the capacity at some point"
   . classify locked "starts locked"
   . classify (locked && any (> 0) got) "opens during the run"
   $ got === oracle rAllowance rStart rOps

-- | A reading and a charged total, unrelated: any point of the state space.
data Point = Point Allowance Fresh Charged
  deriving Show

instance Arbitrary Point where
  arbitrary = do
    fmax <- choose (1_000, 20_000_000)
    f    <- choose (0, 100 * fmax)
    c    <- choose (-10 * fmax, 200 * fmax)
    return (Point (Allowance fmax) (Fresh f) (Charged c))
  shrink (Point (Allowance fmax) (Fresh f) (Charged c)) =
       [ Point (Allowance fmax) (Fresh f') (Charged c) | f' <- shrink f, f' >= 0 ]
    ++ [ Point (Allowance fmax) (Fresh f) (Charged c') | c' <- shrink c ]

prop_bounds :: Point -> NonNegative Int -> Property
prop_bounds (Point al fresh ch) (NonNegative d) =
  let now   = credit al fresh ch
      later = credit al (Fresh (unFresh fresh + d)) ch
  in counterexample (show (now, later))
   $ now >= 0 .&&. now <= capacity al .&&. later >= now

prop_zero :: Point -> Positive Int -> Positive Int -> Property
prop_zero (Point al fresh _) (Positive k) (Positive n) =
  let ch = Charged (kappa fresh + k)          -- past what was granted: dry
  in credit al fresh ch === 0 .&&. charge al fresh n ch === ch

prop_reset :: Point -> Property
prop_reset (Point al fresh _) =
  credit al fresh (resetCharged al fresh) === capacity al

-- | Behind the lock the bucket reads zero and a charge changes nothing, at
-- any reading up to the lock's end. Lock, age and charge are drawn to the
-- allowance's scale, a few capacities at most.
prop_locked :: Point -> Property
prop_locked (Point al fresh _) =
  forAllShrink gen shr $ \(lock, age, n) ->
    let ch  = lockedCharged fresh (Fresh lock)
        now = Fresh (unFresh fresh + age)
    in classify (age == lock) "at the lock's end"
     . classify (n > capacity al) "charged more than a capacity"
     $ credit al now ch === 0 .&&. charge al now n ch === ch
  where
    gen = do
      lock <- choose (0, 4 * capacity al)
      age  <- frequency [ (1, pure lock), (4, choose (0, lock)) ]  -- the end is the edge
      n    <- choose (1, 2 * capacity al)
      return (lock, age, n)
    shr (lock, age, n) =
         [ (lock', min age lock', n) | lock' <- shrink lock, lock' >= 0 ]
      ++ [ (lock, age', n)           | age'  <- shrink age,  age'  >= 0 ]
      ++ [ (lock, age, n')           | n'    <- shrink n,    n'    >= 1 ]

-- | A connection opened later never holds more credit than one opened
-- earlier, at any reading after both have opened: reconnecting buys nothing.
prop_later_never_more :: Point -> Property
prop_later_never_more (Point al fresh _) =
  forAllShrink gen shr $ \(lock, d1, d) ->
    let f1    = Fresh (unFresh fresh + d1)
        now   = Fresh (unFresh f1 + d)
        early = credit al now (lockedCharged fresh (Fresh lock))
        late  = credit al now (lockedCharged f1 (Fresh lock))
    in classify (late > 0) "the later one has credit"
     . classify (early == capacity al) "the earlier one is full"
     . counterexample (show (early, late))
     $ late <= early
  where
    gen = (,,) <$> choose (0, 4 * capacity al)
               <*> choose (0, 4 * capacity al)
               <*> choose (0, 4 * capacity al)
    shr (lock, d1, d) =
         [ (lock', d1, d) | lock' <- shrink lock, lock' >= 0 ]
      ++ [ (lock, d1', d) | d1'   <- shrink d1,   d1'   >= 0 ]
      ++ [ (lock, d1, d') | d'    <- shrink d,    d'    >= 0 ]

prop_clock :: Positive Double -> NonNegative Double -> NonNegative Double -> Property
prop_clock (Positive rate) (NonNegative t0) (NonNegative dt) =
  let origin = Time (realToFrac t0)
      later  = Time (realToFrac (t0 + dt))
  in freshAt rate origin origin === Fresh 0 .&&. freshAt rate origin later >= Fresh 0
     .&&. freshAt rate origin later >= freshAt rate origin origin

-- | A pool list of @n@ positions, and steps: the index swapped in, by address,
-- and the positions holding credit at that step.
data Reindexes = Reindexes Int [(Map Int [Int], [Int])]
  deriving Show

instance Arbitrary Reindexes where
  arbitrary = do
    n     <- choose (1, 4)
    steps <- resize 6 $ listOf1 $ do
               idx <- Map.fromList <$> (sublistOf addresses >>= mapM (\a -> (,) a <$> sublistOf [0 .. n - 1]))
               (,) idx <$> sublistOf [0 .. n - 1]
    return (Reindexes n steps)
  shrink (Reindexes n steps) =
    [ Reindexes n steps' | steps' <- shrinkList (const []) steps, not (null steps') ]

addresses :: [Int]
addresses = [0 .. 4]

-- | A connection's rule follows every reindex: after each one, its rank is the
-- one the new index and the buckets' credit give, though the rule was made,
-- and had looked its buckets up, before any of them.
prop_rule_follows_reindex :: Reindexes -> Property
prop_rule_follows_reindex (Reindexes n steps) =
    classify (any gains (zip (Map.empty : map fst steps) (map fst steps))) "an address gains positions" $
    classify (any moved (zip (map fst steps) (drop 1 (map fst steps)))) "positions move, membership kept" $
      observed === expected
  where
    expected = [ [ rankFor Other (not (null ps)) (any (`elem` credited) ps)
                 | a <- addresses, let ps = Map.findWithDefault [] a idx ]
               | (idx, credited) <- steps ]
    observed = runSimOrThrow $ do
      pa <- newPoolAllowances (Allowance 1_000) (const (Fresh 0)) (Fresh 1_000_000)
      atomically $ rebuild pa (Time 0) n Map.empty
      rules <- mapM (\a -> fst <$> mkEgressRule pa [] a (\_ -> return Other) (Time 0)) addresses
      -- every rule looks its buckets up once, before the first reindex
      mapM_ (\r -> atomically (r (Time 0))) rules
      forM steps $ \(idx, credited) -> do
        atomically $ do
          reindex pa idx
          bs <- readTVar (paBuckets pa)
          forM_ (IntMap.toList bs) $ \(i, tv) ->
            writeTVar tv (if i `elem` credited then Charged (-1_000) else Charged 1_000_000_000)
        mapM (\r -> atomically (r (Time 0))) rules
    gains (old, new) = or [ null (Map.findWithDefault [] a old) && not (null ps)
                          | (a, ps) <- Map.toList (new :: Map Int [Int]) ]
    moved (old, new) = or [ not (null ps) && not (null ps') && ps /= ps'
                          | (a, ps) <- Map.toList old, let ps' = Map.findWithDefault [] a new ]

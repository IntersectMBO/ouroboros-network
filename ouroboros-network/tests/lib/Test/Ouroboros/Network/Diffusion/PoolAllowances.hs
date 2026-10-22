{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ouroboros.Network.Diffusion.PoolAllowances
  ( tests
    -- * For the egress rule's tests
  , fullCharged
  , freshAt
  , relaysOf
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_)
import Control.Monad.Class.MonadTime.SI (Time (..), diffTime)
import Control.Monad.IOSim (runSimOrThrow)
import Data.ByteString.Char8 qualified as BSC
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Ouroboros.Network.Diffusion.PoolAllowances
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.PoolAllowances"
    [ testProperty "agrees with the token bucket after every step" prop_oracle
    , testProperty "credit within bounds and monotone in fresh"     prop_bounds
    , testProperty "nothing charged at zero credit"                  prop_zero
    , testProperty "a full bucket reads the capacity"                prop_full
    , testProperty "an empty bucket reads zero and fills at kappa"   prop_empty
    , testProperty "a connection's rule follows every reindex"        prop_rule_follows_reindex
    ]

-- | A full bucket at this reading: the capacity's worth of credit.
fullCharged :: Allowance -> Fresh -> Charged
fullCharged al fresh = Charged (kappa fresh - capacity al)

-- | A clock reading: fresh bytes growing at a rate from an origin, the
-- reading the tests use where production reads consensus's count.
freshAt :: Double -> Time -> Time -> Fresh
freshAt rate t0 now = Fresh (max 0 (floor (rate * realToFrac (now `diffTime` t0) :: Double)))

-- | @n@ pools' relay lists, distinct from every other generation's.
relaysOf :: Int -> Int -> [NonEmpty RelayAccessPoint]
relaysOf generation n =
  [ RelayAccessDomain (BSC.pack ("pool" ++ show generation ++ "-" ++ show i)) 3001 :| []
  | i <- [1 .. n] ]

-- | What happens to a bucket: fresh bytes are announced, or bytes are served.
data Op = Advance Int | Charge Int
  deriving Show

-- | How a bucket starts: full, or empty.
data Start = Full | Empty
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
    start <- elements [Full, Empty]
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
      shrinkStart Full  = []
      shrinkStart Empty = [Full]

-- | The oracle: an explicit token bucket. Tokens start full or at zero, an
-- advance adds κ of the bytes capped at the capacity, a charge subtracts
-- while tokens are positive and may run them into debt, and the credit
-- shown is the tokens floored at zero. Refills follow κ of the cumulative
-- reading so the rounding is the implementation's.
oracle :: Allowance -> Start -> [Op] -> [Int]
oracle al start = go (tokens0 start) (Fresh 0)
  where
    tokens0 Full  = capacity al
    tokens0 Empty = 0
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
    charged0 Full  = fullCharged al (Fresh 0)
    charged0 Empty = emptyCharged (Fresh 0)
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
      empty = case rStart of { Empty -> True; Full -> False }
  in classify (zeros > 0) "runs dry at some point"
   . classify (zeros > length got `div` 2) "dry more than half the time"
   . classify (any (== capacity rAllowance) got) "reads the capacity at some point"
   . classify empty "starts empty"
   . classify (empty && any (> 0) got) "earns credit during the run"
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

prop_full :: Point -> Property
prop_full (Point al fresh _) =
  credit al fresh (fullCharged al fresh) === capacity al

-- | An empty bucket reads zero at its reading, takes no charge there, and
-- at a later reading holds what κ granted in between, up to the capacity.
prop_empty :: Point -> Positive Int -> NonNegative Int -> Property
prop_empty (Point al fresh _) (Positive n) (NonNegative d) =
  let ch    = emptyCharged fresh
      later = Fresh (unFresh fresh + d)
  in counterexample (show (credit al later ch))
   $    credit al fresh ch === 0
   .&&. charge al fresh n ch === ch
   .&&. credit al later ch === min (capacity al) (kappa later - kappa fresh)

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
    expected = [ [ rankFor Other False (if null ps then Nothing else Just (any (`elem` credited) ps))
                 | a <- addresses, let ps = Map.findWithDefault [] a idx ]
               | (idx, credited) <- steps ]
    observed = runSimOrThrow $ do
      pa <- newPoolAllowances (return (Allowance 1_000)) (return (Fresh 0)) 1_000_000
      _  <- atomically $ rebuild pa (relaysOf 0 n) Map.empty
      rules <- mapM (\a -> fst <$> mkEgressRule pa [] a (\_ -> return Other) (Time 0)) addresses
      -- every rule looks its buckets up once, before the first reindex
      mapM_ (\r -> atomically (r (Time 0))) rules
      forM steps $ \(idx, credited) -> do
        atomically $ do
          reindex pa idx
          bs <- readTVar (paBuckets pa)
          forM_ (IntMap.toList bs) $ \(i, (_, tv)) ->
            writeTVar tv (if i `elem` credited then Charged (-1_000) else Charged 1_000_000_000)
        mapM (\r -> atomically (r (Time 0))) rules
    gains (old, new) = or [ null (Map.findWithDefault [] a old) && not (null ps)
                          | (a, ps) <- Map.toList (new :: Map Int [Int]) ]
    moved (old, new) = or [ not (null ps) && not (null ps') && ps /= ps'
                          | (a, ps) <- Map.toList old, let ps' = Map.findWithDefault [] a new ]

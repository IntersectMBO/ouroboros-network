{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ouroboros.Network.Diffusion.EgressRule (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Monad.IOSim (runSimOrThrow)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Word (Word16)
import Network.Mux (MiniProtocolNum (..), Rank (..))
import Network.Mux.Egress.Floor (FloorClass (..), floorClass)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Ouroboros.Network.Diffusion.PoolAllowances

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.EgressRule"
    [ testProperty "a connection's rule and sink follow the decision table" prop_rule
    , testProperty "the floor's classes are the rule's tiers" prop_floorClass_follows_rank
    ]

-- | What happens to a connection, at a time in seconds: its rank is asked,
-- its scheduled bytes on a protocol are reported, or the pool list is
-- rebuilt under it.
data Op = Ask Double
        | Charge Double Word16 Int
        | Rebuild Double
  deriving Show

data Case = Case {
    cStanding  :: Standing,
    cPools     :: [Int],       -- ^ the matched buckets' charged totals; none for a stranger
    cUncharged :: [Word16],
    cLock      :: Double,      -- ^ the stranger lock, in seconds of the clock
    cOps       :: [Op]
  }
  deriving Show

freshMax :: Int
freshMax = 100_000

allowance :: Allowance
allowance = Allowance freshMax

-- | A kilobyte of fresh bytes a second.
clock :: Time -> Fresh
clock = freshAt 1000 (Time 0)

at :: Double -> Time
at t = Time (realToFrac t)

instance Arbitrary Case where
  arbitrary = do
    cStanding  <- frequency [ (1, pure LocalRoot), (1, pure Partner), (4, pure Other) ]
    k          <- frequency [ (1, pure 0), (2, choose (1, 3)) ]
    cPools     <- vectorOf k (choose (- cap, 3 * cap))
    cUncharged <- sublistOf [2, 3, 8, 18]
    cLock      <- frequency [ (1, pure 0), (3, choose (0, 100)) ]
    n          <- choose (0, 60)
    deltas     <- vectorOf n (choose (0, 5 :: Double))
    cOps       <- forM (scanl1 (+) deltas) $ \t -> frequency
                    [ (3, pure (Ask t))
                    , (3, Charge t <$> elements [2, 3, 4, 8, 10, 18, 19] <*> choose (1, 2 * freshMax))
                    , (1, pure (Rebuild t)) ]
    return Case { cStanding, cPools, cUncharged, cLock, cOps }
    where cap = capacity allowance
  shrink c@Case { cStanding, cPools, cUncharged, cLock, cOps } =
       [ c { cOps = ops }         | ops <- shrinkList (const []) cOps ]
    ++ [ c { cPools = ps }        | ps  <- shrinkList (const []) cPools ]
    ++ [ c { cUncharged = us }    | us  <- shrinkList (const []) cUncharged ]
    ++ [ c { cStanding = Other }  | cStanding /= Other ]
    ++ [ c { cLock = 0 }          | cLock /= 0 ]

-- | The table, stepped by hand with the ranks written out, so the rule's own
-- 'rankFor' is judged rather than echoed: ranks at every ask and the pool
-- buckets at the end.
model :: Case -> ([Rank], [Charged])
model Case { cStanding, cPools, cUncharged, cLock, cOps } =
    go (map Charged cPools) (lockedCharged (clock (at 0)) (clock (at cLock))) cOps
  where
    matched = not (null cPools)
    go pools _ [] = ([], pools)
    go pools stranger (op : ops) =
      case op of
           Ask t ->
             let fresh  = clock (at t)
                 hasAny | matched   = any ((> 0) . credit allowance fresh) pools
                        | otherwise = credit allowance fresh stranger > 0
                 rank = case cStanding of
                             LocalRoot -> Rank 0
                             Partner   -> Rank 1
                             Other | not hasAny -> Rank 4
                                   | matched    -> Rank 2
                                   | otherwise  -> Rank 3
                 (ranks, final) = go pools stranger ops
             in (rank : ranks, final)
           Charge t num n
             | num `elem` cUncharged -> go pools stranger ops
             | matched   -> go (map (charge allowance (clock (at t)) n) pools) stranger ops
             | otherwise -> go pools (charge allowance (clock (at t)) n stranger) ops
           Rebuild t ->
             go (map (const (resetCharged allowance (clock (at t)))) pools) stranger ops

-- | The same, through 'mkEgressRule' and the buckets in IOSim.
run :: Case -> ([Rank], [Charged])
run Case { cStanding, cPools, cUncharged, cLock, cOps } = runSimOrThrow $ do
    pa <- newPoolAllowances allowance clock (clock (at cLock))
    let key   = "peer" :: String
        k     = length cPools
        index = if k > 0 then Map.singleton key [0 .. k - 1] else Map.empty
    atomically $ rebuild pa (at 0) k index
    buckets0 <- readTVarIO (paBuckets pa)
    forM_ (zip [0 ..] cPools) $ \(i, c) ->
      atomically $ writeTVar (buckets0 IntMap.! i) (Charged c)
    (rule, sink) <- mkEgressRule pa (map MiniProtocolNum cUncharged) key
                                 (const (return cStanding)) (at 0)
    ranks <- fmap concat . forM cOps $ \op ->
      case op of
           Ask t          -> (: []) <$> atomically (rule (at t))
           Charge t num n -> [] <$ atomically (sink (at t) (MiniProtocolNum num) n)
           Rebuild t      -> [] <$ atomically (rebuild pa (at t) k index)
    buckets <- readTVarIO (paBuckets pa)
    final   <- mapM readTVarIO (IntMap.elems buckets)
    return (ranks, final)

prop_rule :: Case -> Property
prop_rule c@Case { cStanding, cPools, cLock, cOps } =
    classify (cStanding == Other) "one of the rest"
  . classify (not (null cPools)) "a pool relay"
  . classify lockedStranger "a locked stranger"
  . classify (lockedStranger && any askedAfter cOps) "a stranger asked after its lock"
  . classify (any isRebuild cOps) "the list is rebuilt under it"
  . classify (Rank 4 `elem` fst got) "goes to rest at some point"
  . classify (Rank 2 `elem` fst got) "served as a pool relay at some point"
  . classify (Rank 3 `elem` fst got) "served as a stranger at some point"
  $ got === model c
  where
    got = run c
    lockedStranger = cStanding == Other && null cPools && cLock > 0
    askedAfter (Ask t) = t > cLock
    askedAfter _       = False
    isRebuild Rebuild {} = True
    isRebuild _          = False

-- | What gives the floor's tier numbers their meaning is 'rankFor'. Over
-- every standing and credit state: a local root has no class, a partner is
-- class 0, a pool relay with credit class 1, a stranger with credit class 2,
-- and a peer without credit has none. A renumbering of the tiers on either
-- side fails here.
prop_floorClass_follows_rank :: Property
prop_floorClass_follows_rank = once $ conjoin
  [ counterexample (show (standing, matched, hasAny))
  $ floorClass tier === expected
  | standing <- [LocalRoot, Partner, Other]
  , matched  <- [False, True]
  , hasAny   <- [False, True]
  , let Rank tier = rankFor standing matched hasAny
        expected = case standing of
                        LocalRoot -> Nothing
                        Partner   -> Just (FloorClass 0)
                        Other | not hasAny -> Nothing
                              | matched    -> Just (FloorClass 1)
                              | otherwise  -> Just (FloorClass 2)
  ]

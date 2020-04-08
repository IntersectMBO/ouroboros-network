{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC        -Wall
                       -Wcompat
                       -Wincomplete-uni-patterns
                       -Wincomplete-record-updates
                       -Wpartial-fields
                       -Widentities
                       -Wredundant-constraints
                       -Wmissing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.AdditiveGroup
import           Data.List (unfoldr)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           GHC.Real (denominator, numerator)

import           Graphics.Rendering.Chart.Easy hiding (x0)
import           Graphics.Rendering.Chart.Backend.Cairo

import qualified Numeric.LinearAlgebra as M

import qualified Statistics.Distribution as S
import qualified Statistics.Distribution.Binomial as S

newtype Shown = Shown (Int -> ShowS)
instance Show Shown where showsPrec p (Shown f) = f p

showQ :: Rational -> Shown
showQ q = Shown $ \p -> showParen (p > 10) $ shows (numerator q) . showChar '/' . shows (denominator q)

-----

mAbove :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double
mAbove a b = M.fromBlocks [[a],[b]]

mBeside :: M.Matrix Double -> M.Matrix Double -> M.Matrix Double
mBeside a b = M.fromBlocks [[a,b]]

-- | One sum per row of the given matrix
rowSums :: M.Matrix Double -> M.Vector Double
rowSums m = m M.#> M.konst 1 (snd (M.size m))

-----

pairs :: (Ord y, Num y) => [x] -> (x -> y) -> [(x, y)]
pairs xs f = [ (x, 0 `max` f x) | x <- xs ]

main :: IO ()
main = do
  toFile def "plot1.png" plot1
  toFile def "plot1_50.png" (plot1n 50)
  toFile def "plot2.png" plot2
  toFile def "plot3.png" plot3
  toFile def "plot3_10.png" (plot3n 10)
  toFile def "plot3_25.png" (plot3n 25)
  toFile def "plot3_50.png" (plot3n 50)
  toFile def "plot3_75.png" (plot3n 75)
  toFile def "plot3_100.png" (plot3n 100)

  putStrLn $ "f=0.5 alpha=V2 (1/2) (1/2) k=5  " <> show (wedge2 0.5 (V2 (1/2) (1/2)) 5)
  putStrLn $ "f=0.5 alpha=V2 (1/2) (1/2) k=10 " <> show (wedge2 0.5 (V2 (1/2) (1/2)) 10)

  putStrLn $ "f=0.5 alpha=V3 (1/3) (1/3) (1/3) k=5  " <> show (wedge3 0.5 (V3 (1/3) (1/3) (1/3)) 5)
  putStrLn $ "f=0.5 alpha=V3 (1/3) (1/3) (1/3) k=10 " <> show (wedge3 0.5 (V3 (1/3) (1/3) (1/3)) 10)

  putStrLn $ "n=50  f=0.5 alpha=V3 (1/3) (1/3) (1/3) k=10 " <> show (wedge3n 50 0.75 (V3 (1/3) (1/3) (1/3)) 10)
  putStrLn $ "n=75  f=0.5 alpha=V3 (1/3) (1/3) (1/3) k=10 " <> show (wedge3n 75 0.75 (V3 (1/3) (1/3) (1/3)) 10)
  putStrLn $ "n=100 f=0.5 alpha=V3 (1/3) (1/3) (1/3) k=10 " <> show (wedge3n 100 0.75 (V3 (1/3) (1/3) (1/3)) 10)

{-  let f = 0.50
      alphas = V3 (1/8) (1/8) (3/4)
      n = 100
      t = fst $ t3_synch n f alphas k
      probs@Probs{..} = mkProbs f alphas
      V3 a b c = abc
      V3 a' b' c' = abc'
  print probs

  mapM_ (\(s, p) -> putStrLn $ s <> ": " <> show p) $
    [ ("no + 1L + 2L + 3L", noLeader + oneLeader + twoLeaders + threeLeaders)
    , ("no + any", noLeader + anyLeaders)
    , ("a + a'", a + a')
    , ("b + b'", b + b')
    , ("c + c'", c + c')
    , ("f - phi f 1", f - phi f 1)
    , ("abc1 A3 / 1L", pickABC A3 abc1 / oneLeader)
    , ("abc2 A3 / 2L", pickABC A3 abc2 / twoLeaders)
    , ("abc2' A3", pickABC A3 abc2')
    ]

  print $ rowSums t
-}
-----

phi :: Double -> Double -> Double
phi f alpha = 1 - (1 - f) ** alpha

-----

-- Probability of more than k empty slots in some window of 2k Praos slots
--
-- Compare to the Byron chain density invariant.
--
violation :: Double -> Int -> Double
violation f k = p
  where
    -- P(more than k empty in 2k trials)
    p = S.complCumulative d (fromIntegral k)

    -- A biased coin determines whether a slot is empty
    d = S.binomial (2 * k) (1 - f)

plot1 = do
    layout_title .= "Probability of more than k empty slots in 2k slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"
    setColors [opaque red, opaque green, opaque blue, opaque black]

    let ks = [1 .. 1000] :: [Int]
    plot $ line "f=0.25" [pairs ks (violation 0.25)]
    plot $ line "f=0.5" [pairs ks (violation 0.5)]
    plot $ line "f=0.75" [pairs ks (violation 0.75)]
    plot $ line "f=0.51" [pairs ks (violation 0.51)]

-- | This is a biased martingale (ie random walk on the integer line with fixed
-- biased probability)), as in Gambler's Ruin. I'd anticipate a closed-form
-- answer, but I haven't tracked it down yet, so Markov it is

data SlotStatus = ActiveSlot | EmptySlot
  deriving (Eq, Ord, Show)

violationn :: Int -> Double -> Int -> Double
violationn n f k =
    1 - M.atIndex tn 0 {- ie Nothing -}
  where
    s0 = Seq.replicate (2*k) ActiveSlot

    ss :: [Maybe (Seq.Seq SlotStatus)]
    ss = Nothing : map Just (Set.toList $ go1 0 Set.empty s0)
      where
        go1 i acc x
            | i > 2*k          = acc
            | Set.member x acc = acc
            | otherwise        = go2 i (Set.insert x acc) x
        go2 i acc x = case x of
          Seq.Empty  -> Set.singleton Seq.Empty
          _ Seq.:<| y -> (push EmptySlot . push ActiveSlot) acc
            where
              push :: SlotStatus -> Set.Set (Seq.Seq SlotStatus) -> Set.Set (Seq.Seq SlotStatus)
              push s acc' = go1 (i+1) acc' (y Seq.:|> s)

    -- transition matrix
    t :: M.Matrix Double  -- square over ss
    t =
      (c M.>< c) [ tprob row col | row <- ss, col <- ss ]
      where
        c = length ss

        actives = length . Seq.filter (ActiveSlot ==)

        tprob Nothing Nothing = 1
        tprob Nothing Just{}  = 0
        tprob (Just s1) mbS2 = case (s1, mbS2) of
            (Seq.Empty, Just (_ Seq.:|> _)) -> error "impossible!"
            (_ Seq.:<| _, Just Seq.Empty) -> error "impossible!"

            (Seq.Empty, Nothing)        -> f
            (Seq.Empty, Just Seq.Empty) -> 1 - f

            (_ Seq.:<| y1, Nothing)             ->
                if k == actives y1 then f else 0
            (_ Seq.:<| y1, Just (y2 Seq.:|> s)) ->
                if y1 /= y2 then 0 else case s of
                  ActiveSlot
                      | k == actives y1 -> 0
                      | otherwise       -> f
                  EmptySlot  -> 1 - f

    tn :: M.Vector Double   -- over ss
    tn = foldl (M.<#) t0 (replicate n t)
      where
        -- assume all chains start at origin
        t0 :: M.Vector Double   -- over ss
        t0 = M.vector [ if s == Just s0 then 1 else 0 | s <- ss ]

plot1n n = do
    layout_title .= "Probability of more than k empty slots in " <> show n <> " slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"
    setColors [opaque red, opaque green, opaque blue, opaque black]

    let ks = [1 .. 8] :: [Int]
    plot $ line "f=0.25" [pairs ks (violationn n 0.25)]
    plot $ line "f=0.5" [pairs ks (violationn n 0.5)]
    plot $ line "f=0.75" [pairs ks (violationn n 0.75)]
    plot $ line "f=0.51" [pairs ks (violationn n 0.51)]

-----

mkPl wedge ks color f shape qalphas = do
    setColors [opaque color]
    setShapes [shape]
    plot $ line
      ""
      [pairs ks (wedge f alphas)]
    plot $ points
      (unwords ["f=" <> show f, "as=" <> show (fmap showQ qalphas)])
      (pairs ks (wedge f alphas))
  where
    alphas = fmap fromRational qalphas

-----

data V2 a = V2 a a deriving (Functor, Show)

wedge2 :: Double -> V2 Double -> Int -> Double
wedge2 f (V2 alpha1 alpha2) k =
    (d1 * d2 / f) ^ k
  where
    d1 = phi f alpha1
    d2 = phi f alpha2

plot2 = do
    layout_title .= "Probability of 2 nodes in consensus wedging in k eff.ly synch. active slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"

    let ks = [1 .. 10] :: [Int]
        pl = mkPl wedge2 ks

    pl red   0.25 PointShapeCross $ V2 (1/2) (1/2)
    pl red   0.25 PointShapePlus  $ V2 (1/4) (3/4)
    pl green 0.50 PointShapeCross $ V2 (1/2) (1/2)
    pl green 0.50 PointShapePlus  $ V2 (1/4) (3/4)
    pl blue  0.75 PointShapeCross $ V2 (1/2) (1/2)
    pl blue  0.75 PointShapePlus  $ V2 (1/4) (3/4)
    pl black 0.99 PointShapeCross $ V2 (1/2) (1/2)
    pl black 0.99 PointShapePlus  $ V2 (1/4) (3/4)

-----

-- | States with three nodes in which at least one pair of selected chains does
-- not intersect after origin
data AtLeastTwo3 =
    -- | Our chain doesn't intersect with either of theirs after Origin, but
    -- their two chains do intersect after Origin
    Two3A | Two3B | Two3C
    -- | No pair of chains intersects after Origin
  | Three3

data V3 a = V3 a a a deriving (Functor, Show)

instance Applicative V3 where
  pure a = V3 a a a
  V3 f0 f1 f2 <*> V3 a0 a1 a2 = V3 (f0 a0) (f1 a1) (f2 a2)

instance AdditiveGroup a => AdditiveGroup (V3 a) where
  zeroV = V3 zeroV zeroV zeroV
  V3 l0 l1 l2 ^+^ V3 r0 r1 r2 = V3 (l0 ^+^ r0) (l1 ^+^ r1) (l2 ^+^ r2)
  negateV (V3 x0 x1 x2) = V3 (negateV x0) (negateV x1) (negateV x2)

wedge3 :: Double -> V3 Double -> Int -> Double
wedge3 f (V3 alpha1 alpha2 alpha3) k =
    -- there's no wedge iff we don't end up in the error state
    1 - M.atIndex tk 0 {- ie Nothing -}
  where
    a = phi f alpha1; a' = 1 - a
    b = phi f alpha2; b' = 1 - b
    c = phi f alpha3; c' = 1 - c

    ss = [Two3A, Two3B, Two3C, Three3]

    -- partial transition matrix, excluding the error state's row and column
    t :: M.Matrix Double  -- square over ss
    t =
      M.cmap (/ f) $   -- we're asserting at least one leads
      (n M.>< n) [ cell row col | row <- ss, col <- ss ]
      where
        n = length ss

        -- assumption: a non-leader has equal probability of selecting between
        -- the chains forged by the slot leaders; hence the divisions by 2
        cell Two3A Two3A =
            a * (b * c' + b' * c) / 2   -- c/b selects b/c
          + a * b * c  -- the new b and c will still intersect after Origin
        cell Two3A Two3B = a * b * c' / 2   -- c selects a
        cell Two3A Two3C = a * b' * c / 2   -- b selects a

        cell Two3B Two3A = b * a * c' / 2
        cell Two3B Two3B = b * (a * c' + a' * c) / 2 + a * b * c
        cell Two3B Two3C = b * a' * c / 2

        cell Two3C Two3A = c * a * b' / 2
        cell Two3C Two3B = c * a' * b / 2
        cell Two3C Two3C = c * (a * b' + a' * b) / 2 + a * b * c

        cell Three3 Three3 = a * b * c
        cell Three3      s = cell s s
        cell _      Three3 = 0

    tk :: M.Vector Double   -- over ss'
    tk = foldl (M.<#) t0' (replicate k t')
      where
        -- add in the error state
        ss' = Nothing : map Just ss

        -- assume all chains start at origin
        t0' :: M.Vector Double   -- over ss'
        t0' = M.vector [ case s of Just Three3 -> 1; _ -> 0 | s <- ss' ]

        -- full transition matrix
        t' :: M.Matrix Double   -- square over ss'
        t' =
            -- once in error, always in error
            M.row [ case s of Nothing -> 1; _ -> 0 | s <- ss' ]
          `mAbove`
            -- the density missing from each row of t is the probability of
            -- error
            (M.asColumn (1 - rowSums t) `mBeside` t)

plot3 = do
    layout_title .= "Probability of 3 nodes in consensus wedging in k eff.ly synch. active slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"

    let ks = [1 .. 10] :: [Int]
        pl = mkPl wedge3 ks

    pl red   0.25 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl red   0.25 PointShapePlus  $ V3 (1/8) (1/8) (3/4)
    pl green 0.50 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl green 0.50 PointShapePlus  $ V3 (1/8) (1/8) (3/4)
    pl blue  0.75 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl blue  0.75 PointShapePlus  $ V3 (1/8) (1/8) (3/4)
    pl black 0.99 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl black 0.99 PointShapePlus  $ V3 (1/8) (1/8) (3/4)

-----

-- | one of three nodes
--
data ABC = A3 | B3 | C3
  deriving (Eq, Ord, Show)

pickABC :: ABC -> V3 a -> a
pickABC A3 (V3 a _ _) = a
pickABC B3 (V3 _ b _) = b
pickABC C3 (V3 _ _ c) = c

-- | always @> 0@
--
type Depth = Int

-- | always and @<= k@
--
newtype Depth1 = D1 Depth
  deriving (Eq, Ord, Show)

-- | sum is always @<= k@
--
data Depth2 = D2 !Depth !Depth
  deriving (Eq, Ord, Show)

-- | abstract state of the 1 to 3 distinct selected chains
--
data Chains3
  = Consensus3
    -- ^ one distinct chain
  | Chains2of3 ABC Depth1
    -- ^ two distinct chains
    --
    -- We identify the node that is isolated on one side of the intersection.
  | Chains3of3a Depth1
    -- ^ three distinct chains; one distinct intersection
  | Chains3of3b ABC Depth2
    -- ^ three distinct chains; two distinct intersections, one shallow and one
    -- deep
    --
    -- We identify the node that is isolated on one side of the deep
    -- intersection.
  | Wedged3
    -- ^ wedged
    --
    -- At least two selected chains intersect more than @k@ blocks back.
  deriving (Eq, Ord, Show)

-- | The depth of the deepest intersection
_chainsDepth3 :: K -> Chains3 -> Depth
_chainsDepth3 k = \case
    Consensus3                    -> 0
    Chains2of3  _loner (D1 d)     -> d
    Chains3of3a        (D1 d)     -> d
    Chains3of3b _loner (D2 d1 d2) -> d1 + d2
    Wedged3                       -> k + 1

class MyEnum a where
  initEnum :: K -> Maybe a
  nextEnum :: K -> a -> Maybe a

type K = Int

instance MyEnum ABC where
  initEnum _k = pure A3
  nextEnum _k = \case
      A3 -> pure B3
      B3 -> pure C3
      C3 -> Nothing

instance MyEnum Depth1 where
  initEnum k = do
      guard $ k >= 1
      pure $ D1 1
  nextEnum k (D1 d) = case compare d k of
      LT -> pure $ D1 (succ d)
      EQ -> Nothing
      GT -> error "impossible"

instance MyEnum Depth2 where
  initEnum k = do
      guard $ k >= 2
      pure $ D2 1 1
  nextEnum k (D2 d1 d2) = case compare (d1+d2) k of
      LT -> pure $ D2 d1 (succ d2)
      EQ -> if d1 == 1 then Nothing else pure $ D2 (pred d1) 1
      GT -> error $ "impossible: " <> show (k, d1, d2)

-- 'Wedged3' is viable for lower 'K' than is 'Chains3of3a', which is viable for
-- lower 'K' than is 'Chains3of3b'
--
-- There are @7k - 1@ possible 'Chains3' values.
instance MyEnum Chains3 where
  initEnum _k = pure Wedged3
  nextEnum k = \case
      Wedged3             -> pure Consensus3
      Consensus3          ->
          (Chains2of3 <$> initEnum k <*> initEnum k)
      Chains2of3 loner d  ->
          (Chains2of3  <$> pure loner       <*> nextEnum k d) <|>
          (Chains2of3  <$> nextEnum k loner <*> initEnum k  ) <|>
          (Chains3of3a <$> initEnum k                       )
      Chains3of3a d       ->
          (Chains3of3a <$> nextEnum k d             ) <|>
          (Chains3of3b <$> initEnum k <*> initEnum k)
      Chains3of3b loner d ->
          (Chains3of3b <$> pure loner       <*> nextEnum k d) <|>
          (Chains3of3b <$> nextEnum k loner <*> initEnum k  )

-- | With no clock skew, no network latency, no computational latency, and fair
-- selection by non-leaders amongst competing chains
--
transitionProb :: Int -> Double -> V3 Double -> Chains3 -> Chains3 -> Double
transitionProb k f alphas = go0
  where
    -- a wedge is permanent
    go0 Wedged3 s2 = eq s2 Wedged3
    go0 s1 s2 =
        eq s1 s2 * noLeader +   -- states hold steady without leaders
        go s1 s2

    -- how Wedged3 is reached (requires multiple leaders)
    go s1 Wedged3 = case s1 of
        Consensus3                   -> eq k 0 * manyLeaders
        Chains2of3 loner (D1 d)      -> eq d k * isLeader loner
        Chains3of3a (D1 d)           -> eq k d * manyLeaders
        Chains3of3b loner (D2 d1 d2) -> eq (d1 + d2) k * isLeader loner
        _                            -> 0
      where
        isLeader n = pickABC n abcAnd

    -- how Consensus3 is reached (requires exactly one leader)
    go _ Consensus3 = oneLeader

    -- how Chains2of3 is reached (requires exactly two leaders)
    go s (Chains2of3 loner' (D1 d')) = coinFlip $ case s of
        Consensus3 ->
            eq d' 1 * isLeader loner'
        Chains2of3 loner (D1 d) ->
            half1 + 2 * quarter2
          where
            half1    = neq loner loner' * eq d' 1        * isNotLeader loner
            -- this one can happen two ways
            quarter2 =  eq loner loner' * eq d' (succ d) * isLeader loner'
        Chains3of3a (D1 d) ->
            eq d' (succ d) * isLeader loner'
        Chains3of3b loner (D2 d1 d2) ->
            half1 + half2
          where
            half1 =
                neq loner loner' * eq d' (succ d1) *
                isNotLeader loner
            half2 =
                eq loner loner' * eq d' (succ (d1 + d2)) *
                isLeader loner'
        _ -> 0
      where
        isLeader    n = pickABC n abc2
        isNotLeader n = pickABC n abc2'
        coinFlip p = p / 2   -- the non-leader didn't follow loner'

    -- how Chains3of3 is reached (requires exactly three leaders)
    Consensus3 `go` Chains3of3a (D1 1) =
        threeLeaders
    Chains3of3a (D1 d) `go` Chains3of3a (D1 d') =
        eq d' (succ d) *
        threeLeaders

    -- how Chains3of3b is reached (requires exactly three leaders)
    Chains2of3 loner (D1 d) `go` Chains3of3b loner' (D2 d1' d2') =
        eq loner loner' *
        eq d1' 1 * eq d2' d *
        threeLeaders 
    Chains3of3b loner (D2 d1 d2) `go` Chains3of3b loner' (D2 d1' d2') =
        eq loner loner' *
        eq d1' (succ d1) * eq d2' d2 *
        threeLeaders 

    -- no other transition is possible
    go _ _ = 0
      
    eq x y = if x == y then 1 else 0
    neq x y = 1 - eq x y

    Probs{..} = mkProbs f alphas

-----

data Probs = Probs
    { abc, abc' :: V3 Double

    , abc1 :: V3 Double
    , abcAnd :: V3 Double

    , abc2, abc2' :: V3 Double

    , manyLeaders :: Double
    , anyLeaders :: Double
    , noLeader :: Double
    , oneLeader :: Double
    , twoLeaders :: Double
    , threeLeaders :: Double
    }
  deriving (Show)

mkProbs :: Double -> V3 Double -> Probs
mkProbs f alphas = Probs{..}
  where
    abc @(V3 a  b  c ) = phi f `fmap` alphas   -- P(x leads)
    abc'@(V3 a' b' c') = pure 1 ^-^ abc   -- P(x does not lead)

    abc1 = V3   -- P(only x leads)
        (a * b' * c')
        (a' * b * c')
        (a' * b' * c)    
    abc2' = V3   -- P(two leaders excluding x)
        (a' * b  * c )
        (a  * b' * c )
        (a  * b  * c')

    abcAnd = abc ^-^ abc1   -- P(multiple leaders including x)

    abc2 =   -- P(two leaders including x)
        abcAnd ^-^ pure threeLeaders

    -- these are the basic events
    manyLeaders = anyLeaders - oneLeader   -- P (multiple leaders)
    anyLeaders = f   -- P (any leaders)
    noLeader = 1 - f   -- P (no leader)
    oneLeader =   -- P (exactly one leader)
        a  * b' * c' +
        a' * b  * c' +
        a' * b' * c
    twoLeaders =   -- P(two nodes lead)
        a' * b  * c  +
        a  * b' * c  +
        a  * b  * c'
    threeLeaders =   -- P(all nodes lead)
        a * b * c

-----

wedge3n :: Int -> Double -> V3 Double -> Int -> Double
wedge3n n f alphas k =
    -- there's no wedge iff we don't end up in the error state
    M.atIndex tn 0 {- ie Wedged3 -}
  where
    (_t, tn) = t3_synch n f alphas k

t3_synch :: Int -> Double -> V3 Double -> Int -> (M.Matrix Double, M.Vector Double)
t3_synch n f alphas k =
    (t, tn)
  where
    ss :: [Chains3]
    ss = case initEnum k of
        Nothing -> []
        Just e0 -> e0 : unfoldr (fmap (\x -> (x, x)) . nextEnum k) e0

    -- transition matrix
    t :: M.Matrix Double  -- square over ss
    t =
      (c M.>< c) [ transitionProb k f alphas row col | row <- ss, col <- ss ]
      where
        c = length ss

    tn :: M.Vector Double   -- over ss
    tn = foldl (M.<#) t0 (replicate n t)
      where
        -- assume all chains start at origin
        t0 :: M.Vector Double   -- over ss
        t0 = M.vector [ case s of Consensus3 -> 1; _ -> 0 | s <- ss ]

plot3n n = do
    layout_title .= "Probability of 3 nodes in consensus wedging in " ++ show n ++ " synch. slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"

    let ks = [1 .. 10] :: [Int]
        pl = mkPl (wedge3n n) ks

    pl red   0.25 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl red   0.25 PointShapePlus  $ V3 (1/8) (1/8) (3/4)
    pl green 0.50 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl green 0.50 PointShapePlus  $ V3 (1/8) (1/8) (3/4)
    pl blue  0.75 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl blue  0.75 PointShapePlus  $ V3 (1/8) (1/8) (3/4)
    pl black 0.99 PointShapeCross $ V3 (1/3) (1/3) (1/3)
    pl black 0.99 PointShapePlus  $ V3 (1/8) (1/8) (3/4)

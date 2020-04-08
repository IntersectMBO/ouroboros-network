{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -O2 #-}

module Main (module Main) where

import           Debug.Trace

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.AdditiveGroup
import           Data.Bits
import qualified Data.Default
import qualified Data.Foldable as F
import           Data.List (unfoldr)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Real (denominator, numerator)

import           Graphics.Rendering.Chart.Easy hiding (x0)
import           Graphics.Rendering.Chart.Backend.Cairo

import           Numeric.RootFinding (RiddersParam (..), Root (..), ridders)

import qualified Numeric.LinearAlgebra as M

import qualified Statistics.Distribution as S
import qualified Statistics.Distribution.Binomial as S

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Sparse

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

pairs :: [x] -> (x -> y) -> [(x, y)]
--pairs xs f = [ let y = 0 `max` f x in y `seq` (x, y) | x <- xs ]
pairs xs f = [ let y = f x in y `seq` (x, y) | x <- xs ]

main :: IO ()
main = do
  mainPlots
  mainCheck
  putStrLn ""
  pure () `asTypeOf` mainTable

mainPlots :: IO ()
mainPlots = do
  pure () `asTypeOf` do
    toFile def "plot1_100.png" (plot1 [1 .. 100])
    toFile def "plot1_10000.png" (plot1 $ map (100 *) [1 .. 100])
    toFile def "plot1_1000000.png" (plot1 $ map (10000 *) [1 .. 100])

    let ((plot1n_50, plot1n_2k50), (plot1n_100, plot1n_2k100), plot1n_10k) = plot1n 50 100
    toFile def "plot1_50_bits.png" plot1n_50
    toFile def "plot1_2k50_bits.png" plot1n_2k50
    toFile def "plot1_100_bits.png" plot1n_100
    toFile def "plot1_2k100_bits.png" plot1n_2k100
    toFile def "plot1_10k_bits.png" plot1n_10k

    toFile def "plot2.png" plot2
    toFile def "plot3_10.png" (plot3n 10)
    toFile def "plot3_25.png" (plot3n 25)
    toFile def "plot3_50.png" (plot3n 50)
    toFile def "plot3_75.png" (plot3n 75)
    toFile def "plot3_100.png" (plot3n 100)

  toFile def "plot4_paperCG_50.png"  (plotPaperCG 50)
  toFile def "plot4_paperCG_75.png"  (plotPaperCG 75)
  toFile def "plot4_paperCG_100.png" (plotPaperCG 100)
  toFile def "plot4_paperCG_1000.png" (plotPaperCG 1000)

  toFile def "plot5_paperCP_50.png"  (plotPaperCP 50)
  toFile def "plot5_paperCP_75.png"  (plotPaperCP 75)
  toFile def "plot5_paperCP_100.png" (plotPaperCP 100)
  toFile def "plot5_paperCP_1000.png" (plotPaperCP 1000)

  toFile def "plot6.png" plotThm6_c
  toFile def "plot7.png" plotThm6_s
  toFile def "plot8.png" plotThm5_c

mainCheck :: IO ()
mainCheck = do
  putStrLn "Check"
  let k = 2
      f = 0.50
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

  putStrLn $ "transition matrix row sums " <> show (rowSums t)

mainTable :: IO ()
mainTable = do
  let fs = [0.5, 0.75]
      ks = [5, 10]
      ns = [50, 75, 100]

  putStrLn "Wedges 2"
  F.forM_ fs $ \f -> F.forM_ ks $ \k -> do
    putStrLn $ "f=" <> show f <> " k=" <> show k <> " " <> show (wedge2 f (V2 (1/2) (1/2)) k)

  let alphas3 = V3 (1/3) (1/3) (1/3)
  putStrLn "Wedges 3"
  F.forM_ fs $ \f -> F.forM_ ks $ \k -> F.forM_ ns $ \n -> do
    putStrLn $ "n=" <> show n <> " f=" <> show f <> " k=" <> show k <> " " <> show (wedge3n n f alphas3 k)

  putStrLn "Sparseness 3"
  F.forM_ fs $ \f -> F.forM_ [1 .. 10] $ \k -> do
    putStrLn $ "n=(50,100) f=" <> show f <> " k=" <> show k <> " " <> show (violationn 50 100 f k)

  putStrLn "Paper CG"
  F.forM_ (0.05 : 0.25 : fs) $ \f -> F.forM_ (ns ++ [1000]) $ \n -> F.forM_ [1, 2, 3] $ \delta -> do
    let g x = invPaperCG n f delta (1 / x)
    putStrLn $ "n=" <> show n <> " f=" <> show f <> " Delta=" <> show delta <> " " <> show (g 1, g 10, g 100, g 1000, g 10000, g 100000, g 1000000)

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

plot1 ks = do
    layout_title .= "Probability of more than k empty slots in 2k slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"
    setColors [opaque purple, opaque red, opaque green, opaque brown, opaque blue, opaque black]

    plot $ line "f=0.25" [pairs ks (violation 0.25)]
    plot $ line "f=0.50" [pairs ks (violation 0.50)]
    plot $ line "f=0.60" [pairs ks (violation 0.60)]
    plot $ line "f=0.66" [pairs ks (violation 0.66)]
    plot $ line "f=0.70" [pairs ks (violation 0.70)]
    plot $ line "f=0.75" [pairs ks (violation 0.75)]

-----

{-
data SlotStatus = EmptySlot | ActiveSlot
  deriving (Eq, Ord, Show)

-- | This is a biased martingale (ie random walk on the integer line with fixed
-- biased probability)), as in Gambler's Ruin. I've found close-form answers
-- for the \"play until you're broke or double your money\" cases, but none for
-- the case where you also stop after certain number of coin flips. So I'm
-- using a direct O(2^n)-space Markov option :frown:

empties :: Seq.Seq SlotStatus -> Int
empties = length . Seq.filter (EmptySlot ==)

showRow :: Maybe (Seq.Seq SlotStatus) -> String
showRow = \case
    Nothing -> "  " <> showCol Nothing
    Just s -> show (empties s) ++ " " <> showCol (Just s)

dump :: (M.Matrix Double, [Maybe (Seq.Seq SlotStatus)]) -> IO ()
dump (t, ss) = do
    putStrLn $ "        " <> concatMap (\s -> " " <> showCol s) ss
    F.forM_ (zip rows ss) $ \(row, s) ->
      putStrLn $ showRow s <> " " <> go row
  where
    rows = tail $ lines $ show t   -- drop the dims

    go "" = ""
    go ('0' : '.' : '0' : s) = "   " ++ go s
    go (c:s) = c : go s

showCol :: Maybe (Seq.Seq SlotStatus) -> String
showCol = \case
    Nothing -> "viol"
    Just s -> flip map (F.toList s) $ \case
        ActiveSlot -> '1'
        EmptySlot -> '0'

violationn :: Int -> Double -> Int -> Double
violationn n f k =
    M.atIndex (fst $ violationn_t n f k) 0 {- ie Nothing -}

violationn_t :: Int -> Double -> Int -> (M.Vector Double, (M.Matrix Double, [Maybe (Seq.Seq SlotStatus)]))
violationn_t n f k =
    (tn, (t, ss))
  where
    s0 = Seq.replicate (2*k) ActiveSlot

    ss :: [Maybe (Seq.Seq SlotStatus)]
    ss = Nothing : map Just (filter ((<= k) . empties) $ Set.toList $ go1 0 Set.empty s0)
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

        tprob Nothing Nothing = 1
        tprob Nothing Just{}  = 0
        tprob (Just s1) mbS2 = case (s1, mbS2) of
            (Seq.Empty, Just (_ Seq.:|> _)) -> error "impossible!"
            (_ Seq.:<| _, Just Seq.Empty) -> error "impossible!"

            (Seq.Empty, Nothing)        -> f
            (Seq.Empty, Just Seq.Empty) -> 1 - f

            (_ Seq.:<| y1, Nothing)             ->
                if k == empties y1 then f else 0
            (_ Seq.:<| y1, Just (y2 Seq.:|> s)) ->
                if y1 /= y2 then 0 else case s of
                  ActiveSlot -> f
                  EmptySlot
                      | k == empties y1 -> 0
                      | otherwise       -> 1 - f

    tn :: M.Vector Double   -- over ss
    tn = foldl (M.<#) t0 (replicate n t)
      where
        -- assume all chains start at origin
        t0 :: M.Vector Double   -- over ss
        t0 = M.vector [ if s == Just s0 then 1 else 0 | s <- ss ]
-}

violationn :: Int -> Int -> Double -> Int -> ((Double, Double), (Double, Double), Double)
violationn n1 n2 f k =
    ( (pn1, p2kn1)
    , (pn2, p2kn2)
    , p10k
    )
  where
    pBAD x = x U.! 0 {- ie BAD -}

    !pn1   = pBAD (go n1)
    !p2kn1 = pBAD (go (2*k + n1))
    !pn2   = pBAD (go n2)
    !p2kn2 = pBAD (go (2*k + n2))

    !p10k = pBAD (go (10*k))

    t = violationn_t f k

    -- assume all chains start at Origin
    t0 :: U.Vector Double
    t0 = U.generate (4^k+1) $ \i -> if 4^k == i then 1 else 0

    go :: Int -> U.Vector Double
    go n = foldl vmat t0 (replicate n t)

violationn_t :: Double -> Int -> Sparse2
violationn_t f k = t
  where
    -- we have 4^k + 1 states; 2^(2k) bitvectors and then a BAD sink state
    --
    -- NOTE exactly 4^k/2 - binomCoefficient (2k k)/2 of those states are
    -- unreachable (they are supplanted by BAD because they have >k empty
    -- slots), so a bit less than half of our space consumption is unnecessary.

    -- 1 is ActiveSlot, 0 is EmptySlot
    tooManyEmpties bv = popCount bv < k

    -- transition matrix
    t :: Sparse2
    t = mkSparse2 (4^k) mkBAD mkOK
      where
        mkBAD 0   = 1   -- once BAD always BAD
        mkBAD col = toBadFrom $ toEnum (col - 1)

        toBadFrom :: Word64 -> Double
        toBadFrom bv
            -- bv is unreachable, but make this explicitly 0 anyway
            | tooManyEmpties bv = 0

            -- another empty slot is a violation
            | tooManyEmpties bvE = 1 - f

            | otherwise = 0
          where
            -- bv ---[empty]--> bvE
            bvE = shiftL (clearBit bv (2*k - 1)) 1

        mkOK col =
            ((fromEnum i1, w1), (fromEnum i2, w2))
          where
            ((i1, w1), (i2, w2)) = fromAETo (toEnum col)

        fromAETo :: Word64 -> ((Word64, Double), (Word64, Double))
        fromAETo bv
            -- bv is unreachable
            | tooManyEmpties bv       =
              ((0, 0), (0, 0))

            -- bv can only be reached via an active slot
            | x && tooManyEmpties bvE =
              ((bvA, p), (0, 0))

            | otherwise               =
              ((bvA, p), (bvE, p))
          where
            p = if x then f else 1 - f

            -- the latest slot
            x = testBit bv 0

            -- bvA ---[x]--> bv
            --
            -- NOTE if x is active, then bvA and bv have the same number of
            -- empty slots
            bvA = setBit bvE (2*k - 1)

            -- bvE ---[x]--> bv
            bvE = shiftR bv 1

plot1n ::
    Int -> Int ->
    ( (EC (Layout Int Double) (), EC (Layout Int Double) ())
    , (EC (Layout Int Double) (), EC (Layout Int Double) ())
    , EC (Layout Int Double) ()
    )
plot1n n1 n2 =
    ( (plot1n_n fst3 n1, plot1n_n2k fst3 n1)
    , (plot1n_n snd3 n2, plot1n_n2k snd3 n2)
    , plot1n_10k
    )
  where
    fst3 (x, _, _) = x
    snd3 (_, x, _) = x
    thd3 (_, _, x) = x

    ks = [1 .. 13] :: [Int]
    vv50 = map (violationn n1 n2 0.50) ks
    vv60 = map (violationn n1 n2 0.60) ks
    vv66 = map (violationn n1 n2 0.66) ks
    vv70 = map (violationn n1 n2 0.70) ks
    vv75 = map (violationn n1 n2 0.75) ks

    lu vv i = vv !! (i - 1)

    plot1n_n ::
        (((Double, Double), (Double, Double), Double) -> (Double, Double)) ->
        Int -> EC (Layout Int Double) ()
    plot1n_n prj n = do
        layout_title .= "Probability of more than k empty slots in a 2k slot span within " <> show n <> " slots"
        (layout_x_axis . laxis_title) .= "k"
        (layout_y_axis . laxis_title) .= "Probability"
        setColors [opaque red, opaque green, opaque brown, opaque blue, opaque black]

        plot $ line "f=0.50" [pairs ks (fst . prj . lu vv50)]
        plot $ line "f=0.60" [pairs ks (fst . prj . lu vv60)]
        plot $ line "f=0.66" [pairs ks (fst . prj . lu vv66)]
        plot $ line "f=0.70" [pairs ks (fst . prj . lu vv70)]
        plot $ line "f=0.75" [pairs ks (fst . prj . lu vv75)]

    plot1n_n2k ::
        (((Double, Double), (Double, Double), Double) -> (Double, Double)) ->
        Int -> EC (Layout Int Double) ()
    plot1n_n2k prj n = do
        layout_title .= "Probability of more than k empty slots in a 2k slot span within 2k+" <> show n <> " slots"
        (layout_x_axis . laxis_title) .= "k"
        (layout_y_axis . laxis_title) .= "Probability"
        setColors [opaque red, opaque green, opaque brown, opaque blue, opaque black]

        plot $ line "f=0.50" [pairs ks (snd . prj . lu vv50)]
        plot $ line "f=0.60" [pairs ks (snd . prj . lu vv60)]
        plot $ line "f=0.66" [pairs ks (snd . prj . lu vv66)]
        plot $ line "f=0.70" [pairs ks (snd . prj . lu vv70)]
        plot $ line "f=0.75" [pairs ks (snd . prj . lu vv75)]

    plot1n_10k :: EC (Layout Int Double) ()
    plot1n_10k = do
        layout_title .= "Probability of more than k empty slots in a 2k slot span within 10k slots"
        (layout_x_axis . laxis_title) .= "k"
        (layout_y_axis . laxis_title) .= "Probability"
        setColors [opaque red, opaque green, opaque brown, opaque blue, opaque black]

        plot $ line "f=0.50" [pairs ks (thd3 . lu vv50)]
        plot $ line "f=0.60" [pairs ks (thd3 . lu vv60)]
        plot $ line "f=0.66" [pairs ks (thd3 . lu vv66)]
        plot $ line "f=0.70" [pairs ks (thd3 . lu vv70)]
        plot $ line "f=0.75" [pairs ks (thd3 . lu vv75)]

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

-----

invPaperCG :: Int -> Double -> Int -> Double -> Int
invPaperCG n f delta p = ceiling $
    (negate power / c / alpha) * 20 * del + 3 * del
  where
    power = log (p / fromIntegral n / del)
    del = fromIntegral delta :: Double

    alpha = 1
    c = f * ((1 - f) ^ delta)

paperCG :: Int -> Double -> Int -> Int -> Double
paperCG n f delta s =
    fromIntegral n * del *
    exp power
  where
    del = fromIntegral delta :: Double

    alpha = 1
    c = f * ((1 - f) ^ delta)

    power = negate $
        c * alpha * (fromIntegral s - 3 * del) / (20 * del)

plotPaperCG n = do
    layout_title .= "Theorem 6(CG) with R = " ++ show n ++ " slots"
    (layout_x_axis . laxis_title) .= "k (= s / 40)"
    (layout_y_axis . laxis_title) .= "Probability"

    let ks = [1 .. 200] :: [Int]
--        ks = [1 .. 1000] :: [Int]
        pl color f shape delta = do
            let tau = c * alpha / 4
                alpha = 1
                c = f * ((1 - f) ^ delta)
            setColors [opaque color]
            setShapes [shape]
            plot $ line
              ""
              [pairs ks (min 1 . max (-100000) . log . paperCG n f delta . (*40))]
            plot $ points
              (unwords ["f=" <> show f, "D=" <> show delta, "tau=" <> show tau])
              (pairs ks (min 1 . max (-100000) . log . paperCG n f delta . (*40)))

{-    pl black 0.05 PointShapeCross 1
    pl black 0.05 PointShapePlus  2
    pl black 0.05 PointShapePlus  3
    pl black 0.05 PointShapePlus  4
    pl red   0.25 PointShapeCross 1
    pl red   0.25 PointShapePlus  2
    pl green 0.50 PointShapeCross 1
    pl green 0.50 PointShapePlus  2
    pl blue  0.75 PointShapeCross 1
    pl blue  0.75 PointShapePlus  2
-}

    pl black   0.25 PointShapePlus  3

-----

paperCP :: Int -> Double -> Int -> Int -> Double
paperCP n f delta k
    | p <= 0.5 || p <= 0     = error "CP: bad (alpha, f, delta)"
    | frac >= 1 || frac <= 0 = error "CP: bad (alpha, f, delta)"
    | otherwise              =
    fromIntegral n *
    (2 ** negate power)
  where
    alpha = 1
    p = alpha * ((1 - f) ^ delta)

    -- /I think/ this is the constant hidden by the omega notation in Theorem 5
    -- (mutatis mutandi wrt the base of 2 instead of e)
    frac = negate $ log p / log 2   -- ie @- log_2 p@

    power = frac * fromIntegral (k - delta)

plotPaperCP n = do
    layout_title .= "Theorem 5 (CP) with R = " ++ show n ++ " slots"
    (layout_x_axis . laxis_title) .= "k"
    (layout_y_axis . laxis_title) .= "Probability"

    let ks = [1 .. 150] :: [Int]
        pl color f shape delta = do
            setColors [opaque color]
            setShapes [shape]
            plot $ line
              ""
              [pairs ks (min 1 . paperCP n f delta)]
            plot $ points
              (unwords ["f=" <> show f, "D=" <> show delta])
              (pairs ks (min 1 . paperCP n f delta))

    pl black 0.05 PointShapeCross  1
    pl black 0.05 PointShapePlus   2
    pl black 0.05 PointShapeCircle 3
    pl black 0.05 PointShapeStar   4
    pl red   0.25 PointShapeCross  1
    pl red   0.25 PointShapePlus   2
    let wf = 1 - (1 / sqrt 2 + 0.01)
    pl green wf   PointShapeCross  1
    pl green wf   PointShapePlus   2
    pl blue 0.49  PointShapeCross  1

-----

thm6_c :: Int -> Double -> Double
thm6_c delta f = f * ((1 - f) ^ delta)

plotThm6_c = do
    layout_title .= "c = f * ((1 - f) ^ Delta)"
    (layout_x_axis . laxis_title) .= "f"
    (layout_y_axis . laxis_title) .= "c"

    let fs = map (/100) [0 .. 100] :: [Double]
        pl color delta = do
            setColors [opaque color]
            setShapes [PointShapeCross]
            plot $ line
              ""
              [pairs fs (min 1 . thm6_c delta)]
            plot $ points
              (unwords ["D=" <> show delta])
              (pairs fs (min 1 . thm6_c delta))

    pl red `mapM_` [1..4]
    pl green `mapM_` [5..9]
    pl blue `mapM_` [10..14]
    pl black `mapM_` [15..20]

    let ds = [1..20]
        pl2 s color fhat maxc = do
            setColors [opaque color]
            setShapes [PointShapeCross]
            plot $ line
              s
              [[(fhat delta, maxc delta) | delta <- ds ]]

    pl2 "CG fhat" black thm6_fhat thm6_maxc
    pl2 "CP fhat" brown thm5_fhat thm5_maxc

-----

thm6_fhat :: Int -> Double
thm6_fhat delta =
    case ridders Data.Default.def (0 + epsilon, 1 - epsilon) thm6_c_1deriv of
        NotBracketed -> error "NotBracketed"
        SearchFailed -> error "SearchFailed"
        Root r       -> r
  where
    epsilon = 1e-10
    del = fromIntegral delta

    -- Theorem 6 (CG) requires tau = alpha * (f * (1 - f) ^ delta) / 4, and
    -- we'd like to choose f to maximize it

    -- product rule aplied to thm6_c
    thm6_c_1deriv f = ((1 - f) ^ delta) - del * f * ((1 - f) ^ (delta - 1))

thm6_maxc :: Int -> Double
thm6_maxc delta = thm6_c delta (thm6_fhat delta)

thm5_fhat :: Int -> Double
thm5_fhat delta = 1 - 2 ** (negate 1 / del)
  where
    -- Theorem 5 (CP) requires (1 - f) ^ delta > 1/2, which is equivalent to
    -- requiring f <= 1 - 2 ^ (-1/delta)

    del = fromIntegral delta

thm5_maxc :: Int -> Double
thm5_maxc delta = thm6_c delta (thm5_fhat delta)

thm56_maxc :: Int -> Double
thm56_maxc delta = thm5_maxc delta `min` thm6_maxc delta

-- | In units of k
thm56_mins :: Double -> Int -> Double
thm56_mins alpha delta = 4 / thm56_maxc delta / alpha

plotThm6_s = do
    layoutlr_title .= "Bounds of prereqs of Theorem 6 (CG) for tau = k / s"
    (layoutlr_x_axis . laxis_title) .= "Delta"
    (layoutlr_left_axis . laxis_title) .= "dimensionless, 0 to 1/2"
    (layoutlr_right_axis . laxis_title) .= "s / k (40 = 1 day)"

    let ds = [1 .. 20] :: [Int]
        pl lr lrp s color f = do
            setColors [opaque color]
            setShapes [PointShapeCross]
            _ <- lr $ line
              ""
              [pairs ds f]
            _ <- lrp $ points
              s
              (pairs ds f)
            pure ()

    pl plotLeft  plotLeft  "f6 = argmax_f c(f, Delta)"           red    thm6_fhat
    pl plotLeft  plotLeft  "c6 = c(f6, Delta)"                   blue   thm6_maxc
    pl plotLeft  plotLeft  "f5 = 2^(-Delta) + 1"                 green  thm5_fhat
    pl plotLeft  plotLeft  "c5 = c(f5, Delta)"                   yellow thm5_maxc
    pl plotLeft  plotLeft  "cmax = c(min f5 f6, Delta)"          black  thm56_maxc
{-
    pl plotRight plotRight "k / s = cmax * alpha / 4; alpha=1.0" yellow (thm56_mins 1)
    pl plotRight plotRight "alpha=0.7"                           brown  (thm56_mins 0.7)
-}
    pl plotRight plotRight "k / s = cmax * alpha / 4; alpha=1.0" yellow (thm56_mins 0.5)

-----

thm5_c :: Int -> Double -> Double
thm5_c delta f = (1 - f) ^ delta

plotThm5_c = do
    layout_title .= "c = (1 - f) ^ Delta"
    (layout_x_axis . laxis_title) .= "f"
    (layout_y_axis . laxis_title) .= "c"

    let fs = map (/100) [0 .. 100] :: [Double]
        pl color delta = do
            setColors [opaque color]
            setShapes [PointShapeCross]
            plot $ line
              ""
              [pairs fs (min 1 . thm5_c delta)]
            plot $ points
              (unwords ["D=" <> show delta])
              (pairs fs (min 1 . thm5_c delta))

    pl red `mapM_` [1..4]
    pl green `mapM_` [5..9]
    pl blue `mapM_` [10..14]
    pl black `mapM_` [15..20]

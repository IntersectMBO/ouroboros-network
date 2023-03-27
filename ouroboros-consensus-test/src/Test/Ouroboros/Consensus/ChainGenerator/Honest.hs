{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Ouroboros.Consensus.ChainGenerator.Honest (
    -- * Generating
    ChainSchedule (ChainSchedule),
    CheckedHonestRecipe (UnsafeCheckedHonestRecipe, chrEhcgDensity, chrWin),
    HonestRecipe (HonestRecipe),
    HonestLbl,
    NoSuchHonestChainSchedule (BadDeltaScg, BadKcp, BadLen),
    SomeCheckedHonestRecipe (SomeCheckedHonestRecipe),
    SomeHonestChainSchedule (SomeHonestChainSchedule),
    checkHonestRecipe,
    countChainSchedule,
    uniformTheHonestChain,
    -- * Testing
    EhcgLbl,
    EhcgViolation (EhcgViolation, ehcgvPopCount, ehcgvWindow),
    HonestChainViolation (BadCount, BadEhcgWindow, BadLength),
    checkHonestChain,
    prettyChainSchedule,
    prettyWindow,
    transitionMatrix,
  ) where

import           Control.Arrow ((***))
import           Control.Monad (void, when)
import qualified Control.Monad.Except as Exn
import           Data.Monoid (Endo (Endo, appEndo))
import           Data.Proxy (Proxy (Proxy))
import           Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as V
import qualified Numeric.LinearAlgebra as Mat
import           Prelude hiding (words)
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc, Delta (Delta), Kcp (Kcp), Len (Len), Scg (Scg), ascVal)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (E (ActiveSlotE, SlotE), S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some

-----

data HonestRecipe = HonestRecipe !Kcp !Scg !Delta !Len
  deriving (Eq, Read, Show)

data CheckedHonestRecipe base hon = UnsafeCheckedHonestRecipe {
    chrEhcgDensity :: !(BV.SomeDensityWindow S.NotInverted)
  ,
    chrWin         :: !(C.Contains SlotE base hon)
  }
  deriving (Eq, Read, Show)

data SomeCheckedHonestRecipe =
    forall base hon.
    SomeCheckedHonestRecipe
        !(Proxy base)
        !(Proxy hon)
        !(CheckedHonestRecipe base hon)

instance Show SomeCheckedHonestRecipe where
    showsPrec p (SomeCheckedHonestRecipe base hon recipe) =
        Some.runShowsPrec p
      $ Some.showCtor SomeCheckedHonestRecipe "SomeCheckedHonestRecipe"
            `Some.showArg` base
            `Some.showArg` hon
            `Some.showArg` recipe

instance Read SomeCheckedHonestRecipe where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeCheckedHonestRecipe "SomeCheckedHonestRecipe"
            <*> Some.readArg
            <*> Some.readArg
            <*> Some.readArg

data NoSuchHonestChainSchedule =
    -- | 'Scg' must be greater than 'Delta'
    BadDeltaScg
  |
    -- | must have @0 <= 'Kcp' < 'Scg' - 'Delta'@
    BadKcp
  |
    -- | 'Len' must be positive
    BadLen
  deriving (Eq, Read, Show)

checkHonestRecipe :: HonestRecipe -> Exn.Except NoSuchHonestChainSchedule SomeCheckedHonestRecipe
checkHonestRecipe recipe = do
    when (s <= d) $ Exn.throwError BadDeltaScg

    when (l <= 0) $ Exn.throwError BadLen

    when (k < 0 || s - d <= k) $ Exn.throwError BadKcp

    C.withTopWindow (C.Lbl @HonestLbl) l $ \base topWindow -> do
        C.SomeWindow Proxy slots <- pure topWindow

        pure $ SomeCheckedHonestRecipe base Proxy UnsafeCheckedHonestRecipe {
            chrEhcgDensity = BV.SomeDensityWindow (C.Count (k + 1)) (C.Count (s - d))
          ,
            chrWin         = slots
          }
  where
    HonestRecipe (Kcp k) (Scg s) (Delta d) (Len l) = recipe

-----

-- | The leader schedule of an honest /chain/
--
-- The represented chain grows by one block per active slot. A different data
-- type would represent a leader schedule in which leaders to not extend a block
-- from the previous active slot.
--
-- INVARIANT: at least one active slot
data ChainSchedule base inner =
    ChainSchedule
        !(C.Contains SlotE base inner)
        !(C.Vector inner SlotE S)
  deriving (Eq, Read, Show)

countChainSchedule :: ChainSchedule base inner -> C.Size inner ActiveSlotE
countChainSchedule sched =
    BV.countActivesInV S.notInverted v
  where
    ChainSchedule _slots v = sched

prettyWindow :: C.Contains SlotE base inner -> String -> String
prettyWindow win s =
    -- for example, i=0 n=1 should be "[)"
    replicate i ' ' <> "[" <> replicate (n - theOpenBracket) ' ' <> ")" <> s
  where
    C.Count i = C.windowStart win
    C.Count n = C.windowSize win

    theOpenBracket = 1

prettyChainSchedule ::
  forall base inner.
     ChainSchedule base inner
  -> String
  -> [String]
prettyChainSchedule sched s =
    map (replicate (C.getCount shift) ' ' <>)
  $ [ prettyWindow slots s
    , V.foldMap (Endo . S.showS) (C.getVector v) `appEndo` ""
    ]
  where
    ChainSchedule slots v = sched

    shift = C.windowStart slots

data SomeHonestChainSchedule =
     forall base hon.
     SomeHonestChainSchedule
         !(Proxy base)
         !(Proxy hon)
         !(ChainSchedule base hon)

instance Show SomeHonestChainSchedule where
    showsPrec p (SomeHonestChainSchedule base hon sched) =
        Some.runShowsPrec p
      $ Some.showCtor SomeHonestChainSchedule "SomeHonestChainSchedule"
            `Some.showArg` base
            `Some.showArg` hon
            `Some.showArg` sched

instance Read SomeHonestChainSchedule where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeHonestChainSchedule "SomeHonestChainSchedule"
            <*> Some.readArg
            <*> Some.readArg
            <*> Some.readArg

data HonestLbl

data RemainingHcgWindowsLbl

{- TODO

Our original intent was for 'uniformTheHonestChain' to generate a chain
according to the given 'Asc' and then toggle /as few as possible/ empty slots
to active such that the Enriched Chain Growth Assumption is satisfied.

However, the minimality of that is very difficult to ensure, in general. It's
an integer linear programming (ILP) problem, where:

  * Each empty slot in a sparse window is a binary aka ("zero-one") variable.

  * The objective is to minimize the sum of those binary variables.

  * Each sparse window imposes a minimum bound on the number of variables in
    that window that must be toggled.

Because all of the coefficients in the objective (the minimization) and each
constraint (one per window) are positive, this is a /set covering problem/:
each variable is an empty slot in some number of sparse windows, and we want to
choose as few such slots as possible such that no sparse window remain.

Moreover, each window needs a different number of slots (because some windows
may be more sparse than others). Because some windows may require multiple
empty slots, this is a /set multi-covering problem/.

Lastly, we can only toggle an empty slot once (recall that it's a /zero-one/
variable), so this is a /set multi-cover with multiplicity constraints
problem/.

After some searching, I found this reference

  * Qiang-Sheng Hua, Yuexuan Wang, Dongxiao Yu, Francis C. M. Lau: Dynamic
    programming based algorithms for set multicover and multiset multicover
    problems. Theor. Comput. Sci. 411(26-28): 2467-2474 (2010)

which includes the claim "[...] we give the first known exact algorithm for the
MSMC or the SMC with multiplicity constraints problem".

It /may/ be the case that our problem is particularly degenerate, and so easier
to solve. In particular, our variables are binary, their coefficients in the
constraints are binary, and in fact our constraints form a banded matrix. Only
the lower bound of each constraint can be greater than 1. But I have not yet
recognized how to do it.

The closest I have come to a solution is an incremental algorithm that toggles
one empty slot at a time. I /think/ the completeness of this algorithm requires
that the next toggle is one of the empty slots that occurs in the most number
of sparse windows. However, I have not proved that. Moreover, even if it is
true, a counterexample proves that choosing an arbitrary such slot is not
sufficient:

>     A   B   C
> -----   -----
> 01110111011101110
>     -----   -----

In this example, only slots A, B, and C occur in two sparse windows, so those
are the candidates for the next toggle by my conjecture. If we suppose that
each window is only missing one active slot (eg need 4 actives per 5 slots),
then toggling B would require three toggles in total, whereas toggling A and C
would solve the problem with just two toggles.

-}

-- | A 'ChainSchedule' that satisfies 'checkHonestChain'
--
-- The distribution this function samples from is not simple to describe. It
-- begins by drawing a sample of length 'Len' from the Bernoulli process
-- induced by the active slot coefficient 'Asc'. (IE 'Len' many i.i.d. samples
-- from @Uniform(f)@). Then it slides an @s-d@ window along that leader
-- schedule, toggling as many empty slots as necessary to ensure every such
-- window has at least @k+1@ active slots, ie Enriched Praos Chain
-- Growth---HOWEVER, it is a difficult computation to add a /strictly minimal/
-- number of active slots to the output of the Bernoulli process (it's an
-- integer programming optimization problem as far as we can tell). We have
-- settled for a relatively simple algorithm that approaches the minimal number
-- of toggles (TODO calculate how tight the bounds are).
--
-- NOTE: when @'Asc' >> (k+1)/(s-d)@ the sample is most likely directly from
-- the Bernoulli process.
--
-- NOTE: when @'Asc' = 0@ the resulting leader schedule is periodic and has
-- exactly @k+1@ active slots in every @s-d@ window.
--
-- NOTE: when @'Asc' << (k+1)/(s-d)@ the sample is likely to be the
-- concatentation of a few long periodic and minimal runs with the occasional
-- window with @k+2@ active slots demarcating the runs.
--
-- TODO sample from a disjunction of generators that explore interesting
-- boundary conditions. EG one would fill in each sparse window by setting the
-- /youngest/ empty slots in it.
uniformTheHonestChain ::
  forall base hon g.
     R.RandomGen g
  => Maybe Asc   -- ^ 'Nothing' means @0@, which induces a periodic chain
  -> CheckedHonestRecipe base hon
  -> g
  -> ChainSchedule base hon
{-# INLINABLE uniformTheHonestChain #-}
uniformTheHonestChain mbAsc recipe g0 = wrap $ C.createV $ do
    BV.SomeDensityWindow (C.Count (toEnum -> numerator)) (C.Count (toEnum -> denominator)) <- pure chrEhcgDensity
    let _ = numerator   :: C.Var hon ActiveSlotE
        _ = denominator :: C.Var hon SlotE

    g <- R.newSTGenM g0

    -- randomly initialize the bitstring
    mv <- C.replicateMV sz $ case mbAsc of
        Nothing  -> pure $ S.mkActive S.inverted
        Just asc -> S.genAsc asc `R.applySTGen` g

    -- ensure at least one slot is filled
    --
    -- This applies even if not even Len < Scg - Delta.
    void $ BV.fillInWindow S.notInverted (C.Count 1 `BV.SomeDensityWindow` sz) g mv

    -- fill the first window up to a sample from the stationary distribution
    --
    -- We don't simply fill it to k, since that would give the first window a
    -- different distribution than the later windows.
    rtot <- do
        density' <- case mbAsc of
            Nothing  -> pure chrEhcgDensity
            Just asc -> do
                pc <- genFirstWindowPopCount asc chrEhcgDensity `R.applySTGen` g
                pure $ BV.SomeDensityWindow (C.Count pc) (C.toSize denominator)

        -- TODO clear the window first?

        -- NB @withWindow@ truncates if it would reach past @slots@
        C.SomeWindow Proxy ehcg <- pure $ C.withWindow sz (C.Lbl @EhcgLbl) (C.Count 0) (C.toSize denominator)
        tot <- C.frWinVar ehcg <$> BV.fillInWindow S.notInverted density' g (C.sliceMV ehcg mv)

        firstSlot <- BV.testMV S.notInverted mv (C.Count 0)
        newSTRef $ (if firstSlot then subtract 1 else id) $ (tot :: C.Var hon ActiveSlotE)

    C.SomeWindow Proxy remainingFullWindows <- do
        -- "number of windows that fit" is usually "total - windowWidth + 1",
        -- but we do not add the one here because the previous init step above
        -- already handled the first window
        let numRemainingFullWindows = sz C.- C.getCount denominator
        pure $ C.withWindow sz (C.Lbl @RemainingHcgWindowsLbl) (C.Count 1) numRemainingFullWindows

    -- visit all subsequent windows that do not reach beyond @slots@
    --
    -- Visiting a window ensures it has at least k+1 active slots; thus the
    -- first window beyond @slots@ will have at least k-1 actives in its actual
    -- slots. We assume slots beyond @slots@ are active; thus the first window
    -- beyond has at least k active slots. And subsequent windows can only have
    -- more active slots than that; thus we don't need to visit windows that
    -- reach beyond @slots@.
    --
    -- LOOP INVARIANT: @rtot@ contains the count active slots in the current window excluding its youngest slot
    --
    -- LOOP INVARIANT: @numerator - 1 <= rtot@
    --
    -- This loop only alters the final slot in each window. That is key to this
    -- whole function being a /uniform/ sampler. In particular:
    --
    --     * Every excessive empty slot in the first window has an equal chance
    --       to be filled in (by the init step above).
    --
    --     * If some subsequent window is sparse, then its final slot is filled
    --       in (by this loop). It must never fill in any older slot in the
    --       window because those slots have already been sampled (either by
    --       the init step above or by previous iterations of this loop).
    --
    --     * Every slot that was not filled in was drawn from @mbAsc@.
    --
    --     * In total: the init step uniformly fills the first window up to
    --       @numerator@, and then each slot not in the first window is either
    --       forced to @1@ by its preceding @denominator - 1@ samples or is
    --       sampled from @mbAsc@.
    C.forRangeM_ (C.windowSize remainingFullWindows) $ \(C.frWin remainingFullWindows -> islot) -> do
        -- NB will not be truncated
        C.SomeWindow Proxy ehcgSlots <- pure $ C.withWindow sz (C.Lbl @EhcgLbl) islot (C.toSize denominator)

        tot <- do
            tot <- readSTRef rtot
            end <- BV.testMV S.notInverted mv (C.windowLast ehcgSlots)
            pure $ (if end then (+1) else id) $ tot

        let sparse = tot == numerator - 1   -- see LOOP INVARIANT

        tot' <- if not sparse then pure tot else do
            BV.setMV S.notInverted mv (C.windowLast ehcgSlots)
            pure numerator

        start <- BV.testMV S.notInverted mv (C.windowStart ehcgSlots)
        writeSTRef rtot $! (if start then subtract 1 else id) $ tot'

    pure mv
  where
    UnsafeCheckedHonestRecipe {
        chrEhcgDensity
      ,
        chrWin         = slots
      }  = recipe

    sz  = C.windowSize slots :: C.Size hon SlotE   -- ie 'Len'

    wrap v = ChainSchedule slots v

-----

-- | The population count of a sample drawn from the stationary distribution of
-- the idealized honest chain generator
genFirstWindowPopCount :: R.RandomGen g => Asc -> BV.SomeDensityWindow S.NotInverted -> g -> (Int, g)
genFirstWindowPopCount asc densityWindow g =
    let q :: Double
        (q, g') = R.random g   -- note that q is between 0 and 1.

        cumul = tail $ scanl (+) 0 $ map abs $ Mat.toList sd

        -- A sample drawn from the weighted categorical distribution that is
        -- the Markov chain's stationary distribution.
        i = length $ takeWhile (\x -> x / last cumul < q) cumul
    in
    (n + i, g')
  where
    f = ascVal asc

    BV.SomeDensityWindow (C.Count n) (C.Count d) = densityWindow

    -- the non-normalized stationary distribution
    --
    -- 1) See
    -- https://stephens999.github.io/fiveMinuteStats/markov_chains_discrete_stationary_dist.html
    --
    -- 2) The stationary distribution is the eigenvector with eigenvalue 1.
    --
    -- 3) We want the /left/ eigenvector (TODO is this fundamental to transition matrices?).
    --
    -- Thus we want a vector v such that
    --
    -- > v A       = v
    -- > v A  - v  = 0
    -- > v (A - I) = 0
    --
    -- We find that using 'Mat.null1' of the transpose of A - I.
    sd :: Mat.Vector Double
    sd = Mat.null1 $ Mat.tr $ transitionMatrix f n d - Mat.ident (d - n + 1)

{-
    This would be equally (?) exact, but it makes the test suite about 2-3
    times slower than does 'Mat.null1'.

    sd :: Mat.Vector Double
    sd = lvecs Mat.! Mat.maxIndex vals

    (Mat.cmap realPart -> vals, Mat.tr -> Mat.cmap realPart -> lvecs) = Mat.eig (Mat.tr p)
-}

{-
    TODO This makes the test suite 3-5 times faster than does 'Mat.null1', but
    I don't know how to bound its inaccuracy. Maybe there's an argument that
    for some X after X * (s-d) steps (instead of 1024) it's certainly "close
    enough" to the stationary distribution for our specific use case?

    -- a brute-force way to (hopefully) find the stationary distribution
    -- (recall the way in which 'stimes' is optimized) sd :: Mat.Vector Double
    -- sd = (Mat.! 0) $ stimes (2^(10 :: Int) :: Int) p
-}

-- | Transition matrix of an Markov Chain where each state is the (idealized?)
-- number of active slots in an EHCG window
--
-- The indices @i@ and @j@ are @n@ less than state they represent; in other
-- words, they count the /extra/ active slots, not just the active slots.
--
-- This is a /stochastic matrix/ because every /row/ sums to 1. And every state
-- is reachable from every other, since @0 < asc@ -- hence it's an
-- /irreducible/ Markov chain. This chain is also /aperiodic/: since every
-- state can step to itself, there's no multiple >1 constraining how many steps
-- it takes for a state to recur. Irrreducibility and aperiodicity imply the
-- /stationary distribution/ exist and is unique.
--
-- We have one lingering doubt about whether this system perfectly models the
-- behavior of the loop in 'uniformTheHonestChain' that visits the remaining
-- windows, hence the "idealized?" above. In particular, these transitions are
-- the result of assuming that the active slots within a window are uniformly
-- distributed after it has been visited by that loop. Specifically, we assume
-- that the probability of the oldest slot being active is the population count
-- divided by the window width. Our lingering doubt is that we're
-- overestimating the degrees of freedom such that somehow a slot's value from
-- /before/ some window is unexpectedly influencing the value of the first slot
-- in that window in a way that is not accounted for by the number of active
-- slots in the immediately preceding window, ie that the Markov Property does
-- not actually hold. I think the seed for this doubt was Esgen's earlier
-- insight that a chain with /exactly/ @n@ actives in every @d@ slot window is
-- periodic. In that case, if there are more than @d@ contiguous windows that
-- each have exactly @n@ actives, then there is perfect auto-correlation
-- throughout those windows, and it spans farther than one window. That's our
-- suspicion for the kind of counterexample of the Markov Property of the
-- system we intend for this transition matrix to model.
--
-- That said, even if it's not a perfect model, we would guess it's usually a
-- fine approximation to use for sampling the density of the first window.
transitionMatrix :: (Enum a, Fractional a, Mat.Container Mat.Vector a) => a -> Int -> Int -> Mat.Matrix a
{-# SPECIALIZE transitionMatrix :: Double -> Int -> Int -> Mat.Matrix Double #-}
transitionMatrix f n d =
    Mat.build (sz, sz) $ curry $ (. (fromEnum *** fromEnum)) $ \case
      (0, 0) -> (dd - dd * f + nn * f) / dd
      (0, 1) -> (     dd * f - nn * f) / dd

      (i, j)
        | d - n == i, j == i - 1
        -> 1 - f
        | d - n == i, j == i
        -> f

        | j == i - 1 -> (x - x * f)                   / dd
        | j == i     -> (2 * x * f + dd - dd * f - x) / dd
        | j == i + 1 -> (dd * f - x * f)              / dd
        where
          x = toEnum i + nn

      _ -> 0
  where
    sz = d - n + 1

    dd = toEnum d
    nn = toEnum n

{-

Derivation of the transition matrix used in 'stationaryWindowSize'.
The x/d summand is the chance we're sliding the window off of an active slot.
The (d - x)/d summand is the chance we're sliding the window off of an empty slot.

One reflecting barrier at n.

    P( n -> n     ) = (n/d) * 1         +   ((d - n)/d) * (1 - f)
    P( n -> n + 1 ) = (n/d) * 0         +   ((d - n)/d) * f

And another at d.

    P( d -> d - 1 ) = (d/d) * (1 - f)  +    (0/d) * f
    P( d -> d     ) = (d/d) * f        +    (0/d) * (1 - f)

Otherwise, stuttering random walk for n < x < d.

    P( x -> x - 1 ) = (x/d) * (1 - f)   +   ((d - x)/d) * 0
    P( x -> x     ) = (x/d) * f         +   ((d - x)/d) * (1 - f)
    P( x -> x + 1 ) = (x/d) * 0         +   ((d - x)/d) * f

Algebraically simplified.

    d * P( n -> n     ) = d - df + nf
    d * P( n -> n + 1 ) =     df - nf

    d * P( x -> x - 1 ) = x - xf
    d * P( x -> x     ) = 2xf + d - df - x
    d * P( x -> x + 1 ) = df - xf

        P( d -> d - 1 ) = 1 - f
        P( d -> d     ) = f

-}

-----

data EhcgViolation hon =
    forall skolem.
    EhcgViolation {
        -- | How many active slots 'ehcgvWindow' has
        ehcgvPopCount :: !(C.Size (C.Win EhcgLbl skolem) ActiveSlotE)
      ,
        -- | The ChainGrowth window that doesn't have enough active slots
        ehcgvWindow   :: !(C.Contains SlotE hon (C.Win EhcgLbl skolem))
      }

instance Eq (EhcgViolation hon) where
    EhcgViolation l1 l2 == EhcgViolation r1 r2 =
        Some.runEq
      $ Some.eqCtor EhcgViolation EhcgViolation
            `Some.eqArg` (C.forgetBase, l1, C.forgetBase, r1)
            `Some.eqArg` (C.forgetWindow, l2, C.forgetWindow, r2)

instance Show (EhcgViolation hon) where
    showsPrec p (EhcgViolation x y) =
        Some.runShowsPrec p
      $ Some.showCtor EhcgViolation "EhcgViolation"
            `Some.showArg` x
            `Some.showArg` y

instance Read (EhcgViolation hon) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor EhcgViolation "EhcgViolation"
            <*> Some.readArg
            <*> Some.readArg

data HonestChainViolation hon =
    -- | The chain must have at least one active slot
    BadCount
  |
    BadEhcgWindow !(EhcgViolation hon)
  |
    BadLength !(C.Size hon SlotE)
  deriving (Eq, Read, Show)

-- | An Enriched Honest Chain Growth window
data EhcgLbl

-- | Check the Praos Enriched Chain Growth property
--
-- Definition of /window/ and /anchored window/. A window is a contiguous
-- sequence of slots. A window anchored at a block starts with the slot
-- immediately after that block.
--
-- Definition of /Praos Chain Growth Assumption/. We assume the honest chain
-- contains at least @k@ blocks in every window that contains @s@ slots.
--
-- Definition of /Praos Enriched Chain Growth Assumption/. We assume the honest
-- chain contains at least @k+1@ blocks in every window that contains @s - Δ@
-- slots.
--
-- Definition of /Stability Window/. The @s@ parameter from the Praos Chain
-- Growth property. (At the time of writing, this is @2k@ during Byron and
-- @3k/f@ after Byron on Cardano @mainnet@.)
checkHonestChain ::
  forall base hon.
     HonestRecipe
  -> ChainSchedule base hon
  -> Exn.Except (HonestChainViolation hon) ()
checkHonestChain recipe sched = do
    when (C.getCount sz /= l) $ Exn.throwError $ BadLength sz

    do  let pc = countChainSchedule sched
        when (C.toVar pc <= 0) $ Exn.throwError BadCount

    -- every slot is the first slot of a unique EHCG window
    C.forRangeM_ sz $ \i -> do
        C.SomeWindow Proxy ehcg <- pure $ C.withWindow sz (C.Lbl @EhcgLbl) i (C.Count ehcgWidth)

        let pc = BV.countActivesInV S.notInverted (C.sliceV ehcg v)

        -- generously assume that the slots of this EHCG window that extend past 'Len' are active
        let benefitOfTheDoubt = ehcgWidth - C.getCount (C.windowSize ehcg)

        -- check the density in the EHCG window
        when (C.getCount pc + benefitOfTheDoubt <= k) $ do
            Exn.throwError $ BadEhcgWindow $ EhcgViolation {
                ehcgvPopCount = pc
              ,
                ehcgvWindow   = ehcg
              }

  where
    HonestRecipe (Kcp k) (Scg s) (Delta d) (Len l) = recipe

    -- the general EHCG window contains @'Scg' - 'Delta'@ slots
    ehcgWidth = s - d :: Int

    ChainSchedule hon v = sched

    sz  = C.windowSize hon

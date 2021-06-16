{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC
                -Wall
                -Wcompat
                -Wincomplete-uni-patterns
                -Wincomplete-record-updates
                -Wpartial-fields
                -Widentities
                -Wredundant-constraints
#-}

-- | The probabilities for small total leader counts within a small number of
-- slots, based on the assumption of a uniform stake distribution over a fixed
-- number of pools.
--
-- We explain the calculations in three steps.
--
-- Step 0. Assume uniform stake distribution, active slot coefficient f=0.05,
-- and some number of pools (eg 500).
--
-- Step 1, see 'probLeadLC'. Calculate the exact probabilities for how many
-- pools will be elected in any given slot, but only calculate it for several
-- small counts. The probability diminishes rapidly, so we cover most cases by
-- considering only so many. Currently we consider 0-7 leaders; see 'LC' and
-- 'PerLC'. The key distribution is a binomial: each trial is one pool's attempt
-- at the lottery for the slot, and we only care about the probabilities for 0-7
-- in the support.
--
-- Step 2, see 'probLeadsLCs'. Calculate the exactly probabilities for how many
-- 0-leader, 1-leader, 2-leader, ..., 7-leader slots we see among a small number
-- of slots (eg 5). The key distribution is a multinomial. The typical dice
-- metaphor is instantiated such that each slot represents a roll of a 8-sided
-- die (values 0-7), where each face represents that slot having the
-- corresponding number of leaders; we use Step 1 for the per-face
-- probabilities. (In some sense, we're actualy rolling a 9-sided die; there's
-- an additional face that means " more than 7 leaders ", but our calculations
-- are organized to never involve that case.) So, for 5 slots, we roll 5d8. The
-- pmf of the multinomial distribution then computes eg the probability of
-- seeing eg two 1-leader slots and one 4-leader slot.
--
-- Step 3, see 'tableProbLeadsLC'. We ultimately want to know probabilities for
-- the total number of leaders we'll see during those slots (5 in our example).
-- The two 1-leader slots and one 4-leader slot example above would be
-- abstracted as the simpler " 6 leaders ". The multinomial distribution only
-- regards the die faces as categorical data; it does not act upon the fact that
-- the faces are numbers (ie leader counts) that can be summed. So we do that
-- manually as our final step. We also aggregate the probabilities of
-- multinomial events that correspond to the same total count; this summation is
-- sound because the multinomial events are all exclusive.
--
-- We end up with a table: the probability for seeing 0-7 total leaders during
-- the specified number of slots (5 in our example). Any unallocated probability
-- covers the cases where there is more than 8 total leaders (either in one slot
-- or spread out over the 5). But the probability of more than 8 is relatively
-- unlikely, so our table contains most of the information that is necessary for
-- higher-level judgments.

module Main (main) where

import           Control.Monad.ST (runST)
import           Data.Foldable (forM_)
import           Data.STRef (modifySTRef, newSTRef, readSTRef)
import           Data.Semigroup (Max (..), Min (..))
import           Data.Traversable (mapAccumL)
import           Numeric.SpecFunctions (factorial)
import qualified Statistics.Distribution as Stat
import qualified Statistics.Distribution.Binomial as Stat
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

main :: IO ()
main = withArgs doReport

withArgs :: ((?howManyPools :: Int, ?howManySlots :: Int) => IO ()) -> IO ()
withArgs k = getArgs >>= \case
  (mapM readMaybe -> Just [pc, sc])
    | pc > 0 && sc > 0 -> do
        let ?howManyPools = pc
            ?howManySlots = sc
        k
  _ -> die $
       "Needs exactly two arguments"
    <> ": "
    <> "the pool count (eg 500) and the slot count (eg 7), both positive."

{-------------------------------------------------------------------------------
  Report
-------------------------------------------------------------------------------}

putStrLn2 :: String -> IO ()
putStrLn2 s = hPutStrLn stderr s

doReport :: (?howManyPools :: Int, ?howManySlots :: Int) => IO ()
doReport = do
  putStrLn2 $
        "These are the probabilities of seeing so many leaders within a"
     <> " "
     <> "sequence of"
     <> " "
     <> "Delta="
     <> showCount ?howManySlots "slot"
     <> ", "
     <> "assuming a uniform distribution among"
     <> " "
     <> showCount ?howManyPools "stake pool"
     <> " and "
     <> "the active slot coefficient f=0.05"
     <> ". "
     <> "We conjecture that that stake distribution"
     <> " "
     <> "maximizes the occurrence of slots with multiple leaders"
     <> ". "
     <> "There can be no more contemporary forks than leaders"
     <> "."

  putStrLn2 ""
  forM_ ((,) <$> allLCs <*> tableProbLeadsLC) $ \(c, p) -> do
    putStrLn $ show (frLC c) <> ":\t" <> show p
  putStrLn2 $
    ">"  <> show (frLC maxLC) <> ":\t" <> show (1 - sum tableProbLeadsLC)

  putStrLn2 ""
  putStrLn2 $
    "approximate seconds in a year:\t" <> show (365 * 24 * 60 * 60 :: Integer)

-- | The number theory partitions of 'howManySlots'
--
-- <https://en.wikipedia.org/wiki/Partition_(number_theory)>
--
-- This is the subset of the our multinomial's support that we're interested in
-- (ie that doesn't involve too many leaders in any one slot).
allPerLCs :: (?howManySlots :: Int) => [PerLC SC]
allPerLCs =
      filter ((== ?howManySlots) . sum . fmap frSC)
    $ sequence
    $ perLC allSCs

-- | How many leaders were observed
totalLeaders :: PerLC SC -> Maybe LC
totalLeaders xs = toLC $ sum $ (*) <$> fmap frLC allLCs <*> fmap frSC xs

showCount :: Int -> String -> String
showCount mag unit = show mag <> " " <> unit <> case mag of
  1 -> ""
  _ -> "s"

{-------------------------------------------------------------------------------
  Counting slots
-------------------------------------------------------------------------------}

-- | A small count of slots
newtype SC = MkSC Int
  deriving (Eq, Ord, Show)

minSC :: SC
minSC = MkSC 0

maxSC :: (?howManySlots :: Int) => SC
maxSC = MkSC ?howManySlots

frSC :: SC -> Int
frSC (MkSC i) = i

allSCs :: (?howManySlots :: Int) => [SC]
allSCs = map MkSC [frSC minSC .. frSC maxSC]

{-------------------------------------------------------------------------------
  Counting leaders
-------------------------------------------------------------------------------}

-- | A small count of leaders
newtype LC = MkLC Int
  deriving (Eq, Ord, Show)

frLC :: LC -> Int
frLC (MkLC i) = i

minLC :: LC
minLC = maybe (error "minLC empty") getMin $ foldMap (Just . Min) allLCs

maxLC :: LC
maxLC = maybe (error "maxLC empty")  getMax $ foldMap (Just . Max) allLCs

toLC :: Int -> Maybe LC
toLC i =
    if minLC <= candidate && candidate <= maxLC
    then Just candidate
    else Nothing
  where
    candidate = MkLC i

-- | One field per valid 'LC' value
data PerLC a = PerLC !a !a !a !a !a !a !a !a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

perLC :: a -> PerLC a
perLC = pure

instance Applicative PerLC where
  pure a = PerLC a a a a a a a a
  (<*>)
    (PerLC f0 f1 f2 f3 f4 f5 f6 f7)
    (PerLC a0 a1 a2 a3 a4 a5 a6 a7) =
    PerLC
      (f0 a0)
      (f1 a1)
      (f2 a2)
      (f3 a3)
      (f4 a4)
      (f5 a5)
      (f6 a6)
      (f7 a7)

-- | @'lookupLC' lc 'allLCs' == lc@
lookupLC :: LC -> PerLC a -> a
lookupLC lc (PerLC x0 x1 x2 x3 x4 x5 x6 x7) = case frLC lc of
  0 -> x0
  1 -> x1
  2 -> x2
  3 -> x3
  4 -> x4
  5 -> x5
  6 -> x6
  7 -> x7
  i -> error $ "bad leader count: " ++ show i

allLCs :: PerLC LC
allLCs = snd $ mapAccumL step 0 (perLC ())
  where
    step i () = (i + 1, MkLC i)

{-------------------------------------------------------------------------------
  Probabilities
-------------------------------------------------------------------------------}

type Prob = Double

-- | The probability some exact number of pools lead the slot
probLeadLC :: (?howManyPools :: Int) => LC -> Prob
probLeadLC lc =
    Stat.probability distr (frLC lc)
  where
    -- TODO I think we might be more accurate customizing this calculation,
    -- since we can reassociate the @(0.95 ** recip ?howManyPools) ^
    -- (?howManyPools - lc)@ factor to @0.95 ** (recip ?howManyPools *
    -- (?howManyPools - lc))@.
    distr = Stat.binomial ?howManyPools phi

    -- probability a pool leads the slot, assuming active slot coefficient
    -- f=0.05
    phi :: Prob
    phi = 1 - 0.95 ** relStake

    relStake :: Double
    relStake = recip $ fromIntegral ?howManyPools

-- | The probability more than 'maxLC' pools lead the slot
_probTooMany :: (?howManyPools :: Int) => Prob
_probTooMany = 1 - sum (fmap probLeadLC allLCs)

-- | The multinomial probabilities for the given small counts occuring in so
-- many times among the slots
probLeadsLCs :: (?howManyPools :: Int, ?howManySlots :: Int) => PerLC SC -> Prob
probLeadsLCs xs =
    -- TODO I was surprised to find that the @statistics@ package does not
    -- define the multinomial distribution. Perhaps there is no established
    -- implementation that is both fast enouch and accurate enough over enough
    -- of the parameter space for the maintainers' standards.
    -- 
    -- This expression is the PDF, according to Wikipedia.
    factorial n * product factors
  where
    n = ?howManySlots

    -- Our exclusive events probabilities do not add up to one, which
    -- superficially seems like a bug. It is OK, however, because this progrom
    -- only calculates the probability of multinomial events that preclude a
    -- single slot having more than 'maxLC' leaders, which is the exclusive
    -- event that would carry the missing probability. In other words, there
    -- should be an additional factor with @p='_probTooMany'@ here, but we only
    -- ever call it with an @x=0@ and that simplifies down to @1@. So it is
    -- safe to omit that factor altogether.
    factors =
          (\p x ->  p ^ x / factorial x)
      <$> fmap probLeadLC allLCs
      <*> fmap frSC xs

-- | The probabilities that each small count is the total among the slots
tableProbLeadsLC :: (?howManyPools :: Int, ?howManySlots :: Int) => PerLC Prob
tableProbLeadsLC = runST $ do
    refs <- sequence $ perLC (newSTRef 0)

    id $
      forM_ allPerLCs        $ \x ->
      forM_ (totalLeaders x) $ \lc -> do
        modifySTRef (lookupLC lc refs) (+ probLeadsLCs x)

    mapM readSTRef refs

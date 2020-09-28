{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Test the bias of a coin via its binomial sampling distribution.
--
-- Import this module qualified. Better yet, import
-- "Test.QuickCheck.Randomized.Coin" qualified instead.

module Test.QuickCheck.Randomized.Coin.Rule (
  -- * Testing
  Arguments (..),
  Coin (..),
  Face (..),
  Rule (..),
  applyArguments,
  applyRule,
  mkRule,
  withRule,
  -- * Making coins
  fromBias,
  fromProperty,
  fromPropertyLabel,
  -- * Analysis
  falseFail,
  falsePass,
  isActuallyTwoSided,
  ) where

import           Data.Coerce (coerce)
import           Data.Type.Coercion (coerceWith)
import           GHC.Generics

import           Test.QuickCheck (arbitrary)
import           Test.QuickCheck.Gen
import qualified Test.QuickCheck.Gen.Unsafe
import           Test.QuickCheck.Property

import           Test.QuickCheck.Randomized.Common
import           Test.QuickCheck.Randomized.Distribution.Binomial
                     (Binomial (..))
import qualified Test.QuickCheck.Randomized.Distribution.Binomial as Binomial

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | The two possible outcomes of a coin toss.
--
-- A biased coin is the archetypal example for this statistical test, where the
-- @p@ parameter of the Bernoulli distribution is the probability of each
-- (idealized) toss landing 'Heads'.
--
-- This wording lets us use the terminology " heads or tails " for each toss
-- instead of " success or failure ", which are already in use in the broader
-- context of a test suite.
data Face = Heads | Tails
  deriving (Eq, Show)

type instance Not Heads = Tails
type instance Not Tails = Heads

{-------------------------------------------------------------------------------
  Test inputs
-------------------------------------------------------------------------------}

-- | The parameters of The Binomial Rule.
data Arguments = Arguments {
    alpha    :: !(MaxBound (Prob FalseFail))
    -- ^ The resulting test will fail with a probability @<= 'alpha'@ when
    -- applied to a correct coin.
    --
    -- NOTE Use 'neverProb' to back-compute 'alpha' from an estimate of how
    -- many times the rule will /ever/ be applied to /a correct coin/. EG 10
    -- times per week, conservatively, for a nightly test, for a project
    -- lifetime of 20 years is 10400 executions of the nightly test suite. Then
    -- multiply that by the (average) number of statistical tests in the suite
    -- to find the 'Count' argument to 'neverProb'.
  , expected :: !(Binomial Heads)
    -- ^ The sampling distribution of a correct coin.
  }
  deriving (Generic, Show, Validate)

-- | See 'mkRule' and 'applyRule'.
data Rule = Rule {
    arguments :: !Arguments
    -- ^ The arguments that induced this rule. The 'tosses' is needed to apply
    -- the rule, but the rest is for convenience only, such as inclusion in the
    -- 'counterexample' message.
  , interval  :: !(Interval (Count Heads))
    -- ^ The resulting test fails iff the number of heads in the sample is not
    -- in this interval.
  }
  deriving (Generic, Show, Validate)

{-------------------------------------------------------------------------------
  Binomial test logic
-------------------------------------------------------------------------------}

-- | The most demanding rule with approximately-equal tail regions such that
-- each satisfies @'alpha' / 2@.
--
-- Is 'Nothing' if the rule is degenerate such that it can never fail. In other
-- words, if @'tosses' . 'expected'@ is too low for the other 'Arguments'.
--
-- NOTE One of the tail regions may be empty; see 'isActuallyTwoSided'.
mkRule :: Arguments -> Maybe Rule
mkRule arguments = assertValid arguments $
    if   interval == Binomial.support expected
    then Nothing
    else Just rule
  where
    Arguments {
        alpha
      , expected
      } = arguments
    Binomial {
        bias
      , tosses
      } = expected

    rule :: Rule
    rule = Rule {
        arguments
      , interval
      }

    interval :: Interval (Count Heads)
    interval  = Interval
        justEnoughHeads
        ( coerceWith (tradeMaxBound tradeCount) $
          complementMinBound
            (complementCount tosses)
            justEnoughTails
        )

    justEnoughHeads :: MinBound (Count Heads)
    justEnoughHeads = Binomial.quantile expected tailProb

    justEnoughTails :: MinBound (Count Tails)
    justEnoughTails =
        Binomial.quantile
          Binomial {
              bias   = coerceWith tradeProb $ complementProb bias
            , tosses
            }
          tailProb

    -- One equal tail of the two-sided test.
    --
    -- NOTE Usually much smaller than the bias, so the one's complement here
    -- would lose precision.
    tailProb :: forall (face :: Face). MinBound (Prob (InRange face))
    tailProb = MinBound $ coerceWith unsafeCoerceProb $ (unMaxBound alpha) / 2

{-------------------------------------------------------------------------------
  Analysis
-------------------------------------------------------------------------------}

-- | Whether the rule's 'interval' induces two non-empty rejection regions.
isActuallyTwoSided :: Rule -> Bool
isActuallyTwoSided rule =
    MinBound 0 /= lo && n /= hi
  where
    Rule {
        arguments
      , interval
      } = rule
    Arguments {
        expected
      } = arguments

    Interval lo hi = interval

    Interval _ n = Binomial.support expected

-- | See 'FalseFail'; the /specification/ in this case is that the coin's
-- actual bias is exactly @'Binomial.bias' . 'expected'@.
falseFail :: Rule -> Prob FalseFail
falseFail rule =
    coerceWith unsafeCoerceProb alpha'
  where
    Rule {
        arguments
      , interval
      } = rule
    Arguments {
        expected
      } = arguments

    alpha' :: ProbNot (InRange Heads)
    alpha' =
        complementProb $ Binomial.intervalCdf expected interval

-- | See 'FalsePass'; the /specification/ in this case is that the rule is
-- forgiven if it passes when the coin is " only slightly " incorrect, ie if
-- its actual bias is within 'Leniency' of @'Binomial.bias' . 'expected'@,
-- inclusive of the bounds.
falsePass :: Rule -> Leniency (Prob Heads) -> Prob FalsePass
falsePass rule leniency =
    coerceWith unsafeCoerceProb beta'
  where
    Rule {
        arguments
      , interval
      } = rule
    Arguments {
        expected
      } = arguments
    Binomial {
        bias
      , tosses
      } = expected

    beta' :: Prob (InRange Heads)
    beta' =
        risk (-) `max` risk (+)
      where
        -- How much the offset sampling distribution overlaps with the correct
        -- sampling distribution's acceptance interval.
        risk :: (Prob Heads -> Prob Heads -> Prob Heads) -> Prob (InRange Heads)
        risk o =
            -- If the leniency extends the bias to 0 and/or 1, then no coin
            -- exists beyond that: the leniency has trivialized that side of
            -- the test.
            if 0 == bias' || 1 == bias' then 0 else
            Binomial.intervalCdf (mkD bias') interval
          where
            bias' :: Prob Heads
            bias' = clipProb $ o bias $ unLeniency leniency

        mkD bias' = Binomial {
            bias   = bias'
          , tosses
          }

{-------------------------------------------------------------------------------
  Propertys
-------------------------------------------------------------------------------}

-- | A coin can be tossed to come up 'Heads' or 'Tails'.
--
-- For interop with QuickCheck, we represent a coin as a 'Property'. The
-- interpretation is as follows.
--
-- o If the underlying predicate is 'False', the toss fatally fails. EG
--   'applyRule' will fail with the relevant counterexample.
--
-- o If the underlying predicate is 'True', then the coin is 'Heads' iff the
--   string 'headsLabel' has been applied via 'label' or 'classify'. It's
--   'Tails' otherwise.
--
-- o If the underlying predicate involved 'discard', then that toss is rejected
--   and we attempt another toss.
data Coin = Coin {
    headsLabel :: !String
    -- ^ The label via 'label' or 'classify' that determines 'Heads'.
  , toss       :: !Property
  }

-- | A coin with the given bias.
fromBias :: Prob Heads -> Coin
fromBias bias = fromProperty $
    if 0 == bias then property False else
    forAll arbitrary $ \p -> property $ p <= bias

-- | Wrapper around 'mkRule'.
--
-- It's @'property' False@ if 'mkRule' yields 'Nothing'.
withRule :: Arguments -> (Rule -> Property) -> Property
withRule arguments k =
    withValid arguments $
    case mkRule arguments of
      Nothing   ->
          counterexample "The rule is degenerate." False
      Just rule ->
          withValid rule $
          k rule

-- | Apply the induced rule to the coin.
applyArguments :: Arguments -> Coin -> Property
applyArguments arguments coin =
    withRule arguments $ \rule -> applyRule rule coin

-- | Apply the rule to the coin.
--
-- Our use here involves the same caveats as 'idempotentIOProperty'. Moreover,
-- QuickCheck's /verbose/ /checking/ will print a result for every toss of the
-- coin!
--
-- NOTE Any 'expectFailure' within the coin is ignored: the argument @'Coin' p@
-- and @'Coin' ('expectFailure' p)@ will induce the same resulting 'Property'
-- via 'applyRule'.
applyRule :: Rule -> Coin -> Property
applyRule rule coin =
    withValid rule $ countHeads (0 :: Count Heads) tosses
  where
    Coin {
        headsLabel
      , toss
      } = coin

    Rule {
        arguments
      , interval
      } = rule
    Arguments {
        expected
      } = arguments
    Binomial {
        tosses
      } = expected

    decide :: Count Heads -> Property
    decide acc = case compareToInterval acc interval of
        LT -> err "Too few Heads!"
        EQ -> property True
        GT -> err "Too many Heads!"
      where
        err :: String -> Property
        err s =
            counterexample (s <> show acc <> " for " <> show rule) False

    -- IO Gen Prop ~# IO (Seed -> Size -> Prop)
    --
    -- Gen IO Prop ~# Seed -> Size -> IO Prop
    --
    -- commute m seed size = (\g -> g seed size) <$> m
    --
    -- QuickCheck itself uses the IO layer for eg verbose printing via
    -- callbacks, catching pure exceptions, timeouts, etc. The IO layer is
    -- /also/ used for stuff the user injected via 'ioProperty' eg.
    --
    -- Our use here involves the same caveats as 'idempotentIOProperty'.
    commute :: IO (Gen Prop) -> Gen (IO Prop)
    commute = Test.QuickCheck.Gen.Unsafe.promote

    embed :: IO Property -> Gen Prop
    embed =
        fmap (MkProp . IORose . fmap unProp) .
        commute .
        fmap unProperty

    -- TODO accumulate labels etc for Tails, to include in counterexample
    -- message?
    countHeads :: Count Heads -> Count (Trial Face) -> Property
    countHeads acc i
        | i <= (0 :: Count (Trial Face)) = decide acc
        | otherwise                      = MkProperty $ do
            -- NOTE It is this bind that splits the PRNG state.
            rose <- unProp <$> unProperty toss :: Gen (Rose Result)
            embed $ do
              MkRose res _roses <- reduceRose rose
              pure $ case ok res of
                Nothing    -> countHeads acc i
                Just False -> MkProperty $ pure $ MkProp rose
                Just True  -> do
                  let hit1 = headsLabel `elem` classes res
                      hit2 = headsLabel `elem` labels res
                      upd = if hit1 || hit2 then (+1) else id
                  countHeads (upd acc) (i - 1)

-- | Use a 'Property' as a coin. A 'Property' sample is 'Heads' if the
-- specified label is present via 'label' or 'classify', 'Tails' otherwise.
--
-- NOTE A 'discard' induces a replacement toss. Any counterexample from the
-- coin causes the whole statistical test to fail.
fromPropertyLabel :: String -> Property -> Coin
fromPropertyLabel headsLabel toss = Coin {
    headsLabel
  , toss
  }

-- | Use a 'Property' as a coin. A successful 'Property' is 'Heads' and a
-- failing 'Property' is 'Tails'.
--
-- NOTE A 'discard' induces a replacement toss.
fromProperty :: Property -> Coin
fromProperty prop = Coin {
    headsLabel
  , toss
  }
  where
    headsLabel = "quickcheck-randomized:Heads"
    toss       = onEachResult tweakResult prop

    tweakResult :: Result -> Result
    tweakResult res = case ok res of
        Nothing    -> res
        Just False -> res'{ok = Just True}
        Just True  -> res'{labels  = headsLabel : labels res}
      where
        res' = res {
            classes = filter (/= headsLabel) $ classes res
          , labels  = filter (/= headsLabel) $ labels res
          }

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

onEachResult :: (Result -> Result) -> Property -> Property
onEachResult f =
    coerce $
    (fmap $ fmap f :: Gen (Rose Result) -> Gen (Rose Result))

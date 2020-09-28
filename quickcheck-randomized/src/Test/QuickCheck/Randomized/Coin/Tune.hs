{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Tune the "Test.QuickCheck.Randomized.Coin" test.
--
-- Import this module qualified. Better yet, import
-- "Test.QuickCheck.Randomized.Coin" qualified instead.

module Test.QuickCheck.Randomized.Coin.Tune (
  -- * Tune 'leniency'
  LeniencyArguments (..),
  TunedLeniency (..),
  tuneLeniency,
  -- * Tune 'tosses'
  TossesArguments (..),
  TunedTosses (..),
  applyTossesArguments,
  tuneTosses,
  ) where

import           Control.Monad (guard)
import           Data.Maybe (fromMaybe)
import           GHC.Generics

import           Test.QuickCheck.Property

import           Test.QuickCheck.Randomized.Coin.Rule
import           Test.QuickCheck.Randomized.Common
import           Test.QuickCheck.Randomized.Distribution.Binomial
                     (Binomial (..))

{-------------------------------------------------------------------------------
  Tuning tosses
-------------------------------------------------------------------------------}

-- | See 'tuneTosses'.
data TossesArguments = TossesArguments {
    alpha     :: !(MaxBound (Prob FalseFail))
    -- ^ See 'alpha' for 'Arguments'.
  , beta      :: !(MaxBound (Prob FalsePass))
  , bias      :: !(Prob Heads)
  , leniency  :: !(Leniency (Prob Heads))
    -- ^ Defines the condition for 'FalsePass'.
    --
    -- NOTE It does not affect 'FalseFail'.
  , maxTosses :: !(MaxBound (Count (Trial Face)))
  }
  deriving (Generic, Show, Validate)

-- | See 'tuneTosses'.
data TunedTosses
    = OkTosses !(MinBound (Count (Trial Face))) !Rule
      -- ^ The minimum number of tosses. For convenience, we also include the
      -- corresponding rule.
    | InsufficientMaxTosses
      -- ^ Even 'maxTosses' could not satisfy the remaining 'TossesArguments'.
  deriving (Show)

-- | Finds the mininum number of tosses up to 'maxTosses' that induces a
-- non-degenerate rule that satisfies the specified 'alpha' and 'beta' limits,
-- if any.
tuneTosses :: TossesArguments -> TunedTosses
tuneTosses hyper =
    assertValid hyper $
    case smallestInIntervalMaybe interval predicate of
      Nothing             -> InsufficientMaxTosses
      Just (tosses, rule) -> OkTosses (MinBound tosses) rule
  where
    TossesArguments {
        alpha
      , beta
      , bias
      , leniency
      , maxTosses
      } = hyper

    interval :: Interval (Count (Trial Face))
    interval = MinBound 0 `Interval` maxTosses

    mkD :: Count (Trial Face) -> Prob Heads -> Binomial Heads
    mkD k b = Binomial {
        bias   = b
      , tosses = k
      }

    -- Upward-closed because risk is antitone wrt number of tosses and @(<= q)@
    -- is downward-closed.
    predicate :: Count (Trial Face) -> Maybe (Count (Trial Face), Rule)
    predicate k = do
        rule <- mkRule Arguments {
            alpha
          , expected = mkD k bias
          }
        guard $ falsePass rule leniency <= unMaxBound beta
        pure (k, rule)

-- | Apply the induced rule to the coin.
applyTossesArguments :: TossesArguments -> Coin -> Property
applyTossesArguments hyper coin =
    case tuneTosses hyper of
        OkTosses (MinBound _tosses) rule -> applyRule rule coin
        InsufficientMaxTosses            ->
          counterexample
            (prefix <> "The " <> show hyper <> " are unsatisfiable.")
            False
  where
    prefix = "applyTossesArguments: "

{-------------------------------------------------------------------------------
  Tuning leniency
-------------------------------------------------------------------------------}

-- | See 'tuneLeniency'.
data LeniencyArguments = LeniencyArguments {
    beta         :: !(MaxBound (Prob FalsePass))
  , leniencyGrid :: !(Count (Leniency (Prob Heads)))
    -- ^ How many evenly-spaced leniencies from the half-open @(0, 1]@ to
    -- consider.
  , rule         :: !Rule
  }
  deriving (Generic, Show, Validate)

-- | See 'tuneLeniency'.
data TunedLeniency
    = OkLeniency !(MinBound (Leniency (Prob Heads)))
    | DegenerateLeniency
      -- ^ There is no coin outside of the necessary leniency interval.
  deriving (Show)

-- | Finds the mininum 'leniency' on the given grid that satisfies the
-- specified 'alpha' and 'beta' limits, if any.
tuneLeniency :: LeniencyArguments -> TunedLeniency
tuneLeniency hyper = assertValid hyper $ do
    case smallestInInterval leniencyInterval predicate of
      Nothing ->
          -- The grid includes a leniency of 1, which totally trivializes the
          -- test for any bias, and so even a 'beta' of 0 will be satisfiable.
          error "impossible"
      Just i  ->
          if degenerate then DegenerateLeniency else
          OkLeniency (MinBound leniency)
        where
          leniency :: Leniency (Prob Heads)
          leniency = mkL i

          degenerate :: Bool
          degenerate =
              bias - unLeniency leniency <= 0 &&
              bias + unLeniency leniency >= 1
  where
    LeniencyArguments {
        beta
      , leniencyGrid
      , rule
      } = hyper
    Rule {
        arguments
      } = rule
    Arguments {
        expected
      } = arguments
    Binomial {
        bias
      } = expected

    -- Leniency must be positive.
    leniencyInterval :: Interval (Count (Leniency (Prob Heads)))
    leniencyInterval = MinBound 1 `Interval` MaxBound leniencyGrid

    -- Monotonic. Positive if the argument is. Always @<= 1@.
    mkL :: Count (Leniency (Prob Heads)) -> Leniency (Prob Heads)
    mkL (Count i) =
        Leniency $ Prob $
        fromIntegral i / fromIntegral (unCount leniencyGrid)

    -- Upward-closed because the risk is antitone wrt leniency and @(<= q)@ is
    -- downward-closed.
    predicate :: Count (Leniency (Prob Heads)) -> Bool
    predicate i = falsePass rule (mkL i) <= unMaxBound beta

_example1 :: TunedTosses
_example1 = tuneTosses TossesArguments {
    alpha     = MaxBound $ Prob 1e-6
  , beta      = MaxBound $ Prob 1e-3
  , bias      = Prob 0.01
  , leniency  = Leniency $ Prob 0.03
  , maxTosses = MaxBound $ Count $ 10 ^ (6 :: Int)
  }

_example2 :: TunedLeniency
_example2 = tuneLeniency LeniencyArguments {
    beta         = MaxBound $ Prob 1e-2
  , leniencyGrid = Count $ 10 ^ (6 :: Int)
  , rule
  }
  where
    rule = fromMaybe (error "_example2") $ mkRule Arguments {
        alpha    = MaxBound $ Prob 1e-6
      , expected = Binomial {
          bias   = Prob 0.2
        , tosses = Count $ 10 ^ (5 :: Int)
        }
      }

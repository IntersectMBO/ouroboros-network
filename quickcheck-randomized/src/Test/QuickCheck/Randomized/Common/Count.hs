{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.QuickCheck.Randomized.Common.Count (
  -- * Count
  Count_ (..),
  Count,
  CountNot,
  complementCount,
  tradeCount,
  unsafeCoerceCount,
  ) where

import           Control.Exception (assert)
import           Data.Proxy
import           Data.Type.Coercion
import           GHC.Generics
import           Quiet (Quiet (..))


import           Test.QuickCheck.Randomized.Common.Complements
import           Test.QuickCheck.Randomized.Common.Events
import           Test.QuickCheck.Randomized.Common.Validation

-- | A count, @0 <= k < infinity@.
--
-- The phantom arguments indicate the corresponding event, including whether or
-- not it is complemented.
--
-- NOTE We use a signed representation so that underflow is detectable.
newtype Count_ (pol :: Bool) (ev :: k) = Count {unCount :: Int}   -- TODO Integer?
  deriving stock (Generic)
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real)
  deriving (Show) via Quiet (Count_ pol ev)

-- | Non-negative
instance Validate (Count_ pol ev) where
  validate (Count k) =
      invalidityUnless (k >= 0) $
      "Invalid count! " <> show k <> " is negative."

-- | The event is not complemented.
type Count    = Count_ True

-- | The event is complemented.
type CountNot = Count_ False

-- | See 'Test.QuickCheck.Randomized.Common.Probability.unsafeProb'.
unsafeCoerceCount :: Count_ pol1 (ev1 :: k1) `Coercion` Count_ pol2 (ev2 :: k2)
unsafeCoerceCount = Coercion

-- | See 'Test.QuickCheck.Randomized.Common.Probability.complementProb'.
complementCount :: forall pol1 pol2 k ev.
     Complements pol1 pol2
  => Count (Trial k) -> Count_ pol1 (ev :: k) -> Count_ pol2 ev
complementCount (Count n) (Count k) =
    assert (n >= k) $
    Count $ n - k
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)

-- | See 'Test.QuickCheck.Randomized.Common.Probability.tradeProb'.
tradeCount :: forall pol1 pol2 k ev1 ev2.
     (Complements pol1 pol2, Complements ev1 ev2)
  => Count_ pol1 (ev1 :: k) `Coercion` Count_ pol2 ev2
tradeCount =
    Coercion
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)
    _ = complements (Proxy :: Proxy ev1)  (Proxy :: Proxy ev2)

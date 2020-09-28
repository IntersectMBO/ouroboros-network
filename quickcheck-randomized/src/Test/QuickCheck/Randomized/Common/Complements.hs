{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.QuickCheck.Randomized.Common.Complements (
  Not,
  Complements,
  complements,
  ) where

import           Data.Proxy

-- | In a general sense, @'Not' a@ is the complement of @a@.
--
-- For 'Bool' it's a type-level @not@. For /events/, it's the complementary
-- event, eg heads versus tails.
type family Not (a :: k) :: k

type instance Not True  = False
type instance Not False = True

-- | This constraint allows @pol1@ to determine @pol2@ and vice versa.
type Complements pol1 pol2 = (pol1 ~ Not pol2, pol2 ~ Not pol1)

-- | Invoked in order to incorporate semantic constraints that GHC would
-- otherwise consider redundant.
complements :: forall k proxy (a :: k) b.
       Complements a b => proxy a -> proxy b -> ()
complements _ _ =
    ()
  where
    _ = (Proxy :: Proxy a) `asTypeOf` (Proxy :: Proxy (Not b))
    _ = (Proxy :: Proxy b) `asTypeOf` (Proxy :: Proxy (Not a))


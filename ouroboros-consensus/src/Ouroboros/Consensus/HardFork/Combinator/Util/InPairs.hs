{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
module Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (
    -- * InPairs
    InPairs(..)
  , hmap
  , hcmap
  , hpure
  , hcpure
    -- * Requiring
  , Requiring(..)
  , RequiringBoth(..)
  , requiring
  , requiringBoth
  ) where

import           Data.SOP.Strict hiding (hcmap, hcpure, hmap, hpure)

import           Ouroboros.Consensus.Util.SOP

{-------------------------------------------------------------------------------
  InPairs
-------------------------------------------------------------------------------}

-- | We have an @f x y@ for each pair @(x, y)@ of successive list elements
data InPairs (f :: k -> k -> *) (xs :: [k]) where
  PNil  :: InPairs f '[x]
  PCons :: f x y -> InPairs f (y ': zs) -> InPairs f (x ': y ': zs)

hmap :: SListI xs => (forall x y. f x y -> g x y) -> InPairs f xs -> InPairs g xs
hmap = hcmap (Proxy @Top)

hcmap :: forall proxy c f g xs. All c xs
      => proxy c
      -> (forall x y. (c x, c y) => f x y -> g x y)
      -> InPairs f xs -> InPairs g xs
hcmap _ f = go
  where
    go :: All c xs' => InPairs f xs' -> InPairs g xs'
    go PNil         = PNil
    go (PCons x xs) = PCons (f x) (go xs)

hpure :: (SListI xs, IsNonEmpty xs) => (forall x y. f x y) -> InPairs f xs
hpure = hcpure (Proxy @Top)

hcpure :: forall proxy c xs f. (All c xs, IsNonEmpty xs)
       => proxy c
       -> (forall x y. (c x, c y) => f x y) -> InPairs f xs
hcpure _ f =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} -> go sList
  where
    go :: (c x, All c xs') => SList xs' -> InPairs f (x ': xs')
    go SNil  = PNil
    go SCons = PCons f (go sList)

{-------------------------------------------------------------------------------
  RequiringBoth
-------------------------------------------------------------------------------}

data Requiring h f x y = Require {
      provide :: h x -> f x y
    }

data RequiringBoth h f x y = RequireBoth {
      provideBoth :: h x -> h y -> f x y
    }

requiring :: SListI xs => NP h xs -> InPairs (Requiring h f) xs -> InPairs f xs
requiring np =
      requiringBoth np
    . hmap (\f -> RequireBoth $ \hx _hy -> provide f hx)

requiringBoth :: NP h xs -> InPairs (RequiringBoth h f) xs -> InPairs f xs
requiringBoth = flip go
  where
    go :: InPairs (RequiringBoth h f) xs -> NP h xs -> InPairs f xs
    go PNil         _              = PNil
    go (PCons f fs) (x :* y :* zs) = PCons (provideBoth f x y) (go fs (y :* zs))

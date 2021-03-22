{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- > import Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
module Ouroboros.Consensus.HardFork.Combinator.Util.Tails (
    Tails (..)
    -- * Convenience constructors
  , mk1
  , mk2
  , mk3
    -- * SOP-like operators
  , hcmap
  , hcpure
  , hmap
  , hpure
  ) where

import           Data.Kind (Type)
import           Data.SOP.Strict hiding (hcmap, hcpure, hmap, hpure)
import qualified Data.SOP.Strict as SOP

{-------------------------------------------------------------------------------
  Tails
-------------------------------------------------------------------------------}

-- | For every tail @(x ': xs)@ of the list, an @f x y@ for every @y@ in @xs@
data Tails (f :: k -> k -> Type) (xs :: [k]) where
  TNil  :: Tails f '[]
  TCons :: NP (f x) xs -> Tails f xs -> Tails f (x ': xs)

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

mk1 :: Tails f '[x]
mk1 = TCons Nil TNil

mk2 :: f x y -> Tails f '[x, y]
mk2 xy = TCons (xy :* Nil) mk1

mk3 :: f x y -> f x z -> f y z -> Tails f '[x, y, z]
mk3 xy xz yz = TCons (xy :* xz :* Nil) (mk2 yz)

{-------------------------------------------------------------------------------
  SOP-like operators
-------------------------------------------------------------------------------}

hmap :: SListI xs
     => (forall x y. f x y -> g x y)
     -> Tails f xs -> Tails g xs
hmap = hcmap (Proxy @Top)

hcmap :: forall proxy c f g xs. All c xs
      => proxy c
      -> (forall x y. (c x, c y) => f x y -> g x y)
      -> Tails f xs -> Tails g xs
hcmap p g = go
  where
    go :: All c xs' => Tails f xs' -> Tails g xs'
    go TNil           = TNil
    go (TCons fs fss) = TCons (SOP.hcmap p g fs) (go fss)

hpure :: SListI xs => (forall x y. f x y) -> Tails f xs
hpure = hcpure (Proxy @Top)

hcpure :: forall proxy f c xs. All c xs
       => proxy c
       -> (forall x y. (c x, c y) => f x y) -> Tails f xs
hcpure p f = go sList
  where
    go :: All c xs' => SList xs' -> Tails f xs'
    go SNil  = TNil
    go SCons = TCons (SOP.hcpure p f) (go sList)

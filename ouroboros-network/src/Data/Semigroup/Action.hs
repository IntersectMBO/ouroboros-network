{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Semigroup.Action where

-- | Semigroup action.  It should satisfy:
--
-- @
--   x <| s0 <| s1 = x <| s0 <> s1
-- @
class Semigroup s => SAct s x where
    (<|) :: x -> s -> x

-- binds less than '<>'
infixr 5 <|

instance Semigroup s => SAct s s where
    (<|) = (<>)

-- this instance could be generalised to other functors than representable
-- ones (i.e. @y -> x@ for a fixed type @y@).
instance SAct s x => SAct s (y -> x) where
    f <| s = \y -> f y <| s

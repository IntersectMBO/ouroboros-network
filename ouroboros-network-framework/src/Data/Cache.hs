{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Cache
  ( Cache (..)
  , withCacheA
  , traceWithCache
  , mapTraceWithCache
  )
  where

import Control.Monad (when)
import Control.Tracer (Tracer, traceWith)

-- | Cache newtype wrapper allows to perform an action only if the cache
-- is not up-to-date, i.e. different than another value dimmed more recent.
--
newtype Cache a = Cache { getCache :: a }
  deriving (Eq, Show, Semigroup, Monoid, Functor)

-- | Run a computation that depends on a certain cached value, only if the
-- the most recent one is different.
--
withCacheA :: (Applicative m, Eq a) => Cache a -> a -> (a -> m ()) -> m ()
withCacheA (Cache a) a' action =
    when (a /= a') $
      action a'

-- | Trace with cache only performs the tracing when the cached value is
-- different than the most recent one.
--
traceWithCache :: (Applicative m, Eq a) => Tracer m a -> Cache a -> a -> m ()
traceWithCache tracer cache a =
    withCacheA cache a (traceWith tracer)

-- | Trace with cache only performs the tracing when the cached value is
-- different than the most recent one. And applies a function to the cache
-- value before tracing.
--
mapTraceWithCache :: (Applicative m, Eq a)
                  => (a -> b) -> Tracer m b -> Cache a -> a -> m ()
mapTraceWithCache f tracer cache a =
    withCacheA cache a (traceWith tracer . f)

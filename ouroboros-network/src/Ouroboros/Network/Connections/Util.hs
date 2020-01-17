{-# LANGUAGE BangPatterns #-}

module Ouroboros.Network.Connections.Util
  ( forContinuation
  ) where

-- | `forM` but in continuation passing style.
--
-- @
--   Cont t = forall r . (t -> IO r) -> IO r
--
--   forContinuation :: [i] -> (i -> Cont t) -> Cont [t]
-- @
--
-- This is useful for instance when constructing a bunch of
-- `Ouroboros.Network.Connections.Socket.Server.server`s. When the final
-- continuation here is called, each of the servers is listening (although
-- not running accept loops) and when the continuation finishes, they will all
-- be torn down.
forContinuation :: [i] -> (i -> ((n -> IO r) -> IO r)) -> ([n] -> IO r) -> IO r
forContinuation = go []
  where
  go !acc []     _  k = k (reverse acc)
  go !acc (i:is) mk k = mk i $ \n -> go (n : acc) is mk k

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Signal where

import qualified Deque.Lazy as Deque
import           Deque.Lazy (Deque)
import qualified Data.Foldable as Deque (toList)
import qualified Data.Signal as Signal
import           Data.Signal (Signal)

import           Control.Monad.Class.MonadTime (Time)

import           Test.QuickCheck


-- | Check a property over a 'Signal'. The property should be true at all times.
--
-- On failure it shows the @n@ most recent signal values.
--
signalProperty :: forall a. Int -> (a -> String)
               -> (a -> Bool) -> Signal a -> Property
signalProperty atMost showSignalValue p =
    go 0 mempty . Signal.eventsToList . Signal.toChangeEvents
  where
    go :: Int -> Deque (Time, a) -> [(Time, a)] -> Property
    go !_ !_ []                   = property True
    go !n !q ((t, x) : txs) | p x = next
      where
        next
          | n < atMost = go (n+1) (              Deque.snoc (t,x)  q) txs
          | otherwise  = go n     ((Deque.tail . Deque.snoc (t,x)) q) txs

    go !_ !recent ((t, x) : _) = counterexample details (property False)
      where
        details =
          unlines [ "Last " ++ show atMost ++ " signal values:"
                  , unlines [ show t' ++ "\t: " ++ showSignalValue x'
                            | (t',x') <- Deque.toList recent ]
                  , "Property violated at: " ++ show t
                  , "Invalid signal value:"
                  , showSignalValue x
                  ]

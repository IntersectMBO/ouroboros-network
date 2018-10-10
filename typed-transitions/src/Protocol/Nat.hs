{-# LANGUAGE RankNTypes #-}

module Protocol.Nat
  ( Nat (..)
  , uniformNat
  , elimFreeT
  ) where

import Control.Monad.Trans.Free

-- | A natural transformation which also gives the next 'Nat f g' inside 'g'.
-- This representation, as opposed to a simple
--   forall t . f t -> g t
-- makes it possible to express more intricate logic by way of capturing
-- terms in a closure.
newtype Nat f g = Nat
  { runNat :: forall t . f t -> g (t, Nat f g)
  }

uniformNat :: Functor g => (forall t . f t -> g t) -> Nat f g
uniformNat nat = Nat (\it -> fmap (flip (,) (uniformNat nat)) (nat it))

-- | Eliminate a 'FreeT f' by injecting it into 'm'.
elimFreeT :: ( Monad m ) => Nat f m -> FreeT f m t -> m (t, Nat f m)
elimFreeT nat (FreeT m) = do
  term <- m
  case term of
    Pure t -> pure (t, nat)
    Free next -> do
      (m', nat') <- runNat nat next
      elimFreeT nat' m'

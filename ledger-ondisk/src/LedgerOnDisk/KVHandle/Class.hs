{-# LANGUAGE RankNTypes #-}
-- |

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module LedgerOnDisk.KVHandle.Class where

import Data.Kind
import Control.Monad.IO.Class

class KVable a where
  type KVKeySet a :: Type
  type KVDiff a :: Type
  type KVFromDB a :: Type

  applyDiff :: KVDiff a -> a -> a
  constructFromDB :: forall f. Functor f => (KVFromDB a -> f (KVFromDB a)) -> a -> f a

class KVable kv => KVHandle kv t | t -> kv where
  type KVMonad t :: (Type -> Type) -> Constraint
  type KVMonad t = MonadIO
  type KVReadSet t :: Type
  type KVErr t :: Type
  type KVErr t = ()

  prepare :: KVMonad t m => t -> KVKeySet kv -> m (KVReadSet t)
  submit :: KVMonad t m => t -> KVReadSet t -> (kv -> (KVDiff kv, a)) -> m (Either (KVErr t) a)

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Enum.Min
  ( MinEnum (..)
  ) where

import Data.Proxy
import Data.Kind (Type)
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

import Generics.SOP


newtype MinEnum (n :: Nat) a = MinEnum { getMinEnum :: a }

genericFromEnum :: forall a. IsEnumType a => a -> Int
genericFromEnum a = case from a of
      SOP ns -> go ns
    where
      go :: NS b bs -> Int
      go Z {}  = 0
      go (S x) = 1 + go x

genericToEnum :: forall a. IsEnumType a => Int -> a
genericToEnum = to . SOP . go
  where
    go :: Int -> NS (NP I) (Code a)
    go n | n < 0 = error "genericToEnum: negative value"
    go 0 = unsafeCoerce (Z Nil)
    go n = unsafeCoerce (S (go (n - 1)))

instance (KnownNat n, IsEnumType a) => Enum (MinEnum n a) where
    fromEnum (MinEnum a) = genericFromEnum a + fromIntegral (natVal (Proxy :: Proxy n))
    toEnum   n           = MinEnum $ genericToEnum (n - fromIntegral (natVal (Proxy :: Proxy n)))

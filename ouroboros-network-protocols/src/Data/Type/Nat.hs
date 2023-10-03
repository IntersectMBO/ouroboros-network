{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Data.Type.Nat
  ( N (..)
  , Nat (Zero, Succ)
  , natToInt
  , unsafeIntToNat
  , QueueDepth
  , queueDepthNat
  , queueFDepthNat
  ) where

import           Data.Singletons
import           Data.Type.Queue
import           Unsafe.Coerce

-- | A type level inductive natural number.
data N = Z | S N

-- | A value level inductive natural number, indexed by the corresponding type
-- level natural number 'N'.
--
-- This is often needed when writing pipelined peers to be able to count the
-- number of outstanding pipelined yields, and show to the type checker that
-- 'SenderCollect' and 'SenderDone' are being used correctly.
--
newtype Nat (n :: N) = UnsafeInt Int
  deriving Show via Int

data IsNat (n :: N) where
  IsZero ::          IsNat Z
  IsSucc :: Nat n -> IsNat (S n)

toIsNat :: Nat n -> IsNat n
toIsNat (UnsafeInt 0) = unsafeCoerce IsZero
toIsNat (UnsafeInt n) = unsafeCoerce (IsSucc (UnsafeInt (pred n)))

pattern Zero :: () => Z ~ n => Nat n
pattern Zero <- (toIsNat -> IsZero) where
  Zero = UnsafeInt 0

pattern Succ :: () => (m ~ S n) => Nat n -> Nat m
pattern Succ n <- (toIsNat -> IsSucc n) where
  Succ (UnsafeInt n) = UnsafeInt (succ n)

{-# COMPLETE Zero, Succ #-}

natToInt :: Nat n -> Int
natToInt (UnsafeInt n) = n

unsafeIntToNat :: Int -> Nat n
unsafeIntToNat = UnsafeInt

type instance Sing = Nat
instance SingI Z     where sing = Zero
instance SingI n
      => SingI (S n) where sing = Succ sing


type        QueueDepth :: Queue ps -> N
type family QueueDepth q where
  QueueDepth Empty    = Z
  QueueDepth (a <| q) = S (QueueDepth q)


queueDepthNat :: SingQueue q -> Nat (QueueDepth q)
queueDepthNat (UnsafeSingQueue n) = (UnsafeInt n)

queueFDepthNat :: SingQueueF f q -> Nat (QueueDepth q)
queueFDepthNat  SingEmptyF     = Zero
queueFDepthNat (SingConsF _ q) = Succ (queueFDepthNat q)

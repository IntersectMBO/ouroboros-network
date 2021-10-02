{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE ViewPatterns             #-}

-- | Type level queues.
--
module Data.Type.Queue where

import           Data.Kind (Type)
import           Unsafe.Coerce (unsafeCoerce)

import           Data.Singletons


-- | Transition kind.
--
data Trans ps where
    Tr :: forall ps. ps -> ps -> Trans ps


-- | Singleton for @'Trans' ps@ kind.
--
type SingTrans :: Trans ps -> Type
data SingTrans tr where
    SingTr :: forall ps (st :: ps) (st' :: ps).
              SingTrans (Tr st st')

-- | Queue kind.  The type level queue is used to push pipelined transitions
-- and pop them from its other side when one is requesting to collect pipelined
-- results.
--
data Queue ps where
  Empty :: Queue ps
  Cons  :: Trans ps -> Queue ps -> Queue ps

-- | Cons type alias
--
type  (<|) :: Trans ps -> Queue ps -> Queue ps
type a <| as = Cons a as
infixr 5 <|

-- | Snoc operator
--
type (|>) :: Queue ps -> Trans ps -> Queue ps
type family as |> b where
     Empty     |> b = Cons b Empty
     (a <| as) |> b = a <| (as |> b)
infixr 5 |>

-- | Singleton data type which allows to track the types of kind
-- @'Queue' ps@ and store a value which depends on a queued transition.
--
-- TODO: an optimised version of 'SingQueueF' which does not recurs.
--
type SingQueueF :: (ps -> ps -> Type) -> Queue ps -> Type
data SingQueueF f q where
    SingEmptyF :: SingQueueF f Empty
    SingConsF  :: forall ps f (st :: ps) (st' :: ps) (q :: Queue ps).
                  !(f st st')
               -> !(SingQueueF f q)
               -> SingQueueF f (Tr st st' <| q)

deriving instance
         (forall (st :: ps) (st' :: ps). Show (f st st'))
      => Show (SingQueueF f (q :: Queue ps))


-- | A synonym for 'SingConsF'
--
(<|) :: forall ps f (st :: ps) (st' :: ps) (q :: Queue ps).
        f st st'
     -> SingQueueF f q
     -> SingQueueF f (Tr st st' <| q)
(<|) = SingConsF


-- | Term level '|>' (snoc).
--
(|>) :: forall ps f (st :: ps) (st' :: ps) (q :: Queue ps).
        SingQueueF f q
     -> f st st'
     -> SingQueueF f (q |> Tr st st')
(|>)  SingEmptyF     !f  = SingConsF f SingEmptyF
(|>) (SingConsF f q) !f' = SingConsF f (q |> f')


queueFDepth :: SingQueueF f q -> Int
queueFDepth  SingEmptyF      = 0
queueFDepth (SingConsF _ q') = 1 + queueFDepth q'

-- | A space efficient singleton for a non-empty 'Queue' type.  It has two
-- public constructors 'SingSingleton' and 'SingCons'.
--
-- We use a 'newtype' rather than a 'GADT' which would allow to restrict 'q' to
-- non-empty types.  The safe api is not checked by the type checker, it is
-- rather declared to be safe, e.g. 'SingSingleton' and 'SingCons' are declared
-- to define a complete pattern match.  Using 'UnsafeSingQueue' does not guarantee
-- that the changes to the internal representation reflect correctly the changes
-- at the type level nevertheless using it allows to reduce computational
-- complexity (see 'snoc' below).
--
type SingQueue :: Queue ps -> Type
newtype SingQueue q = UnsafeSingQueue Int
  deriving Show

queueDepth :: SingQueue q -> Int
queueDepth (UnsafeSingQueue depth) = depth

type instance Sing = SingQueue
instance SingI Empty            where sing = SingEmpty
instance SingI               q
      => SingI (Tr st st' <| q) where sing = SingCons sing

-- | 'IsQueue' is an auxiliary type which allows to pattern match if the queue
-- is a singleton or not.  The 'toIsQueue' function converts 'SingQueue' to
-- 'IsQueue' in an efficient way.
--
-- 'IsQueue' mimics an inductive definition, but instead recursion, it is using
-- 'SingQueue' in its 'IsCons' constructor.
--
type IsQueue :: Queue ps -> Type
data IsQueue q where
    IsEmpty :: IsQueue Empty
    IsCons  :: forall ps (st :: ps) (st' :: ps) (q :: Queue ps).
               SingQueue               q
            -> IsQueue   (Tr st st' <| q)

-- | Transform 'SingQueue' to 'IsQueue'.  Although this function is using
-- 'unsafeCoerce' it is safe.
--
toIsQueue :: SingQueue q -> IsQueue q
toIsQueue (UnsafeSingQueue n) | n < 0
                              = error "toIsQueue: invalid value"
toIsQueue (UnsafeSingQueue 0) = unsafeCoerce  IsEmpty
toIsQueue (UnsafeSingQueue n) = unsafeCoerce (IsCons (UnsafeSingQueue $ pred n))
  -- we subtract one, because 'IsCons' constructor takes singleton for the
  -- remaining part of the list.

-- | A safe 'SingQueue' bidirectional pattern for queues which holds exactly
-- one element.
--
pattern SingEmpty :: ()
                  => q ~ Empty
                  => SingQueue q
pattern SingEmpty <- (toIsQueue -> IsEmpty)
  where
    SingEmpty = UnsafeSingQueue 0


-- | A safe 'SingQueue' bidirectional pattern for queues of length 2 or more.
--
pattern SingCons :: forall ps (q :: Queue ps).
                    ()
                 => forall (st :: ps) (st' :: ps) (q' :: Queue ps).
                    (q ~ (Tr st st' <| q'))
                 => SingQueue q'
                    -- ^ singleton for the remaining part of the queue
                 -> SingQueue q
pattern SingCons n <- (toIsQueue -> IsCons n)
  where
    SingCons (UnsafeSingQueue n) = UnsafeSingQueue (succ n)

{-# COMPLETE SingEmpty, SingCons #-}


-- | A singleton for singleton queue.
--
singSingleton :: SingQueue (Tr st st' <| Empty)
singSingleton = SingCons SingEmpty


-- | 'snoc'
--
-- /complexity:/ @O(1)@
--
-- It is accessing the internal representation of 'SingQueue' in an unsafe way.
-- It is possible to implement it using the safe api but then it would be @O(n)@
-- instead.
--
snoc :: forall ps (st :: ps) (st' :: ps) (q :: Queue ps).
        SingQueue  q
     -> SingTrans      (Tr st st')
     -> SingQueue (q |> Tr st st')
snoc (UnsafeSingQueue n) _ = UnsafeSingQueue (succ n)


uncons :: SingQueue (Tr st st <| q)
       -> SingQueue              q
uncons (SingCons q) = q


toSingQueue :: SingQueueF f q -> SingQueue q
toSingQueue  SingEmptyF     = SingEmpty
toSingQueue (SingConsF _ q) = SingCons (toSingQueue q)

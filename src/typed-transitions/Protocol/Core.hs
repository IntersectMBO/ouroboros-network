{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Protocol.Core where

import Data.Kind (Type)
import Data.Typeable

-- |
-- = Using a type transition system to describe either side of a
-- request/response application.

-- | The other definitions in this module are to support or to use this
-- datatype. It's somewhat like a pipe or conduit, but it's one-sided. It
-- yields to and awaits from the same end. The type of thing which is produced
-- or expected depends upon the type transition system 'tr', which also
-- controls, by way of the partition 'p' (see the type family 'Partition'),
-- at what point a yielder becomes an awaiter.
data Peer p (tr :: st -> st -> Type) (from :: Status st) (to :: Status st) f t where
  PeerDone :: t -> Peer p tr end end f t
  PeerHole :: f (Peer p tr from to f t) -> Peer p tr from to f t
  -- | When yielding a transition, there must be proof that, under 'p', the
  -- transition either holds or releases control. This is enough to allow GHC
  -- to deduce that the continuation in a complementary 'PeerAwait' (one which
  -- is awaiting on the same initial state) will either
  --   - continue to await if this one continues to yield (hold)
  --   - begin to yield if this one begins to await (release)
  -- See the defition of 'connect', where this property is used explicitly.
  --
  -- Typeable is here for the benefit of casting a transition on unknown
  -- endpoints into one on a known endpoint, to facilitate passing them
  -- through a channel. It's also in the 'PeerAwait' constructor.
  PeerYield
    :: ( Typeable from
       , TrControl p from inter ~ control
       )
    => Exchange p tr from inter control
    -> Peer p tr (ControlNext control 'Yielding 'Awaiting inter) to f t
    -> Peer p tr ('Yielding from) to f t
  PeerAwait
    :: ( Typeable from )
    => (forall inter . tr from inter -> Peer p tr (ControlNext (TrControl p from inter) 'Awaiting 'Yielding inter) to f t)
    -> Peer p tr ('Awaiting from) to f t

-- | In a client/server model, one side will be awaiting when the other is
-- receiving. This type will be included in a description of either side of
-- the a protocol application. See 'Peer'.
data Status k where
  Awaiting :: k -> Status k
  Yielding :: k -> Status k

-- | This would be far more useful if GHC could understand that
--
--   forall st . Complement (Complement st) = st
--
-- See 'stepping'.
type family Complement (status :: st -> Status st) :: st -> Status st where
  Complement 'Awaiting = 'Yielding
  Complement 'Yielding = 'Awaiting

-- | Description of control flow: either hold onto it or release it.
data Control where
  Hold    :: Control
  Release :: Control

-- | 'p' partitions 'k' by picking, for each 'st :: k', one of two options,
-- here called 'a' and 'b'. Church encoding of boolean.
--
-- To use a 'Peer p (tr :: k -> k -> Type)', there must be an instance of
-- 'Parition p st a b' for all 'st :: k'.
type family Partition (p :: r) (st :: k) (a :: t) (b :: t) :: t

-- | Picks 'Hold if 'from' and 'to' lie in the same partition.
-- Picks 'Release otherwise.
--
-- To make sense of this, think of how to implement XOR on a Church boolean.
--
--   cb1 `xor` cb2 = \true false -> cb1 (cb2 false true) (cb2 true false)
--
-- It's the same thing except that we're picking 'Hold if the XOR is false
-- (they are the same) and 'Release otherwise.
type TrControl p from to =
  Partition p from (Partition p to 'Hold 'Release)
                   (Partition p to 'Release 'Hold)

-- | This family is used in the definition of 'Peer' to determine whether, given
-- a parituclar transition, a yielding side continues to yield or begins
-- awaiting, and vice verse for an awaiting side.
type family ControlNext control a b where
  ControlNext 'Hold    a b = a
  ControlNext 'Release a b = b

-- | Included in every yield, this gives not only the transition, but a
-- 'Control' code to hold or release. The constructor for yielding will
-- require that 'Hold or 'Release is equal to an application of 'TrControl'
-- on the relevant state endpoints. In this way, every transition either
-- picks out exactly one of 'Hold or 'Release, or the type family is "stuck"
-- and the program won't type check; it means you can't have a transition which
-- unifies with _both_ 'Hold and 'Release, which would allow the yielding and
-- awaiting sides to get out of sync.
data Exchange p tr from to control where
  Part :: tr from to -> Exchange p tr from to 'Hold
  -- | Radio terminology. End of transmission and response is expected.
  Over :: tr from to -> Exchange p tr from to 'Release

-- | Get the transition from an 'Exchange'.
exchangeTransition :: Exchange p tr from to control -> tr from to
exchangeTransition (Part tr) = tr
exchangeTransition (Over tr) = tr

-- | Boilerplate: hoist a natural transformation through a 'Peer'.
hoistPeer
  :: ( Functor g )
  => (forall t . f t -> g t)
  -> Peer p tr from to f t
  -> Peer p tr from to g t
hoistPeer nat term = case term of
  PeerDone t -> PeerDone t
  PeerHole f -> PeerHole (fmap (hoistPeer nat) (nat f))
  PeerYield ex next -> PeerYield ex (hoistPeer nat next)
  PeerAwait k -> PeerAwait (hoistPeer nat . k)

done :: t -> Peer p tr end end m t
done = PeerDone

hole :: f (Peer p tr from to f t) -> Peer p tr from to f t
hole = PeerHole

yield
  :: ( Typeable from
     , TrControl p from inter ~ control
     )
  => Exchange p tr from inter control
  -> Peer p tr (ControlNext control 'Yielding 'Awaiting inter) to f t
  -> Peer p tr ('Yielding from) to f t
yield = PeerYield

over
  :: ( Typeable from
     , TrControl p from inter ~ 'Release
     )
  => tr from inter
  -> Peer p tr ('Awaiting inter) to f t
  -> Peer p tr ('Yielding from) to f t
over tr = yield (Over tr)

part
  :: ( Typeable from
     , TrControl p from inter ~ 'Hold
     )
  => tr from inter
  -> Peer p tr ('Yielding inter) to f t
  -> Peer p tr ('Yielding from) to f t
part tr = yield (Part tr)

await
  :: ( Typeable from )
  => (forall inter . tr from inter -> Peer p tr (ControlNext (TrControl p from inter) 'Awaiting 'Yielding inter) to f t)
  -> Peer p tr ('Awaiting from) to f t
await = PeerAwait

-- | Akin to '>>=' but the continuation carries it from the 'inter'mediate
-- state to the end state.
andThen
  :: ( Functor f )
  => Peer p tr from inter f s
  -> (s -> Peer p tr inter to f t)
  -> Peer p tr from to f t
andThen peer k = case peer of
  PeerDone t -> k t
  PeerHole m -> PeerHole $ (fmap (`andThen` k) m)
  PeerYield tr next -> PeerYield tr $ next `andThen` k
  PeerAwait l -> PeerAwait $ (`andThen` k) . l

feedbackLoop
  :: ( Functor f )
  => (r -> Peer p tr st st f r)
  -> (r -> Peer p tr st sx f x)
feedbackLoop k r = k r `andThen` feedbackLoop k

-- | Connect two peers agreeing on the initial state and with common effect
-- functor. Run until either or both complete, discarding any remaining
-- 'Peer' term continuation.
connect
  :: ( Monad m )
  => Peer p tr (st begin)            endA m a
  -> Peer p tr (Complement st begin) endB m b
  -> m (These a b)
connect _            (PeerDone b) = pure (That b)
connect (PeerDone a) _            = pure (This a)
connect peerA (PeerHole m) = m >>= connect peerA
connect (PeerHole m) peerB = m >>= flip connect peerB
connect (PeerAwait k) (PeerYield exchange n) = case exchange of
  Part tr -> connect (k tr) n
  Over tr -> fmap flipThese (connect n (k tr))
connect (PeerYield exchange n) (PeerAwait k) = case exchange of
  Part tr -> connect n (k tr)
  Over tr -> fmap flipThese (connect (k tr) n)
-- NB: this is a complete pattern match.
-- (PeerAwait _) (PeerAwait _) deadlock is not possible.
-- (PeerYield _ _) (PeerYield _ _) interjection is not possible.

data These a b = This a | That b | Those a b
  deriving (Show)

flipThese :: These a b -> These b a
flipThese term = case term of
  This a -> That a
  That b -> This b
  Those a b -> Those b a

-- | Like 'connect' but if one side ends before the other, the continuation is
-- retained.
-- This is not ready.
stepping
  :: forall p tr st begin endA endB fa fb a b .
     ( Functor fa, Functor fb
     )
  => Peer p tr (st begin)            endA fa a
  -> Peer p tr (Complement st begin) endB fb b
  -> Stepping p tr endA endB fa fb a b
-- If either peer is at a hole, stop there, even if the other one is done.
-- This avoids the problematic
--   (PeerHole _, PeerDone b)
-- case in which GHC rejects the program because it doesn't know that
--   Complement . Complement = Id
stepping (PeerHole fa) peerB         = LeftHole (fmap (flip stepping peerB) fa)
stepping peerA         (PeerHole fb) = RightHole (fmap (stepping peerA) fb)
stepping (PeerDone a) (PeerDone b)   = BothDone a b
{-
stepping peerA@(PeerHole _) (PeerDone b) =
  -- We need to give a
  --   Peer p tr (Complement (Complement st) begin) endA fa a
  -- GHC can't figure out that (Complement (Complement st)) ~ st for all st,
  -- so we'll tell it to take a break.
  let peerA' :: Peer p tr (Complement (Complement st) begin) endA fa a
      peerA' = unsafeCoerce peerA
  in  RightDone b peerA'
-}
-- These 2 cases, GHC can figure it out, because it knows that 'begin' is
-- 'Yielding' or 'Awaiting'.
stepping peerA@(PeerYield _ _) (PeerDone b) = RightDone b peerA
stepping peerA@(PeerAwait _)   (PeerDone b) = RightDone b peerA
stepping (PeerDone a) peerB@(PeerYield _ _) = LeftDone a peerB
stepping (PeerDone a) peerB@(PeerAwait _)   = LeftDone a peerB
stepping (PeerAwait k) (PeerYield exchange n) = case exchange of
  Part tr -> stepping (k tr) n
  Over tr -> flipStepping (stepping n (k tr))
stepping (PeerYield exchange n) (PeerAwait k) = case exchange of
  Part tr -> stepping n (k tr)
  Over tr -> flipStepping (stepping (k tr) n)

flipStepping
  :: ( Functor fa
     , Functor fb
     )
  => Stepping p tr endA endB fa fb a b
  -> Stepping p tr endB endA fb fa b a
flipStepping term = case term of
  BothDone a b -> BothDone b a
  LeftDone a k -> RightDone a k
  RightDone b k -> LeftDone b k
  LeftHole it -> RightHole (fmap flipStepping it)
  RightHole it -> LeftHole (fmap flipStepping it)

data Stepping p tr endA endB fa fb a b where
  BothDone  :: a -> b -> Stepping p tr endA endB fa fb a b
  LeftDone  :: a -> Peer p tr (Complement st endA) endB fb b -> Stepping p tr (st endA) endB fa fb a b
  RightDone :: b -> Peer p tr (Complement st endB) endA fa a -> Stepping p tr endA (st endB) fa fb a b
  LeftHole  :: fa (Stepping p tr endA endB fa fb a b) -> Stepping p tr endA endB fa fb a b
  RightHole :: fb (Stepping p tr endA endB fa fb a b) -> Stepping p tr endA endB fa fb a b

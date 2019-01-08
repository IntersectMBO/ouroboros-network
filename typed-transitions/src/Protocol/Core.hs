{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Protocol.Core where

import Data.Kind (Type)

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
  -- | Lift an effect.
  PeerLift :: f (Peer p tr from to f t) -> Peer p tr from to f t
  -- | When yielding a transition, there must be proof that, under 'p', the
  -- transition either holds or releases control. This is enough to allow GHC
  -- to deduce that the continuation in a complementary 'PeerAwait' (one which
  -- is awaiting on the same initial state) will either
  --   - continue to await if this one continues to yield (hold)
  --   - begin to yield if this one begins to await (release)
  -- See the defition of 'connect', where this property is used explicitly.
  PeerYield
    :: ( TrControl p from inter ~ control )
    => Exchange p tr from inter control
    -> Peer p tr (ControlNext control Yielding Awaiting Finished inter) to f t
    -> Peer p tr (Yielding from) to f t
  PeerAwait
    :: (forall inter . tr from inter
                    -> Peer p tr (ControlNext (TrControl p from inter)
                                              Awaiting Yielding Finished inter)
                            to f t)
    -> Peer p tr (Awaiting from) to f t

-- | In a client/server model, one side will be awaiting when the other is
-- receiving. This type will be included in a description of either side of
-- the a protocol application. See 'Peer'.
data Status k where
  Awaiting :: k -> Status k
  Yielding :: k -> Status k
  Finished :: k -> Status k

-- | This would be far more useful if GHC could understand that
--
--   forall st . Complement (Complement st) = st
--
-- See 'stepping'.
type family Complement (status :: st -> Status st) :: st -> Status st where
  Complement Awaiting = Yielding
  Complement Yielding = Awaiting
  Complement Finished = Finished

-- | Description of control flow: either hold onto it or release it.
data Control where
  Hold      :: Control
  Release   :: Control
  Terminate :: Control

-- | 'p' partitions 'k' by picking, for each 'st :: k', one of 3 options,
-- here called 'a', 'b', and 'c'.
--
-- To use a 'Peer p (tr :: k -> k -> Type)', there must be an instance of
-- 'Parition p st a b c' for all 'st :: k'.
type family Partition (p :: r) (st :: k) (a :: t) (b :: t) (c :: t) :: t

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
  Partition p from (Partition p to Hold Release Terminate)
                   (Partition p to Release Hold Terminate)
                   -- This part is weird. If 'from' is in the terminal set,
                   -- then ...
                   Terminate

-- | This family is used in the definition of 'Peer' to determine whether, given
-- a parituclar transition, a yielding side continues to yield or begins
-- awaiting, and vice verse for an awaiting side.
type family ControlNext control a b c where
  ControlNext Hold      a b c = a
  ControlNext Release   a b c = b
  ControlNext Terminate a b c = c

-- | Included in every yield, this gives not only the transition, but a
-- 'Control' code to hold or release. The constructor for yielding will
-- require that 'Hold or 'Release is equal to an application of 'TrControl'
-- on the relevant state endpoints. In this way, every transition either
-- picks out exactly one of 'Hold or 'Release, or the type family is "stuck"
-- and the program won't type check; it means you can't have a transition which
-- unifies with _both_ 'Hold and 'Release, which would allow the yielding and
-- awaiting sides to get out of sync.
data Exchange p tr from to control where
  Part :: tr from to -> Exchange p tr from to Hold
  -- | Radio terminology. End of transmission and response is expected.
  Over :: tr from to -> Exchange p tr from to Release
  Out  :: tr from to -> Exchange p tr from to Terminate

-- | Get the transition from an 'Exchange'.
exchangeTransition :: Exchange p tr from to control -> tr from to
exchangeTransition (Part tr) = tr
exchangeTransition (Over tr) = tr
exchangeTransition (Out tr)  = tr

-- | Boilerplate: hoist a natural transformation through a 'Peer'.
hoistPeer
  :: ( Functor g )
  => (forall x . f x -> g x)
  -> Peer p tr from to f t
  -> Peer p tr from to g t
hoistPeer nat term = case term of
  PeerDone t -> PeerDone t
  PeerLift f -> PeerLift (fmap (hoistPeer nat) (nat f))
  PeerYield ex next -> PeerYield ex (hoistPeer nat next)
  PeerAwait k -> PeerAwait (hoistPeer nat . k)

done :: t -> Peer p tr end end m t
done = PeerDone

lift :: f (Peer p tr from to f t) -> Peer p tr from to f t
lift = PeerLift

yield
  :: ( TrControl p from inter ~ control )
  => Exchange p tr from inter control
  -> Peer p tr (ControlNext control Yielding Awaiting Finished inter) to f t
  -> Peer p tr (Yielding from) to f t
yield = PeerYield

out
  :: ( TrControl p from inter ~ Terminate )
  => tr from inter
  -> Peer p tr (Finished inter) to f t
  -> Peer p tr (Yielding from) to f t
out tr = yield (Out tr)

over
  :: ( TrControl p from inter ~ Release )
  => tr from inter
  -> Peer p tr (Awaiting inter) to f t
  -> Peer p tr (Yielding from) to f t
over tr = yield (Over tr)

part
  :: ( TrControl p from inter ~ Hold )
  => tr from inter
  -> Peer p tr (Yielding inter) to f t
  -> Peer p tr (Yielding from) to f t
part tr = yield (Part tr)

await
  :: (forall inter . tr from inter
                  -> Peer p tr (ControlNext (TrControl p from inter)
                                            Awaiting Yielding Finished inter)
                          to f t)
  -> Peer p tr (Awaiting from) to f t
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
  PeerLift m -> PeerLift $ (fmap (`andThen` k) m)
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
  -> m (Either a b)
connect _            (PeerDone b) = pure (Right b)
connect (PeerDone a) _            = pure (Left a)
connect peerA (PeerLift m) = m >>= connect peerA
connect (PeerLift m) peerB = m >>= flip connect peerB
connect (PeerAwait k) (PeerYield exchange n) = case exchange of
  Part tr -> connect (k tr) n
  Over tr -> fmap flipEither (connect n (k tr))
  Out  tr -> fmap flipEither (connect n (k tr))
connect (PeerYield exchange n) (PeerAwait k) = case exchange of
  Part tr -> connect n (k tr)
  Over tr -> fmap flipEither (connect (k tr) n)
  Out  tr -> fmap flipEither (connect (k tr) n)
-- NB: this is a complete pattern match.
-- (PeerAwait _) (PeerAwait _) deadlock is not possible.
-- (PeerYield _ _) (PeerYield _ _) interjection is not possible.

flipEither :: Either a b -> Either b a
flipEither = either Right Left

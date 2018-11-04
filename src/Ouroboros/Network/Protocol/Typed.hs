{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Using a type transition system to describe either side of a
-- request\/response application.
module Ouroboros.Network.Protocol.Typed (
    Peer(..),
    Status(..),
    Partition,
    done,
    hole,
    part,
    over,
    out,
    await,
    exchangeTransition,
    ) where

import           Data.Kind     (Type)
import           Data.Typeable


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
    -> Peer p tr (ControlNext control 'Yielding 'Awaiting 'Finished inter) to f t
    -> Peer p tr ('Yielding from) to f t
  PeerAwait
    :: ( Typeable from )
    => (forall inter . tr from inter -> Peer p tr (ControlNext (TrControl p from inter) 'Awaiting 'Yielding 'Finished inter) to f t)
    -> Peer p tr ('Awaiting from) to f t

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
  Complement 'Awaiting = 'Yielding
  Complement 'Yielding = 'Awaiting
  Complement 'Finished = 'Finished

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
  Partition p from (Partition p to 'Hold 'Release 'Terminate)
                   (Partition p to 'Release 'Hold 'Terminate)
                   -- This part is weird. If 'from' is in the terminal set,
                   -- then ...
                   'Terminate

-- | This family is used in the definition of 'Peer' to determine whether, given
-- a parituclar transition, a yielding side continues to yield or begins
-- awaiting, and vice verse for an awaiting side.
type family ControlNext control a b c where
  ControlNext 'Hold      a b c = a
  ControlNext 'Release   a b c = b
  ControlNext 'Terminate a b c = c

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
  Out  :: tr from to -> Exchange p tr from to 'Terminate

-- | Get the transition from an 'Exchange'.
exchangeTransition :: Exchange p tr from to control -> tr from to
exchangeTransition (Part tr) = tr
exchangeTransition (Over tr) = tr
exchangeTransition (Out tr)  = tr

done :: t -> Peer p tr end end m t
done = PeerDone

hole :: f (Peer p tr from to f t) -> Peer p tr from to f t
hole = PeerHole

yield
  :: ( Typeable from
     , TrControl p from inter ~ control
     )
  => Exchange p tr from inter control
  -> Peer p tr (ControlNext control 'Yielding 'Awaiting 'Finished inter) to f t
  -> Peer p tr ('Yielding from) to f t
yield = PeerYield

out
  :: ( Typeable from
     , TrControl p from inter ~ 'Terminate
     )
  => tr from inter
  -> Peer p tr ('Finished inter) to f t
  -> Peer p tr ('Yielding from) to f t
out tr = yield (Out tr)

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
  => (forall inter . tr from inter -> Peer p tr (ControlNext (TrControl p from inter) 'Awaiting 'Yielding 'Finished inter) to f t)
  -> Peer p tr ('Awaiting from) to f t
await = PeerAwait


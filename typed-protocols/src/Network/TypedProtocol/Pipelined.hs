{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

module Network.TypedProtocol.Pipelined
  ( PeerPipelined (..)
  , PeerSender (..)
  , PeerReceiver (..)
  , Outstanding
  , N (..)
  , Nat (Zero, Succ)
  , natToInt
  , unsafeIntToNat
  , fmapPeerPipelined
  ) where

import           Unsafe.Coerce (unsafeCoerce)

import           Network.TypedProtocol.Core


-- | A description of a peer that engages in a protocol in a pipelined fashion.
--
-- This is very much like 'Peer', and can work with the same protocol state
-- machine descriptions, but allows the peer to pipeline the execution of
-- the protocol.
--
-- This wraps a 'PeerSender', but works for any internal collect type @c@, and
-- with the starting condition of zero outstanding pipelined responses.
--
data PeerPipelined ps (pr :: PeerRole) (st :: ps) m a where
  PeerPipelined :: PeerSender    ps pr st Z c m a
                -> PeerPipelined ps pr st     m a

deriving instance Functor m => Functor (PeerPipelined ps (pr :: PeerRole) (st :: ps) m)

-- | More general than 'fmap', as it allows to change the protocol.
--
fmapPeerPipelined :: (forall c. PeerSender ps pr st Z c m a -> PeerSender ps' pr st' Z c m b)
                  -> PeerPipelined ps  pr st  m a
                  -> PeerPipelined ps' pr st' m b
fmapPeerPipelined f (PeerPipelined peer) = PeerPipelined (f peer)


-- | This is the pipelined variant of 'Peer'.
--
-- In particular it has two extra type arguments:
--
--  * @(n :: 'Outstanding')@ records the number of outstanding pipelined
--    responses. Note that when this is 'Z' then we have no such outstanding
--    responses, and we are in an equivalent situation to a normal
--    non-pipelined 'Peer'
--
--  * @c@ records the internal type of the pipelined responses.
--
data PeerSender ps (pr :: PeerRole) (st :: ps) (n :: Outstanding) c m a where

  -- | Same idea as normal 'Peer' 'Effect'.
  SenderEffect   :: m (PeerSender ps pr st n c m a)
                 ->    PeerSender ps pr st n c m a

  -- | Same idea as normal 'Peer' 'Done'.
  SenderDone     :: !(NobodyHasAgency st)
                 -> a
                 -> PeerSender ps pr st Z c m a

  -- | A normal non-pipelined 'Yield'.
  --
  -- Note that we cannot mix pipelined and normal synchronous syle, so this
  -- can only be used when there are no outstanding pipelined responses.
  --
  -- The @n ~ 'Z'@ constraint provides the type level guarantees that there
  -- are no outstanding pipelined responses.
  --
  SenderYield    :: !(WeHaveAgency pr st)
                 -> Message    ps st st'
                 -> PeerSender ps pr st' Z c m a
                 -> PeerSender ps pr st  Z c m a

  -- | A normal non-pipelined 'Await'. Note that this can only be used .
  --
  -- Note that we cannot mix pipelined and normal synchronous syle, so this
  -- can only be used when there are no outstanding pipelined responses.
  --
  -- The @n ~ 'Z'@ constraint provides the type level guarantees that there
  -- are no outstanding pipelined responses.
  --
  SenderAwait    :: !(TheyHaveAgency pr st)
                 -> (forall st'. Message    ps st st'
                              -> PeerSender ps pr st' Z c m a)
                 -> PeerSender              ps pr st  Z c m a

  -- | A pipelined equivalent of 'Yield'. The key difference is that instead
  -- of moving into the immediate next state @st'@, the sender jumps directly
  -- to state @st''@ and a seperate 'PeerReceiver' has to be supplied which
  -- will get from @st'@ to @st''@. This sets up an outstanding pipelined
  -- receive. The queue of outstanding pipelined receive actions 'PeerReceiver'
  -- are executed in order, as messages arrive from the remote peer.
  --
  -- The type records the fact that the number of outstanding pipelined
  -- responses increases by one.
  --
  SenderPipeline :: !(WeHaveAgency   pr st)
                 -> Message ps st st'
                 -> PeerReceiver ps pr (st'  :: ps) (st'' :: ps) m c
                 -> PeerSender   ps pr (st'' :: ps) (S n) c m a
                 -> PeerSender   ps pr (st   :: ps)    n  c m a

  -- | Collect the result of a previous pipelined receive action.
  --
  -- This (optionally) provides two choices:
  --
  -- * Continue without a pipelined result
  -- * Continue with a pipelined result
  --
  -- Since presenting the first choice is optional, this allows expressing
  -- both a blocking collect and a non-blocking collect. This allows
  -- implementations to express policies such as sending a short sequence
  -- of messages and then waiting for all replies, but also a maximum
  -- pipelining policy that keeps a large number of messages in flight but
  -- collects results eagerly.
  --
  -- The type records the fact that when collecting a response, the number of
  -- outstanding pipelined responses decreases by one. The type also guarantees
  -- that it can only be used when there is at least one outstanding response.
  --
  SenderCollect  :: Maybe (PeerSender ps pr (st :: ps) (S n) c m a)
                 -> (c ->  PeerSender ps pr (st :: ps)    n  c m a)
                 ->        PeerSender ps pr (st :: ps) (S n) c m a

deriving instance Functor m => Functor (PeerSender ps (pr :: PeerRole) (st :: ps) (n :: Outstanding) c m)

data PeerReceiver ps (pr :: PeerRole) (st :: ps) (stdone :: ps) m c where

  ReceiverEffect :: m (PeerReceiver ps pr st stdone m c)
                 ->    PeerReceiver ps pr st stdone m c

  ReceiverDone   :: c -> PeerReceiver ps pr stdone stdone m c

  ReceiverAwait  :: !(TheyHaveAgency pr st)
                 -> (forall st'. Message ps st st'
                              -> PeerReceiver ps pr st' stdone m c)
                 -> PeerReceiver ps pr st stdone m c


-- | Type level count of the number of outstanding pipelined yields for which
-- we have not yet collected a receiver result. Used in 'PeerSender' to ensure
-- 'SenderCollect' is only used when there are outstanding results to collect,
-- and to ensure 'SenderYield', 'SenderAwait' and 'SenderDone' are only used
-- when there are none.
--
type Outstanding = N

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

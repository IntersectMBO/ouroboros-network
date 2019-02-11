{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}


module Network.TypedProtocol.Pipelined where

import Network.TypedProtocol.Core


-- | A description of a peer that engages in a protocol in a pipelined fashion.
--
-- This is very much like 'Peer', and can work with the same protocol state
-- machine descriptions, but allows the peer to pipeline the execution of
-- the protocol.
--
-- This wraps a 'PeerSender', but works for any internal collect type @c@, and
-- with the starting condition of zero outstanding pipelined responses.
--
data PeerPipelined ps (pk :: PeerKind) (st :: ps) m a where
  PeerPipelined :: PeerSender    ps pk st Z c m a
                -> PeerPipelined ps pk st     m a

-- | This is the pipelined variant of 'Peer'.
--
-- In particular it has two extra type arguments:
--
--  * @(n :: 'Outstanding')@ records the number of outstanding pipelined
--    responses.
--
--  * @c@ records the internal type of the pipelined responses.
--
data PeerSender ps (pk :: PeerKind) (st :: ps) (n :: Outstanding) c m a where

  -- | Same idea as normal 'Peer' 'Effect'.
  SenderEffect   :: m (PeerSender ps pk st n c m a)
                 ->    PeerSender ps pk st n c m a

  -- | Same idea as normal 'Peer' 'Done'.
  SenderDone     :: !(NobodyHasAgency st)
                 -> a
                 -> PeerSender ps pk st Z c m a

  -- | A normal non-pipelined 'Yield'. Note that this can only be used for
  -- messages that keep agency (or terminal messages), not for ones that
  -- give away agency, since the 'PeerSender' has no equivalent of 'Await'.
  -- The only way to await is via a pipelined yield: 'SenderPipeline'.
  --
  SenderYield    :: !(WeHaveAgency pk st)
                 -> Message ps st st'
                 -> PeerSender   ps pk (st' :: ps) n c m a
                 -> PeerSender   ps pk (st  :: ps) n c m a

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
  SenderPipeline :: !(WeHaveAgency   pk st)
                 -> Message ps st st'
                 -> PeerReceiver ps pk (st'  :: ps) (st'' :: ps) m c
                 -> PeerSender   ps pk (st'' :: ps) (S n) c m a
                 -> PeerSender   ps pk (st   :: ps)    n  c m a

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
  -- of messages and then waiting for all replies, but also a maximum pipelining
  -- policy that keeps a large number of messages in flight but collects results
  -- eagerly.
  --
  -- The type records the fact that when collecting a response, the number of
  -- outstanding pipelined responses decreases by one. The type also guarantees
  -- that it can only be used when there is at least one outstanding response.
  --
  SenderCollect  :: Maybe (PeerSender ps pk (st :: ps) (S n) c m a)
                 -> (c ->  PeerSender ps pk (st :: ps)    n  c m a)
                 ->        PeerSender ps pk (st :: ps) (S n) c m a


-- | Type level count of the number of outstanding pipelined yields for which
-- we have not yet collected a receiver result. Used in 'PeerSender' to ensure
-- 'SenderCollect' and 'SenderDone' are only used when there are, and
-- respectively are not, outstanding results to collect.
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
data Nat (n :: N) where
  Zero ::          Nat Z
  Succ :: Nat n -> Nat (S n)


data PeerReceiver ps (pk :: PeerKind) (st :: ps) (stdone :: ps) m c where

  ReceiverEffect :: m (PeerReceiver ps pk st stdone m c)
                 ->    PeerReceiver ps pk st stdone m c

  ReceiverDone   :: c -> PeerReceiver ps pk stdone stdone m c

  ReceiverAwait  :: !(TheyHaveAgency pk st)
                 -> (forall st'. Message ps st st'
                              -> PeerReceiver ps pk st' stdone m c)
                 -> PeerReceiver ps pk st stdone m c


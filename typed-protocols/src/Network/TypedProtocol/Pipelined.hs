{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}


module Network.TypedProtocol.Pipelined where

import Network.TypedProtocol.Core


data PeerPipelined ps pk st m a where
  PeerPipelined :: PeerSender    ps pk st Z c m a
                -> PeerPipelined ps pk st     m a

data PeerSender ps (pk :: PeerKind) (st :: ps) (n :: Outstanding) c m a where

  SenderEffect   :: m (PeerSender ps pk st n c m a)
                 ->    PeerSender ps pk st n c m a

  SenderDone     :: !(NobodyHasAgency st)
                 -> a
                 -> PeerSender ps pk st Z c m a

  SenderYield    :: !(WeHaveAgency pk st)
                 -> Message ps st st'
                 -> PeerSender   ps pk (st' :: ps) n c m a
                 -> PeerSender   ps pk (st  :: ps) n c m a

  SenderPipeline :: !(WeHaveAgency   pk st)
                 -> Message ps st st'
                 -> PeerReceiver ps pk (st'  :: ps) (st'' :: ps) m c
                 -> PeerSender   ps pk (st'' :: ps) (S n) c m a
                 -> PeerSender   ps pk (st   :: ps)    n  c m a

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


{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


-- | 
--
module Network.TypedProtocol.Proofs where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Data.Void (Void, absurd)


data AgencyProofs ps = AgencyProofs {

       proofByContradiction_ClientAndServerHaveAgency
         :: forall (st :: ps).
            ClientHasAgency st
         -> ServerHasAgency st
         -> Void,

       proofByContradiction_NobodyAndClientHaveAgency
         :: forall (st :: ps).
            NobodyHasAgency st
         -> ClientHasAgency st
         -> Void,

       proofByContradiction_NobodyAndServerHaveAgency
         :: forall (st :: ps).
            NobodyHasAgency st
         -> ServerHasAgency st
         -> Void
     }


-- | The 'connect' function takes two peers that agree on a protocol and runs
-- them in lock step, until (and if) they complete.
--
-- The 'connect' function serves a few purposes.
--
-- * The fact we can define this function at at all proves some minimal
-- sanity property of the typed protocol framework.
--
-- * It demonstrates that all protocols defined in the framework can be run
-- with synchronous communication rather than requiring buffered communication.
--
-- * It is useful for testing peer implementations against each other in a
-- minimalistic setting. The typed framework guarantees
--
connect :: forall ps (st :: ps) m a b.
           Monad m
        => AgencyProofs ps
        -> Peer AsClient st m a
        -> Peer AsServer st m b
        -> m (a, b)
connect AgencyProofs{..} = go
  where
    go :: forall (st' :: ps).
          Peer AsClient st' m a
       -> Peer AsServer st' m b
       -> m (a, b)
    go  (Done !_stA a)      (Done !_stB b)      = return (a, b)
    go  (Effect a )          b                  = a >>= \a' -> go a' b
    go   a                  (Effect b)          = b >>= \b' -> go a  b'
    go  (Yield !_stA msg a) (Await !_stB b)     = go  a     (b msg)
    go  (Await !_stA a)     (Yield !_stB msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield stA _ _) (Yield stB _ _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stA stB)

    go (Await stA _)   (Await stB _)   =
      absurd (proofByContradiction_ClientAndServerHaveAgency stB stA)

    go (Done  stA _)   (Yield stB _ _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stA stB)

    go (Done  stA _)   (Await stB _)   =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stA stB)

    go (Yield stA _ _) (Done stB _)    =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stB stA)

    go (Await stA _)   (Done stB _)    =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stB stA)


connectPipelined :: forall ps (st :: ps) m a b.
                    Monad m
                 => AgencyProofs ps
                 -> PeerSender AsClient st m a
                 -> Peer       AsServer st m b
                 -> m (a, b)

connectPipelined AgencyProofs{..} = goSender
  where
    goSender :: forall (st' :: ps).
                PeerSender AsClient st' m a
             -> Peer       AsServer st' m b
             -> m (a, b)

    goSender  (SenderDone !_stA a) (Done !_stB b) = return (a, b)
    goSender  (SenderEffect a)      b             = a >>= \a' -> goSender a' b
    goSender  a                    (Effect b)     = b >>= \b' -> goSender a  b'

    goSender  (SenderYield !_stA msg r a) (Await !_stB b) =
      goReceiver r (b msg) >>= \b' -> goSender a b'


    -- Proofs that the remaining cases are impossible
    goSender (SenderDone stA _)      (Yield stB _ _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stA stB)

    goSender (SenderDone stA _)      (Await stB _) =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stA stB)

    goSender (SenderYield stA _ _ _) (Done stB _) =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stB stA)

    goSender (SenderYield stA _ _ _) (Yield stB _ _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stA stB)

    -- note that this is where there is actually non-determinism
    -- in the order of effects. Here we're picking one order that is
    -- equivalent to no pipelining at all. So this at least demonstrates
    -- that the pipelining admits the canonical non-pipelined order.


    goReceiver :: forall (st' :: ps) (stdone :: ps).
                  PeerReceiver AsClient st' stdone m
               -> Peer         AsServer st'        m b
               -> m (Peer      AsServer     stdone m b)

    goReceiver  ReceiverDone       b         = return b
    goReceiver (ReceiverEffect a)  b         = a >>= \a' -> goReceiver a' b
    goReceiver  a                 (Effect b) = b >>= \b' -> goReceiver a  b'

    goReceiver (ReceiverAwait !_ a) (Yield !_ msg b) = goReceiver (a msg) b


    -- Proofs that the remaining cases are impossible
    goReceiver (ReceiverAwait stA _) (Done  stB _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stB stA)

    goReceiver (ReceiverAwait stA _) (Await stB _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stB stA)


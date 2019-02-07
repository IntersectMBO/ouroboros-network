{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.Proofs (
  -- * About these proofs
  -- $about

  -- * Connect proof
  connect,
  TerminalStates(..),

  -- * Pipelining proofs
  -- | Additional proofs specific to the pipelining features
  connectPipelined,
  forgetPipelined,
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Data.Void (absurd)

-- $about
--
-- Typed languages such as Haskell can embed proofs. In total languages this
-- is straightforward: a value inhabiting a type is a proof of the property
-- corresponding to the type.
--
-- In languages like Haskell that have ⊥ as a value of every type, things
-- are slightly more complicated. We have to demonstrate that the value that
-- inhabits the type of interest is not ⊥ which we can do by evaluation.
--
-- This idea crops up frequently in advanced type level programming in Haskell.
-- For example @Refl@ proofs that two types are equal have to have a runtime
-- representation that is evaluated to demonstrate it is not ⊥ before it
-- can be relied upon.
--
-- The proofs here are about the nature of typed protocols in this framework.
-- The 'connect' and 'connectPipelined' proofs rely on a few lemmas about
-- the individual protocol. See 'AgencyProofs'.




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
-- minimalistic setting.
--
connect :: forall ps (st :: ps) m a b.
           ( Protocol ps
           , Monad m
           )
        => Peer ps AsClient st m a
        -> Peer ps AsServer st m b
        -> m (a, b, TerminalStates ps)
connect = go
  where
    go :: forall (st' :: ps).
          Peer ps AsClient st' m a
       -> Peer ps AsServer st' m b
       -> m (a, b, TerminalStates ps)
    go (Done stA a)    (Done stB b)    = return (a, b, TerminalStates stA stB)
    go (Effect a )      b              = a >>= \a' -> go a' b
    go  a              (Effect b)      = b >>= \b' -> go a  b'
    go (Yield _ msg a) (Await _ b)     = go  a     (b msg)
    go (Await _ a)     (Yield _ msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield (ClientAgency stA) _ _) (Yield (ServerAgency stB) _ _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stA stB)

    go (Await (ServerAgency stA) _)   (Await (ClientAgency stB) _)   =
      absurd (proofByContradiction_ClientAndServerHaveAgency stB stA)

    go (Done  stA _)            (Yield (ServerAgency stB) _ _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stA stB)

    go (Done  stA _)            (Await (ClientAgency stB) _)   =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stA stB)

    go (Yield (ClientAgency stA) _ _) (Done stB _)    =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stB stA)

    go (Await (ServerAgency stA) _)   (Done stB _)    =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stB stA)


-- | The terminal states for the protocol. Used in 'connect' and
-- 'connectPipelined' to return the states in which the peers terminated.
--
data TerminalStates ps where
     TerminalStates :: forall (st :: ps).
                       NobodyHasAgency st
                    -> NobodyHasAgency st
                    -> TerminalStates ps

-- | Analogous to 'connect' but for pipelined peers.
--
connectPipelined :: forall ps (st :: ps) m a b.
                    ( Monad m
                    , Protocol ps
                    )
                 => PeerSender ps AsClient st m a
                 -> Peer       ps AsServer st m b
                 -> m (a, b, TerminalStates ps)

connectPipelined = goSender
  where
    goSender :: forall (st' :: ps).
                PeerSender ps AsClient st' m a
             -> Peer       ps AsServer st' m b
             -> m (a, b, TerminalStates ps)

    goSender  (SenderDone stA a)   (Done stB b)   = return (a, b, terminals)
      where terminals = TerminalStates stA stB
    goSender  (SenderEffect a)      b             = a >>= \a' -> goSender a' b
    goSender  a                    (Effect b)     = b >>= \b' -> goSender a  b'

    goSender  (SenderYield _ msg r a) (Await _ b) =
      goReceiver r (b msg) >>= \b' -> goSender a b'


    -- Proofs that the remaining cases are impossible
    goSender (SenderDone stA _)               (Yield (ServerAgency stB) _ _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stA stB)

    goSender (SenderDone stA _)               (Await (ClientAgency stB) _) =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stA stB)

    goSender (SenderYield (ClientAgency stA) _ _ _) (Done stB _) =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stB stA)

    goSender (SenderYield (ClientAgency stA) _ _ _) (Yield (ServerAgency stB) _ _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stA stB)

    -- note that this is where there is actually non-determinism
    -- in the order of effects. Here we're picking one order that is
    -- equivalent to no pipelining at all. So this at least demonstrates
    -- that the pipelining admits the canonical non-pipelined order.


    goReceiver :: forall (st' :: ps) (stdone :: ps).
                  PeerReceiver ps AsClient st' stdone m
               -> Peer         ps AsServer st'        m b
               -> m (Peer      ps AsServer     stdone m b)

    goReceiver  ReceiverDone       b         = return b
    goReceiver (ReceiverEffect a)  b         = a >>= \a' -> goReceiver a' b
    goReceiver  a                 (Effect b) = b >>= \b' -> goReceiver a  b'

    goReceiver (ReceiverAwait _ a) (Yield _ msg b) = goReceiver (a msg) b


    -- Proofs that the remaining cases are impossible
    goReceiver (ReceiverAwait (ServerAgency stA) _) (Done  stB _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stB stA)

    goReceiver (ReceiverAwait (ServerAgency stA) _) (Await (ClientAgency stB) _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stB stA)


-- | Prove that we have a total conversion from pipelined peers to regular
-- peers. This is a sanity property that shows that pipelining did not give
-- us extra expressiveness or to break the protocol state machine.
--
forgetPipelined
  :: forall ps (pk :: PeerKind) (st :: ps) m a.
     Functor m
  => PeerSender ps pk st m a
  -> Peer       ps pk st m a
forgetPipelined = goSender
  where
    goSender :: forall st'.
                PeerSender ps pk st' m a
             -> Peer       ps pk st' m a

    goSender (SenderDone  stok       k) = Done stok k
    goSender (SenderEffect           k) = Effect (goSender <$> k)
    goSender (SenderYield stok msg r k) = Yield stok msg (goReceiver k r)

    goReceiver :: forall stCurrent stNext.
                  PeerSender   ps pk stNext m a
               -> PeerReceiver ps pk stCurrent stNext m
               -> Peer         ps pk stCurrent m a

    goReceiver s  ReceiverDone          = goSender s
    goReceiver s (ReceiverEffect     k) = Effect (goReceiver s <$> k)
    goReceiver s (ReceiverAwait stok k) = Await stok (goReceiver s . k)


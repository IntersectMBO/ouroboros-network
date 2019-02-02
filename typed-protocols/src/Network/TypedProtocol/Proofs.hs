{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeInType #-}


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
  AgencyProofs(..),

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
import Data.Void (Void, absurd)

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


-- | The 'connect' and 'connectPipelined' proofs rely on lemmas about the
-- protocol. Specifically they rely on the property that each protocol state
-- is labelled with the agency of one peer or the other, or neither, but never
-- both. Or to put it another way, the protocol states should be partitioned
-- into those with agency for one peer, or the other or neither.
--
-- The way the labelling is encoded does not automatically enforce this
-- property. It is technically possible to set up the labelling for a protocol
-- so that one state is labelled as having both peers with agency, or declaring
-- simultaneously that one peer has agency and that neither peer has agency
-- in a particular state.
--
-- So the overall proofs rely on lemmas that say that the labelling has been
-- done correctly. This type bundles up those three lemmas.
--
-- Specifically proofs that it is impossible for a protocol state to have:
--
-- * client having agency and server having agency
-- * client having agency and nobody having agency
-- * server having agency and nobody having agency
--
-- These lemmas are structured as proofs by contradiction, e.g. stating
-- \"if the client and the server have agency for this state then it leads to
-- contradiction\". Contradiction is represented as the 'Void' type that has
-- no values except ⊥.
--
-- For example for the ping\/pong protocol, it has three states, and if we set
-- up the labelling correctly we have:
--
-- > data PingPong where
-- >   StIdle :: PingPong
-- >   StBusy :: PingPong
-- >   StDone :: PingPong
-- >
-- > data ClientHasAgency st where
-- >   TokIdle :: ClientHasAgency StIdle
-- >
-- > data ServerHasAgency st where
-- >   TokBusy :: ServerHasAgency StBusy
-- >
-- > data NobodyHasAgency st where
-- >   TokDone :: NobodyHasAgency StDone
--
-- So now we can prove that if the client has agency for a state then there
-- are no cases in which the server has agency.
--
-- > proofByContradiction_ClientAndServerHaveAgency TokIdle tok =
-- >   case tok of {}
--
-- For this protocol there is only one state in which the client has agency,
-- the idle state. By pattern matching on the state token for the server
-- agency we can list all the cases in which the server also has agency for
-- the idle state. There are of course none of these so we give the empty
-- set of patterns. GHC can check that we are indeed correct about this.
-- This also requires the @EmptyCase@ language extension.
--
-- To get this completeness checking it is important to compile modules
-- containing these lemmas with @-Wincomplete-patterns@, which is implied by
-- @-Wall@.
--
-- All three lemmas follow the same pattern.
--
data AgencyProofs ps = AgencyProofs {

       -- | Lemma that if the client has agency for a state, there are no
       -- cases in which the server has agency for the same state.
       --
       proofByContradiction_ClientAndServerHaveAgency
         :: forall (st :: ps).
            ClientHasAgency st
         -> ServerHasAgency st
         -> Void,

       -- | Lemma that if the nobody has agency for a state, there are no
       -- cases in which the client has agency for the same state.
       --
       proofByContradiction_NobodyAndClientHaveAgency
         :: forall (st :: ps).
            NobodyHasAgency st
         -> ClientHasAgency st
         -> Void,

       -- | Lemma that if the nobody has agency for a state, there are no
       -- cases in which the server has agency for the same state.
       --
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
-- minimalistic setting.
--
connect :: forall ps (st :: ps) m a b.
           Monad m
        => AgencyProofs ps
        -> Peer ps AsClient st m a
        -> Peer ps AsServer st m b
        -> m (a, b)
connect AgencyProofs{..} = go
  where
    go :: forall (st' :: ps).
          Peer ps AsClient st' m a
       -> Peer ps AsServer st' m b
       -> m (a, b)
    go  (Done _stA a)      (Done _stB b)      = return (a, b)
    go  (Effect a )          b                  = a >>= \a' -> go a' b
    go   a                  (Effect b)          = b >>= \b' -> go a  b'
    go  (Yield _stA msg a) (Await _stB b)     = go  a     (b msg)
    go  (Await _stA a)     (Yield _stB msg b) = go (a msg) b

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


connectPipelined :: forall ps (st :: ps) m a b.
                    Monad m
                 => AgencyProofs ps
                 -> PeerSender ps AsClient st m a
                 -> Peer       ps AsServer st m b
                 -> m (a, b)

connectPipelined AgencyProofs{..} = goSender
  where
    goSender :: forall (st' :: ps).
                PeerSender ps AsClient st' m a
             -> Peer       ps AsServer st' m b
             -> m (a, b)

    goSender  (SenderDone _stA a) (Done _stB b) = return (a, b)
    goSender  (SenderEffect a)      b             = a >>= \a' -> goSender a' b
    goSender  a                    (Effect b)     = b >>= \b' -> goSender a  b'

    goSender  (SenderYield _stA msg r a) (Await _stB b) =
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


{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE TypeOperators            #-}


-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.Proofs
  ( -- * About these proofs
    -- $about
    -- * Connect proof
    connect
  , connectNonPipelined
  , TerminalStates (..)
    -- * Pipelining proofs
    -- | Additional proofs specific to the pipelining features
  , forgetPipelined
    -- ** Auxiliary functions
  , pipelineInterleaving
  ) where

import           Control.Monad.Class.MonadSTM

import           Data.Function ((&))
import           Data.Kind (Type)
import           Data.Singletons
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer

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
connectNonPipelined
   :: forall ps (pr :: PeerRole) (initSt :: ps) m stm a b.
      (Monad m, SingI pr)
   => Peer ps             pr  NonPipelined Empty initSt m stm a
   -> Peer ps (FlipAgency pr) NonPipelined Empty initSt m stm b
   -> m (a, b, TerminalStates ps pr)
connectNonPipelined = go
  where
    singPeerRole :: Sing pr
    singPeerRole = sing

    go :: forall (st :: ps).
          Peer ps             pr  NonPipelined Empty st m stm a
       -> Peer ps (FlipAgency pr) NonPipelined Empty st m stm b
       -> m (a, b, TerminalStates ps pr)
    go (Done reflA a)  (Done reflB b)  = return (a, b, terminals)
      where
        terminals :: TerminalStates ps pr
        terminals = TerminalStates (sing :: Sing st)
                                    reflA
                                   (sing :: Sing st)
                                    reflB
    go (Effect a )      b              = a >>= \a' -> go a' b
    go  a              (Effect b)      = b >>= \b' -> go a  b'
    go (Yield _ msg a) (Await _ b)     = go  a     (b msg)
    go (Await _ a)     (Yield _ msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield reflA _ _) (Yield reflB _ _) =
      case exclusionLemma_ClientAndServerHaveAgency singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Await reflA _)   (Await reflB _)   =
      case exclusionLemma_ClientAndServerHaveAgency singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Done  reflA _) (Yield reflB _ _)   =
      case terminationLemma_2 singPeerRole reflB reflA of
        ReflNobodyHasAgency -> case reflB of {}

    go (Done  reflA _) (Await reflB _)     =
      case terminationLemma_2 singPeerRole reflB reflA of
        ReflNobodyHasAgency -> case reflB of {}

    go (Yield reflA _ _) (Done reflB _)    =
      case terminationLemma_1 singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Await reflA _)   (Done reflB _)    =
      case terminationLemma_1 singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}


-- | The terminal states for the protocol. Used in 'connect' and
-- 'connectPipelined' to return the states in which the peers terminated.
--
data TerminalStates ps (pr :: PeerRole) where
     TerminalStates
       :: forall ps pr (st :: ps) (st' :: ps).
          Sing st
       -> ReflRelativeAgency (StateAgency st)
                              NobodyHasAgency
                             (Relative             pr  (StateAgency st))
       -> Sing st'
       -> ReflRelativeAgency (StateAgency st')
                              NobodyHasAgency
                             (Relative (FlipAgency pr) (StateAgency st'))
       -> TerminalStates ps pr

--
-- Remove Pipelining
--

-- | Singletons for types of kind `Trans`.
--
type STrans :: Trans ps -> Type
data STrans tr where
   STr :: STrans (Tr st st')

-- | `SQueue` tracks the order of transitions.  We either have an
-- explicit `Message` or a `STrans` singleton, both are pushed by
-- `YieldPipelined` operation.
--
-- Note: if not the order of arguments 'SQueue' could be given a category
-- instance
type SQueue :: forall ps -> PeerRole -> ps -> Queue ps -> ps -> Type
data SQueue ps pr st q st' where
  ConsMsgQ :: ( SingI st
              , SingI st'
              , ActiveState st
              )
           => (ReflRelativeAgency (StateAgency st)
                                   WeHaveAgency
                                  (Relative pr (StateAgency st)))
           -> Message ps st st'
           -> SQueue  ps pr st' q st''
           -> SQueue  ps pr st  q st''

  ConsTrQ  :: STrans (Tr st st')
           -> SQueue ps pr st'               q  st''
           -> SQueue ps pr st  (Tr st st' <| q) st''

  EmptyQ   :: SQueue ps pr st Empty st

-- | Push a `ConsMsgQ` to the back of `SQueue`.
--
snocMsgQ :: ( SingI st'
            , SingI st''
            , ActiveState st'
            )
         => (ReflRelativeAgency (StateAgency st')
                                 WeHaveAgency
                                (Relative pr (StateAgency st')))
         -> Message ps st' st''
         -> SQueue  ps pr st q st'
         -> SQueue  ps pr st q st''
snocMsgQ stok msg (ConsMsgQ stok' msg' pq) =
  ConsMsgQ stok' msg' (snocMsgQ stok msg pq)
snocMsgQ stok msg (ConsTrQ str pq) =
  ConsTrQ str (snocMsgQ stok msg pq)
snocMsgQ stok msg EmptyQ =
  ConsMsgQ stok msg EmptyQ

-- | Push a `STrans (Tr st st')` to the back of `SQueue`.
--
snocTrQ :: SingI st'
        => STrans (Tr st' st'')
        -> SQueue ps pr st  q                 st'
        -> SQueue ps pr st (q |> Tr st' st'') st''
snocTrQ tr (ConsMsgQ stok msg pq) =
  ConsMsgQ stok msg (snocTrQ tr pq)
snocTrQ tr (ConsTrQ tr' pq) =
  ConsTrQ tr' (snocTrQ tr pq)
snocTrQ tr EmptyQ =
  ConsTrQ tr EmptyQ


-- | Prove that we have a total conversion from pipelined peers to regular
-- peers. This is a sanity property that shows that pipelining did not give
-- us extra expressiveness or to break the protocol state machine.
forgetPipelined
    :: forall ps (pr :: PeerRole) (pl :: Pipelined) (initSt :: ps) m a.
       MonadSTM m
    => [Bool]
    -- ^ interleaving choices for pipelining allowed by
    -- `Collect` and `CollectSTM` primitive. False values or `[]` give no
    -- pipelining.  For the 'CollectSTM' primitive, the stm action must not
    -- block otherwise even if the choice is to pipeline more (a 'True' value),
    -- we'll actually collect a result.
    -> Peer ps (pr :: PeerRole) pl           Empty initSt m (STM m) a
    -> Peer ps  pr              NonPipelined Empty initSt m (STM m) a
forgetPipelined cs0 = go cs0 EmptyQ
  where
    go :: [Bool]
       -> SQueue ps pr            st q     st'
       -> Peer   ps pr pl            q     st' m (STM m) a
       -> Peer   ps pr 'NonPipelined Empty st  m (STM m) a

    go cs pq     (Effect k)         = Effect         $ go cs pq <$> k
    go cs EmptyQ (Yield stok msg k) = Yield stok msg $ go cs EmptyQ k
    go cs EmptyQ (Await stok k)     = Await stok     $ go cs EmptyQ . k
    go _  EmptyQ (Done stok a)      = Done  stok a

    go cs pq     (YieldPipelined stok msg k) =
       -- push message and promised transition to `SQueue`.
       go cs ( pq
             & snocMsgQ stok msg
             & snocTrQ STr
             )
             k

    go cs (ConsMsgQ stok msg pq) k  = Yield stok msg $ go cs pq k

    go (True:cs') pq              (Collect _ (Just k) _) =
       go cs' pq k
    go (_:cs)    (ConsTrQ STr pq) (Collect stok _ k) =
       Await stok $ go cs (ConsTrQ STr pq) . k
    go []        (ConsTrQ STr pq) (Collect stok _ k) =
       Await stok $ go [] (ConsTrQ STr pq) . k

    go (True:cs) pq@(ConsTrQ STr pq') (CollectSTM stok stmK k) =
      Effect $ atomically $      (go cs pq <$> stmK)
                   `orElse` pure (Await stok $ go cs (ConsTrQ STr pq') . k)
    go (_:cs)       (ConsTrQ STr pq') (CollectSTM stok _ k) =
      Effect $ atomically $ pure (Await stok $ go cs (ConsTrQ STr pq') . k)
    go []           (ConsTrQ STr pq') (CollectSTM stok _ k) =
      Effect $ atomically $ pure (Await stok $ go [] (ConsTrQ STr pq') . k)

    go cs (ConsTrQ _ pq) (CollectDone k) =
       go cs pq k


-- | Analogous to 'connectNonPipelined' but also for pipelined peers.
--
-- Since pipelining allows multiple possible interleavings, we provide a
-- @[Bool]@ parameter to control the choices. Each @True@ will trigger picking
-- the first choice in the @SenderCollect@ construct (if possible), leading
-- to more results outstanding. This can also be interpreted as a greater
-- pipeline depth, or more messages in-flight.
--
-- Note that this term proves deadlock freedom of any protocol under the
-- assumption that each side can buffer all messages resulted from pipelining.
-- As a counter example let us consider a 'PingPong' protocol if client side
-- pipelines messages, while the server side is a 'NonPipelined' 'Peer' and
-- consider a communication channel which is implemented using two bounded
-- queues.  If the client will keep sending ping messages it will eventually
-- block not being able to send more, and the remote side will also be
-- eventually blocked not being able to send more replies.  But this can be
-- more subtle than that.  This can happen despite using `CollectSTM`, because
-- it is not an atomic operation to read from the channel.  `CollectSTM` in our
-- current implementation starts a new thread which reads from the network.  We
-- use an stm transaction to wait on its result, however the reading thread can
-- be preempted after reading a message, giving space for the server side
-- to send one more reply, and blocking on sending the next one which can block the
-- client as well (server is not reading), leaving both in a deadlock. This
-- however is only possible when the channels are not prepared to buffer enough
-- messages that result from pipelining.  In protocols which are using system
-- bearers (e.g. TCP), resource consumption resulted from pipelining must be
-- taken into account (which can lead to dropped packets, and thus protocol
-- errors).
--
-- This can be exercised using a QuickCheck style generator.
--
connect
  :: forall ps (pr :: PeerRole)
               (pl :: Pipelined)
               (pl' :: Pipelined)
               (st :: ps) m a b.
       (MonadSTM m, SingI pr)
    => [Bool]
    -> [Bool]
    -> Peer ps             pr  pl  Empty st m (STM m) a
    -> Peer ps (FlipAgency pr) pl' Empty st m (STM m) b
    -> m (a, b, TerminalStates ps pr)
connect csA csB a b =
    connectNonPipelined (forgetPipelined csA a)
                        (forgetPipelined csB b)

-- | A reference specification for interleaving of requests and responses
-- with pipelining, where the environment can choose whether a response is
-- available yet.
--
-- This also supports bounded choice where the maximum number of outstanding
-- in-flight responses is limited.
--
pipelineInterleaving :: Int    -- ^ Bound on outstanding responses
                     -> [Bool] -- ^ Pipelining choices
                     -> [req] -> [resp] -> [Either req resp]
pipelineInterleaving omax cs0 reqs0 resps0 =
    go 0 cs0 (zip [0 :: Int ..] reqs0)
             (zip [0 :: Int ..] resps0)
  where
    go o (c:cs) reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) (c:cs) reqs' resps
      | c && o < omax   = Left  req   : go (o+1)    cs  reqs' resps
      | otherwise       = Right resp  : go (o-1)    cs  reqs  resps'

    go o []     reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) [] reqs' resps
      | otherwise       = Right resp  : go (o-1) [] reqs  resps'

    go _ _ [] resps     = map (Right . snd) resps
    go _ _ (_:_) []     = error "pipelineInterleaving: not enough responses"

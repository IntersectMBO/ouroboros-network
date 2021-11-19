{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
-- TODO: the 'Functor' instance of 'Peer' is undecidable
{-# LANGUAGE UndecidableInstances     #-}

-- | Protocol EDSL.
--
-- __Note__: 'Network.TypedProtocol.Peer.Client.Client' and
-- 'Network.TypedProtocol.Peer.Server.Server' patterns are easier to use.
--
module Network.TypedProtocol.Peer (Peer (..)) where

import           Data.Kind (Type)
import           Data.Singletons

import           Control.Monad.Class.MonadSTM

import           Network.TypedProtocol.Core as Core


-- | A description of a peer that engages in a protocol.
--
-- The protocol describes what messages peers /may/ send or /must/ accept.
-- A particular peer implementation decides what to actually do within the
-- constraints of the protocol.
--
-- Peers engage in a protocol in either the client or server role. Of course
-- the client role can only interact with the serve role for the same protocol
-- and vice versa.
--
-- 'Peer' has several type arguments:
--
-- * the protocol itself;
-- * the client\/server role;
-- *.the current protocol state;
-- * the monad in which the peer operates; and
-- * the type of any final result once the peer terminates.
--
-- For example:
--
-- > pingPongClientExample :: Int -> Peer PingPong AsClient StIdle m ()
-- > pingPongServerExample ::        Peer PingPong AsServer StIdle m Int
--
-- The actions that a peer can take are:
--
-- * to perform local monadic effects
-- * to terminate with a result (but only in a terminal protocol state)
-- * to send a message (but only in a protocol state in which we have agency)
-- * to wait to receive a message (but only in a protocol state in which the
--   other peer has agency)
--
-- The 'Yield', 'Await' and 'Done' constructors require to provide an evidence
-- that the appropriate peer has agency.  This information is suplied using
-- one of the constructors of 'ReflRelativeAgency'.
--
-- While this evidence must be provided, the types guarantee that it is not
-- possible to supply incorrect evidence.  The
-- 'Network.TypedProtocol.Peer.Client' or 'Network.TypedProtocol.Peer.Server'
-- pattern synonyms provide this evidence automatically.
--
type Peer :: forall ps
          -> PeerRole
          -> Pipelined
          -> Queue ps
          -> ps
          -> (Type -> Type)
          -- ^ monad's kind
          -> Type
          -> Type
data Peer ps pr pl q st m a where

  -- | Perform a local monadic effect and then continue.
  --
  -- Example:
  --
  -- > Effect $ do
  -- >   ...          -- actions in the monad
  -- >   return $ ... -- another Peer value
  --
  Effect
    :: forall ps pr pl q st m a.
       m (Peer ps pr pl q st m a)
    -- ^ monadic continuation
    ->    Peer ps pr pl q st m a

  -- | Send a message to the other peer and then continue. This takes the
  -- message and the continuation. It also requires evidence that we have
  -- agency for this protocol state and thus are allowed to send messages.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency MsgPing $ ...
  --
  Yield
    :: forall ps pr pl (st :: ps) (st' :: ps) m a.
       ( SingI (PeerHasAgency st)
       , SingI (ProtocolState st')
       )
    => (ReflRelativeAgency (StateAgency st)
                            WeHaveAgency
                           (Relative pr (StateAgency st)))
    -- ^ agency singleton
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr pl Empty st' m a
    -- ^ continuation
    -> Peer ps pr pl Empty st  m a

  -- | Waits to receive a message from the other peer and then continues.
  -- This takes the continuation that is supplied with the received message. It
  -- also requires evidence that the other peer has agency for this protocol
  -- state and thus we are expected to wait to receive messages.
  --
  -- Note that the continuation that gets supplied with the message must be
  -- prepared to deal with /any/ message that is allowed in /this/ protocol
  -- state. This is why the continuation /must/ be polymorphic in the target
  -- state of the message (the third type argument of 'Message').
  --
  -- Example:
  --
  -- > Await ReflClientAgency $ \msg ->
  -- > case msg of
  -- >   MsgDone -> ...
  -- >   MsgPing -> ...
  --
  Await
    :: forall ps pr pl (st :: ps) m a.
       SingI (PeerHasAgency st)
    => (ReflRelativeAgency (StateAgency st)
                            TheyHaveAgency
                           (Relative pr (StateAgency st)))
    -- ^ agency singleton
    -> (forall (st' :: ps). Message ps st st'
        -> Peer ps pr pl Empty st' m a)
    -- ^ continuation
    -> Peer     ps pr pl Empty st  m a

  -- | Terminate with a result. A state token must be provided from the
  -- 'NobodyHasAgency' states, so show that this is a state in which we can
  -- terminate.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency
  -- >        MsgDone
  -- >       (Done ReflNobodyAgency TokDone result)
  --
  Done
    :: forall ps pr pl (st :: ps) m a.
       SingI (ProtocolState st)
    => (ReflRelativeAgency (StateAgency st)
                            NobodyHasAgency
                           (Relative pr (StateAgency st)))
    -- ^ (no) agency singleton
    -> a
    -- ^ returned value
    -> Peer ps pr pl Empty st m a

  --
  -- Pipelining primitives
  --

  -- | Pipelined send which. Note that the continuation decides from which
  -- state we pipeline next message, and the gap is pushed at the back of
  -- the queue.
  --
  YieldPipelined
    :: forall ps pr (st :: ps) (st' :: ps) q st'' m a.
       ( SingI (PeerHasAgency st)
       , SingI (ProtocolState st')
       )
    => (ReflRelativeAgency (StateAgency st)
                            WeHaveAgency
                           (Relative pr (StateAgency st)))
    -- ^ agency singleton
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr 'Pipelined (q |> Tr st' st'') st'' m a
    -- ^ continuation
    -> Peer ps pr 'Pipelined  q                 st   m a

  -- | Partially collect promised transition.
  --
  Collect
    :: forall ps pr (st' :: ps) (st'' :: ps) q st m a.
       SingI (PeerHasAgency st')
    => (ReflRelativeAgency (StateAgency st')
                            TheyHaveAgency
                           (Relative pr (StateAgency st')))
    -- ^ agency singleton
    -> Maybe (Peer ps pr 'Pipelined (Tr st' st'' <| q) st m a)
    -- ^ continuation, executed if no message has arrived so far
    -> (forall (stNext :: ps). Message ps st' stNext
        -> Peer ps pr 'Pipelined (Tr stNext st'' <| q) st m a)
    -- ^ continuation
    -> Peer     ps pr 'Pipelined (Tr st'    st'' <| q) st m a

  -- | Collect the identity transition.
  --
  -- 'CollectDone' allows to defer popping @Tr ps st st@ from the queue
  -- after a message is received (in 'Collect' callback), unlike 'Collect'
  -- which needs to know the transition type at compile time.
  --
  CollectDone
    :: forall ps pr (st :: ps) q (st' :: ps) m a.
       Peer ps pr 'Pipelined              q  st' m a
    -- ^ continuation
    -> Peer ps pr 'Pipelined (Tr st st <| q) st' m a

  -- The 'Peer' driver will race two transactions, the peer continuation versus
  -- next message.
  --
  -- Note: the driver, or interpreter if you wish, will build the stm
  -- transaction.  This allows us to cleanly handle the thread which is
  -- responsible for
  --
  CollectSTM
    :: forall ps pr (st' :: ps) (st'' :: ps) q (st :: ps) m a.
       SingI (PeerHasAgency st')
    => (ReflRelativeAgency (StateAgency st')
                            TheyHaveAgency
                           (Relative pr (StateAgency st')))
    -- ^ agency singleton
    -> STM m (Peer ps pr 'Pipelined (Tr st' st'' <| q) st m a)
    -- ^ continuation, which is executed if it wins the race with the next
    -- message.
    -> (forall stNext. Message ps st' stNext
        -> Peer ps pr 'Pipelined (Tr stNext st'' <| q) st m a)
    -- ^ continuation
    -> Peer     ps pr 'Pipelined (Tr st'    st'' <| q) st m a


deriving instance (Functor m, Functor (STM m)) => Functor (Peer ps (pr :: PeerRole) pl q (st :: ps) m)

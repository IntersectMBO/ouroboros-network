{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Actions for running 'Peer's with a 'Driver'
--
module Network.TypedProtocol.Driver
  ( -- * Introduction
    -- $intro
    -- * Driver interface
    Driver (..)
  , DriverState (..)
  , SomeMessage (..)
    -- * Running a peer
  , runPeerWithDriver
    -- * Re-exports
  , DecodeStep (..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Class.MonadSTM

import           Data.Type.Queue
import           Data.Singletons

import           Network.TypedProtocol.Codec (DecodeStep (..), SomeMessage (..))
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer


-- $intro
--
-- A 'Peer' is a particular implementation of an agent that engages in a
-- typed protocol. To actually run one we need a source and sink for the typed
-- protocol messages. These are provided by a 'Channel' and a 'Codec'. The
-- 'Channel' represents one end of an untyped duplex message transport, and
-- the 'Codec' handles conversion between the typed protocol messages and
-- the untyped channel.
--
-- So given the 'Peer' and a compatible 'Codec' and 'Channel' we can run the
-- peer in some appropriate monad. The peer and codec have to agree on
-- the same protocol and role in that protocol. The codec and channel have to
-- agree on the same untyped medium, e.g. text or bytes. All three have to
-- agree on the same monad in which they will run.
--
-- This module provides drivers for normal and pipelined peers. There is
-- very little policy involved here so typically it should be possible to
-- use these drivers, and customise things by adjusting the peer, or codec
-- or channel.
--
-- It is of course possible to write custom drivers and the code for these ones
-- may provide a useful starting point. The 'runDecoder' function may be a
-- helpful utility for use in custom drives.
--


-- | 'Driver' can be interrupted and might need to resume.  'DriverState'
-- records its state, so it can resume.
--
data DriverState ps (pr :: PeerRole) (st :: ps) bytes failure dstate m
    = DecoderState   (DecodeStep bytes failure m (SomeMessage st))
                     !dstate
      -- ^ 'tryRecvMessage' can return either 'DecodeStep' with
      -- current 'dstate' or a parsed message.

    | DriverState    !dstate
      -- ^ dstate which was returned by 'recvMessage'

    | DriverStateSTM (STM m (SomeMessage st, dstate))
                     !dstate
      -- ^ 'recvMessageSTM' might leave us with an stm action.


--
-- Driver interface
--

data Driver ps (pr :: PeerRole) bytes failure dstate m =
        Driver {
          -- | Send a message.
          --
          sendMessage    :: forall (st :: ps) (st' :: ps).
                            SingI st
                         => SingI st'
                         => ActiveState st
                         => (ReflRelativeAgency (StateAgency st)
                                                 WeHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> Message ps st st'
                         -> m ()

        , -- | Receive a message, a blocking action which reads from the network
          -- and runs the incremental decoder until a full message is decoded.
          -- As an input it might receive a 'DecodeStep' previously started with
          -- 'tryRecvMessage'.
          --
          -- It could be implemented in terms of 'recvMessageSTM', but in some
          -- cases it can be easier (or more performant) to have a different
          -- implementation.
          --
          recvMessage    :: forall (st :: ps).
                            SingI st
                         => ActiveState st
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> DriverState ps pr st bytes failure dstate m
                         -> m (SomeMessage st, dstate)

        , -- | 'tryRecvMessage' is used to interpret @'Collect' _ (Just k') k@.
          -- If it returns we will continue with @k@, otherwise we keep the
          -- decoder state @DecodeStep@ and continue pipelining using @k'@.
          --
          -- 'tryRecvMessage' ought to be non-blocking.
          --
          -- It also could be implemented in terms of 'recvMessageSTM', but
          -- there are cases where a separate implementation would be simpler
          -- or more performant as it does not need to relay on STM but instead
          -- relay on non-blocking IO.
          --
          tryRecvMessage :: forall (st :: ps).
                            SingI st
                         => ActiveState st
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> DriverState ps pr st bytes failure dstate m
                         -> m (Either (DriverState ps pr st bytes failure dstate m)
                                      ( SomeMessage st
                                      , dstate
                                      ))

        , -- | Construct a non-blocking stm action which awaits for the
          -- message.
          --
          recvMessageSTM :: forall (st :: ps).
                            SingI st
                         => ActiveState st
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> DriverState ps pr st bytes failure dstate m
                         -> m (STM m (SomeMessage st, dstate))

        , startDState    :: dstate
        }


--
-- Running peers
--


-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr pl bytes failure dstate m a.
     MonadSTM m
  => Driver ps pr bytes failure dstate m
  -> Peer ps st pr pl (Empty :: Queue ps st) m (STM m) a
  -> m (a, dstate)
runPeerWithDriver Driver{ sendMessage
                        , recvMessage
                        , tryRecvMessage
                        , recvMessageSTM
                        , startDState } =
    goEmpty startDState
  where
    goEmpty
       :: forall st'.
          dstate
       -> Peer ps st' pr pl 'Empty m (STM m) a
       -> m (a, dstate)
    goEmpty !dstate (Effect k) = k >>= goEmpty dstate

    goEmpty !dstate (Done _ x) = return (x, dstate)

    goEmpty !dstate (Yield refl msg k) = do
      sendMessage refl msg
      goEmpty dstate k

    goEmpty !dstate (Await refl k) = do
      (SomeMessage msg, dstate') <- recvMessage refl (DriverState dstate)
      goEmpty dstate' (k msg)

    goEmpty !dstate (YieldPipelined refl msg k) = do
      sendMessage refl msg
      go singSingleton (DriverState dstate) k


    go :: forall st1 st2 st3 (q' :: Queue ps st3).
          SingQueue (Tr st1 st2 <| q') st3
       -> DriverState ps pr st1 bytes failure dstate m
       -> Peer ps st3 pr pl (Tr st1 st2 <| q') m (STM m) a
       -> m (a, dstate)
    go q !dstate (Effect k) = k >>= go q dstate

    go q !dstate (YieldPipelined
                  refl
                  (msg :: Message ps st3 st')
                  (k   :: Peer ps st'' pr pl ((Tr st1 st2 <| q') |> Tr st' st'') m (STM m) a))
                = do
      sendMessage refl msg
      go (q `snoc` (SingTr :: SingTrans (Tr st' st''))
            :: SingQueue ((Tr st1 st2 <| q') |> Tr st' st'') st'')
         dstate k

    go (SingCons q) !dstate (Collect refl Nothing k) = do
      (SomeMessage msg, dstate') <- recvMessage refl dstate
      go (SingCons q) (DriverState dstate') (k msg)

    go q@(SingCons q') !dstate (Collect refl (Just k') k) = do
      r <- tryRecvMessage refl dstate
      case r of
        Left dstate' ->
          go q (dstate') k'
        Right (SomeMessage msg, dstate') ->
          go (SingCons q') (DriverState dstate') (k msg)

    go (SingCons SingEmpty) (DriverState dstate) (CollectDone k) =
      goEmpty dstate k

    go (SingCons q@SingCons {}) (DriverState dstate) (CollectDone k) =
      go q (DriverState dstate) k

    go q@(SingCons q') !dstate (CollectSTM refl k' k) = do
      stm <- recvMessageSTM refl dstate
      -- Note that the 'stm' action also returns next @dstate@.  For this
      -- reason, using a simpler 'CollectSTM' which takes `STM m Message` as an
      -- argument and computes the result by itself is not possible.
      r <- atomically $
             Left  <$> stm
         <|> Right <$> k'
      case r of
        Left (SomeMessage msg, dstate') ->
          go (SingCons q') (DriverState dstate') (k msg)
        Right k'' ->
          go q (DriverStateSTM stm (getDState dstate)) k''


    go _q             DecoderState {}        CollectDone {} =
      -- 'CollectDone' can only be executed once `Collect` or `CollectSTM` was
      -- effective, which means we cannot receive a partial decoder here.
      error "runPeerWithDriver: unexpected parital decoder"

    go _q             DriverStateSTM {}      CollectDone {} =
      -- 'CollectDone' can only placed when once `Collect` or `CollectSTM` was
      -- effective, we cannot have 'DriverStateSTM' at this stage.
      error "runPeerWithDriver: unexpected driver state"

    --
    -- lenses
    --

    getDState :: forall (st' :: ps). DriverState ps pr st' bytes failure dstate m -> dstate
    getDState (DecoderState   _ dstate) = dstate
    getDState (DriverState      dstate) = dstate
    getDState (DriverStateSTM _ dstate) = dstate

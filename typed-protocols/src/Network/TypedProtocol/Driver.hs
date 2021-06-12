{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

-- | Actions for running 'Peer's with a 'Driver'
--
module Network.TypedProtocol.Driver
  ( -- * Introduction
    -- $intro
    -- * Driver interface
    Driver (..)
  , SomeMessage (..)
    -- * Running a peer
  , runPeerWithDriver
  ) where

import           Data.Singletons

import           Network.TypedProtocol.Core


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


--
-- Driver interface
--

data Driver ps (pr :: PeerRole) dstate m =
        Driver {
          sendMessage    :: forall (st :: ps) (st' :: ps).
                            SingI st
                         => (ReflRelativeAgency (StateAgency st)
                                                 WeHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> Message ps st st'
                         -> m ()

        , recvMessage    :: forall (st :: ps).
                            SingI st
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> dstate
                         -> m (SomeMessage st, dstate)

        , tryRecvMessage :: forall (st :: ps).
                            SingI st
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> dstate
                         -> m (Maybe (SomeMessage st, dstate))

        , startDState    :: dstate
        }

-- | When decoding a 'Message' we only know the expected \"from\" state. We
-- cannot know the \"to\" state as this depends on the message we decode. To
-- resolve this we use the 'SomeMessage' wrapper which uses an existential
-- type to hide the \"to"\ state.
--
data SomeMessage (st :: ps) where
     SomeMessage :: Message ps st st' -> SomeMessage st


--
-- Running peers
--

-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr pl q dstate m a.
     Monad m
  => Driver ps pr dstate m
  -> Peer ps pr pl q st m a
  -> dstate
  -> m (a, dstate)
runPeerWithDriver Driver{sendMessage, recvMessage, tryRecvMessage} =
    flip go
  where
    go :: forall st' q'.
          dstate
       -> Peer ps pr pl q' st' m a
       -> m (a, dstate)
    go dstate (Effect k) = k >>= go dstate
    go dstate (Done _ x) = return (x, dstate)

    go dstate (Yield refl msg k) = do
      sendMessage refl msg
      go dstate k

    go dstate (Await refl k) = do
      (SomeMessage msg, dstate') <- recvMessage refl dstate
      go dstate' (k msg)

    go dstate (YieldPipelined refl msg k) = do
      sendMessage refl msg
      go dstate k

    go dstate (Collect refl Nothing k) = do
      (SomeMessage msg, dstate') <- recvMessage refl dstate
      go dstate' (k msg)

    go dstate (Collect refl (Just k') k) = do
      mc <- tryRecvMessage refl dstate
      case mc of
        Nothing                         -> go dstate  k'
        Just (SomeMessage msg, dstate') -> go dstate' (k msg)

    go dstate (CollectDone k) = go dstate k

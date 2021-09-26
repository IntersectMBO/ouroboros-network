{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

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
    -- * Re-exports
  , DecodeStep (..)
  ) where

import           Data.Kind (Type)
import           Data.Singletons
import           Unsafe.Coerce (unsafeCoerce)

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


--
-- Driver interface
--

data Driver ps (pr :: PeerRole) bytes failure dstate m =
        Driver {
          -- | Send a message.
          --
          sendMessage    :: forall (st :: ps) (st' :: ps).
                            ( SingI (PeerHasAgency st)
                            , SingI (ProtocolState st')
                            )
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
          recvMessage    :: forall (st :: ps).
                            SingI (PeerHasAgency st)
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> Either ( DecodeStep bytes failure m (SomeMessage st)
                                   , dstate
                                   )
                                   dstate
                         -> m (SomeMessage st, dstate)

        , -- | 'tryRecvMessage' is used to interpret @'Collect' _ (Just k') k@.
          -- If it returns we will continue with @k@, otherwise we keep the
          -- decoder state @DecodeStep@ and continue pipelining using @k'@.
          --
          -- 'tryRecvMessage' ought to be non-blocking.
          --
          tryRecvMessage :: forall (st :: ps).
                            SingI (PeerHasAgency st)
                         => (ReflRelativeAgency (StateAgency st)
                                                 TheyHaveAgency
                                                (Relative pr (StateAgency st)))
                         -> Either    ( DecodeStep bytes failure m (SomeMessage st)
                                      , dstate
                                      )
                                      dstate
                         -> m (Either ( DecodeStep bytes failure m (SomeMessage st)
                                      , dstate
                                      )
                                      ( SomeMessage st
                                      , dstate
                                      ))

        , startDState    :: dstate
        }


--
-- Running peers
--


-- | A space efficient singleton for a non-empty 'Queue' type.  It has two
-- public constructors 'SingSingleton' and 'SingCons'.
--
-- We use a 'newtype' rather than a 'GADT' which would allow to restrict 'q' to
-- non-empty types.  The safe api is not checked by the type checker, it is
-- rather declared to be safe, e.g. 'SingSingleton' and 'SingCons' are declared
-- to define a complete pattern match.  Using 'UnsafeSingQueue' does not guarantee
-- that the changes to the internal representation reflect correctly the changes
-- at the type level nevertheless using it allows to reduce computational
-- complexity (see 'snoc' below).
--
type SingQueue :: Queue ps -> Type
newtype SingQueue q = UnsafeSingQueue Int


-- | 'IsQueue' is an auxiliary type which allows to pattern match if the queue
-- is a singleton or not.  The 'toIsQueue' function converts 'SingQueue' to
-- 'IsQueue' in an efficient way.
--
-- 'IsQueue' mimics an inductive definition, but instead recursion, it is using
-- 'SingQueue' in its 'IsCons' constructor.
--
type IsQueue :: Queue ps -> Type
data IsQueue q where
    IsEmpty :: IsQueue Empty
    IsCons  :: forall ps (st :: ps) (st' :: ps) (q :: Queue ps).
               SingQueue               q
            -> IsQueue   (Tr st st' <| q)

-- | Transform 'SingQueue' to 'IsQueue'.  Although this function is using
-- 'unsafeCoerce' it is safe.
--
toIsQueue :: SingQueue q -> IsQueue q
toIsQueue (UnsafeSingQueue n) | n < 0
                              = error "toIsQueue: invalid value"
toIsQueue (UnsafeSingQueue 0) = unsafeCoerce  IsEmpty
toIsQueue (UnsafeSingQueue n) = unsafeCoerce (IsCons (UnsafeSingQueue $ pred n))
  -- we subtract one, because 'IsCons' constructor takes singleton for the
  -- remaining part of the list.

-- | A safe 'SingQueue' bidirectional pattern for queues which holds exactly
-- one element.
--
pattern SingEmpty :: ()
                  => q ~ Empty
                  => SingQueue q
pattern SingEmpty <- (toIsQueue -> IsEmpty)
  where
    SingEmpty = UnsafeSingQueue 0


-- | A safe 'SingQueue' bidirectional pattern for queues of length 2 or more.
--
pattern SingCons :: forall ps (q :: Queue ps).
                    ()
                 => forall (st :: ps) (st' :: ps) (q' :: Queue ps).
                    (q ~ (Tr st st' <| q'))
                 => SingQueue q'
                    -- ^ singleton for the remaining part of the queue
                 -> SingQueue q
pattern SingCons n <- (toIsQueue -> IsCons n)
  where
    SingCons (UnsafeSingQueue n) = UnsafeSingQueue (succ n)

{-# COMPLETE SingEmpty, SingCons #-}


-- | A singleton for singleton queue.
--
singSingleton :: SingQueue (Tr st st' <| Empty)
singSingleton = SingCons SingEmpty


-- | 'snoc'
--
-- /complexity:/ @O(1)@
--
-- It is accessing the internal representation of 'SingQueue' in an unsafe way.
-- It is possible to implement it using the safe api but then it would be @O(n)@
-- instead.
--
snoc :: forall ps (st :: ps) (st' :: ps) (q :: Queue ps).
        SingQueue  q
     -> SingTrans      (Tr st st')
     -> SingQueue (q |> Tr st st')
snoc  SingEmpty                     _ = SingCons (UnsafeSingQueue 1)
snoc (SingCons (UnsafeSingQueue n)) _ = SingCons (UnsafeSingQueue (succ n))


uncons :: SingQueue (Tr st st <| q)
       -> SingQueue              q
uncons (SingCons q) = q


-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr pl bytes failure dstate m a.
     Monad m
  => Driver ps pr bytes failure dstate m
  -> Peer ps pr pl Empty st m a
  -> dstate
  -> m (a, dstate)
runPeerWithDriver Driver{sendMessage, recvMessage, tryRecvMessage} =
    flip goEmpty
  where
    goEmpty
       :: forall st'.
          dstate
       -> Peer ps pr pl 'Empty st' m a
       -> m (a, dstate)
    goEmpty !dstate (Effect k) = k >>= goEmpty dstate

    goEmpty !dstate (Done _ x) = return (x, dstate)

    goEmpty !dstate (Yield refl msg k) = do
      sendMessage refl msg
      goEmpty dstate k

    goEmpty !dstate (Await refl k) = do
      (SomeMessage msg, dstate') <- recvMessage refl (Right dstate)
      goEmpty dstate' (k msg)

    goEmpty !dstate (YieldPipelined refl msg k) = do
      sendMessage refl msg
      go singSingleton (Right dstate) k


    go :: forall st1 st2 st3 q'.
          SingQueue (Tr st1 st2 <| q')
       -> Either ( DecodeStep bytes failure m (SomeMessage st1)
                 , dstate
                 )
                 dstate
       -> Peer ps pr pl (Tr st1 st2 <| q') st3 m a
       -> m (a, dstate)
    go q !dstate (Effect k) = k >>= go q dstate

    go q !dstate (YieldPipelined
                  refl
                  (msg :: Message ps st3 st')
                  (k   :: Peer ps pr pl ((Tr st1 st2 <| q') |> Tr st' st'') st'' m a))
                = do
      sendMessage refl msg
      go (q `snoc` (SingTr :: SingTrans (Tr st' st'')))
         dstate k

    go (SingCons q) !dstate (Collect refl Nothing k) = do
      (SomeMessage msg, dstate') <- recvMessage refl dstate
      go (SingCons q) (Right dstate') (k msg)

    go q@(SingCons q') !dstate (Collect refl (Just k') k) = do
      r <- tryRecvMessage refl dstate
      case r of
        Left dstate' ->
          go q (Left dstate') k'
        Right (SomeMessage msg, dstate') ->
          go (SingCons q') (Right dstate') (k msg)

    go (SingCons SingEmpty) (Right dstate) (CollectDone k) =
      goEmpty dstate k

    go q@(SingCons (SingCons {})) (Right dstate) (CollectDone k) =
      go (uncons q) (Right dstate) k

    go _q             Left {}        CollectDone {} =
      -- 'CollectDone' can only be executed once `Collect` was effective, which
      -- means we cannot receive a partial decoder here.
      error "runPeerWithDriver: unexpected parital decoder"

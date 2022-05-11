{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Actions for running 'Peer's with a 'Driver'.  This module should be
-- imported qualified.
--
module Network.TypedProtocol.Stateful.Driver
  ( -- * Running a peer
    runPeerWithDriver
    -- * Re-exports
  , DecodeStep (..)
  , Driver (..)
  , DriverState (..)
  , SomeMessage (..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Class.MonadSTM

import           Data.Kind (Type)
import           Data.Type.Queue
import           Data.Singletons

import           Network.TypedProtocol.Codec (DecodeStep (..), SomeMessage (..))
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver (DriverState (..))
import           Network.TypedProtocol.Stateful.Peer
import           Unsafe.Coerce (unsafeCoerce)

data Driver ps (pr :: PeerRole) bytes failure dstate f m =
        Driver {
          -- | Send a message.
          --
          sendMessage    :: forall (st :: ps) (st' :: ps).
                            SingI st
                         => SingI st'
                         => ActiveState st
                         => ReflRelativeAgency (StateAgency st)
                                                WeHaveAgency
                                               (Relative pr (StateAgency st))
                         -> f st'
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
                         => ReflRelativeAgency (StateAgency st)
                                                TheyHaveAgency
                                               (Relative pr (StateAgency st))
                         -> f st
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
                         => ReflRelativeAgency (StateAgency st)
                                                TheyHaveAgency
                                               (Relative pr (StateAgency st))
                         -> f st
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
                         => ReflRelativeAgency (StateAgency st)
                                                TheyHaveAgency
                                               (Relative pr (StateAgency st))
                         -> f st
                         -> DriverState ps pr st bytes failure dstate m
                         -> m (STM m (SomeMessage st, dstate))

        , startDState    :: dstate
        }


--
-- Running peers
--

type F :: forall ps. (ps -> Type) -> ps -> ps -> Type
data F f st st' where
  F :: !(f st) -> F f st st'

-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr pl bytes failure dstate (f :: ps -> Type) m a.
     MonadSTM m
  => Driver ps pr bytes failure dstate f m
  -> f st
  -> Peer ps pr pl Empty st f m (STM m) a
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
       -> f st'
       -> Peer ps pr pl 'Empty st' f m (STM m) a
       -> m (a, dstate)
    goEmpty !dstate !f (Effect k) = k >>= goEmpty dstate f

    goEmpty !dstate  _ (Done _ x) = return (x, dstate)

    goEmpty !dstate  _ (Yield refl !f msg k) = do
      sendMessage refl f msg
      goEmpty dstate f k

    goEmpty !dstate !f (Await refl k) = do
      (SomeMessage msg, dstate') <- recvMessage refl f (DriverState dstate)
      case k f msg of
        (k', f') -> goEmpty dstate' f' k'

    goEmpty !dstate  _ (YieldPipelined refl f msg k) = do
      sendMessage refl f msg
      go (SingConsF (F f) SingEmptyF) (DriverState dstate) k


    go :: forall st1 st2 st3 q'.
          SingQueueF (F f) (Tr st1 st2 <| q')
       -> DriverState ps pr st1 bytes failure dstate m
       -> Peer ps pr pl (Tr st1 st2 <| q') st3 f m (STM m) a
       -> m (a, dstate)
    go q !dstate (Effect k) = k >>= go q dstate

    go q !dstate (YieldPipelined
                  refl f
                  (msg :: Message ps st3 st')
                  (k   :: Peer ps pr pl ((Tr st1 st2 <| q') |> Tr st' st'') st'' f m (STM m) a))
                = do
      sendMessage refl f msg
      go (q |> (F f :: F f st' st''))
         dstate k

    go (SingConsF (F f) q) !dstate (Collect refl Nothing k) = do
      (SomeMessage msg, dstate') <- recvMessage refl f dstate
      case k f msg of
        (k', f') -> go (SingConsF (F f') q) (DriverState dstate') k'

    go q@(SingConsF (F f) q') !dstate (Collect refl (Just k') k) = do
      r <- tryRecvMessage refl f dstate
      case r of
        Left dstate' ->
          go q dstate' k'
        Right (SomeMessage msg, dstate') ->
          case k f msg of
            (k'', f'') -> go (SingConsF (F f'') q') (DriverState dstate') k''

    go (SingConsF (F (f :: f stX)) SingEmptyF)
       (DriverState dstate)
       (CollectDone k :: Peer ps pr pl (Cons (Tr stX stY) Empty) stZ f m stm a) =
      -- we collected all messages, which means that we reached the type:
      -- @Peer ps pr pl (Tr st st <| Empty) st f m stm a@
      -- but GHC has trouble to infer this.
      -- TODO: provide some aid for GHC to avoid the `unsafeCoerce`.
      goEmpty dstate (coerce f) k
        where
          coerce :: f stA -> f stZ
          coerce = unsafeCoerce

    go (SingConsF _ q@SingConsF {}) (DriverState dstate) (CollectDone k) =
      go q (DriverState dstate) k

    go q@(SingConsF (F f) q') !dstate (CollectSTM refl k' k) = do
      stm <- recvMessageSTM refl f dstate
      -- Note that the 'stm' action also returns next @dstate@.  For this
      -- reason, using a simpler 'CollectSTM' which takes `STM m Message` as an
      -- argument and computes the result by itself is not possible.
      r <- atomically $
             Left  <$> stm
         <|> Right <$> k'
      case r of
        Left (SomeMessage msg, dstate') ->
          case k f msg of
            (k'', f'') -> go (SingConsF (F f'') q') (DriverState dstate') k''
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

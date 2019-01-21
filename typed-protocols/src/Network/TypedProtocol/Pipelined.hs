{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

-- | This module defines the core of the typed protocol framework.
--
-- The typed protocol framework is used to define, test and execute protocols.
-- It also provides some abstractions for untyped channels and for encoding and
-- decoding protocol messages from untyped channels.
--
-- The typed protocol framework guarantees:
--
-- * agreement on which messages can be sent and received;
-- * the absence of race conditions; and
-- * the absence of deadlock.
--
-- It is a simple form of session typing. The trade-off to achieve these
-- guarantees it that places constraints on the kinds of protocol that can be
-- expressed. In particular it requires that protocols be defined as a state
-- transition system. It requires for each protocol state that one of the two
-- peers be able to send and the other must be ready to receive.
--
-- This means it is not possible to express protocols such as TCP where there
-- are protocol states where a single peer can both send and receive, however
-- it is suitable for most application-level protocols. In particular many
-- application-level protocols are completely in-order and synchronous.
--
-- In many (but not all) cases it is also possible to pipeline these protocols
-- so that network latency can be hidden and full use made of the available
-- bandwidth. Special support is provided to run protocols in a pipelined way,
-- without having to change the protocol definition.
--
-- The protocols in this framework assume an underlying \"reliable ordered\"
-- connection. A \"reliable ordered\" connection is a term of art means one
-- where the receiving end receives any prefix of the messages sent by the
-- sending end. It is not reliable in the colloquial sense as it does not
-- ensure that anything actually arrives, only that /if/ any message arrives,
-- all the previous ones did too, and in the order in which they were sent.
--
module Network.TypedProtocol.Pipelined where

import           Network.TypedProtocol.Core hiding (Peer(..))
--import qualified Network.TypedProtocol.Core as Core
import           Control.Monad.Class.MonadSTM



data PeerSender (pk :: PeerKind) (st :: ps) m a where

  Effect :: m (PeerSender pk st m a)
         ->    PeerSender pk st m a

  Done   :: (CurrentAgency pk (AgencyInState st) ~ Finished)
         => a
         -> PeerSender pk st m a

  Yield  :: (CurrentAgency pk (AgencyInState st) ~ Yielding)
         => Message st st'
         -> PeerReceiver pk (st'  :: ps) (st'' :: ps) m
         -> PeerSender   pk (st'' :: ps) m a
         -> PeerSender   pk (st   :: ps) m a

data PeerReceiver (pk :: PeerKind) (st :: ps) (st' :: ps) m where

  Effect'   :: m (PeerReceiver pk st st' m)
            ->    PeerReceiver pk st st' m

  Completed :: PeerReceiver pk st st m

  Await     :: (CurrentAgency pk (AgencyInState st) ~ Awaiting)
            => StateToken st
            -> (forall st''. Message st st'' -> PeerReceiver pk st'' st' m)
            -> PeerReceiver pk st st' m

data ReceiveHandler pk ps m where
     ReceiveHandler :: PeerReceiver pk (st :: ps) (st' :: ps) m
                    -> ReceiveHandler pk ps m

runPipelinedPeerWithCodec
  :: forall ps (st :: ps) pk m a bytes.
     (Monad m, MonadSTM m)
  => (forall (st'' :: ps). StateToken st'' -> bytes -> Maybe (SomeMessage st''))
  -> [bytes]
  -> PeerSender pk st m a
  -> m (Maybe a)
runPipelinedPeerWithCodec decode msgs0 sender = do
    queue <- atomically $ newTBQueue 10
    fork $ runReceiver' queue msgs0
    runSender queue sender
  where
    runSender :: forall st.
                 TBQueue m (ReceiveHandler pk ps m)
              -> PeerSender pk st m a
              -> m (Maybe a)

    runSender queue (Effect k) = k >>= runSender queue

    runSender _queue (Done x) = return (Just x)

    runSender queue (Yield _msg receiver k) = do
      {- serialise and send msg -}
      atomically $ writeTBQueue queue (ReceiveHandler receiver)
      runSender queue k

    runReceiver' :: TBQueue m (ReceiveHandler pk ps m) -> [bytes] -> m ()
    runReceiver' queue msgs = do
      receiver <- atomically $ readTBQueue queue
      case receiver of
        ReceiveHandler handler ->
          runReceiver decode msgs handler >>= runReceiver' queue

runReceiver :: Monad m
            => (forall (st'' :: ps). StateToken st'' -> bytes -> Maybe (SomeMessage st''))
            -> [bytes]
            -> PeerReceiver pk (st :: ps) (st' :: ps) m
            -> m [bytes]
runReceiver decode msgs (Effect' k) = k >>= runReceiver decode msgs

runReceiver _decode msgs Completed = return msgs

runReceiver decode (msg:msgs) (Await tok k) =
    case decode tok msg of
      Just (SomeMessage msg') -> runReceiver decode msgs (k msg')
      Nothing                 -> undefined

runReceiver _ [] (Await _ _) = undefined


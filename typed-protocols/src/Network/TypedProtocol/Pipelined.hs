{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Pipelined where

import Network.TypedProtocol.Core hiding (Peer(..))



data PeerSender (pk :: PeerKind) (st :: ps) m a where

  SenderEffect :: m (PeerSender pk st m a)
               ->    PeerSender pk st m a

  SenderDone   :: NobodyHasAgency st
               -> a
               -> PeerSender pk st m a

  PipelinedYield
               :: WeHaveAgency pk st
               -> Message st st'
               -> PeerReceiver pk (st' :: ps) (st :: ps) m
               -> PeerSender   pk (st  :: ps) m a
               -> PeerSender   pk (st  :: ps) m a

  SenderYield  :: WeHaveAgency pk st
               -> Message st st'
               -> PeerSender pk (st' :: ps) m a
               -> PeerSender pk (st :: ps) m a

data PeerReceiver (pk :: PeerKind) (st :: ps) (stdone :: ps) m where

  ReceiverEffect :: m (PeerReceiver pk st stdone m)
                 ->    PeerReceiver pk st stdone m

  ReceiverDone   :: PeerReceiver pk stdone stdone m

  ReceiverAwait  :: TheyHaveAgency pk st
                 -> (forall st'. Message st st' -> PeerReceiver pk st' stdone m)
                 -> PeerReceiver pk st stdone m


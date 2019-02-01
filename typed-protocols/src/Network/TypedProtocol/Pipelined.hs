{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}


module Network.TypedProtocol.Pipelined where

import Network.TypedProtocol.Core



data PeerSender ps (pk :: PeerKind) (st :: ps) m a where

  SenderEffect :: m (PeerSender ps pk st m a)
               ->    PeerSender ps pk st m a

  SenderDone   :: !(NobodyHasAgency st)
               -> a
               -> PeerSender ps pk st m a

  SenderYield  :: !(WeHaveAgency pk st)
               -> Message ps st st'
               -> PeerReceiver ps pk (st'  :: ps) (st'' :: ps) m
               -> PeerSender   ps pk (st'' :: ps) m a
               -> PeerSender   ps pk (st   :: ps) m a


data PeerReceiver ps (pk :: PeerKind) (st :: ps) (stdone :: ps) m where

  ReceiverEffect :: m (PeerReceiver ps pk st stdone m)
                 ->    PeerReceiver ps pk st stdone m

  ReceiverDone   :: PeerReceiver ps pk stdone stdone m

  ReceiverAwait  :: !(TheyHaveAgency pk st)
                 -> (forall st'. Message ps st st'
                              -> PeerReceiver ps pk st' stdone m)
                 -> PeerReceiver ps pk st stdone m


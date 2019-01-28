{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Pipelined where

import Network.TypedProtocol.Core



data PeerSender ps (pk :: PeerKind) (st :: ps) m a where

  SenderEffect :: m (PeerSender ps pk st m a)
               ->    PeerSender ps pk st m a

  SenderDone   :: NobodyHasAgency st
               -> a
               -> PeerSender ps pk st m a

  SenderYield  :: WeHaveAgency pk st
               -> Message ps st st'
               -> PeerReceiver ps pk (st'  :: ps) (st'' :: ps) m
               -> PeerSender   ps pk (st'' :: ps) m a
               -> PeerSender   ps pk (st   :: ps) m a


data PeerReceiver ps (pk :: PeerKind) (st :: ps) (stdone :: ps) m where

  ReceiverEffect :: m (PeerReceiver ps pk st stdone m)
                 ->    PeerReceiver ps pk st stdone m

  ReceiverDone   :: PeerReceiver ps pk stdone stdone m

  ReceiverAwait  :: TheyHaveAgency pk st
                 -> (forall st'. Message ps st st'
                              -> PeerReceiver ps pk st' stdone m)
                 -> PeerReceiver ps pk st stdone m

forgetPipelinining
  :: forall (pk :: PeerKind) (st :: ps) m a. Functor m
  => PeerSender pk st m a
  -> Peer pk st m a
forgetPipelinining (SenderEffect mp)   = Effect (forgetPipelinining <$> mp)
forgetPipelinining (SenderDone stok a) = Done stok a
forgetPipelinining (PipelinedYield stok msg recv next) = Yield stok msg (go recv next)
 where
  go :: PeerReceiver pk stCurrent stNext m
     -> PeerSender pk stNext m a
     -> Peer pk stCurrent m a
  go (ReceiverEffect mr) next' = Effect (flip go next' <$> mr)
  go ReceiverDone        next' = forgetPipelinining next'
  go (ReceiverAwait stok'' f) next' = Await stok'' (flip go next' . f)
forgetPipelinining (SenderYield stok msg next) = Yield stok msg (forgetPipelinining next)

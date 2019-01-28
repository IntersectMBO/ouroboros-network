{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Pipelined where

import Network.TypedProtocol.Core



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

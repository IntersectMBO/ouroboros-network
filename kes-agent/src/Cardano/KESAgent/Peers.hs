{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Cardano.KESAgent.Peers
where

import Cardano.KESAgent.Protocol
import Cardano.Crypto.KES.Class
import Network.TypedProtocol.Core

kesReceiver :: forall (k :: *)
             . KESAlgorithm k
            => (SignKeyKES k -> IO ())
            -> Peer (KESProtocol k) AsClient InitialState IO ()
kesReceiver receiveKey =
  Await (ServerAgency TokInitial) $ \VersionMessage -> go
  where
    go :: Peer (KESProtocol k) AsClient IdleState IO ()
    go = Await (ServerAgency TokIdle) $ \(KeyMessage sk) ->
            Effect $ do
              receiveKey sk
              return go

kesPusher :: forall (k :: *)
           . KESAlgorithm k
          => (IO (SignKeyKES k))
          -> (IO (SignKeyKES k))
          -> Peer (KESProtocol k) AsServer InitialState IO ()
kesPusher currentKey nextKey =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      sk <- currentKey
      return $ Yield (ServerAgency TokIdle) (KeyMessage sk) go
  where
    go :: Peer (KESProtocol k) AsServer IdleState IO ()
    go = Effect $ do
      sk <- nextKey
      return $ Yield (ServerAgency TokIdle) (KeyMessage sk) go

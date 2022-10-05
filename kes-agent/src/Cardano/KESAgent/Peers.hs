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
            -> Peer (KESProtocol k) AsClient IdleState IO ()
kesReceiver receiveKey =
  go
  where
    go = Await (ServerAgency TokIdle) $ \(Message sk) ->
            Effect $ do
              receiveKey sk
              return go

kesPusher :: forall (k :: *)
           . KESAlgorithm k
          => (IO (SignKeyKES k))
          -> (IO (SignKeyKES k))
          -> Peer (KESProtocol k) AsServer IdleState IO ()
kesPusher currentKey nextKey =
  Effect $ do
    sk <- currentKey
    return $ Yield (ServerAgency TokIdle) (Message sk) go
  where
    go = Effect $ do
      sk <- nextKey
      return $ Yield (ServerAgency TokIdle) (Message sk) go


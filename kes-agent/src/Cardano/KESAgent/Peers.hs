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
  Effect $ do
    return $
      Await (ServerAgency TokInitial) $ \VersionMessage ->
        Effect $ return go
  where
    go :: Peer (KESProtocol k) AsClient IdleState IO ()
    go = Await (ServerAgency TokIdle) $ \(KeyMessage sk) ->
            Effect $ do
              receiveKey sk
              return go

kesPusher :: forall (k :: *)
           . KESAlgorithm k
          => (IO (SignKeyKES k))
          -> (IO (Maybe (SignKeyKES k)))
          -> Peer (KESProtocol k) AsServer InitialState IO ()
kesPusher currentKey nextKey =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      sk <- currentKey
      return $ Yield (ServerAgency TokIdle) (KeyMessage sk) go
  where
    go :: Peer (KESProtocol k) AsServer IdleState IO ()
    go = Effect $ do
      skMay <- nextKey
      case skMay of
        Nothing -> return $
          Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd ()
        Just sk -> return $
          Yield (ServerAgency TokIdle) (KeyMessage sk) go

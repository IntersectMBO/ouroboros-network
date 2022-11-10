{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Cardano.KESAgent.Peers
where

import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert

import Cardano.Crypto.KES.Class

import Network.TypedProtocol.Core

kesReceiver :: forall (c :: *)
             . KESAlgorithm (KES c)
            => (SignKeyWithPeriodKES (KES c) -> OCert c -> IO ())
            -> Peer (KESProtocol c) AsClient InitialState IO ()
kesReceiver receiveKey =
  Effect $ do
    return $
      Await (ServerAgency TokInitial) $ \VersionMessage ->
        Effect $ return go
  where
    go :: Peer (KESProtocol c) AsClient IdleState IO ()
    go = Await (ServerAgency TokIdle) $ \(KeyMessage sk oc) ->
            Effect $ do
              receiveKey sk oc
              return go

kesPusher :: forall (c :: *)
           . KESAlgorithm (KES c)
          => (IO (SignKeyWithPeriodKES (KES c), OCert c))
          -> (IO (Maybe (SignKeyWithPeriodKES (KES c), OCert c)))
          -> Peer (KESProtocol c) AsServer InitialState IO ()
kesPusher currentKey nextKey =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      (sk, oc) <- currentKey
      return $ Yield (ServerAgency TokIdle) (KeyMessage sk oc) go
  where
    go :: Peer (KESProtocol c) AsServer IdleState IO ()
    go = Effect $ do
      skOcMay <- nextKey
      case skOcMay of
        Nothing -> return $
          Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd ()
        Just (sk, oc) -> return $
          Yield (ServerAgency TokIdle) (KeyMessage sk oc) go

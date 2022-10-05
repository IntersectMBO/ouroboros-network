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

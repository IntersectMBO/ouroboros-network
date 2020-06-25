{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.KeepAlive.Codec
  ( codecKeepAlive
  , byteLimitsKeepAlive
  , timeLimitsKeepAlive
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime (DiffTime)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeWord)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Protocol.KeepAlive.Type


codecKeepAlive
  :: forall m.
     MonadST m
  => Codec KeepAlive CBOR.DeserialiseFailure m ByteString
codecKeepAlive = mkCodecCborLazyBS encodeMsg decodeMsg
   where
     encodeMsg :: forall (pr :: PeerRole) st st'.
                  PeerHasAgency pr st
               -> Message KeepAlive st st'
               -> CBOR.Encoding
     encodeMsg (ClientAgency TokClient) MsgKeepAlive         = CBOR.encodeWord 0
     encodeMsg (ServerAgency TokServer) MsgKeepAliveResponse = CBOR.encodeWord 1
     encodeMsg (ClientAgency TokClient) MsgDone              = CBOR.encodeWord 2

     decodeMsg :: forall (pr :: PeerRole) s (st :: KeepAlive).
                  PeerHasAgency pr st
               -> CBOR.Decoder s (SomeMessage st)
     decodeMsg stok = do
       key <- CBOR.decodeWord
       case (stok, key) of
         (ClientAgency TokClient, 0) -> pure (SomeMessage MsgKeepAlive)
         (ServerAgency TokServer, 1) -> pure (SomeMessage MsgKeepAliveResponse)
         (ClientAgency TokClient, 2) -> pure (SomeMessage MsgDone)

         (ClientAgency TokClient, _) ->
           fail ("codecKeepAlive.StClient: unexpected key:" ++ show key)
         (ServerAgency TokServer, _) ->
           fail ("codecKeepAlive.StServer: unexpected key: " ++ show key)


byteLimitsKeepAlive :: ProtocolSizeLimits KeepAlive ByteString
byteLimitsKeepAlive = ProtocolSizeLimits {
      sizeLimitForState,
      dataSize = fromIntegral . BSL.length
    }
  where
    sizeLimitForState :: PeerHasAgency (pr :: PeerRole) (st :: KeepAlive)
                      -> Word
    sizeLimitForState (ClientAgency TokClient) = 1
    sizeLimitForState (ServerAgency TokServer) = 1


timeLimitsKeepAlive :: ProtocolTimeLimits KeepAlive
timeLimitsKeepAlive = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: PeerHasAgency (pr :: PeerRole) (st :: KeepAlive)
                      -> Maybe DiffTime
    timeLimitForState (ClientAgency TokClient) = waitForever
    timeLimitForState (ServerAgency TokServer) = shortWait

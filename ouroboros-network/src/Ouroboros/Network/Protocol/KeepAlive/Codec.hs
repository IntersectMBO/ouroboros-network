{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.KeepAlive.Codec
  ( codecKeepAlive
  , codecKeepAliveId
  , byteLimitsKeepAlive
  , timeLimitsKeepAlive
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime (DiffTime)

import           Data.ByteString.Lazy (ByteString)

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


byteLimitsKeepAlive :: (bytes -> Word) -> ProtocolSizeLimits KeepAlive bytes
byteLimitsKeepAlive = ProtocolSizeLimits sizeLimitForState
  where
    sizeLimitForState :: PeerHasAgency (pr :: PeerRole) (st :: KeepAlive)
                      -> Word
    sizeLimitForState (ClientAgency TokClient) = smallByteLimit
    sizeLimitForState (ServerAgency TokServer) = smallByteLimit


timeLimitsKeepAlive :: ProtocolTimeLimits KeepAlive
timeLimitsKeepAlive = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: PeerHasAgency (pr :: PeerRole) (st :: KeepAlive)
                      -> Maybe DiffTime
    timeLimitForState (ClientAgency TokClient) = waitForever
    timeLimitForState (ServerAgency TokServer) = shortWait


codecKeepAliveId
  :: forall m.
     ( Monad   m
     )
  => Codec KeepAlive CodecFailure m (AnyMessage KeepAlive)
codecKeepAliveId = Codec encodeMsg decodeMsg
   where
     encodeMsg :: forall (pr :: PeerRole) st st'.
                  PeerHasAgency pr st
               -> Message KeepAlive st st'
               -> AnyMessage KeepAlive
     encodeMsg _ = AnyMessage

     decodeMsg :: forall (pr :: PeerRole) (st :: KeepAlive).
                  PeerHasAgency pr st
               -> m (DecodeStep (AnyMessage KeepAlive)
                          CodecFailure m (SomeMessage st))
     decodeMsg stok = return $ DecodePartial $ \bytes -> return $
       case (stok, bytes) of
         (ClientAgency TokClient, Just (AnyMessage msg@(MsgKeepAlive)))
             -> DecodeDone (SomeMessage msg) Nothing
         (ServerAgency TokServer, Just (AnyMessage msg@(MsgKeepAliveResponse)))
             -> DecodeDone (SomeMessage msg) Nothing
         (ClientAgency TokClient, Just (AnyMessage msg@(MsgDone)))
             -> DecodeDone (SomeMessage msg) Nothing
         (_, _) -> DecodeFail (CodecFailure "codecKeepAliveId: no matching message")

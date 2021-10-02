{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.KeepAlive.Codec
  ( codecKeepAlive_v2
  , codecKeepAliveId
  , byteLimitsKeepAlive
  , timeLimitsKeepAlive
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime (DiffTime)

import           Data.ByteString.Lazy (ByteString)
import           Data.Singletons
import           Text.Printf

import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen,
                     decodeWord, decodeWord16)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen,
                     encodeWord, encodeWord16)
import qualified Codec.CBOR.Read as CBOR

import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.Core

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.KeepAlive.Type
import           Ouroboros.Network.Protocol.Limits


codecKeepAlive_v2
  :: forall m.
     MonadST m
  => Codec KeepAlive CBOR.DeserialiseFailure m ByteString
codecKeepAlive_v2 = mkCodecCborLazyBS encodeMsg decodeMsg
   where
     encodeMsg :: forall st st'.
                  Message KeepAlive st st'
               -> CBOR.Encoding
     encodeMsg (MsgKeepAlive (Cookie c)) =
          CBOR.encodeListLen 2
       <> CBOR.encodeWord 0
       <> CBOR.encodeWord16 c
     encodeMsg (MsgKeepAliveResponse (Cookie c)) =
          CBOR.encodeListLen 2
       <> CBOR.encodeWord 1
       <> CBOR.encodeWord16 c
     encodeMsg MsgDone =
          CBOR.encodeListLen 1
       <> CBOR.encodeWord 2

     decodeMsg :: forall s (st :: KeepAlive).
                  SingI (PeerHasAgency st)
               => CBOR.Decoder s (SomeMessage st)
     decodeMsg = do
       len <- CBOR.decodeListLen
       key <- CBOR.decodeWord
       case (sing :: SingPeerHasAgency st, len, key) of
         (SingClientHasAgency SingClient, 2, 0) -> do
             cookie <- CBOR.decodeWord16
             return (SomeMessage $ MsgKeepAlive $ Cookie cookie)
         (SingServerHasAgency SingServer, 2, 1) -> do
             cookie <- CBOR.decodeWord16
             return (SomeMessage $ MsgKeepAliveResponse $ Cookie cookie)
         (SingClientHasAgency SingClient, 1, 2) -> pure (SomeMessage MsgDone)

         (stok, _, _) ->
           fail (printf "codecKeepAlive (%s) unexpected key (%d, %d)" (show stok) key len)


byteLimitsKeepAlive :: (bytes -> Word) -> ProtocolSizeLimits KeepAlive bytes
byteLimitsKeepAlive = ProtocolSizeLimits sizeLimitForState
  where
    sizeLimitForState :: SingPeerHasAgency (st :: KeepAlive)
                      -> Word
    sizeLimitForState (SingClientHasAgency SingClient) = smallByteLimit
    sizeLimitForState (SingServerHasAgency SingServer) = smallByteLimit


timeLimitsKeepAlive :: ProtocolTimeLimits KeepAlive
timeLimitsKeepAlive = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: SingPeerHasAgency (st :: KeepAlive)
                      -> Maybe DiffTime
    timeLimitForState (SingClientHasAgency SingClient) = waitForever
    timeLimitForState (SingServerHasAgency SingServer) = Just 60 -- TODO: #2505 should be 10s.


codecKeepAliveId
  :: forall m.
     ( Monad   m
     )
  => Codec KeepAlive CodecFailure m (AnyMessage KeepAlive)
codecKeepAliveId = Codec encodeMsg decodeMsg
   where
     encodeMsg :: forall st st'.
                  SingI (PeerHasAgency st)
               => Message KeepAlive st st'
               -> AnyMessage KeepAlive
     encodeMsg = AnyMessage

     decodeMsg :: forall (st :: KeepAlive).
                  SingI (PeerHasAgency st)
               => m (DecodeStep (AnyMessage KeepAlive)
                                CodecFailure m (SomeMessage st))
     decodeMsg = return $ DecodePartial $ \bytes -> return $
        case (sing :: SingPeerHasAgency st, bytes) of
         (SingClientHasAgency SingClient, Just (AnyMessage msg@(MsgKeepAlive {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingServerHasAgency SingServer, Just (AnyMessage msg@(MsgKeepAliveResponse {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingClientHasAgency SingClient, Just (AnyMessage msg@(MsgDone)))
             -> DecodeDone (SomeMessage msg) Nothing
         (_, _) -> DecodeFail (CodecFailure "codecKeepAliveId: no matching message")

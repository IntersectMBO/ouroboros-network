{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
import           Control.Monad.Class.MonadTime.SI (DiffTime)

import           Data.ByteString.Lazy (ByteString)
import           Text.Printf

import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen,
                     decodeWord, decodeWord16)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen,
                     encodeWord, encodeWord16)
import qualified Codec.CBOR.Read as CBOR

import           Network.TypedProtocol.Codec.CBOR

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
                  ActiveState st
               => StateToken st
               -> CBOR.Decoder s (SomeMessage st)
     decodeMsg stok = do
       len <- CBOR.decodeListLen
       key <- CBOR.decodeWord
       case (stok, len, key) of
         (SingClient, 2, 0) -> do
             cookie <- CBOR.decodeWord16
             return (SomeMessage $ MsgKeepAlive $ Cookie cookie)
         (SingServer, 2, 1) -> do
             cookie <- CBOR.decodeWord16
             return (SomeMessage $ MsgKeepAliveResponse $ Cookie cookie)
         (SingClient, 1, 2) -> pure (SomeMessage MsgDone)

         (SingDone, _, _) -> notActiveState stok

         (_, _, _) ->
           fail (printf "codecKeepAlive (%s, %s) unexpected key (%d, %d)" (show (activeAgency :: ActiveAgency st)) (show stok) key len)


byteLimitsKeepAlive :: (bytes -> Word) -> ProtocolSizeLimits KeepAlive bytes
byteLimitsKeepAlive = ProtocolSizeLimits sizeLimitForState
  where
    sizeLimitForState :: ActiveState st
                      => StateToken (st :: KeepAlive)
                      -> Word
    sizeLimitForState SingClient = smallByteLimit
    sizeLimitForState SingServer = smallByteLimit
    sizeLimitForState a@SingDone = notActiveState a


timeLimitsKeepAlive :: ProtocolTimeLimits KeepAlive
timeLimitsKeepAlive = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: ActiveState st
                      => StateToken (st :: KeepAlive)
                      -> Maybe DiffTime
    timeLimitForState SingClient = Just 97
    timeLimitForState SingServer = Just 60 -- TODO: #2505 should be 10s.
    timeLimitForState a@SingDone = notActiveState a


codecKeepAliveId
  :: forall m.
     ( Monad   m
     )
  => Codec KeepAlive CodecFailure m (AnyMessage KeepAlive)
codecKeepAliveId = Codec encodeMsg decodeMsg
   where
     encodeMsg :: forall st st'.
                  StateTokenI st
               => ActiveState st
               => Message KeepAlive st st'
               -> AnyMessage KeepAlive
     encodeMsg = AnyMessage

     decodeMsg :: forall (st :: KeepAlive).
                  ActiveState st
               => StateToken st
               -> m (DecodeStep (AnyMessage KeepAlive)
                                CodecFailure m (SomeMessage st))
     decodeMsg stok = return $ DecodePartial $ \bytes -> return $
        case (stok, bytes) of
         (SingClient, Just (AnyMessage msg@(MsgKeepAlive {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingServer, Just (AnyMessage msg@(MsgKeepAliveResponse {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingClient, Just (AnyMessage msg@(MsgDone)))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingDone, _)
             -> notActiveState stok
         (_, _) -> DecodeFail (CodecFailure "codecKeepAliveId: no matching message")

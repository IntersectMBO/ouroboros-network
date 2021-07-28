{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Ouroboros.Network.Protocol.Trans.Hello.Codec
  ( codecHello
  , byteLimitsHello
  , timeLimitsHello
  , codecHelloId
  ) where


import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.Trans.Hello.Type
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Util.ShowProxy

import qualified Data.ByteString.Lazy as LBS

import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Text.Printf


byteLimitsHello
    :: forall ps (stIdle :: ps) bytes .
       ProtocolSizeLimits ps bytes
    -> ProtocolSizeLimits (Hello ps stIdle) bytes
byteLimitsHello ProtocolSizeLimits { sizeLimitForState, dataSize }
    = ProtocolSizeLimits {
          sizeLimitForState = sizeLimitForStateHello,
          dataSize
        }
  where 
    sizeLimitForStateHello :: forall (pr :: PeerRole) (st :: Hello ps stIdle).
                              PeerHasAgency pr st -> Word
    sizeLimitForStateHello (ClientAgency TokHello) =
      smallByteLimit
    sizeLimitForStateHello (ClientAgency (TokClientTalk tok)) =
      sizeLimitForState (ClientAgency tok)
    sizeLimitForStateHello (ServerAgency (TokServerTalk tok)) =
      sizeLimitForState (ServerAgency tok)


timeLimitsHello
    :: forall ps (stIdle :: ps).
       ProtocolTimeLimits ps
    -> ProtocolTimeLimits (Hello ps stIdle)
timeLimitsHello ProtocolTimeLimits { timeLimitForState } =
    ProtocolTimeLimits {
        timeLimitForState = timeLimitForStateHello
      }
  where
    timeLimitForStateHello :: forall (pr :: PeerRole) (st :: Hello ps stIdle).
                               PeerHasAgency pr st -> Maybe DiffTime
    timeLimitForStateHello (ClientAgency TokHello) = waitForever
    timeLimitForStateHello (ClientAgency (TokClientTalk tok)) =
      timeLimitForState (ClientAgency tok)
    timeLimitForStateHello (ServerAgency (TokServerTalk tok)) =
      timeLimitForState (ServerAgency tok)


-- | 'codecHello' provides a flat encoding of the original codec and 'MsgHello'.
-- 'MsgTalk' is invisible.  It assumes that the top level encoding of @ps@
-- protocol is a list of at least one element (the tag of a message).
--
codecHello
    :: forall ps (stIdle :: ps) m.
       ( MonadST m
       , ShowProxy ps
       , ShowProxy stIdle
       , forall (st :: ps). Show (ClientHasAgency st)
       , forall (st :: ps). Show (ServerHasAgency st)
       )
    => Word
    -- ^ tag for 'MsgHello'
    -> (forall (pr :: PeerRole) (st :: ps) (st' :: ps).
               PeerHasAgency pr st
            -> Message ps st st' -> CBOR.Encoding)
    -> (forall (pr :: PeerRole) (st :: ps) s.
               PeerHasAgency pr st
            -> Int
            -> Word
            -> CBOR.Decoder s (SomeMessage st))
    -- ^ continuation which receives agency, tag, and length. Last two are
    -- decoded values.
    -> Codec (Hello ps stIdle) CBOR.DeserialiseFailure m LBS.ByteString
codecHello helloTag encode decode = mkCodecCborLazyBS encodeHello decodeHello
  where
    encodeHello :: forall (pr  :: PeerRole)
                     (st  :: Hello ps stIdle)
                     (st' :: Hello ps stIdle).
                  PeerHasAgency pr st
               -> Message (Hello ps stIdle) st st'
               -> CBOR.Encoding
    encodeHello (ClientAgency TokHello) MsgHello =
      encodeListLen 1 <> encodeWord helloTag
    encodeHello (ClientAgency (TokClientTalk tok)) (MsgTalk msg) =
      encode (ClientAgency tok) msg
    encodeHello (ServerAgency (TokServerTalk tok)) (MsgTalk msg) =
      encode (ServerAgency tok) msg

    decodeHello :: forall (pr :: PeerRole) (st :: Hello ps stIdle) s.
                   PeerHasAgency pr st
                -> CBOR.Decoder s (SomeMessage st)
    decodeHello stok = do
      len <- decodeListLen
      key <- decodeWord
      case (key, len, stok) of
        -- 'MsgHello'
        (tag, 1, ClientAgency TokHello) | tag == helloTag ->
          return (SomeMessage MsgHello)

        -- inlined client messages
        (_,   _, ClientAgency (TokClientTalk tok)) -> do
          SomeMessage msg <- decode (ClientAgency tok) len key
          return (SomeMessage (MsgTalk msg))

        -- inlined server messages
        (_,   _, ServerAgency (TokServerTalk tok)) -> do
          SomeMessage msg <- decode (ServerAgency tok) len key
          return (SomeMessage (MsgTalk msg))

        --
        --  failure per protocol state
        --
        (_, _, _) ->
          fail (printf "codec (%s) at (%s) unexpected key (%d, %d)"
                 (showProxy (Proxy :: Proxy (Hello ps stIdle)))
                 (show stok)
                 key len)

codecHelloId
  :: forall ps (stIdle :: ps) m.
     ( Monad m
     , forall (st :: ps). Show (ClientHasAgency st)
     , forall (st :: ps). Show (ServerHasAgency st)
     )
  => Codec        ps         CodecFailure m (AnyMessage        ps)
  -> Codec (Hello ps stIdle) CodecFailure m (AnyMessage (Hello ps stIdle))
codecHelloId Codec { decode } =
    Codec { encode = encodeHello, decode = decodeHello }
  where
    encodeHello :: forall (pr  :: PeerRole)
                          (st  :: Hello ps stIdle)
                          (st' :: Hello ps stIdle).
                   PeerHasAgency pr st
                -> Message (Hello ps stIdle) st st'
                -> AnyMessage (Hello ps stIdle)
    encodeHello _ = AnyMessage


    decodeHello :: forall (pr :: PeerRole)
                          (st :: Hello ps stIdle).
                   PeerHasAgency pr st
                -> m (DecodeStep (AnyMessage (Hello ps stIdle))
                                 CodecFailure m (SomeMessage st))
    decodeHello stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
      (ClientAgency TokHello,              Just (AnyMessage msg@MsgHello))      ->
        return $ DecodeDone (SomeMessage msg) Nothing

      (ClientAgency (TokClientTalk tok), Just (AnyMessage (MsgTalk msg))) -> do
        decoder <- decode (ClientAgency tok)
        res <- runDecoder [AnyMessage msg] decoder
        return $
          case res of
            Left  failure            -> DecodeFail failure
            Right (SomeMessage msg') -> DecodeDone (SomeMessage (MsgTalk msg')) Nothing

      (ServerAgency (TokServerTalk tok), Just (AnyMessage (MsgTalk msg))) -> do
        decoder <- decode (ServerAgency tok)
        res <- runDecoder [AnyMessage msg] decoder
        return $
          case res of
            Left  failure            -> DecodeFail failure
            Right (SomeMessage msg') -> DecodeDone (SomeMessage (MsgTalk msg')) Nothing

      (ClientAgency tok, _) -> return (DecodeFail (CodecFailure ("codecTxSubmissionId2: no matching message at " ++ show tok)))
      (ServerAgency tok, _) -> return (DecodeFail (CodecFailure ("codecTxSubmissionId2: no matching message at " ++ show tok)))

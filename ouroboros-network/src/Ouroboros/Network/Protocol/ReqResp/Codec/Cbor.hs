{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.ReqResp.Codec.Cbor where

import Prelude
import Control.Monad.ST

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import Protocol.Codec

import Ouroboros.Network.Protocol.ReqResp.Type
import Ouroboros.Network.Protocol.Codec.Cbor (cborDecoder)

codecReqResp
  :: forall request response s.
     ( Serialise request, Serialise response )
  => Codec (ST s) Text Encoding ByteString (ReqRespMessage request response) StIdle
codecReqResp = codecIdle

codecIdle
  :: forall request response s.
     ( Serialise request, Serialise response )
  => Codec (ST s) Text Encoding ByteString (ReqRespMessage request response) StIdle
codecIdle = Codec
  { encode = cborEncodeIdle
  , decode = cborDecoder cborDecodeIdle
  }
 where
  cborEncodeIdle 
    :: Encoder (ReqRespMessage request response) StIdle (Encoded Encoding (Codec (ST s) Text Encoding ByteString (ReqRespMessage request response)))
  cborEncodeIdle = Encoder $ \tr -> case tr of
    MsgRequest request -> Encoded (CBOR.encode request) codecBusy

  cborDecodeIdle
    :: CBOR.Decoder s (Decoded (ReqRespMessage request response) StIdle (Codec (ST s) Text Encoding ByteString (ReqRespMessage request response)))
  cborDecodeIdle = do
    request <- CBOR.decode
    pure $ Decoded (MsgRequest request) codecBusy

codecBusy
  :: forall request response s.
     ( Serialise request, Serialise response )
  => Codec (ST s) Text Encoding ByteString (ReqRespMessage request response) StBusy
codecBusy = Codec
  { encode = cborEncodeBusy
  , decode = cborDecoder cborDecodeBusy
  }
 where
  cborEncodeBusy
    :: Encoder (ReqRespMessage request response) StBusy (Encoded Encoding (Codec (ST s) Text Encoding ByteString (ReqRespMessage request response)))
  cborEncodeBusy = Encoder $ \tr -> case tr of
    MsgResponse response-> Encoded (CBOR.encode response) codecDone

  cborDecodeBusy
    :: CBOR.Decoder s (Decoded (ReqRespMessage request response) StBusy (Codec (ST s) Text Encoding ByteString (ReqRespMessage request response)))
  cborDecodeBusy = do
      response <- CBOR.decode
      pure $ Decoded (MsgResponse response) codecDone

codecDone :: Codec (ST s) Text Encoding ByteString (ReqRespMessage request response) StDone
codecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Fold $ pure $ Complete [] $ pure $ Left $ T.pack "ReqRespProtocol: no transitions from done"
  }

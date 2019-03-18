{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Network.Mux.Control (
      ControlMsg (..)
    , encodeCtrlMsg
    , decodeCtrlMsg
    ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise.Class
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (catMaybes, mapMaybe)
import           GHC.Stack

import           Ouroboros.Network.Mux.Types

data ControlMsg = MsgInitReq [Version]
                | MsgInitRsp Version
                | MsgInitFail String
                deriving (Eq, Show)

encodeCtrlMsg :: ControlMsg -> CBOR.Encoding
encodeCtrlMsg (MsgInitReq versions) = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodeVersions versions
encodeCtrlMsg (MsgInitRsp version)  = CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> encode version
encodeCtrlMsg (MsgInitFail msg)     = CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encode msg

encodeVersions :: [Version] -> CBOR.Encoding
encodeVersions versions =
    CBOR.encodeListLen (fromIntegral $ length versions)
    <> foldr (\v b -> encode v <> b) mempty versions

decodeCtrlMsg :: HasCallStack => CBOR.Decoder s ControlMsg
decodeCtrlMsg = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case key of
         0 -> MsgInitReq . mapMaybe decodeVersionTerm <$> decodeVersions
         1 -> MsgInitRsp <$> decode
         2 -> MsgInitFail <$> decode
         a -> throw $ MuxError MuxControlUnknownMessage
                      ("unknown control message type " ++ show a)
                      callStack

  where
    decodeVersions :: CBOR.Decoder s [CBOR.Term]
    decodeVersions = do
      n <- CBOR.decodeListLen
      replicateM n CBOR.decodeTerm

    decodeVersionTerm :: CBOR.Term -> Maybe Version
    decodeVersionTerm t =
      let bs = toLazyByteString (CBOR.encodeTerm t)
      in case CBOR.deserialiseFromBytes (decode :: CBOR.Decoder s Version) bs of
        Left _       -> Nothing
        Right (_, v) -> Just v

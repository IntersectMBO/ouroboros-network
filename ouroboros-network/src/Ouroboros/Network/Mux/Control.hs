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
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           GHC.Stack

import           Ouroboros.Network.Mux.Types

data ControlMsg = MsgInitReq [Version]
                | MsgInitRsp Version
                | MsgInitFail String
                deriving (Eq, Show)

encodeCtrlMsg :: ControlMsg -> CBOR.Encoding
encodeCtrlMsg (MsgInitReq versions) = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodeVersionMap versions
encodeCtrlMsg (MsgInitRsp version)  = CBOR.encodeListLen 3 <> CBOR.encodeWord 1 <> encode version

encodeVersions :: [Version] -> CBOR.Encoding
encodeVersions versions =
    CBOR.encodeListLen (fromIntegral $ length versions)
    <> foldr (\v b -> encode v <> b) mempty versions

decodeCtrlMsg :: HasCallStack => CBOR.Decoder s ControlMsg
decodeCtrlMsg = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case key of
         0 -> MsgInitReq <$> decodeVersions
         1 -> MsgInitRsp <$> decode
         2 -> MsgInitFail <$> decode
         a -> throw $ MuxError MuxControlUnknownMessage
                      ("unknown control message type " ++ show a)
                      callStack

  where
    decodeVersions :: CBOR.Decoder s [Version]
    decodeVersions = do
        n <- CBOR.decodeListLen
        fmap catMaybes (replicateM n tryDecodeVersion)

    tryDecodeVersion :: CBOR.Decoder s (Maybe Version)
    tryDecodeVersion = do
      v <- CBOR.decodeWord
      n <- decode
      case v of
        0 -> return $ Just (Version0 n)
        1 -> return $ Just (Version1 n)
        _ -> return Nothing

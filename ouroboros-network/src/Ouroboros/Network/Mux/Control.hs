{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Network.Mux.Control where

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

data ControlMsg = MsgInitReq (M.Map VersionNumber Version)
                | MsgInitRsp Version
                | MsgInitFail String
                deriving Show

encodeCtrlMsg :: ControlMsg -> CBOR.Encoding
encodeCtrlMsg (MsgInitReq versions) = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodeVersionMap versions
encodeCtrlMsg (MsgInitRsp version)  = CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> encode version
encodeCtrlMsg (MsgInitFail msg)     = CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encode msg

encodeVersionMap :: M.Map VersionNumber Version -> CBOR.Encoding
encodeVersionMap versions =
    CBOR.encodeMapLen (fromIntegral $ M.size versions)
    <> foldr (\v b -> encode v <> b) mempty (M.elems versions)

decodeCtrlMsg :: HasCallStack => CBOR.Decoder s ControlMsg
decodeCtrlMsg = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case key of
         0 -> MsgInitReq . M.mapMaybeWithKey decodeVersionTerm <$> decodeVersionMap
         1 -> MsgInitRsp <$> decode
         2 -> MsgInitFail <$> decode
         a -> throw $ MuxError MuxControlUnknownMessage
                      ("unknown control message type " ++ show a)
                      callStack

  where
    decodeVersionMap :: CBOR.Decoder s (M.Map VersionNumber CBOR.Term)
    decodeVersionMap = do
        n <- CBOR.decodeMapLen
        let decodeEntry = do
              !k <- CBOR.decodeWord
              !v <- CBOR.decodeTerm
              case k of
                   0 -> return $ Just (VersionNumber0, v)
                   1 -> return $ Just (VersionNumber1, v)
                   _ -> return Nothing
        fmap (M.fromList . catMaybes) (replicateM n decodeEntry)

    decodeVersionTerm VersionNumber0 t =
        let bs = toLazyByteString (CBOR.encodeTerm t)
            v_e  = CBOR.deserialiseFromBytes (Version0 . NetworkMagic <$> decode) bs in
        case v_e of
             Left  e      -> throw e
             Right (_, v) -> Just v

    decodeVersionTerm VersionNumber1 t =
        let bs = toLazyByteString (CBOR.encodeTerm t)
            v_e  = CBOR.deserialiseFromBytes (Version1 . NetworkMagic <$> decode) bs in
        case v_e of
             Left  e      -> throw e
             Right (_, v) -> Just v



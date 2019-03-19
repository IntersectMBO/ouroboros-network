{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Network.Mux.Control (
      ControlMsg (..)
    ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise.Class
import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Data.Maybe (catMaybes)

import           Ouroboros.Network.Mux.Types

data ControlMsg = MsgInitReq [Version]
                | MsgInitRsp Version
                | MsgInitFail String
                deriving (Eq, Show)

encodeVersions :: [Version] -> CBOR.Encoding
encodeVersions versions =
    CBOR.encodeMapLen (fromIntegral $ length versions)
    <> foldr (\v b -> encode v <> b) mempty versions

instance Serialise ControlMsg where
  encode (MsgInitReq versions) = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodeVersions versions
  encode (MsgInitRsp version)  = CBOR.encodeListLen 3 <> CBOR.encodeWord 1 <> encode version
  encode (MsgInitFail msg)     = CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encode msg

  decode = do
      _ <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case key of
           0 -> MsgInitReq <$> decodeVersions
           1 -> MsgInitRsp <$> decode
           2 -> MsgInitFail <$> decode
           a -> Fail.fail ("unknown control message type " ++ show a)

    where
      -- The only decoding done here is what is specified by a CBOR Map, that is
      -- a key and an unknown Term. Version specific encoding is handled by the serialise instance
      -- of 'Version'.
      decodeVersions :: CBOR.Decoder s [Version]
      decodeVersions = do
        n <- CBOR.decodeMapLen
        let decodeEntry = do
                !vn <- CBOR.decodeWord
                !t <- CBOR.decodeTerm
                let bs = toLazyByteString (encode vn <> CBOR.encodeTerm t)
                case CBOR.deserialiseFromBytes (decode :: CBOR.Decoder s Version) bs of
                     Left _       -> return Nothing
                     Right (_, v) -> return $ Just v

        catMaybes <$> replicateM n decodeEntry


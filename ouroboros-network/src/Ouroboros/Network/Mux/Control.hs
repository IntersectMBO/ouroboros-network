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
import           Data.Maybe (mapMaybe)

import           Ouroboros.Network.Mux.Types

data ControlMsg = MsgInitReq [Version]
                | MsgInitRsp Version
                | MsgInitFail String
                deriving (Eq, Show)

encodeVersions :: [Version] -> CBOR.Encoding
encodeVersions versions =
    CBOR.encodeListLen (fromIntegral $ length versions)
    <> foldr (\v b -> encode v <> b) mempty versions

instance Serialise ControlMsg where
  encode (MsgInitReq versions) = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodeVersions versions
  encode (MsgInitRsp version)  = CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> encode version
  encode (MsgInitFail msg)     = CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encode msg

  decode = do
      _ <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case key of
           0 -> MsgInitReq . mapMaybe decodeVersionTerm <$> decodeVersions
           1 -> MsgInitRsp <$> decode
           2 -> MsgInitFail <$> decode
           a -> Fail.fail ("unknown control message type " ++ show a)

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

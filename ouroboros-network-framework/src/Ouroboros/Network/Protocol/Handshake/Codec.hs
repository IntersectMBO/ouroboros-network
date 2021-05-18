{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.Protocol.Handshake.Codec
  ( codecHandshake

  , byteLimitsHandshake
  , timeLimitsHandshake
  , noTimeLimitsHandshake

  , encodeRefuseReason
  , decodeRefuseReason

    -- ** Version data codec
  , VersionDataCodec (..)
  , cborTermVersionDataCodec
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime
import           Control.Monad (unless, replicateM)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Either (partitionEithers)
import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Text.Printf

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Term     as CBOR

import           Ouroboros.Network.Codec
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Driver.Limits

import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Protocol.Handshake.Type

-- | Codec for version data ('vData' in code) exchanged by the handshake
-- protocol.
--
-- Note: 'extra' type param is instantiated to 'DictVersion'; 'agreedOptions'
-- is instatiated to 'NodeToNodeVersionData' in "Ouroboros.Network.NodeToNode"
-- or to '()' in "Ouroboros.Network.NodeToClient".
--
data VersionDataCodec bytes vNumber vData = VersionDataCodec {
    encodeData :: vNumber -> vData -> bytes,
    -- ^ encoder of 'vData' which has access to 'extra vData' which can bring
    -- extra instances into the scope (by means of pattern matching on a GADT).
    decodeData :: vNumber -> bytes -> Either Text vData
    -- ^ decoder of 'vData'.
  }

-- TODO: remove this from top level API, this is the only way we encode or
-- decode version data.
cborTermVersionDataCodec :: (vNumber -> CodecCBORTerm Text vData)
                         -> VersionDataCodec CBOR.Term vNumber vData
cborTermVersionDataCodec codec = VersionDataCodec {
      encodeData = encodeTerm . codec,
      decodeData = decodeTerm . codec
    }

-- |
-- We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4.  This sets upper limit of 5760 bytes on each message of handshake
-- protocol.
--
maxTransmissionUnit :: Word
maxTransmissionUnit = 4 * 1440

-- | Byte limits
byteLimitsHandshake :: forall vNumber. ProtocolSizeLimits (Handshake vNumber CBOR.Term) ByteString
byteLimitsHandshake = ProtocolSizeLimits stateToLimit (fromIntegral . BL.length)
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: Handshake vNumber CBOR.Term).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokPropose) = maxTransmissionUnit
    stateToLimit (ServerAgency TokConfirm) = maxTransmissionUnit

-- | Time limits.
--
timeLimitsHandshake :: forall vNumber. ProtocolTimeLimits (Handshake vNumber CBOR.Term)
timeLimitsHandshake = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: Handshake vNumber CBOR.Term).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokPropose) = shortWait
    stateToLimit (ServerAgency TokConfirm) = shortWait


noTimeLimitsHandshake :: forall vNumber. ProtocolTimeLimits (Handshake vNumber CBOR.Term)
noTimeLimitsHandshake = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st  :: Handshake vNumber CBOR.Term).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokPropose) = Nothing
    stateToLimit (ServerAgency TokConfirm) = Nothing


-- |
-- @'Handshake'@ codec.  The @'MsgProposeVersions'@ encodes proposed map in
-- ascending order and it expects to receive them in this order.  This allows
-- to construct the map in linear time.  There is also another limiting factor
-- to the number of versions on can present: the whole message must fit into
-- a single TCP segment.
--
codecHandshake
  :: forall vNumber m failure.
     ( MonadST m
     , Ord vNumber
     , Show failure
     )
  => CodecCBORTerm (failure, Maybe Int) vNumber
  -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure m ByteString
codecHandshake versionNumberCodec = mkCodecCborLazyBS encodeMsg decodeMsg
    where
      encodeMsg
        :: forall (pr :: PeerRole) st st'.
           PeerHasAgency pr st
        -> Message (Handshake vNumber CBOR.Term) st st'
        -> CBOR.Encoding

      encodeMsg (ClientAgency TokPropose) (MsgProposeVersions vs) =
        let vs' = Map.toAscList vs
        in
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encodeMapLen (fromIntegral $ length vs')
        <> mconcat [    CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
                     <> CBOR.encodeTerm vParams
                   | (vNumber, vParams) <- vs'
                   ]

      encodeMsg (ServerAgency TokConfirm) (MsgAcceptVersion vNumber vParams) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 1
        <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
        <> CBOR.encodeTerm vParams

      encodeMsg (ServerAgency TokConfirm) (MsgRefuse vReason) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 2
        <> encodeRefuseReason versionNumberCodec vReason

      -- decode a map checking the assumption that
      --  * keys are different
      --  * keys are encoded in ascending order
      -- fail when one of these assumptions is not met
      decodeMap :: Int
                -> Maybe vNumber
                -> [(vNumber, CBOR.Term)]
                -> CBOR.Decoder s (Map vNumber CBOR.Term)
      decodeMap 0  _     !vs = return $ Map.fromDistinctAscList $ reverse vs
      decodeMap !l !prev !vs = do
        vNumberTerm <- CBOR.decodeTerm
        vParams <- CBOR.decodeTerm
        case decodeTerm versionNumberCodec vNumberTerm of
          -- error when decoding un-recognized version; skip the version
          -- TODO: include error in the dictionary
          Left _        -> decodeMap (pred l) prev vs

          Right vNumber -> do
            let next = Just vNumber
            unless (next > prev)
              $ fail "codecHandshake.Propose: unordered version"
            decodeMap (pred l) next ((vNumber, vParams) : vs)

      decodeMsg :: forall (pr :: PeerRole) s (st :: Handshake vNumber CBOR.Term).
                   PeerHasAgency pr st
                -> CBOR.Decoder s (SomeMessage st)
      decodeMsg stok = do
        len <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        case (stok, key, len) of
          (ClientAgency TokPropose, 0, 2) -> do
            l  <- CBOR.decodeMapLen
            vMap <- decodeMap l Nothing []
            pure $ SomeMessage $ MsgProposeVersions vMap
          (ServerAgency TokConfirm, 1, 3) -> do
            v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
            case v of
              -- at this stage we can throw exception when decoding
              -- version number: 'MsgAcceptVersion' must send us back
              -- version which we know how to decode
              Left e -> fail ("codecHandshake.MsgAcceptVersion: not recognized version: " ++ show e)
              Right vNumber ->
                SomeMessage . MsgAcceptVersion vNumber <$> CBOR.decodeTerm
          (ServerAgency TokConfirm, 2, 2) ->
            SomeMessage . MsgRefuse <$> decodeRefuseReason versionNumberCodec

          (ClientAgency TokPropose, _, _) ->
            fail $ printf "codecHandshake (%s) unexpected key (%d, %d)" (show stok) key len
          (ServerAgency TokConfirm, _, _) ->
            fail $ printf "codecHandshake (%s) unexpected key (%d, %d)" (show stok) key len


encodeRefuseReason :: CodecCBORTerm fail vNumber
                   -> RefuseReason vNumber
                   -> CBOR.Encoding
encodeRefuseReason versionNumberCodec (VersionMismatch vs _) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> CBOR.encodeListLen (fromIntegral $ length vs)
      <> foldMap (CBOR.encodeTerm . encodeTerm versionNumberCodec) vs
encodeRefuseReason versionNumberCodec (HandshakeDecodeError vNumber vError) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 1
      <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
      <> CBOR.encodeString vError
encodeRefuseReason versionNumberCodec (Refused vNumber vReason) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 2
      <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
      <> CBOR.encodeString vReason


decodeRefuseReason :: Show failure
                   => CodecCBORTerm (failure, Maybe Int) vNumber
                   -> CBOR.Decoder s (RefuseReason vNumber)
decodeRefuseReason versionNumberCodec = do
    _ <- CBOR.decodeListLen
    tag <- CBOR.decodeWord
    case tag of
      0 -> do
        len <- CBOR.decodeListLen
        rs <- replicateM len
                (decodeTerm versionNumberCodec <$> CBOR.decodeTerm)
        case partitionEithers rs of
          (errs, vNumbers) -> 
            pure $ VersionMismatch vNumbers (mapMaybe snd errs)
      1 -> do
        v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
        case v of
          Left e        -> fail $ "decode HandshakeDecodeError: unknow version: " ++ show e
          Right vNumber -> HandshakeDecodeError vNumber <$> CBOR.decodeString
      2 -> do
        v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
        case v of
          Left e        -> fail $ "decode Refused: unknonwn version: " ++ show e
          Right vNumber -> Refused vNumber <$> CBOR.decodeString
      _ -> fail $ "decode RefuseReason: unknown tag " ++ show tag

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.Protocol.VersionNegotiation.Codec
  ( codecVersionNegotiation
  , SerialiseTerm (..)
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad (unless)
import           Data.Text (Text)
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Term     as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise     as CBOR

import           Network.TypedProtocol.Codec hiding (encode, decode)
import           Ouroboros.Network.Codec (mkCodecCborLazyBS)

import           Ouroboros.Network.Protocol.VersionNegotiation.Type


-- |
-- Decoding proposed version is done in two stages.  First decode
-- a @'CBOR.Term'@, this is done by @'codecVersionNegotiation'@, the second
-- phase is handled by interface defined in this type class and it is used by
-- @'versionNegotiationClientPeer'@ and @'versionNeogitationServerPeer'@.
--
-- todo: find a better place for this class
class SerialiseTerm a where
  encodeTerm :: a -> CBOR.Term
  decodeTerm :: CBOR.Term -> Either Text a

-- |
-- @'VersionNegotiationProtocol'@ codec.  The @'MsgProposeVersions'@ encodes
-- proposed map in ascending order and it expects to receive them in this order.
-- This allows to construct the map in linear time.  There is also another
-- limiting factor to the number of versions on can present: the whole message
-- must fit into a single TCP segment.
--
codecVersionNegotiation
  :: forall vNumber m.
     ( Monad m
     , MonadST m
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Show vNumber
     )
  => Codec (VersionNegotiationProtocol vNumber CBOR.Term) CBOR.DeserialiseFailure m ByteString
codecVersionNegotiation = mkCodecCborLazyBS encode decode
    where
      encode :: forall (pr :: PeerRole) st st'.
                PeerHasAgency pr st
             -> Message (VersionNegotiationProtocol vNumber CBOR.Term) st st'
             -> CBOR.Encoding

      encode (ClientAgency TokPropose) (MsgProposeVersions vs) =
        let vs' = Map.toAscList vs
        in
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encodeMapLen (fromIntegral $ length vs')
        <> mconcat [    CBOR.encode vNumber
                     <> CBOR.encodeTerm vParams
                   | (vNumber, vParams) <- vs'
                   ]

      encode (ServerAgency TokConfirm) (MsgAcceptVersion vNumber vParams) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 1
        <> CBOR.encode vNumber
        <> CBOR.encodeTerm vParams

      encode (ServerAgency TokConfirm) (MsgRefuse vReason) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 2
        <> CBOR.encode vReason

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
        vNumber <- CBOR.decode
        let next = Just vNumber
        unless (next > prev)
          $ fail "codecVersionNegotation.Propose: unordered version"
        vParams <- CBOR.decodeTerm
        decodeMap (pred l) next ((vNumber,vParams) : vs)

      decode :: forall (pr :: PeerRole) s (st :: VersionNegotiationProtocol vNumber CBOR.Term).
                PeerHasAgency pr st
             -> CBOR.Decoder s (SomeMessage st)
      decode stok = do
        _ <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        case (stok, key) of
          (ClientAgency TokPropose, 0) -> do
            l  <- CBOR.decodeMapLen
            vMap <- decodeMap l Nothing []
            pure $ SomeMessage $ MsgProposeVersions vMap
          (ServerAgency TokConfirm, 1) ->
            SomeMessage <$> (MsgAcceptVersion <$> CBOR.decode <*> CBOR.decodeTerm)
          (ServerAgency TokConfirm, 2) -> SomeMessage . MsgRefuse <$> CBOR.decode

          (ClientAgency TokPropose, _) -> fail "codecVersionNegotation.Propose: unexpected key"
          (ServerAgency TokConfirm, _) -> fail "codecVersionNegotation.Confirm: unexpected key"

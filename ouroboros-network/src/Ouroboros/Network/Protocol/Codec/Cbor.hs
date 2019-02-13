{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Protocol.Codec.Cbor where

import           Control.Monad.ST
import           Control.Monad.Class.MonadST (MonadST (..))

import qualified Codec.CBOR.Decoding as CBOR hiding (DecodeAction(Fail, Done))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import qualified Data.ByteString.Lazy          as LBS

-- TODO remove `Protocol.Codec` and qualilfication
import qualified Network.TypedProtocol.Core  as TP
import qualified Network.TypedProtocol.Codec as TP
import           Protocol.Codec

cborCodec
  :: forall ps m. MonadST m
  => (forall (pk :: TP.PeerRole) (st :: ps) (st' :: ps). TP.PeerHasAgency pk st -> TP.Message ps st st' -> CBOR.Encoding)
  -> (forall (pk :: TP.PeerRole) (st :: ps) s.           TP.PeerHasAgency pk st -> CBOR.Decoder s (TP.SomeMessage st))
  -> TP.Codec ps CBOR.DeserialiseFailure m ByteString
cborCodec cborEncode cborDecode =
    TP.Codec {
      TP.encode = \stok msg -> convertCborEncoder  (cborEncode stok) msg,
      TP.decode = \stok     -> convertCborDecoder' (cborDecode stok)
    }

convertCborEncoder :: (a -> CBOR.Encoding) -> a -> ByteString
convertCborEncoder cborEncode =
    CBOR.toStrictByteString
  . cborEncode

convertCborDecoder'
  :: MonadST m
  => (forall s. CBOR.Decoder s a)
  -> m (TP.DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder' cborDecode =
    withLiftST (convertCborDecoder cborDecode)

convertCborDecoder
  :: forall s m a. Functor m
  => (CBOR.Decoder s a)
  -> (forall b. ST s b -> m b)
  -> m (TP.DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder cborDecode liftST =
    go <$> liftST (CBOR.deserialiseIncremental cborDecode)
  where
    go (CBOR.Done  trailing _ x)
      | BS.null trailing       = TP.DecodeDone x Nothing
      | otherwise              = TP.DecodeDone x (Just trailing)
    go (CBOR.Fail _ _ failure) = TP.DecodeFail failure
    go (CBOR.Partial k)        = TP.DecodePartial (fmap go . liftST . k)

-- | Convert a CBOR decoder type into a Protocol.Codec.Decoder.
cborDecoder
  :: forall s fail tr state .
     (String -> fail)
  -> CBOR.Decoder s (Decoded tr state (Codec (ST s) fail CBOR.Encoding ByteString tr))
  -> Decoder fail ByteString (ST s) (Decoded tr state (Codec (ST s) fail CBOR.Encoding ByteString tr))
cborDecoder packError decoder = Fold (idecode [] =<< CBOR.deserialiseIncremental decoder)
  where
  idecode
    :: [ByteString]
    -> CBOR.IDecode s (Decoded tr state (Codec (ST s) fail CBOR.Encoding ByteString tr))
    -> ST s (Choice [ByteString] (ST s) (Either fail (Decoded tr state (Codec (ST s) fail CBOR.Encoding ByteString tr))))
  idecode inputs term = case term of
    CBOR.Fail bs _ (CBOR.DeserialiseFailure _ str) ->
      pure $ Complete (bs : inputs) $ pure $ Left $ packError str
    CBOR.Done bs _ it ->
      pure $ Complete (bs : inputs) $ pure $ Right $ it
    -- At a partial, feed all of the inputs we have in scope before presenting
    -- a 'Response'.
    CBOR.Partial k ->
      let feedInputs
            :: [ByteString]
            -> (Maybe ByteString -> ST s (CBOR.IDecode s (Decoded tr state (Codec (ST s) fail CBOR.Encoding ByteString tr))))
            -> ST s (Choice [ByteString] (ST s) (Either fail (Decoded tr state (Codec (ST s) fail CBOR.Encoding ByteString tr))))
          feedInputs inputs k = case inputs of
            (i : is) -> k (Just i) >>= idecode is
            [] -> pure $ Partial $ Response
              -- Known inputs are exhausted. If the next step is end of stream,
              -- there could be errors, because the 'IDecode' type is too big.
              -- If there's more input, recurse on 'idecode' with the input
              -- chunks.
              { end  = k Nothing >>= \final -> case final of
                  CBOR.Fail _bs _ (CBOR.DeserialiseFailure _ str) ->
                    -- '_bs' ought to be null, but we won't check.
                    pure $ Left $ packError str
                  CBOR.Done _bs _ it ->
                    -- '_bs' ought to be null, but we won't check.
                    pure $ Right it
                  -- The `IDecode` representation isn't as sharp as the
                  -- `Decoder` representation: it can give a partial decode even after
                  -- the end of stream is given, and so the user is in limbo with
                  -- neither a success, nor a failure...
                  CBOR.Partial _ ->
                    pure $ Left $ packError "CBOR decoder is still partial after end of input stream"
              , more = \bss -> Fold $ idecode bss term
              }
      in  feedInputs inputs k

convertCborEncoder_ :: (a -> CBOR.Encoding) -> a -> [ByteString]
convertCborEncoder_ cborEncode =
    LBS.toChunks
  . toLazyByteString
  . CBOR.toBuilder
  . cborEncode

{-# NOINLINE toLazyByteString #-}
toLazyByteString :: BS.Builder -> LBS.ByteString
toLazyByteString = BS.toLazyByteStringWith strategy LBS.empty
  where
    strategy = BS.untrimmedStrategy 800 LBS.smallChunkSize

-- | Convert @'Codec'@ with @'CBOR.Encoding'@ as its concrete representation to
-- a codec with a list of strict @'ByteString'@ as its concrete representation
-- (via lazy bytestring).
--
convertCborCodec
  :: Functor m
  => Codec m fail CBOR.Encoding ByteString tr from
  -> Codec m fail [ByteString]  ByteString tr from
convertCborCodec = mapCodec (convertCborEncoder_ id)

-- | Convert @'Codec'@ with @'CBOR.Encoding'@ as its concrete representation to
-- a codec with strict @'ByteString'@ as its concrete representation.
--
convertCborCodec'
  :: Functor m
  => Codec m fail CBOR.Encoding ByteString tr from
  -> Codec m fail ByteString    ByteString tr from
convertCborCodec' = mapCodec CBOR.toStrictByteString

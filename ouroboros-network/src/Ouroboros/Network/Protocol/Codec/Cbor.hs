{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Protocol.Codec.Cbor where

import           Control.Monad.ST

import           Data.ByteString (ByteString)
import           Data.Text (Text, pack)

import qualified Codec.CBOR.Decoding as CBOR hiding (DecodeAction(Fail, Done))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR

import           Protocol.Codec

-- | Convert a CBOR decoder type into a Protocol.Codec.Decoder.
cborDecoder
  :: forall s tr state .
     CBOR.Decoder s (Decoded tr state (Codec (ST s) Text CBOR.Encoding ByteString tr))
  -> Decoder Text ByteString (ST s) (Decoded tr state (Codec (ST s) Text CBOR.Encoding ByteString tr))
cborDecoder decoder = Fold (idecode [] =<< CBOR.deserialiseIncremental decoder)
  where
  idecode
    :: [ByteString]
    -> CBOR.IDecode s (Decoded tr state (Codec (ST s) Text CBOR.Encoding ByteString tr))
    -> ST s (Choice [ByteString] (ST s) (Either Text (Decoded tr state (Codec (ST s) Text CBOR.Encoding ByteString tr))))
  idecode inputs term = case term of
    CBOR.Fail bs _ (CBOR.DeserialiseFailure _ str) ->
      pure $ Complete (bs : inputs) $ pure $ Left $ pack str
    CBOR.Done bs _ it ->
      pure $ Complete (bs : inputs) $ pure $ Right $ it
    -- At a partial, feed all of the inputs we have in scope before presenting
    -- a 'Response'.
    CBOR.Partial k ->
      let feedInputs
            :: [ByteString]
            -> (Maybe ByteString -> ST s (CBOR.IDecode s (Decoded tr state (Codec (ST s) Text CBOR.Encoding ByteString tr))))
            -> ST s (Choice [ByteString] (ST s) (Either Text (Decoded tr state (Codec (ST s) Text CBOR.Encoding ByteString tr))))
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
                    pure $ Left $ pack str
                  CBOR.Done _bs _ it ->
                    -- '_bs' ought to be null, but we won't check.
                    pure $ Right it
                  -- The `IDecode` representation isn't as sharp as the
                  -- `Decoder` representation: it can give a partial decode even after
                  -- the end of stream is given, and so the user is in limbo with
                  -- neither a success, nor a failure...
                  CBOR.Partial _ ->
                    pure $ Left $ pack "CBOR decoder is still partial after end of input stream"
              , more = \bss -> Fold $ idecode bss term
              }
      in  feedInputs inputs k

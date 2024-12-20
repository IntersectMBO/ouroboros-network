{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
module Test.Ouroboros.Network.Protocol.Utils where

import Codec.CBOR.FlatTerm qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Stateful.Codec qualified as Stateful

import Test.QuickCheck

-- | Generate all 2-splits of a string.
splits2 :: LBS.ByteString -> [[LBS.ByteString]]
splits2 bs = zipWith (\a b -> [a,b]) (LBS.inits bs) (LBS.tails bs)

-- | Generate all 3-splits of a string.
splits3 :: LBS.ByteString -> [[LBS.ByteString]]
splits3 bs =
    [ [a,b,c]
    | (a,bs') <- zip (LBS.inits bs)  (LBS.tails bs)
    , (b,c)   <- zip (LBS.inits bs') (LBS.tails bs') ]

-- | Check that the codec produces a valid CBOR term
-- that is decodeable by CBOR.decodeTerm.
--
prop_codec_cborM
  :: forall ps m. Monad m
  => Codec ps CBOR.DeserialiseFailure m LBS.ByteString
  -> AnyMessage ps
  -> m Bool
prop_codec_cborM codec (AnyMessage msg)
    = case CBOR.deserialiseFromBytes CBOR.decodeTerm $ encode codec msg of
        Left _err               -> return False
        Right (leftover, _term) -> return $ LBS.null leftover


-- | This property checks that the encoder is producing a valid CBOR.  It
-- encodes to 'ByteString' using 'encode' and decodes a 'FlatTerm' from the
-- bytestring which is the fed into 'CBOR.validFlatTerm'.
--
prop_codec_valid_cbor_encoding
  :: forall ps.
     Codec ps CBOR.DeserialiseFailure IO ByteString
  -> AnyMessage ps
  -> Property
prop_codec_valid_cbor_encoding Codec {encode} (AnyMessage msg) =
    case deserialise [] (encode msg) of
      Left  e     -> counterexample (show e) False
      Right terms -> property (CBOR.validFlatTerm terms)
  where
    deserialise :: [CBOR.TermToken]
                -> ByteString
                -> Either CBOR.DeserialiseFailure [CBOR.TermToken]
    deserialise !as bs =
      case CBOR.deserialiseFromBytes CBOR.decodeTermToken bs of
        Left e -> Left e
        Right (bs', a) | LBS.null bs'
                       -> Right (reverse (a : as))
                       | otherwise
                       -> deserialise (a : as) bs'

-- | Check that the codec produces a valid CBOR term
-- that is decodeable by CBOR.decodeTerm.
--
prop_codec_st_cborM
  :: forall ps f m. Monad m
  => Stateful.Codec ps CBOR.DeserialiseFailure f m LBS.ByteString
  -> Stateful.AnyMessage ps f
  -> m Bool
prop_codec_st_cborM codec (Stateful.AnyMessage f msg)
    = case CBOR.deserialiseFromBytes CBOR.decodeTerm $ Stateful.encode codec f msg of
        Left _err               -> return False
        Right (leftover, _term) -> return $ LBS.null leftover

-- | This property checks that the encoder is producing a valid CBOR.  It
-- encodes to 'ByteString' using 'encode' and decodes a 'FlatTerm' from the
-- bytestring which is the fed into 'CBOR.validFlatTerm'.
--
prop_codec_st_valid_cbor_encoding
  :: forall ps f.
     Stateful.Codec ps CBOR.DeserialiseFailure f IO ByteString
  -> Stateful.AnyMessage ps f
  -> Property
prop_codec_st_valid_cbor_encoding Stateful.Codec {Stateful.encode} (Stateful.AnyMessage f msg) =
    case deserialise [] (encode f msg) of
      Left  e     -> counterexample (show e) False
      Right terms -> property (CBOR.validFlatTerm terms)
  where
    deserialise :: [CBOR.TermToken]
                -> ByteString
                -> Either CBOR.DeserialiseFailure [CBOR.TermToken]
    deserialise !as bs =
      case CBOR.deserialiseFromBytes CBOR.decodeTermToken bs of
        Left e -> Left e
        Right (bs', a) | LBS.null bs'
                       -> Right (reverse (a : as))
                       | otherwise
                       -> deserialise (a : as) bs'

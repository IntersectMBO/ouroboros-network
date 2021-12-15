{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
module Test.Ouroboros.Network.Testing.Utils where

import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Codec

import           Test.QuickCheck

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
prop_codec_cborM
  :: forall ps m.
     ( Monad m
     , Eq (AnyMessage ps)
     )
  => Codec ps CBOR.DeserialiseFailure m LBS.ByteString
  -> AnyMessageAndAgency ps
  -> m Bool
prop_codec_cborM codec (AnyMessageAndAgency stok msg)
    = case CBOR.deserialiseFromBytes CBOR.decodeTerm $ encode codec stok msg of
        Left _err               -> return False
        Right (leftover, _term) -> return $ LBS.null leftover


-- | This property checks that the encoder is producing a valid CBOR.  It
-- encodes to 'ByteString' using 'encode' and decodes a 'FlatTerm' from the
-- bytestring which is the fed into 'CBOR.validFlatTerm'.
--
prop_codec_valid_cbor_encoding
  :: forall ps.
     Codec ps CBOR.DeserialiseFailure IO ByteString
  -> AnyMessageAndAgency ps
  -> Property
prop_codec_valid_cbor_encoding Codec {encode} (AnyMessageAndAgency stok msg) =
    case deserialise [] (encode stok msg) of
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

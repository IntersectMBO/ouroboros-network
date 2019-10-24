{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
module Test.Ouroboros.Network.Testing.Utils where

import qualified Data.ByteString.Lazy as LBS
import qualified Codec.CBOR.Read      as CBOR
import qualified Codec.CBOR.Term      as CBOR

import           Network.TypedProtocol.Codec

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
  :: forall ps m. Monad m
  => Codec ps CBOR.DeserialiseFailure m LBS.ByteString
  -> AnyMessageAndAgency ps
  -> m Bool
prop_codec_cborM codec (AnyMessageAndAgency stok msg)
    = case CBOR.deserialiseFromBytes CBOR.decodeTerm $ encode codec stok msg of
        Left _err               -> return False
        Right (leftover, _term) -> return $ LBS.null leftover

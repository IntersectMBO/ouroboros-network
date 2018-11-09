{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Protocol.Codec where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Protocol.Channel

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Protocol.Channel
import Protocol.Codec

data Subject1 where
  Subject1Ping :: Word8 -> Subject1
  Subject1Pong :: Word8 -> Subject1

deriving instance Eq Subject1
deriving instance Show Subject1

genSubject1 :: Gen Subject1
genSubject1 = oneof
  [ Subject1Ping <$> choose (minBound, maxBound)
  , Subject1Pong <$> choose (minBound, maxBound)
  ]

subject1Codec :: Applicative m => Codec Text Subject1 m LBS.ByteString
subject1Codec = Codec
  { encode = subject1Encode
  , decode = subject1Decode
  }

subject1Encode :: Applicative m => Subject1 -> m LBS.ByteString
subject1Encode term = pure $ case term of
  Subject1Ping w -> LBS.pack [0, w]
  Subject1Pong w -> LBS.pack [1, w]

subject1Decode :: Applicative m => Decoder Text LBS.ByteString m Subject1
subject1Decode = Decoder $ \lbs -> pure $ case LBS.uncons lbs of
  Nothing -> Partial subject1Decode
  Just (0, rest) -> Subject1Ping <$> decodeWord rest
  Just (1, rest) -> Subject1Pong <$> decodeWord rest
  Just (w, rest) -> Fail (T.pack ("Unexpected tag " ++ show w)) rest
  where
  decodeWord lbs = case LBS.uncons lbs of
    Nothing -> Partial $ Decoder $ pure . decodeWord
    Just (w, rest) -> Done w rest

-- | The reliable/ordered property from Test.Protocol.Channel, lifted though
-- the codec. If the channels used satisfy 'prop_channel_reliable_ordered'
-- then they should also satisfy this test for arbitrary subject lists.
prop_codec_channel_consistent
  :: forall fail subject piece .
     ( Eq subject, Show subject )
  => Codec fail subject IO piece
  -> [subject]
  -> [subject]
  -> Channel IO piece
  -> Channel IO piece
  -> Property
prop_codec_channel_consistent codec send1 send2 chan1 chan2 =
  prop_channel_reliable_ordered send1 send2 chan1' chan2'
  where
  chan1' :: Channel IO subject
  chan1' = hoistDuplex id
                       (eliminateDecodeFailure (error "decode error"))
                       (codecDuplex codec chan1)
  chan2' :: Channel IO subject
  chan2' = hoistDuplex id
                       (eliminateDecodeFailure (error "decode error"))
                       (codecDuplex codec chan2)

prop_codec_channel_consistent_mvar :: Property
prop_codec_channel_consistent_mvar =
  forAll (vectorOf 10 genSubject1) $ \as ->
    forAll (vectorOf 10 genSubject1) $ \bs ->
      ioProperty $ do
        (chanA, chanB) <- mvarChannels
        pure $ prop_codec_channel_consistent subject1Codec as bs chanA chanB

tests :: TestTree
tests =
  testGroup "Protocol.Codec"
    [ testProperty "mvar channel" prop_codec_channel_consistent_mvar
    ]

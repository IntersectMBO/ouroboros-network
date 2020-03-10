{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import           Data.Binary (decodeOrFail, encode)
import           Data.Word (Word32)

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck ( Arbitrary (..)
                                 , Gen
                                 , Property
                                 , NonNegative(..)
                                 , arbitrary
                                 , choose
                                 , counterexample
                                 , sized
                                 , (.&&.)
                                 , (===))

import           Network.NTP.Client.Packet ( Microsecond (..)
                                    , NtpOffset (..)
                                    , NtpPacket (..)
                                    , clockOffsetPure
                                    , ntpToRealMcs
                                    , realMcsToNtp)

main :: IO ()
main = defaultMain tests

data NtpPacketWithOffset = NtpPacketWithOffset
    { npoNtpPacket       :: NtpPacket
    , npoOffset          :: NtpOffset
    , npoDestinationTime :: Microsecond
    }
    deriving (Show)

genMicro :: Gen Microsecond
genMicro = Microsecond <$> arbitrary

genMicroNotBefore :: Microsecond -> Gen Microsecond
genMicroNotBefore (Microsecond t) = do
    (NonNegative offset) <- arbitrary
    return $ Microsecond $ t + offset


newtype ArbitraryNtpPacket = ArbitraryNtpPacket NtpPacket
    deriving (Show, Eq)

instance Arbitrary ArbitraryNtpPacket where
    arbitrary = do
        ntpParams <- arbitrary
        ntpPoll <- arbitrary
        ntpOriginTime <- genMicro
        ntpReceivedTime <- genMicroNotBefore ntpOriginTime
        ntpTransmitTime <- genMicroNotBefore ntpReceivedTime
        return $ ArbitraryNtpPacket $ NtpPacket {..}

-- An arbitrary instance which generates a packet with a given offset, with
-- ideal symmetric trip time.
-- TODO: This use of `sized` looks strange.

instance Arbitrary NtpPacketWithOffset where
    arbitrary = sized $ \offset -> do
        let drift :: Microsecond
            drift = Microsecond $ fromIntegral offset
        ntpParams <- arbitrary
        ntpPoll <- arbitrary
        ntpOriginTime <- genMicro
        tripTime <- genMicro
        let ntpReceivedTime = ntpOriginTime + tripTime + drift
        ntpTransmitTime <- genMicroNotBefore ntpReceivedTime
        let npoDestinationTime = ntpTransmitTime + tripTime - drift
        return $ NtpPacketWithOffset
            { npoNtpPacket = NtpPacket {..}
            , npoOffset = NtpOffset drift
            , npoDestinationTime = npoDestinationTime
            }

data NtpTime = NtpTime Word32 Word32
    deriving Show

-- Generate arbitrary pairs of @'Word32'@.  The ntp fractions correspond to 232
-- picoseconds (so 1msc correspond to @1000000 `div` 232 = 4310@).  Since we
-- operate with micorseconds we generated them only up this resolution.
instance Arbitrary NtpTime where
    arbitrary = do
        sec <- arbitrary
        frac <- choose (1, (maxBound @Word32 `div` 1000000) * 232 -1)
        return $ NtpTime sec $ frac * (1000000 `div` 232) - 1

newtype NtpMicrosecond = NtpMicrosecond Microsecond
    deriving Show

-- Generate NtpMicrosecond which must be smaller than
-- @'maxBound' \@Word32 - 2200898800@ (we substract 70 years in seconds).
instance Arbitrary NtpMicrosecond where
    arbitrary = (NtpMicrosecond . Microsecond) <$> choose (0, endTime)
       where endTime = (fromIntegral $ maxBound @Word32) - 2208988800

tests:: TestTree
tests =
    testGroup "NTP packets"
    [ testProperty "clockOffsetPure returns clock the offset"         prop_clockOffetPure
    , testProperty "realMcsToNtp is the right nverse of ntpToRealMcs" prop_timeConvert1
    , testProperty "realMcsToNtp is the left inverse of ntpToRealMcs" prop_timeConvert2
    , testProperty "NtpPacket serialize-deserialze round-trip"        prop_NtpPacket_codec
    ]

prop_clockOffetPure :: NtpPacketWithOffset -> Property
prop_clockOffetPure (NtpPacketWithOffset {..})
    = npoOffset === clockOffsetPure npoNtpPacket npoDestinationTime

prop_timeConvert1 :: NtpMicrosecond -> Property
prop_timeConvert1 (NtpMicrosecond x)
    = x === uncurry ntpToRealMcs (realMcsToNtp x)

prop_timeConvert2 :: NtpTime -> Property
prop_timeConvert2 (NtpTime sec frac)
-- Each npt fraction unit correspond to 232 picoseconds, there are 4310 of them in a millisecond.
    = let (sec', frac') = realMcsToNtp $ ntpToRealMcs sec frac
      in sec === sec' .&&. frac `div` 4310 === frac' `div` 4310

prop_NtpPacket_codec :: ArbitraryNtpPacket -> Property
prop_NtpPacket_codec (ArbitraryNtpPacket ntpPacket)
    = case decodeOrFail $ encode ntpPacket of
        Left (_, _, err)     -> counterexample (show ntpPacket ++ " " ++ show err) False
        Right (_, _, packet) -> ntpPacket === packet

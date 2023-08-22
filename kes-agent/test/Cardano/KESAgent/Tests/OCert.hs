{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Tests.OCert
where

import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.PinnedSizedBytes
import Cardano.Crypto.Libsodium
import Cardano.Crypto.Libsodium.Memory.Internal ( MLockedForeignPtr (..) )
import Cardano.Crypto.Libsodium.MLockedBytes.Internal ( MLockedSizedBytes (..) )
import Cardano.Crypto.Libsodium.MLockedSeed
import Cardano.Crypto.Seed
import Test.Crypto.Instances

import Control.Monad.Class.MonadThrow ( bracket )
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "OCert"
          [ testProperty "validate" prop_opcertValidates
          , testProperty "fail invalid KES key" prop_opcertFailsKES
          , testProperty "fail invalid cold ver key" prop_opcertFailsColdVK
          , testProperty "fail modified cert" prop_opcertFailsOpCert
          ]

prop_opcertValidates :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
                     -> PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                     -> Word64
                     -> Word
                     -> Property
prop_opcertValidates seedColdKey seedKESKey serial period =
  ioProperty $ do
    vkKES <- bracket
                (withMLockedSeedFromPSB seedKESKey (genKeyKES @(KES StandardCrypto)))
                forgetSignKeyKES
                deriveVerKeyKES
    let skCold = genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seedColdKey)
    let vkCold = deriveVerKeyDSIGN skCold
    let opcert = makeOCert @StandardCrypto vkKES serial (KESPeriod period) skCold
    let expected = Right ()
        actual = validateOCert vkCold vkKES opcert
    return $ actual === expected

prop_opcertFailsKES :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
                    -> PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                    -> Word64
                    -> Word
                    -> Int
                    -> Property
prop_opcertFailsKES seedColdKey seedKESKey serial period bitToFlip =
  ioProperty $ do
    let seedKESKey' = psbFromByteString . flipBSBit bitToFlip . psbToByteString $ seedKESKey
    vkKES <- bracket
                (withMLockedSeedFromPSB seedKESKey (genKeyKES @(KES StandardCrypto)))
                forgetSignKeyKES
                deriveVerKeyKES
    vkKES' <- bracket
                (withMLockedSeedFromPSB seedKESKey' (genKeyKES @(KES StandardCrypto)))
                forgetSignKeyKES
                deriveVerKeyKES
    let skCold = genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seedColdKey)
    let vkCold = deriveVerKeyDSIGN skCold
    let opcert = makeOCert @StandardCrypto vkKES serial (KESPeriod period) skCold
    let expected = Left "Verification key does not match"
        actual = validateOCert vkCold vkKES' opcert
    return $ actual === expected

prop_opcertFailsColdVK :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
                       -> PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                       -> Word64
                       -> Word
                       -> Int
                       -> Property
prop_opcertFailsColdVK seedColdKey seedKESKey serial period bitToFlip =
  ioProperty $ do
    vkKES <- bracket
                (withMLockedSeedFromPSB seedKESKey (genKeyKES @(KES StandardCrypto)))
                forgetSignKeyKES
                deriveVerKeyKES
    let skCold = genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seedColdKey)
    let vkCold = deriveVerKeyDSIGN skCold
    let skCold' = genKeyDSIGN (mkSeedFromBytes . flipBSBit bitToFlip . psbToByteString $ seedColdKey)
    let vkCold' = deriveVerKeyDSIGN skCold'
    let opcert = makeOCert @StandardCrypto vkKES serial (KESPeriod period) skCold
    let expected = Left "Verification failed"
        actual = validateOCert vkCold' vkKES opcert
    return $ actual === expected

prop_opcertFailsOpCert :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
                       -> PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                       -> Word64
                       -> Word
                       -> Int
                       -> Int
                       -> Property
prop_opcertFailsOpCert seedColdKey seedKESKey serial period bitToFlipSerial bitToFlipPeriod =
  ioProperty $ do
    vkKES <- bracket
                (withMLockedSeedFromPSB seedKESKey (genKeyKES @(KES StandardCrypto)))
                forgetSignKeyKES
                deriveVerKeyKES
    let skCold = genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seedColdKey)
    let vkCold = deriveVerKeyDSIGN skCold
    let opcert = makeOCert @StandardCrypto vkKES serial (KESPeriod period) skCold
    let opcert' = opcert
                      { ocertN = flipWordBit bitToFlipSerial (ocertN opcert)
                      , ocertKESPeriod = KESPeriod (flipWordBit bitToFlipPeriod . unKESPeriod $ ocertKESPeriod opcert)
                      }
    let expected = Left "Verification failed"
        actual = validateOCert vkCold vkKES opcert'
    return $ actual === expected

flipWordBit :: (Num a, Bits a, FiniteBits a) => Int -> a -> a
flipWordBit n w =
  w `xor` (1 `shiftL` n')
  where
    n' = n `mod` finiteBitSize w

flipBSBit :: Int -> ByteString -> ByteString
flipBSBit n bs =
  bs'
  where
    nPos = abs n
    (idxRaw, bitIdx) = nPos `divMod` 8
    idx = idxRaw `mod` BS.length bs
    before = BS.take idx bs
    at = BS.index bs idx
    after = BS.drop (idx + 1) bs
    at' = at `xor` (1 `shiftL` bitIdx)
    bs' = before <> BS.singleton at' <> after

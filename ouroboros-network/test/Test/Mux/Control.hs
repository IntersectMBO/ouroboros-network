{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Mux.Control (
      tests) where

import           Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord)
import           Codec.CBOR.Read (DeserialiseFailure (..), deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise (..))
import           Control.Exception
import           Data.Foldable (foldl')
import qualified Data.ByteString.Lazy as BL
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Control

tests :: TestTree
tests =
  testGroup "MuxControl"
  [ testProperty "MuxControl encode/decode"       prop_mux_encode_decode
  , testProperty "MuxControl unknown version rsp" prop_unknown_version_rsp
  , testProperty "MuxControl unknown version req" prop_unknown_version_req
  , testProperty "MuxControl unknown message"     prop_unknown_version_key
  ]

instance Arbitrary Mx.NetworkMagic where
    arbitrary = Mx.NetworkMagic <$> arbitrary

instance Arbitrary Mx.Version where
    arbitrary = do
        nm <- arbitrary
        elements [Mx.Version0 nm, Mx.Version1 nm]

instance Arbitrary ControlMsg where
    arbitrary = do
        vs <- arbitrary
        v  <- arbitrary
        f  <- arbitrary
        elements [ MsgInitReq vs
                 , MsgInitRsp v
                 , MsgInitFail f]

-- | Verify that decoder . encoder = id for ControlMsg.
prop_mux_encode_decode :: ControlMsg
                       -> Property
prop_mux_encode_decode msg =
    let bs = toLazyByteString $ encodeCtrlMsg msg
        res_e = deserialiseFromBytes decodeCtrlMsg bs in
    case res_e of
         Left  _   -> property False
         Right res -> return msg === res

-- | Verify error handling for MsgInitRsp with an invalid version.
prop_unknown_version_rsp :: Word
                         -> Word
                         -> Property
prop_unknown_version_rsp version len = version > 1 && len > 0 && len < 0xffff ==>
    let blob = toLazyByteString $ invalidControlMessage 1 version len
    in case deserialiseFromBytes decodeCtrlMsg blob of
      Left (DeserialiseFailure _ err)
        -> err === ("unknown version " ++ show version)

      _ -> property False

-- | Verify that unknown versions are skipped in MsgInitReq messages.
prop_unknown_version_req :: Word
                         -> Word
                         -> Mx.Version
                         -> Property
prop_unknown_version_req version len validVersion = version > 1 && len > 0 && len < 0xffff ==>
    let blob = toLazyByteString enc
        res = deserialiseFromBytes decodeCtrlMsg blob in
    case res of
         Left _ -> property False
         Right (_, MsgInitReq resVersions) -> resVersions === [validVersion]
         Right _  -> property False
  where
    enc =
        let term = BL.replicate (fromIntegral len) 0xa in
        encodeListLen 2 <> encodeWord 0 <> encodeListLen 2
        <> foldl' (\b v -> b <> encode v)
            -- unknown (or bad) encoding of a Version
            (encode term)
            [validVersion]

-- | Verify error handling for ControlMsg with invalid key.
prop_unknown_version_key :: Word
                         -> Word
                         -> Property
prop_unknown_version_key key len = key > 2 && len > 0 && len < 0xffff ==> ioProperty $ do
    let blob = toLazyByteString $ invalidControlMessage key 1 len

    res <- try $ return $! deserialiseFromBytes decodeCtrlMsg blob
    case res of
         Left e  ->
             case fromException e of
                  Just me -> return $ Mx.errorType me == Mx.MuxControlUnknownMessage
                  Nothing -> return False

         Right _ -> return False

-- | Encode an invalid Muxcontrol message with the key 'k', version number 'v' and 'len' bytes
-- of '0xa' dummy payload.
invalidControlMessage :: Word -> Word -> Word -> Encoding
invalidControlMessage k v len =
    let term = BL.replicate (fromIntegral len) 0xa in
    encodeListLen 2 <> encodeWord k <> encodeListLen 2 <> encode v <> encode term



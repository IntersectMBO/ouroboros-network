{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Network.NodeToClient.Version (tests) where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.Bits (finiteBitSize)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T

import Cardano.Network.NodeToClient.Version
import Cardano.Network.Version.TestUtils
import Ouroboros.Network.CodecCBORTerm

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Cardano.Network.NodeToClient.Version"
    [ testProperty "nodeToClientVersionCodec"  prop_nodeToClientVersionCodec
    , testProperty "nodeToClientCodecCBORTerm" prop_nodeToClientCodec
    , testProperty "nodeToClientCodecMagicIsTInt"
                   prop_nodeToClientCodecMagicIsTInt
    , testProperty "nodeToClientCodecCBORTermWire" prop_nodeToClientCodecWire
    , testProperty "nodeToClientCodecCBORTermOutOfRange"
                   prop_nodeToClientCodecOutOfRange
    ]

data VersionAndVersionData =
    VersionAndVersionData NodeToClientVersion NodeToClientVersionData
  deriving Show

instance Arbitrary NodeToClientVersion where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary VersionAndVersionData where
    arbitrary =
      VersionAndVersionData
        <$> elements [ minBound .. maxBound]
        <*> (NodeToClientVersionData <$> genNetworkMagic <*> arbitrary)


prop_nodeToClientVersionCodec :: NodeToClientVersion
                              -> Bool
prop_nodeToClientVersionCodec version =
    case decodeTerm (encodeTerm version) of
        Right version' -> version == version'
        Left {}        -> False
  where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToClientVersionCodec


prop_nodeToClientCodec :: VersionAndVersionData -> Bool
prop_nodeToClientCodec (VersionAndVersionData vNumber vData) =
      case decodeData vNumber (encodeData vNumber vData) of
        Right vData' -> networkMagic vData' == networkMagic vData
        Left {}      -> False
    where
      VersionDataCodec { encodeData, decodeData } = nodeToClientVersionDataCodec


-- | Nodes running older code accept the network magic only as a 'CBOR.TInt'
-- (e.g. `cardano-diffusion-1.1.0.0`).  What such a node sees is what `cborg`
-- decodes from bytes: on a 64bit platform a 'CBOR.TInt' for any 'Word32',
-- whatever constructor our encoder used.  On 32bit systems, the decoder will
-- return `CBOR.TInteger` for `Word32` values which don't fit `Int32`.
--
prop_nodeToClientCodecMagicIsTInt :: VersionAndVersionData -> Property
prop_nodeToClientCodecMagicIsTInt (VersionAndVersionData vNumber vData) =
    case CBOR.deserialiseFromBytes CBOR.decodeTerm bytes of
      Right (_, CBOR.TList (CBOR.TInt magic : _)) ->
        toInteger magic === expected
      Right (_, CBOR.TList (CBOR.TInteger magic : _))
        -- This asserts backward compatibility of the codec on 64bit systems.
        | is64BitPlatform
        -> counterexample
            ("unexpected encoding on 64bit platform: TInteger " ++ show magic)
            False

        -- `cborg` returns 'CBOR.TInteger' only for values which don't fit
        -- 'Int': never for a 'Word32' on a 64bit platform.  Otherwise it's an
        -- encoding which older nodes, accepting only 'CBOR.TInt', would reject.
        | magic > toInteger (maxBound :: Int)
        -> magic === expected

        | otherwise
        -> counterexample ("unexpected encoding: TInteger " ++ show magic) False

      Right (_, term) ->
        counterexample ("unexpected encoding: " ++ show term) False
      Left err ->
        counterexample (show err) False
  where
    is64BitPlatform = finiteBitSize (0 :: Int) >= 64
    VersionDataCodec { encodeData } = nodeToClientVersionDataCodec
    bytes = CBOR.toLazyByteString
          . CBOR.encodeTerm
          $ encodeData vNumber vData
    expected = toInteger (unNetworkMagic (networkMagic vData))


-- | Round-trip through the serialised form, as the handshake does.
--
-- `cborg` normalises an unsigned integer to `CBOR.TInt` whenever it fits
-- `Int`, so a term built with `CBOR.TInteger` is handed back as `CBOR.TInt`.
-- `prop_nodeToClientCodec` compares terms directly and so cannot observe this;
-- only a round-trip through the wire can catch a decoder which accepts just
-- one of the two representations.
--
prop_nodeToClientCodecWire :: VersionAndVersionData -> Property
prop_nodeToClientCodecWire (VersionAndVersionData vNumber vData) =
      case CBOR.deserialiseFromBytes CBOR.decodeTerm bytes of
        Right (rest, term)
          | BL.null rest -> decodeData vNumber term === Right vData
          | otherwise    -> counterexample ("trailing bytes: " ++ show rest)
                                           False
        Left err         -> counterexample (show err) False
    where
      VersionDataCodec { encodeData, decodeData } = nodeToClientVersionDataCodec
      bytes = CBOR.toLazyByteString
            . CBOR.encodeTerm
            $ encodeData vNumber vData


-- | The decoder must reject a network magic outside the 'Word32' range.
--
-- This goes through the wire as well, so that `cborg`'s choice between
-- `CBOR.TInt` and `CBOR.TInteger` - which for negative values switches at
-- `minBound :: Int`  - is the one the decoder actually sees.
--
prop_nodeToClientCodecOutOfRange :: NodeToClientVersion
                                 -> OutOfRangeMagic
                                 -> Bool
                                 -> Property
prop_nodeToClientCodecOutOfRange version (OutOfRangeMagic x) query =
      case CBOR.deserialiseFromBytes CBOR.decodeTerm bytes of
        Right (_, term) -> decodeData version term === Left outOfBound
        Left err        -> counterexample (show err) False
    where
      VersionDataCodec { decodeData } = nodeToClientVersionDataCodec
      bytes = CBOR.toLazyByteString
            . CBOR.encodeTerm
            $ encodeNodeToClientVersionDataHelper version x query

      outOfBound :: Text
      outOfBound = T.pack ("networkMagic out of bound: " ++ show x)

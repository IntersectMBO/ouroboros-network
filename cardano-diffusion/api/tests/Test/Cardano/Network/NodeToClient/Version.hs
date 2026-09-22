{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Network.NodeToClient.Version (tests) where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.Bits (finiteBitSize)

import Cardano.Network.NodeToClient.Version

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
        <*> (NodeToClientVersionData . NetworkMagic <$> arbitrary <*> arbitrary)


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

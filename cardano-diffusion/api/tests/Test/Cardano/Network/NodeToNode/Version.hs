{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Network.NodeToNode.Version (tests) where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Exception (SomeException, evaluate, try)
import Data.Bits (finiteBitSize)

import Cardano.Network.NodeToNode.Version
import Cardano.Network.NodeToNode.Version.TestUtils

import Ouroboros.Network.CodecCBORTerm

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Cardano.Network.NodeToNode.Version"
    [ testProperty "nodeToNodeVersionCodec"           prop_nodeToNodeVersionCodec
    , testProperty "nodeToNodeCodecCBORTerm"          prop_nodeToNodeCodec
    , testProperty "nodeToNodeCodecHandleInvalidData" prop_nodeToNodeCodecHandleInvalidData
    , testProperty "nodeToNodeCodecMagicIsTInt"       prop_nodeToNodeCodecMagicIsTInt
    ]

instance Arbitrary NodeToNodeVersion where
    arbitrary = genNodeToNodeVersion
    shrink    = shrinkNodeToNodeVersion

instance Arbitrary NodeToNodeVersionData where
    arbitrary = genNodeToNodeVersionData
    shrink    = shrinkNodeToNodeVersionData

prop_nodeToNodeVersionCodec :: NodeToNodeVersion
                            -> Bool
prop_nodeToNodeVersionCodec version =
    case decodeTerm (encodeTerm version) of
      Right version' -> version == version'
      Left {}        -> False
  where
    CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeVersionCodec

prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Property
prop_nodeToNodeCodec ntnVersion rawNtnData =
    case decodeData ntnVersion (encodeData ntnVersion ntnData) of
      Right ntnData' -> ntnData' === ntnData
      Left err       -> counterexample (show err) False
  where
    ntnData = fixNtnVersionDataForVersion ntnVersion rawNtnData
    VersionDataCodec { encodeData, decodeData } = nodeToNodeVersionDataCodec

prop_nodeToNodeCodecHandleInvalidData :: Property
prop_nodeToNodeCodecHandleInvalidData =
    forAll genInvalidNtnVersionAndDataPair checkEncodeFails
  where
    checkEncodeFails (ntnVersion, ntnData) = ioProperty $ do
        r <- try @SomeException (evaluate (encodeData ntnVersion ntnData))
        case r of
          Left _  -> pure $ property True
          Right _ -> pure $ counterexample explanation False
      where
        explanation =
             show ntnData
          ++ " was encoded successfully, but should have failed for version "
          ++ show ntnVersion
        VersionDataCodec { encodeData } = nodeToNodeVersionDataCodec


-- | Nodes running older code accept the network magic only as a 'CBOR.TInt'
-- (e.g. `cardano-diffusion-1.1.0.0`).  What such a node sees is what `cborg`
-- decodes from bytes: on a 64bit platform a 'CBOR.TInt' for any 'Word32',
-- whatever constructor our encoder used.  On 32bit systems, the decoder will
-- return `CBOR.TInteger` for `Word32` values which don't fit `Int32`.
--
prop_nodeToNodeCodecMagicIsTInt :: NodeToNodeVersion
                                -> NodeToNodeVersionData
                                -> Property
prop_nodeToNodeCodecMagicIsTInt ntnVersion rawNtnData =
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
    ntnData = fixNtnVersionDataForVersion ntnVersion rawNtnData
    is64BitPlatform = finiteBitSize (0 :: Int) >= 64
    VersionDataCodec { encodeData } = nodeToNodeVersionDataCodec
    bytes = CBOR.toLazyByteString
          . CBOR.encodeTerm
          $ encodeData ntnVersion ntnData
    expected = toInteger (unNetworkMagic (networkMagic ntnData))

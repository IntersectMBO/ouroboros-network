{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PublicState where

import Codec.CBOR.FlatTerm qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Codec.Serialise
import Data.ByteString.Lazy qualified as BSL

import Ouroboros.Network.PublicState
import Test.Ouroboros.Network.PublicState.Generators ()

import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Test.Ouroboros.Network.PublicState"
  [ testGroup "CBOR"
    [ testProperty "round trip" prop_publicState_roundTripCBOR
    , testProperty "valid encoding" prop_publicState_validCBOR
    ]
  ]


type Addr = Int

instance Serialise (RemoteAddressEncoding Addr) where
  encode   = encode . getRemoteAddressEncoding
  decode = RemoteAddressEncoding <$> decode

prop_publicState_roundTripCBOR
  :: NetworkState Addr
  -> Property
prop_publicState_roundTripCBOR ns =
  case CBOR.deserialiseFromBytes decodeNetworkState (CBOR.toLazyByteString (encodeNetworkState ns)) of
    Left e -> counterexample (show e) False
    Right (bs, ns') | BSL.null bs
                    -> ns' === ns

                    | otherwise
                    -> counterexample "left over bytes" False

prop_publicState_validCBOR
  :: NetworkState Addr
  -> Property
prop_publicState_validCBOR ns =
    counterexample (show enc)
  . CBOR.validFlatTerm
  . CBOR.toFlatTerm
  $ enc
  where
    enc = encodeNetworkState ns



{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Util.Serialisation (
    roundtrip_SerialiseDisk
  , roundtrip_SerialiseNodeToNode
  , roundtrip_SerialiseNodeToClient
  , WithVersion (..)
    -- * SomeResult
  , SomeResult (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (decode, encode)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy (..))
import           Data.Typeable

import           Ouroboros.Network.Block (HeaderHash, Serialised,
                     fromSerialised, mkSerialised)

import           Ouroboros.Consensus.Block (BlockProtocol, CodecConfig, Header)
import           Ouroboros.Consensus.HeaderValidation (AnnTip, HasAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState, Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints,
                     SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract (ConsensusState)
import           Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Roundtrip

-- | Constraints needed in practice for something to be passed in as an
-- 'Arbitrary' argument to a QuickCheck property.
type Arbitrary' a = (Arbitrary a, Eq a, Show a)

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseDiskConstraints'?
roundtrip_SerialiseDisk
  :: forall blk.
     ( SerialiseDiskConstraints blk
     , HasAnnTip blk

     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (HeaderHash blk)
     , Arbitrary' (LedgerState blk)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ConsensusState (BlockProtocol blk))
     )
  => CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseDisk ccfg =
    [ testProperty "roundtrip block" $
        roundtrip' @blk (encodeDisk ccfg) (decodeDisk ccfg)
    , testProperty "roundtrip Header" $
        roundtrip' @(Header blk) (encodeDisk ccfg) (decodeDisk ccfg)
    , testProperty "roundtrip HeaderHash" $
        roundtrip @(HeaderHash blk) encode decode
    , -- Since the 'LedgerState' is a large data structure, we lower the
      -- number of tests to avoid slowing down the testsuite too much
      adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
      rt (Proxy @(LedgerState blk)) "LedgerState"
    , rt (Proxy @(AnnTip blk)) "AnnTip"
    , rt (Proxy @(ConsensusState (BlockProtocol blk))) "ConsensusState"
    ]
  where
    rt :: forall a. (Arbitrary' a, EncodeDisk blk a, DecodeDisk blk a)
       => Proxy a -> String -> TestTree
    rt _ name =
      testProperty ("roundtrip " <> name) $
        roundtrip @a
          (encodeDisk ccfg)
          (decodeDisk ccfg)

-- | Used to generate arbitrary values for the serialisation roundtrip tests.
-- As the serialisation format can change with the version, not all arbitrary
-- values of the type might be valid for each version.
--
-- For example, a certain constructor can only be used after a certain version
-- and can thus not be generated for any prior versions.
data WithVersion v a = WithVersion v a
  deriving (Eq, Show)

-- | Similar to @Arbitrary'@, but with an 'Arbitrary' instasnce for
-- @('WithVersion' v a)@.
type ArbitraryWithVersion v a = (Arbitrary (WithVersion v a), Eq a, Show a)

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToNodeConstraints'?
roundtrip_SerialiseNodeToNode
  :: forall blk.
     ( SerialiseNodeToNodeConstraints blk
     , Show (NodeToNodeVersion blk)
     , ArbitraryWithVersion (NodeToNodeVersion blk) (HeaderHash blk)
     , ArbitraryWithVersion (NodeToNodeVersion blk) blk
     , ArbitraryWithVersion (NodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (NodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (NodeToNodeVersion blk) (GenTxId blk)

       -- Needed for testing the @Serialised blk@
     , EncodeDisk blk blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
       -- Needed for testing the @Serialised (Header blk)@
     , EncodeDisk blk (Header blk)
     , DecodeDisk blk (Lazy.ByteString -> Header blk)
     )
  => CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseNodeToNode ccfg =
    [ rt (Proxy @(HeaderHash blk)) "HeaderHash"
    , rt (Proxy @blk)              "blk"
    , rt (Proxy @(Header blk))     "Header"
    , rt (Proxy @(GenTx blk))      "GenTx"
    , rt (Proxy @(GenTxId blk))    "GenTxId"
      -- Roundtrip a @'Serialised' blk@
      --
      -- We generate a random @blk@, convert it to 'Serialised' (using
      -- 'encodeDisk', which doesn't add CBOR-in-CBOR), encode it (adding
      -- CBOR-in-CBOR), decode that 'Serialised' and convert (using
      -- 'decodeNodeToNode') it to a @blk@ again.
    , testProperty "roundtrip Serialised blk" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            blk
      -- Same as above but for 'Header'
    , testProperty "roundtrip Serialised Header" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            hdr
      -- Check the compatibility between 'encodeNodeToNode' for @'Serialised'
      -- blk@ and 'decodeNodeToNode' for @blk@.
      --
      -- We generate a random @blk@, convert it to 'Serialised' (using
      -- 'encodeDisk', which doesn't add CBOR-in-CBOR), encode it (adding
      -- CBOR-in-CBOR and making version-specific changes), decode that using
      -- 'decodeNodeToNode' for @blk@, which should handle the CBOR-in-CBOR
      -- correctly.
    , testProperty "roundtrip Serialised blk compat 1" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (dec version)
            blk
      -- Check the compatibility between 'encodeNodeToNode' for @blk@ and
      -- 'decodeNodeToNode' for @'Serialised' blk@.
      --
      -- We generate a random @blk@, encode it using 'encodeNodeToNode'
      -- (adding CBOR-in-CBOR and making any version-specific changes), decode
      -- that as a @'Serialised' blk@ using 'decodeNodeToNode' (removing the
      -- CBOR-in-CBOR from the bytestring) and then convert the @'Serialised'
      -- blk@ to a @blk@ using using 'decodeDisk'.
    , testProperty "roundtrip Serialised blk compat 2" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (enc version)
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            blk
      -- Same as above but for 'Header'
    , testProperty "roundtrip Serialised Header compat 1" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (dec version)
            hdr
    , testProperty "roundtrip Serialised Header compat 2" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (enc version)
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            hdr
    ]
  where
    enc :: SerialiseNodeToNode blk a
        => NodeToNodeVersion blk -> a -> Encoding
    enc = encodeNodeToNode ccfg

    dec :: SerialiseNodeToNode blk a
        => NodeToNodeVersion blk -> forall s. Decoder s a
    dec = decodeNodeToNode ccfg

    rt
      :: forall a.
         ( Arbitrary (WithVersion (NodeToNodeVersion blk) a)
         , Eq a
         , Show a
         , SerialiseNodeToNode blk a
         )
       => Proxy a -> String -> TestTree
    rt _ name =
      testProperty ("roundtrip " <> name) $ \(WithVersion version x) ->
        roundtrip @a (enc version) (dec version) x

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToClientConstraints'?
roundtrip_SerialiseNodeToClient
  :: forall blk.
     ( SerialiseNodeToClientConstraints blk
     , Show (NodeToClientVersion blk)
     , ArbitraryWithVersion (NodeToClientVersion blk) (HeaderHash blk)
     , ArbitraryWithVersion (NodeToClientVersion blk) blk
     , ArbitraryWithVersion (NodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (NodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (NodeToClientVersion blk) (Some (Query blk))
     , ArbitraryWithVersion (NodeToClientVersion blk) (SomeResult blk)

       -- Needed for testing the @Serialised blk@
     , EncodeDisk blk blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     )
  => CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseNodeToClient ccfg =
    [ rt (Proxy @(HeaderHash blk))   "HeaderHash"
    , rt (Proxy @blk)                "blk"
    , rt (Proxy @(GenTx blk))        "GenTx"
    , rt (Proxy @(ApplyTxErr blk))   "ApplTxErr"
    , rt (Proxy @(Some (Query blk))) "Query"
      -- See roundtrip_SerialiseNodeToNode for more info
    , testProperty "roundtrip Serialised blk" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (decodeThroughSerialised (decodeDisk ccfg) (dec version))
            blk
      -- See roundtrip_SerialiseNodeToNode for more info
    , testProperty "roundtrip Serialised blk compat" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (dec version)
            blk
    , testProperty "roundtrip Result" $
        \(WithVersion version (SomeResult query result :: SomeResult blk)) ->
          roundtrip
            (encodeResult ccfg version query)
            (decodeResult ccfg version query)
            result
    ]
  where
    enc :: SerialiseNodeToClient blk a
        => NodeToClientVersion blk -> a -> Encoding
    enc = encodeNodeToClient ccfg

    dec :: SerialiseNodeToClient blk a
        => NodeToClientVersion blk -> forall s. Decoder s a
    dec = decodeNodeToClient ccfg

    rt
      :: forall a.
         ( Arbitrary (WithVersion (NodeToClientVersion blk) a)
         , Eq a
         , Show a
         , SerialiseNodeToClient blk a
         )
       => Proxy a -> String -> TestTree
    rt _ name =
      testProperty ("roundtrip " <> name) $
        \(WithVersion version a) ->
          roundtrip @a (enc version) (dec version) a

{-------------------------------------------------------------------------------
  Serialised helpers
-------------------------------------------------------------------------------}

encodeThroughSerialised
  :: (a -> Encoding)
  -> (Serialised a -> Encoding)
  -> (a -> Encoding)
encodeThroughSerialised enc encSerialised = encSerialised . mkSerialised enc

decodeThroughSerialised
  :: (forall s. Decoder s (Lazy.ByteString -> a))
  -> (forall s. Decoder s (Serialised a))
  -> (forall s. Decoder s a)
decodeThroughSerialised dec decSerialised = do
    serialised <- decSerialised
    fromSerialised dec serialised

{-------------------------------------------------------------------------------
  SomeResult
-------------------------------------------------------------------------------}

-- | To easily generate all the possible @result@s of the 'Query' GADT, we
-- introduce an existential that also bundles the corresponding 'Query' as
-- evidence. We also capture 'Eq', 'Show', and 'Typeable' constraints, as we
-- need them in the tests.
data SomeResult blk where
  SomeResult :: (Eq result, Show result, Typeable result)
             => Query blk result -> result -> SomeResult blk

instance Show (SomeResult blk) where
  show (SomeResult _ result) = show result

instance Eq (SomeResult blk) where
  SomeResult _ (res1 :: result1) == SomeResult _ (res2 :: result2) =
    case eqT @result1 @result2 of
      Nothing   -> False
      Just Refl -> res1 == res2

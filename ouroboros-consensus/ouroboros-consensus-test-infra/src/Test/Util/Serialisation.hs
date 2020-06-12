{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Util.Serialisation (
    roundtrip_all
  , roundtrip_SerialiseDisk
  , roundtrip_SerialiseNodeToNode
  , roundtrip_SerialiseNodeToClient
  , roundtrip_envelopes
  , WithVersion (..)
    -- * SomeResult
  , SomeResult (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (decode, encode)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Function (on)
import           Data.Proxy (Proxy (..))
import           Data.Typeable

import           Ouroboros.Network.Block (HeaderHash, Serialised (..),
                     fromSerialised, mkSerialised)

import           Ouroboros.Consensus.Block
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
import           Ouroboros.Consensus.Util (Dict (..))

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Roundtrip

-- | Constraints needed in practice for something to be passed in as an
-- 'Arbitrary' argument to a QuickCheck property.
type Arbitrary' a = (Arbitrary a, Eq a, Show a)

roundtrip_all
  :: forall blk.
     ( SerialiseDiskConstraints blk
     , SerialiseNodeToNodeConstraints blk
     , SerialiseNodeToClientConstraints blk

     , Show (BlockNodeToNodeVersion blk)
     , Show (BlockNodeToClientVersion blk)

     , HasAnnTip blk

     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (HeaderHash blk)
     , Arbitrary' (LedgerState blk)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ConsensusState (BlockProtocol blk))

     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (SomeBlock (NestedCtxt Header) blk)

     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (HeaderHash blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (Some (Query blk))
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)
     )
  => CodecConfig blk
  -> (forall a. NestedCtxt_ blk Header a -> Dict (Eq a, Show a))
  -> TestTree
roundtrip_all ccfg dictNestedHdr =
    testGroup "Roundtrip" [
        testGroup "SerialiseDisk"         $ roundtrip_SerialiseDisk           ccfg dictNestedHdr
      , testGroup "SerialiseNodeToNode"   $ roundtrip_SerialiseNodeToNode     ccfg
      , testGroup "SerialiseNodeToClient" $ roundtrip_SerialiseNodeToClient   ccfg
      , testProperty "envelopes"          $ roundtrip_envelopes               ccfg
      ]

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
  -> (forall a. NestedCtxt_ blk Header a -> Dict (Eq a, Show a))
  -> [TestTree]
roundtrip_SerialiseDisk ccfg dictNestedHdr =
    [ testProperty "roundtrip block" $
        roundtrip' @blk (encodeDisk ccfg) (decodeDisk ccfg)
    , testProperty "roundtrip Header" $ \hdr ->
        case unnest hdr of
          DepPair ctxt nestedHdr -> case dictNestedHdr (flipNestedCtxt ctxt) of
            Dict ->
              roundtrip'
                (encodeDiskDep ccfg ctxt)
                (decodeDiskDep ccfg ctxt)
                nestedHdr
    , testProperty "roundtrip HeaderHash" $
        roundtrip @(HeaderHash blk) encode decode
      -- Since the 'LedgerState' is a large data structure, we lower the
      -- number of tests to avoid slowing down the testsuite too much
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
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
     , Show (BlockNodeToNodeVersion blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTxId blk)

       -- Needed for testing the @Serialised blk@
     , EncodeDisk blk blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
       -- Needed for testing the @Serialised (Header blk)@
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     , DecodeDiskDep (NestedCtxt Header) blk
     )
  => CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseNodeToNode ccfg =
    [ rt (Proxy @blk)              "blk"
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
            (enc version . SerialisedHeaderFromDepPair . encodeDepPair ccfg . unnest)
            (nest <$> (decodeDepPair ccfg . serialisedHeaderToDepPair =<< dec version))
            hdr
      -- Check the compatibility between 'encodeNodeToNode' for @'Serialised'
      -- blk@ and 'decodeNodeToNode' for @blk@.
    , testProperty "roundtrip Serialised blk compat 1" $
        \(WithVersion version blk) ->
          roundtrip @blk
            (encodeThroughSerialised (encodeDisk ccfg) (enc version))
            (dec version)
            blk
      -- Check the compatibility between 'encodeNodeToNode' for @blk@ and
      -- 'decodeNodeToNode' for @'Serialised' blk@.
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
            (enc version . SerialisedHeaderFromDepPair . encodeDepPair ccfg . unnest)
            (dec version)
            hdr
    , testProperty "roundtrip Serialised Header compat 2" $
        \(WithVersion version hdr) ->
          roundtrip @(Header blk)
            (enc version)
            (nest <$> (decodeDepPair ccfg . serialisedHeaderToDepPair =<< dec version))
            hdr
    ]
  where
    enc :: SerialiseNodeToNode blk a
        => BlockNodeToNodeVersion blk -> a -> Encoding
    enc = encodeNodeToNode ccfg

    dec :: SerialiseNodeToNode blk a
        => BlockNodeToNodeVersion blk -> forall s. Decoder s a
    dec = decodeNodeToNode ccfg

    rt
      :: forall a.
         ( Arbitrary (WithVersion (BlockNodeToNodeVersion blk) a)
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
     , Show (BlockNodeToClientVersion blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (HeaderHash blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (Some (Query blk))
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)

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
        => BlockNodeToClientVersion blk -> a -> Encoding
    enc = encodeNodeToClient ccfg

    dec :: SerialiseNodeToClient blk a
        => BlockNodeToClientVersion blk -> forall s. Decoder s a
    dec = decodeNodeToClient ccfg

    rt
      :: forall a.
         ( Arbitrary (WithVersion (BlockNodeToClientVersion blk) a)
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
  Checking envelopes
-------------------------------------------------------------------------------}

-- | This is similar to the roundtrip tests for headers, except we don't
-- start with a header but some fixed bytestring in the payload. This makes
-- debugging a bit easier as we can focus on just the envelope.
roundtrip_envelopes ::
     forall blk. (
       SerialiseNodeToNode blk (SerialisedHeader blk)
     , HasNestedContent Header blk
     )
  => CodecConfig blk
  -> WithVersion (BlockNodeToNodeVersion blk) (SomeBlock (NestedCtxt Header) blk)
  -> Property
roundtrip_envelopes ccfg (WithVersion v (SomeBlock ctxt)) =
    roundtrip
      (encodeNodeToNode ccfg v . unBase16)
      (Base16 <$> decodeNodeToNode ccfg v)
      (Base16 serialisedHeader)
  where
    serialisedHeader :: SerialisedHeader blk
    serialisedHeader = SerialisedHeaderFromDepPair $
        GenDepPair ctxt (Serialised bs)

    bs :: Lazy.ByteString
    bs = "<PAYLOAD>" -- Something we can easily recognize in test failures

newtype Base16 a = Base16 { unBase16 :: a }

instance HasNestedContent Header blk => Show (Base16 (SerialisedHeader blk)) where
  show = aux . serialisedHeaderToDepPair . unBase16
    where
      aux :: GenDepPair Serialised (NestedCtxt Header blk) -> String
      aux (GenDepPair ctxt (Serialised bs)) =
          "(" <> show ctxt <> "," <> Char8.unpack (Base16.encode bs) <> ")"

instance HasNestedContent Header blk => Eq (Base16 (SerialisedHeader blk)) where
  (==) = aux `on` (serialisedHeaderToDepPair . unBase16)
    where
      aux :: GenDepPair Serialised (NestedCtxt Header blk)
          -> GenDepPair Serialised (NestedCtxt Header blk)
          -> Bool
      aux (GenDepPair ctxt bs) (GenDepPair ctxt' bs') =
          case sameDepIndex ctxt ctxt' of
            Just Refl -> bs == bs'
            Nothing   -> False

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

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Util.Serialisation.Roundtrip (
    -- * Basic test helpers
    roundtrip
  , roundtrip'
    -- * Test skeleton
  , Arbitrary'
  , SomeResult (..)
  , WithVersion (..)
  , prop_hashSize
  , roundtrip_ConvertRawHash
  , roundtrip_SerialiseDisk
  , roundtrip_SerialiseNodeToClient
  , roundtrip_SerialiseNodeToNode
  , roundtrip_all
  , roundtrip_envelopes
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Short as Short
import           Data.Function (on)
import           Data.Typeable

import           Ouroboros.Network.Block (Serialised (..), fromSerialised,
                     mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (AnnTip)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import           Ouroboros.Consensus.Ledger.Query (BlockQuery)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints,
                     SerialiseNodeToNodeConstraints (..))
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util (Dict (..))

import           Test.Tasty
import           Test.Tasty.QuickCheck

{------------------------------------------------------------------------------
  Basic test helpers
------------------------------------------------------------------------------}

roundtrip :: (Eq a, Show a)
          => (a -> Encoding)
          -> (forall s. Decoder s a)
          -> a
          -> Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

-- | Roundtrip property for values annotated with their serialized form
--
-- NOTE: Suppose @a@ consists of a pair of the unannotated value @a'@ and some
-- 'Lazy.ByteString'. The roundtrip property will fail if that
-- 'Lazy.ByteString' encoding is not equal to @enc a'@. One way in which this
-- might happen is if the annotation is not canonical CBOR, but @enc@ does
-- produce canonical CBOR.
roundtrip' :: (Eq a, Show a)
           => (a -> Encoding)  -- ^ @enc@
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtrip' enc dec a = case deserialiseFromBytes dec bs of
    Right (bs', a')
      | Lazy.null bs'
      -> a === a' bs
      | otherwise
      -> counterexample ("left-over bytes: " <> toBase16 bs') False
    Left e
      -> counterexample (show e) $
         counterexample (toBase16 bs) False
  where
    bs = toLazyByteString (enc a)

    toBase16 :: Lazy.ByteString -> String
    toBase16 = Char8.unpack . Base16.encode

{------------------------------------------------------------------------------
  Test skeleton
------------------------------------------------------------------------------}

-- | Constraints needed in practice for something to be passed in as an
-- 'Arbitrary' argument to a QuickCheck property.
type Arbitrary' a = (Arbitrary a, Eq a, Show a)

-- | All roundtrip tests
roundtrip_all
  :: forall blk.
     ( SerialiseDiskConstraints         blk
     , SerialiseNodeToNodeConstraints   blk
     , SerialiseNodeToClientConstraints blk

     , Show (BlockNodeToNodeVersion   blk)
     , Show (BlockNodeToClientVersion blk)

     , StandardHash blk
     , GetHeader    blk

     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (HeaderHash blk)
     , Arbitrary' (LedgerState blk)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ChainDepState (BlockProtocol blk))

     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (Header blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (GenTxId blk)
     , ArbitraryWithVersion (BlockNodeToNodeVersion blk) (SomeSecond (NestedCtxt Header) blk)

     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeSecond BlockQuery blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)
     )
  => CodecConfig blk
  -> (forall a. NestedCtxt_ blk Header a -> Dict (Eq a, Show a))
  -> TestTree
roundtrip_all ccfg dictNestedHdr =
    testGroup "Roundtrip" [
        testGroup "SerialiseDisk"         $ roundtrip_SerialiseDisk         ccfg dictNestedHdr
      , testGroup "SerialiseNodeToNode"   $ roundtrip_SerialiseNodeToNode   ccfg
      , testGroup "SerialiseNodeToClient" $ roundtrip_SerialiseNodeToClient ccfg
      , testProperty "envelopes"          $ roundtrip_envelopes             ccfg
      , testProperty "ConvertRawHash"     $ roundtrip_ConvertRawHash        (Proxy @blk)
      , testProperty "hashSize"           $ prop_hashSize                   (Proxy @blk)
      , testProperty "estimateBlockSize"  $ prop_estimateBlockSize         ccfg
      ]

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseDiskConstraints'?
roundtrip_SerialiseDisk
  :: forall blk.
     ( SerialiseDiskConstraints blk

     , Arbitrary' blk
     , Arbitrary' (Header blk)
     , Arbitrary' (LedgerState blk)
     , Arbitrary' (AnnTip blk)
     , Arbitrary' (ChainDepState (BlockProtocol blk))
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
      -- Since the 'LedgerState' is a large data structure, we lower the
      -- number of tests to avoid slowing down the testsuite too much
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
      rt (Proxy @(LedgerState blk)) "LedgerState"
    , rt (Proxy @(AnnTip blk)) "AnnTip"
    , rt (Proxy @(ChainDepState (BlockProtocol blk))) "ChainDepState"
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
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (WithVersion () a) where
  arbitrary = WithVersion () <$> arbitrary

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
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) blk
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (GenTx blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (ApplyTxErr blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeSecond BlockQuery blk)
     , ArbitraryWithVersion (BlockNodeToClientVersion blk) (SomeResult blk)

       -- Needed for testing the @Serialised blk@
     , EncodeDisk blk blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     )
  => CodecConfig blk
  -> [TestTree]
roundtrip_SerialiseNodeToClient ccfg =
    [ rt (Proxy @blk)                         "blk"
    , rt (Proxy @(GenTx blk))                 "GenTx"
    , rt (Proxy @(ApplyTxErr blk))            "ApplyTxErr"
    , rt (Proxy @(SomeSecond BlockQuery blk)) "BlockQuery"
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
  -> WithVersion (BlockNodeToNodeVersion blk) (SomeSecond (NestedCtxt Header) blk)
  -> Property
roundtrip_envelopes ccfg (WithVersion v (SomeSecond ctxt)) =
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
  ConvertRawHash
-------------------------------------------------------------------------------}

roundtrip_ConvertRawHash
  :: (StandardHash blk, ConvertRawHash blk)
  => Proxy blk -> HeaderHash blk -> Property
roundtrip_ConvertRawHash p h =
    h === fromShortRawHash p (toShortRawHash p h)

prop_hashSize
  :: ConvertRawHash blk
  => Proxy blk -> HeaderHash blk -> Property
prop_hashSize p h =
    hashSize p === fromIntegral (Short.length (toShortRawHash p h))

{-------------------------------------------------------------------------------
  estimateBlockSize
-------------------------------------------------------------------------------}

prop_estimateBlockSize ::
     (SerialiseNodeToNodeConstraints blk, GetHeader blk)
  => CodecConfig blk
  -> WithVersion (BlockNodeToNodeVersion blk) blk
  -> Property
prop_estimateBlockSize ccfg (WithVersion version blk)
  | actualBlockSize > expectedBlockSize
  = counterexample
      ("actualBlockSize > expectedBlockSize: "
         <> show actualBlockSize <> " > "
         <> show expectedBlockSize)
      (property False)
  | actualBlockSize < expectedBlockSize - allowedUnderestimate
  = counterexample
      ("actualBlockSize < expectedBlockSize - allowedUnderestimate: "
         <> show actualBlockSize <> " > "
         <> show expectedBlockSize <> " - "
         <> show allowedUnderestimate)
      (property False)
  | otherwise
  = classify (actualBlockSize == expectedBlockSize) "exact"
  $ classify (actualBlockSize <  expectedBlockSize) "underestimate"
  $ property True
  where
    allowedUnderestimate :: SizeInBytes
    allowedUnderestimate = 10

    actualBlockSize :: SizeInBytes
    actualBlockSize =
          fromIntegral
        . Lazy.length
        . toLazyByteString
        . encodeNodeToNode ccfg version
        $ blk

    expectedBlockSize :: SizeInBytes
    expectedBlockSize =
          estimateBlockSize
        . getHeader
        $ blk

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
             => BlockQuery blk result -> result -> SomeResult blk

instance Show (SomeResult blk) where
  show (SomeResult _ result) = show result

instance Eq (SomeResult blk) where
  SomeResult _ (res1 :: result1) == SomeResult _ (res2 :: result2) =
    case eqT @result1 @result2 of
      Nothing   -> False
      Just Refl -> res1 == res2

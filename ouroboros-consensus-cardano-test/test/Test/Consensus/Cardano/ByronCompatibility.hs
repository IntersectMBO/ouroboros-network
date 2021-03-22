{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Cardano.ByronCompatibility (
    tests
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (Coercible, coerce)
import           Data.SOP.BasicFunctors

import qualified Cardano.Chain.Byron.API as CC

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator (NestedCtxt_ (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (..))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Byron.Generators ()

import           Test.Consensus.Cardano.Generators (epochSlots)
import           Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)

tests :: TestTree
tests = adjustOption reduceTests $
    testGroup "Byron compatibility" [
        testGroup "Byron to Cardano" [
              testProperty "roundtrip block" $
                roundtrip' @ByronToCardano
                  (encodeDisk byronToCardanoCodeConfig)
                  (decodeDisk byronToCardanoCodeConfig)
            , testGroup "SerialiseNodeToNode" $
                roundtrip_SerialiseNodeToNode   byronToCardanoCodeConfig
            , testGroup "SerialiseNodeToClient" $
                roundtrip_SerialiseNodeToClient byronToCardanoCodeConfig
            ]
      , testGroup "Cardano to Byron" [
              testProperty "roundtrip block" $
                roundtrip' @CardanoToByron
                  (encodeDisk cardanoToByronCodeConfig)
                  (decodeDisk cardanoToByronCodeConfig)
            , testGroup "SerialiseNodeToNode" $
                roundtrip_SerialiseNodeToNode   cardanoToByronCodeConfig
            , testGroup "SerialiseNodeToClient" $
                roundtrip_SerialiseNodeToClient cardanoToByronCodeConfig
            ]
      ]
  where
    -- | We're not trying to find edge cases in the roundtrip tests, we just
    -- want to check compatibility. In case of incompatibility, the first test
    -- will probably fail already.
    reduceTests (QuickCheckTests n) = QuickCheckTests (1 `max` (div n 10))

byronCodecConfig :: CodecConfig ByronBlock
byronCodecConfig = ByronCodecConfig epochSlots

byronToCardanoCodeConfig :: CodecConfig ByronToCardano
byronToCardanoCodeConfig = CodecConfigB2C byronCodecConfig

cardanoToByronCodeConfig :: CodecConfig CardanoToByron
cardanoToByronCodeConfig = CodecConfigC2B byronCodecConfig

{------------------------------------------------------------------------------
  Common setup
------------------------------------------------------------------------------}

-- | We don't use Shelley at all in this module, so we just pick some crypto
-- and use that everywhere.
type Crypto = MockCryptoCompatByron

byronNodeToNodeVersion :: BlockNodeToNodeVersion ByronBlock
byronNodeToNodeVersion = ByronNodeToNodeVersion1

byronNodeToClientVersion :: BlockNodeToClientVersion ByronBlock
byronNodeToClientVersion = ByronNodeToClientVersion1

cardanoNodeToNodeVersion :: BlockNodeToNodeVersion (CardanoBlock Crypto)
cardanoNodeToNodeVersion = CardanoNodeToNodeVersion1

cardanoNodeToClientVersion :: BlockNodeToClientVersion (CardanoBlock Crypto)
cardanoNodeToClientVersion = CardanoNodeToClientVersion1

pb :: Proxy ByronBlock
pb = Proxy

toCardanoCodecConfig ::
     CodecConfig ByronBlock
  -> CodecConfig (CardanoBlock Crypto)
toCardanoCodecConfig codecConfigByron =
    CardanoCodecConfig
      codecConfigByron
      ShelleyCodecConfig
      ShelleyCodecConfig
      ShelleyCodecConfig

{------------------------------------------------------------------------------
  Byron to Cardano
------------------------------------------------------------------------------}

-- | Encoded Byron values can be decoded as Cardano values in the following
-- cases:
--
-- * The @HardForkNodeTo(Node|Client)Disabled@ version is used
-- * Blocks and headers stored on disk
--
-- Note that ledger state and all other types stored as part of the ledger
-- snapshot are __not__ forwards compatible.
newtype ByronToCardano                       = B2C        { unB2C        ::         ByronBlock   } deriving (Eq, Show)
newtype instance Header ByronToCardano       = HeaderB2C  { unHeaderB2C  :: Header  ByronBlock   } deriving (Eq, Show)
newtype instance GenTx ByronToCardano        = GenTxB2C   { unGenTxB2C   :: GenTx   ByronBlock   } deriving (Eq, Show)
newtype instance TxId (GenTx ByronToCardano) = GenTxIdB2C { unGenTxIdB2C :: GenTxId ByronBlock   } deriving (Eq, Show)
newtype instance Query ByronToCardano a      = QueryB2C   { unQueryB2C   :: Query   ByronBlock a } deriving (Eq, Show)

newtype instance NestedCtxt_ ByronToCardano f a where
  NestedCtxt_B2C :: NestedCtxt_ ByronBlock     f a
                 -> NestedCtxt_ ByronToCardano f a

deriving instance Show (NestedCtxt_ ByronToCardano Header a)

unNestedCtxt_B2C :: NestedCtxt_ ByronToCardano f a -> NestedCtxt_ ByronBlock f a
unNestedCtxt_B2C (NestedCtxt_B2C ctxt) = ctxt

type instance HeaderHash ByronToCardano = HeaderHash ByronBlock
type instance ApplyTxErr ByronToCardano = ApplyTxErr ByronBlock

instance HasNetworkProtocolVersion ByronToCardano

instance ConvertRawHash ByronToCardano where
  toShortRawHash   _ = toShortRawHash   pb
  fromShortRawHash _ = fromShortRawHash pb
  hashSize         _ = hashSize         pb

data instance CodecConfig ByronToCardano = CodecConfigB2C (CodecConfig ByronBlock)

instance SameDepIndex (NestedCtxt_ ByronToCardano Header) where
  sameDepIndex (NestedCtxt_B2C ctxt1) (NestedCtxt_B2C ctxt2) =
      sameDepIndex ctxt1 ctxt2

instance HasNestedContent Header ByronToCardano where
  unnest hdr = case unnest (unHeaderB2C hdr) of
      DepPair ctxt a -> DepPair (mapNestedCtxt NestedCtxt_B2C ctxt) a
  nest (DepPair ctxt a) =
      HeaderB2C $ nest (DepPair (mapNestedCtxt unNestedCtxt_B2C ctxt) a)

instance ShowQuery (Query ByronToCardano) where
  showResult (QueryB2C query) = showResult query

instance SameDepIndex (Query ByronToCardano) where
  sameDepIndex (QueryB2C q1) (QueryB2C q2) = sameDepIndex q1 q2

{------------------------------------------------------------------------------
  Byron to Cardano: Disk
------------------------------------------------------------------------------}

encodeDiskB2C ::
     forall f byron b2c.
     ( EncodeDisk ByronBlock (f ByronBlock)
     , Coercible byron (f ByronBlock)
     )
  => Proxy f
  -> (b2c -> byron)
  -> CodecConfig ByronToCardano
  -> b2c
  -> Encoding
encodeDiskB2C _ toByron (CodecConfigB2C ccfg) x =
    encodeDisk ccfg (toByron' x)
  where
    toByron' :: b2c -> f ByronBlock
    toByron' = coerce . toByron

decodeDiskB2C ::
     forall f cardano b2c.
     ( DecodeDisk (CardanoBlock Crypto) (f (CardanoBlock Crypto))
     , Coercible cardano (f (CardanoBlock Crypto))
     )
  => Proxy f
  -> (cardano -> b2c)
  -> CodecConfig ByronToCardano
  -> forall s. Decoder s b2c
decodeDiskB2C _ fromCardano (CodecConfigB2C ccfg) =
    fromCardano' <$> decodeDisk (toCardanoCodecConfig ccfg)
  where
    fromCardano' :: f (CardanoBlock Crypto) -> b2c
    fromCardano' = fromCardano . coerce

instance EncodeDisk ByronToCardano ByronToCardano where
  encodeDisk = encodeDiskB2C (Proxy @I) unB2C

instance DecodeDisk ByronToCardano (Lazy.ByteString -> ByronToCardano) where
  decodeDisk = decodeDiskB2C
                 (Proxy @((->) Lazy.ByteString))
                 (fmap (\(BlockByron blk) -> B2C blk))

instance EncodeDiskDep (NestedCtxt Header) ByronToCardano where
  encodeDiskDep (CodecConfigB2C ccfg) =
      encodeDiskDep ccfg . mapNestedCtxt unNestedCtxt_B2C

instance DecodeDiskDep (NestedCtxt Header) ByronToCardano where
  decodeDiskDep (CodecConfigB2C ccfg) =
      decodeDiskDep (toCardanoCodecConfig ccfg) . mapNestedCtxt (NCZ . unNestedCtxt_B2C)

{------------------------------------------------------------------------------
  Byron to Cardano: NodeToNode
------------------------------------------------------------------------------}

encodeNodeToNodeB2C ::
     forall f byron b2c.
     ( SerialiseNodeToNode ByronBlock (f ByronBlock)
     , Coercible byron (f ByronBlock)
     )
  => Proxy f
  -> (b2c -> byron)
  -> CodecConfig ByronToCardano
  -> BlockNodeToNodeVersion ByronToCardano
  -> b2c
  -> Encoding
encodeNodeToNodeB2C _ toByron (CodecConfigB2C ccfg) () x =
    encodeNodeToNode ccfg byronNodeToNodeVersion (toByron' x)
  where
    toByron' :: b2c -> f ByronBlock
    toByron' = coerce . toByron

decodeNodeToNodeB2C ::
     forall f cardano b2c.
     ( SerialiseNodeToNode (CardanoBlock Crypto) (f (CardanoBlock Crypto))
     , Coercible cardano (f (CardanoBlock Crypto))
     )
  => Proxy f
  -> (cardano -> b2c)
  -> CodecConfig ByronToCardano
  -> BlockNodeToNodeVersion ByronToCardano
  -> forall s. Decoder s b2c
decodeNodeToNodeB2C _ fromCardano (CodecConfigB2C ccfg) () =
    fromCardano' <$>
      decodeNodeToNode (toCardanoCodecConfig ccfg) cardanoNodeToNodeVersion
  where
    fromCardano' :: f (CardanoBlock Crypto) -> b2c
    fromCardano' = fromCardano . coerce

instance SerialiseNodeToNode ByronToCardano ByronToCardano where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @I) unB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @I) (\(BlockByron blk) -> B2C blk)

instance SerialiseNodeToNode ByronToCardano (Serialised ByronToCardano) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @Serialised) id
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @Serialised) id

instance SerialiseNodeToNode ByronToCardano (SerialisedHeader ByronToCardano) where
  encodeNodeToNode = encodeNodeToNodeB2C
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader unNestedCtxt_B2C)
  decodeNodeToNode = decodeNodeToNodeB2C
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader (\(NCZ ctxt) -> NestedCtxt_B2C ctxt))

instance SerialiseNodeToNode ByronToCardano (Header ByronToCardano) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @Header) unHeaderB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @Header) (\(HeaderByron hdr) -> HeaderB2C hdr)

instance SerialiseNodeToNode ByronToCardano (GenTx ByronToCardano) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @GenTx) unGenTxB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @GenTx) (\(GenTxByron tx) -> GenTxB2C tx)

instance SerialiseNodeToNode ByronToCardano (GenTxId ByronToCardano) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @WrapGenTxId) unGenTxIdB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @WrapGenTxId) (\(GenTxIdByron txid) -> GenTxIdB2C txid)

instance SerialiseNodeToNodeConstraints ByronToCardano where
  estimateBlockSize = estimateBlockSize . unHeaderB2C

{------------------------------------------------------------------------------
  Byron to Cardano: NodeToClient
------------------------------------------------------------------------------}

encodeNodeToClientB2C ::
     forall f byron b2c.
     ( SerialiseNodeToClient ByronBlock (f ByronBlock)
     , Coercible byron (f ByronBlock)
     )
  => Proxy f
  -> (b2c -> byron)
  -> CodecConfig ByronToCardano
  -> BlockNodeToClientVersion ByronToCardano
  -> b2c
  -> Encoding
encodeNodeToClientB2C _ toByron (CodecConfigB2C ccfg) () x =
    encodeNodeToClient ccfg byronNodeToClientVersion (toByron' x)
  where
    toByron' :: b2c -> f ByronBlock
    toByron' = coerce . toByron

decodeNodeToClientB2C ::
     forall f cardano b2c.
     ( SerialiseNodeToClient (CardanoBlock Crypto) (f (CardanoBlock Crypto))
     , Coercible cardano (f (CardanoBlock Crypto))
     )
  => Proxy f
  -> (cardano -> b2c)
  -> CodecConfig ByronToCardano
  -> BlockNodeToClientVersion ByronToCardano
  -> forall s. Decoder s b2c
decodeNodeToClientB2C _ fromCardano (CodecConfigB2C ccfg) () =
    fromCardano' <$>
      decodeNodeToClient (toCardanoCodecConfig ccfg) cardanoNodeToClientVersion
  where
    fromCardano' :: f (CardanoBlock Crypto) -> b2c
    fromCardano' = fromCardano . coerce

instance SerialiseNodeToClient ByronToCardano ByronToCardano where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @I) unB2C
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @I) (\(BlockByron blk) -> B2C blk)

instance SerialiseNodeToClient ByronToCardano (Serialised ByronToCardano) where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @Serialised) id
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @Serialised) id

instance SerialiseNodeToClient ByronToCardano (GenTx ByronToCardano) where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @GenTx) unGenTxB2C
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @GenTx) (\(GenTxByron tx) -> GenTxB2C tx)

-- | @'ApplyTxErr' 'ByronToCardano'@
instance SerialiseNodeToClient ByronToCardano CC.ApplyMempoolPayloadErr where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @WrapApplyTxErr) id
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @WrapApplyTxErr) (\(ApplyTxErrByron err) -> err)

instance SerialiseNodeToClient ByronToCardano (SomeSecond Query ByronToCardano) where
  encodeNodeToClient = encodeNodeToClientB2C
                         (Proxy @(SomeSecond Query))
                         (\(SomeSecond q) -> SomeSecond (unQueryB2C q))
  decodeNodeToClient = decodeNodeToClientB2C
                         (Proxy @(SomeSecond Query))
                         (\(SomeSecond (QueryIfCurrentByron q)) -> SomeSecond (QueryB2C q))

instance SerialiseResult ByronToCardano (Query ByronToCardano) where
  encodeResult (CodecConfigB2C ccfg) () (QueryB2C q) r =
      encodeResult ccfg byronNodeToClientVersion q r
  decodeResult (CodecConfigB2C ccfg) () (QueryB2C (q :: Query ByronBlock result)) =
      (\(QueryResultSuccess r) -> r) <$>
        decodeResult
          (toCardanoCodecConfig ccfg)
          cardanoNodeToClientVersion
          (QueryIfCurrentByron q :: CardanoQuery
                                      Crypto
                                      (CardanoQueryResult Crypto result))

instance SerialiseNodeToClientConstraints ByronToCardano

{------------------------------------------------------------------------------
  Byron to Cardano: Arbitrary instances
------------------------------------------------------------------------------}

instance Arbitrary ByronToCardano where
  arbitrary = B2C <$> arbitrary

instance Arbitrary (Header ByronToCardano) where
  arbitrary = HeaderB2C <$> (arbitrary `suchThatMap` isRightVersion)
    where
      isRightVersion ::
           WithVersion ByronNodeToNodeVersion (Header ByronBlock)
        -> Maybe (Header ByronBlock)
      isRightVersion (WithVersion version hdr)
        | version == byronNodeToNodeVersion = Just hdr
        | otherwise                         = Nothing

instance Arbitrary (GenTx ByronToCardano) where
  arbitrary = GenTxB2C <$> arbitrary

instance Arbitrary (GenTxId ByronToCardano) where
  arbitrary = GenTxIdB2C <$> arbitrary

instance Arbitrary (SomeSecond Query ByronToCardano) where
  arbitrary = (\(SomeSecond q) -> SomeSecond (QueryB2C q)) <$> arbitrary

instance Arbitrary (SomeResult ByronToCardano) where
  arbitrary = (\(SomeResult q r) -> SomeResult (QueryB2C q) r) <$> arbitrary

{------------------------------------------------------------------------------
  Cardano to Byron
------------------------------------------------------------------------------}

-- | Encoded Cardano values can be decoded as Byron values in the following
-- cases:
--
-- * The @HardForkNodeTo(Node|Client)Disabled@ version is used
-- * Blocks and headers stored on disk
--
-- Note that ledger state and all other types stored as part of the ledger
-- snapshot are __not__ forwards compatible.
newtype CardanoToByron                       = C2B        { unC2B        ::         ByronBlock   } deriving (Eq, Show)
newtype instance Header CardanoToByron       = HeaderC2B  { unHeaderC2B  :: Header  ByronBlock   } deriving (Eq, Show)
newtype instance GenTx CardanoToByron        = GenTxC2B   { unGenTxC2B   :: GenTx   ByronBlock   } deriving (Eq, Show)
newtype instance TxId (GenTx CardanoToByron) = GenTxIdC2B { unGenTxIdC2B :: GenTxId ByronBlock   } deriving (Eq, Show)
newtype instance Query CardanoToByron a      = QueryC2B   { unQueryC2B   :: Query   ByronBlock a } deriving (Eq, Show)

newtype instance NestedCtxt_ CardanoToByron f a where
  NestedCtxt_C2B :: NestedCtxt_ ByronBlock     f a
                 -> NestedCtxt_ CardanoToByron f a

deriving instance Show (NestedCtxt_ CardanoToByron Header a)

unNestedCtxt_C2B :: NestedCtxt_ CardanoToByron f a -> NestedCtxt_ ByronBlock f a
unNestedCtxt_C2B (NestedCtxt_C2B ctxt) = ctxt

type instance HeaderHash CardanoToByron = HeaderHash ByronBlock
type instance ApplyTxErr CardanoToByron = ApplyTxErr ByronBlock

instance HasNetworkProtocolVersion CardanoToByron

instance ConvertRawHash CardanoToByron where
  toShortRawHash   _ = toShortRawHash   pb
  fromShortRawHash _ = fromShortRawHash pb
  hashSize         _ = hashSize         pb

data instance CodecConfig CardanoToByron = CodecConfigC2B (CodecConfig ByronBlock)

instance SameDepIndex (NestedCtxt_ CardanoToByron Header) where
  sameDepIndex (NestedCtxt_C2B ctxt1) (NestedCtxt_C2B ctxt2) =
      sameDepIndex ctxt1 ctxt2

instance HasNestedContent Header CardanoToByron where
  unnest hdr = case unnest (unHeaderC2B hdr) of
      DepPair ctxt a -> DepPair (mapNestedCtxt NestedCtxt_C2B ctxt) a
  nest (DepPair ctxt a) =
      HeaderC2B $ nest (DepPair (mapNestedCtxt unNestedCtxt_C2B ctxt) a)

instance ShowQuery (Query CardanoToByron) where
  showResult (QueryC2B query) = showResult query

instance SameDepIndex (Query CardanoToByron) where
  sameDepIndex (QueryC2B q1) (QueryC2B q2) = sameDepIndex q1 q2

{------------------------------------------------------------------------------
  Cardano to Byron: Disk
------------------------------------------------------------------------------}

encodeDiskC2B ::
     forall f cardano c2b.
     ( EncodeDisk (CardanoBlock Crypto) (f (CardanoBlock Crypto))
     , Coercible cardano (f (CardanoBlock Crypto))
     )
  => Proxy f
  -> (c2b -> cardano)
  -> CodecConfig CardanoToByron
  -> c2b
  -> Encoding
encodeDiskC2B _ toCardano (CodecConfigC2B ccfg) x =
    encodeDisk (toCardanoCodecConfig ccfg) (toCardano' x)
  where
    toCardano' :: c2b -> f (CardanoBlock Crypto)
    toCardano' = coerce . toCardano

decodeDiskC2B ::
     forall f byron c2b.
     ( DecodeDisk ByronBlock (f ByronBlock)
     , Coercible byron (f ByronBlock)
     )
  => Proxy f
  -> (byron -> c2b)
  -> CodecConfig CardanoToByron
  -> forall s. Decoder s c2b
decodeDiskC2B _ fromByron (CodecConfigC2B ccfg) =
    fromByron' <$> decodeDisk ccfg
  where
    fromByron' :: f ByronBlock -> c2b
    fromByron' = fromByron . coerce

instance EncodeDisk CardanoToByron CardanoToByron where
  encodeDisk = encodeDiskC2B (Proxy @I) (BlockByron . unC2B)

instance DecodeDisk CardanoToByron (Lazy.ByteString -> CardanoToByron) where
  decodeDisk = decodeDiskC2B (Proxy @((->) Lazy.ByteString)) (fmap C2B)

instance EncodeDiskDep (NestedCtxt Header) CardanoToByron where
  encodeDiskDep (CodecConfigC2B ccfg) =
      encodeDiskDep (toCardanoCodecConfig ccfg) . mapNestedCtxt (NCZ . unNestedCtxt_C2B)

instance DecodeDiskDep (NestedCtxt Header) CardanoToByron where
  decodeDiskDep (CodecConfigC2B ccfg) =
      decodeDiskDep ccfg . mapNestedCtxt unNestedCtxt_C2B

{------------------------------------------------------------------------------
  Cardano to Byron: NodeToNode
------------------------------------------------------------------------------}

encodeNodeToNodeC2B ::
     forall f cardano c2b.
     ( SerialiseNodeToNode (CardanoBlock Crypto) (f (CardanoBlock Crypto))
     , Coercible cardano (f (CardanoBlock Crypto))
     )
  => Proxy f
  -> (c2b -> cardano)
  -> CodecConfig CardanoToByron
  -> BlockNodeToNodeVersion CardanoToByron
  -> c2b
  -> Encoding
encodeNodeToNodeC2B _ toCardano (CodecConfigC2B ccfg) () x =
    encodeNodeToNode
      (toCardanoCodecConfig ccfg)
      cardanoNodeToNodeVersion
      (toCardano' x)
  where
    toCardano' :: c2b -> f (CardanoBlock Crypto)
    toCardano' = coerce . toCardano

decodeNodeToNodeC2B ::
     forall f byron c2b.
     ( SerialiseNodeToNode ByronBlock (f ByronBlock)
     , Coercible byron (f ByronBlock)
     )
  => Proxy f
  -> (byron -> c2b)
  -> CodecConfig CardanoToByron
  -> BlockNodeToNodeVersion CardanoToByron
  -> forall s. Decoder s c2b
decodeNodeToNodeC2B _ fromByron (CodecConfigC2B ccfg) () =
    fromByron' <$> decodeNodeToNode ccfg byronNodeToNodeVersion
  where
    fromByron' :: f ByronBlock -> c2b
    fromByron' = fromByron . coerce

instance SerialiseNodeToNode CardanoToByron CardanoToByron where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @I) (BlockByron . unC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @I) C2B

instance SerialiseNodeToNode CardanoToByron (Serialised CardanoToByron) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @Serialised) id
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @Serialised) id

instance SerialiseNodeToNode CardanoToByron (SerialisedHeader CardanoToByron) where
  encodeNodeToNode = encodeNodeToNodeC2B
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader (\(NestedCtxt_C2B ctxt) -> NCZ ctxt))
  decodeNodeToNode = decodeNodeToNodeC2B
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader NestedCtxt_C2B)

instance SerialiseNodeToNode CardanoToByron (Header CardanoToByron) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @Header) (HeaderByron . unHeaderC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @Header) HeaderC2B

instance SerialiseNodeToNode CardanoToByron (GenTx CardanoToByron) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @GenTx) (GenTxByron . unGenTxC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @GenTx) GenTxC2B

instance SerialiseNodeToNode CardanoToByron (GenTxId CardanoToByron) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @WrapGenTxId) (GenTxIdByron . unGenTxIdC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @WrapGenTxId) GenTxIdC2B

instance SerialiseNodeToNodeConstraints CardanoToByron where
  estimateBlockSize = estimateBlockSize . unHeaderC2B

{------------------------------------------------------------------------------
  Cardano to Byron: NodeToClient
------------------------------------------------------------------------------}

encodeNodeToClientC2B ::
     forall f cardano c2b.
     ( SerialiseNodeToClient (CardanoBlock Crypto) (f (CardanoBlock Crypto))
     , Coercible cardano (f (CardanoBlock Crypto))
     )
  => Proxy f
  -> (c2b -> cardano)
  -> CodecConfig CardanoToByron
  -> BlockNodeToClientVersion CardanoToByron
  -> c2b
  -> Encoding
encodeNodeToClientC2B _ toCardano (CodecConfigC2B ccfg) () x =
    encodeNodeToClient
      (toCardanoCodecConfig ccfg)
      cardanoNodeToClientVersion
      (toCardano' x)
  where
    toCardano' :: c2b -> f (CardanoBlock Crypto)
    toCardano' = coerce . toCardano

decodeNodeToClientC2B ::
     forall f byron c2b.
     ( SerialiseNodeToClient ByronBlock (f ByronBlock)
     , Coercible byron (f ByronBlock)
     )
  => Proxy f
  -> (byron -> c2b)
  -> CodecConfig CardanoToByron
  -> BlockNodeToClientVersion CardanoToByron
  -> forall s. Decoder s c2b
decodeNodeToClientC2B _ fromByron (CodecConfigC2B ccfg) () =
    fromByron' <$> decodeNodeToClient ccfg byronNodeToClientVersion
  where
    fromByron' :: f ByronBlock -> c2b
    fromByron' = fromByron . coerce

instance SerialiseNodeToClient CardanoToByron CardanoToByron where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @I) (BlockByron . unC2B)
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @I) C2B

instance SerialiseNodeToClient CardanoToByron (Serialised CardanoToByron) where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @Serialised) id
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @Serialised) id

instance SerialiseNodeToClient CardanoToByron (GenTx CardanoToByron) where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @GenTx) (GenTxByron . unGenTxC2B)
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @GenTx) GenTxC2B

-- | @'ApplyTxErr' 'CardanoToByron'@
instance SerialiseNodeToClient CardanoToByron CC.ApplyMempoolPayloadErr where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @WrapApplyTxErr) ApplyTxErrByron
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @WrapApplyTxErr) id

instance SerialiseNodeToClient CardanoToByron (SomeSecond Query CardanoToByron) where
  encodeNodeToClient =
      encodeNodeToClientC2B
        (Proxy @(SomeSecond Query))
        (\(SomeSecond q) -> SomeSecond (QueryIfCurrentByron (unQueryC2B q)))
  decodeNodeToClient =
      decodeNodeToClientC2B
        (Proxy @(SomeSecond Query))
        (\(SomeSecond q) -> SomeSecond (QueryC2B q))

instance SerialiseResult CardanoToByron (Query CardanoToByron) where
  encodeResult (CodecConfigC2B ccfg) () (QueryC2B q) (r :: result) =
      encodeResult
        (toCardanoCodecConfig ccfg)
        cardanoNodeToClientVersion
        (QueryIfCurrentByron q)
        (QueryResultSuccess r :: CardanoQueryResult Crypto result)
  decodeResult (CodecConfigC2B ccfg) () (QueryC2B q) =
      decodeResult ccfg byronNodeToClientVersion q

instance SerialiseNodeToClientConstraints CardanoToByron

{------------------------------------------------------------------------------
  Cardano to Byron: Arbitrary instances
------------------------------------------------------------------------------}

instance Arbitrary CardanoToByron where
  arbitrary = C2B <$> arbitrary

instance Arbitrary (Header CardanoToByron) where
  arbitrary = HeaderC2B <$> (arbitrary `suchThatMap` isRightVersion)
    where
      isRightVersion ::
           WithVersion ByronNodeToNodeVersion (Header ByronBlock)
        -> Maybe (Header ByronBlock)
      isRightVersion (WithVersion version hdr)
        | version == byronNodeToNodeVersion = Just hdr
        | otherwise                         = Nothing

instance Arbitrary (GenTx CardanoToByron) where
  arbitrary = GenTxC2B <$> arbitrary

instance Arbitrary (GenTxId CardanoToByron) where
  arbitrary = GenTxIdC2B <$> arbitrary

instance Arbitrary (SomeSecond Query CardanoToByron) where
  arbitrary = (\(SomeSecond q) -> SomeSecond (QueryC2B q)) <$> arbitrary

instance Arbitrary (SomeResult CardanoToByron) where
  arbitrary = (\(SomeResult q r) -> SomeResult (QueryC2B q) r) <$> arbitrary

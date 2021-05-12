{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common (
    -- * Conditions required by the HFC to support serialisation
    HardForkEncoderException (..)
  , SerialiseConstraintsHFC
  , SerialiseHFC (..)
  , disabledEraException
  , futureEraException
  , pSHFC
    -- * Distinguish first era from the rest
  , FirstEra
  , LaterEra
  , isFirstEra
  , notFirstEra
    -- * Versioning
  , EraNodeToClientVersion (..)
  , EraNodeToNodeVersion (..)
  , HardForkNodeToClientVersion (..)
  , HardForkNodeToNodeVersion (..)
  , HardForkSpecificNodeToClientVersion (..)
  , HardForkSpecificNodeToNodeVersion (..)
  , isHardForkNodeToClientEnabled
  , isHardForkNodeToNodeEnabled
    -- * Dealing with annotations
  , AnnDecoder (..)
    -- * Serialisation of telescopes
  , decodeTelescope
  , encodeTelescope
    -- * Serialisation of sums
  , decodeAnnNS
  , decodeNS
  , encodeNS
    -- * Dependent serialisation
  , decodeNested
  , decodeNestedCtxt
  , encodeNested
  , encodeNestedCtxt
    -- * MismatchEraInfo
  , decodeEitherMismatch
  , encodeEitherMismatch
    -- * Distributive properties
  , distribAnnTip
  , distribQueryIfCurrent
  , distribSerialisedHeader
  , undistribAnnTip
  , undistribQueryIfCurrent
  , undistribSerialisedHeader
    -- * Deriving-via support for tests
  , SerialiseNS (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Control.Exception (Exception, throw)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.Kind (Type)
import           Data.SOP.Strict
import           Data.Word

import           Cardano.Binary (enforceSize)

import           Ouroboros.Network.Block (Serialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.State
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (SimpleTelescope (..), Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Distinguish between the first era and all others
-------------------------------------------------------------------------------}

type family FirstEra (xs :: [Type]) where
  FirstEra (x ': xs) = x

type family LaterEra (xs :: [Type]) where
  LaterEra (x ': xs) = xs

isFirstEra :: forall f xs. All SingleEraBlock xs
           => NS f xs
           -> Either (NS SingleEraInfo (LaterEra xs)) (f (FirstEra xs))
isFirstEra (Z x) = Right x
isFirstEra (S x) = Left (hcmap proxySingle aux x)
  where
    aux :: forall blk. SingleEraBlock blk => f blk -> SingleEraInfo blk
    aux _ = singleEraInfo (Proxy @blk)

-- | Used to construct 'FutureEraException'
notFirstEra :: All SingleEraBlock xs
            => NS f xs -- ^ 'NS' intended to be from a future era
            -> NS SingleEraInfo xs
notFirstEra = hcmap proxySingle aux
  where
    aux :: forall f blk. SingleEraBlock blk => f blk -> SingleEraInfo blk
    aux _ = singleEraInfo (Proxy @blk)

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

-- | Versioning of the specific additions made by the HFC to the @NodeToNode@
-- protocols, e.g., the era tag.
data HardForkSpecificNodeToNodeVersion =
    HardForkSpecificNodeToNodeVersion1
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Versioning of the specific additions made by the HFC to the @NodeToClient@
-- protocols, e.g., the era tag or the hard-fork specific queries.
data HardForkSpecificNodeToClientVersion =
    HardForkSpecificNodeToClientVersion1

    -- | Enable the 'GetCurrentEra' query in 'QueryHardFork'.
  | HardForkSpecificNodeToClientVersion2
  deriving (Eq, Ord, Show, Enum, Bounded)

data HardForkNodeToNodeVersion xs where
  -- | Disable the HFC
  --
  -- This means that only the first era (@x@) is supported, and moreover, is
  -- compatible with serialisation used if the HFC would not be present at all.
  HardForkNodeToNodeDisabled ::
       BlockNodeToNodeVersion x
    -> HardForkNodeToNodeVersion (x ': xs)

  -- | Enable the HFC
  --
  -- Each era can be enabled or disabled individually by passing
  -- 'EraNodeToNodeDisabled' as its configuration, but serialised values will
  -- always include tags inserted by the HFC to distinguish one era from
  -- another. We also version the hard-fork specific parts with
  -- 'HardForkSpecificNodeToNodeVersion'.
  HardForkNodeToNodeEnabled ::
       HardForkSpecificNodeToNodeVersion
    -> NP EraNodeToNodeVersion xs
    -> HardForkNodeToNodeVersion xs

data HardForkNodeToClientVersion xs where
  -- | Disable the HFC
  --
  -- See 'HardForkNodeToNodeDisabled'
  HardForkNodeToClientDisabled ::
       BlockNodeToClientVersion x
    -> HardForkNodeToClientVersion (x ': xs)

  -- | Enable the HFC
  --
  -- See 'HardForkNodeToNodeEnabled'
  HardForkNodeToClientEnabled ::
       HardForkSpecificNodeToClientVersion
    -> NP EraNodeToClientVersion xs
    -> HardForkNodeToClientVersion xs

data EraNodeToNodeVersion blk =
    EraNodeToNodeEnabled !(BlockNodeToNodeVersion blk)
  | EraNodeToNodeDisabled

data EraNodeToClientVersion blk =
    EraNodeToClientEnabled !(BlockNodeToClientVersion blk)
  | EraNodeToClientDisabled

deriving instance Show (BlockNodeToNodeVersion   blk) => Show (EraNodeToNodeVersion   blk)
deriving instance Show (BlockNodeToClientVersion blk) => Show (EraNodeToClientVersion blk)

deriving instance Eq (BlockNodeToNodeVersion   blk) => Eq (EraNodeToNodeVersion   blk)
deriving instance Eq (BlockNodeToClientVersion blk) => Eq (EraNodeToClientVersion blk)

deriving instance SerialiseHFC xs => Show (HardForkNodeToNodeVersion xs)
deriving instance SerialiseHFC xs => Show (HardForkNodeToClientVersion xs)

deriving instance SerialiseHFC xs => Eq (HardForkNodeToNodeVersion xs)
deriving instance SerialiseHFC xs => Eq (HardForkNodeToClientVersion xs)

instance SerialiseHFC xs => HasNetworkProtocolVersion (HardForkBlock xs) where
  type BlockNodeToNodeVersion   (HardForkBlock xs) = HardForkNodeToNodeVersion   xs
  type BlockNodeToClientVersion (HardForkBlock xs) = HardForkNodeToClientVersion xs

isHardForkNodeToNodeEnabled :: HardForkNodeToNodeVersion xs -> Bool
isHardForkNodeToNodeEnabled HardForkNodeToNodeEnabled {} = True
isHardForkNodeToNodeEnabled _                            = False

isHardForkNodeToClientEnabled :: HardForkNodeToClientVersion xs -> Bool
isHardForkNodeToClientEnabled HardForkNodeToClientEnabled {} = True
isHardForkNodeToClientEnabled _                              = False

{-------------------------------------------------------------------------------
  Conditions required by the HFC to support serialisation
-------------------------------------------------------------------------------}

class ( SingleEraBlock                   blk
      , SerialiseDiskConstraints         blk
      , SerialiseNodeToNodeConstraints   blk
      , SerialiseNodeToClientConstraints blk
      , HasNetworkProtocolVersion        blk
      ) => SerialiseConstraintsHFC       blk

pSHFC :: Proxy SerialiseConstraintsHFC
pSHFC = Proxy

-- | Conditions required by the HFC to provide serialisation
--
-- NOTE: Compatibility between HFC enabled and disabled:
--
-- 1. Node-to-node and node-to-client communication is versioned. When the HFC
--    is disabled, we default to the instances for the first era, and so
--    compatibility is preserved by construction.
--
-- 2. On-disk storage is /not/ versioned, and here we make no attempt to be
--    compatible between non-HFC and HFC deployments, /except/ for blocks: we
--    define two methods 'encodeDiskHfcBlock' and 'decodeDiskHfcBlock' which
--    are used for on-disk serialisation of blocks. These methods have
--    defaults which can and probably should be used for deployments that use
--    the HFC from the get-go, but for deployments that only later change to use
--    the HFC these functions can be overriden to provide an on-disk storage
--    format for HFC blocks that is compatible with the on-disk storage of
--    blocks from the first era.
--
-- 3. The converse is NOT supported. Deployments that use the HFC from the start
--    should not use 'HardForkNodeToNodeDisabled' and/or
--    'HardForkNodeToClientDisabled'. Doing so would result in opposite
--    compatibility problems: the on-disk block would include the HFC tag, but
--    sending blocks with the HFC disabled suggests that that tag is unexpected.
--    This would then lead to problems with binary streaming, and we do not
--    currently provide any provisions to resolve these.
class ( CanHardFork xs
      , All SerialiseConstraintsHFC xs
        -- Required for HasNetworkProtocolVersion
      , All (Compose Show EraNodeToNodeVersion)   xs
      , All (Compose Eq   EraNodeToNodeVersion)   xs
      , All (Compose Show EraNodeToClientVersion) xs
      , All (Compose Eq   EraNodeToClientVersion) xs
        -- Required for 'encodeNestedCtxt'/'decodeNestedCtxt'
      , All (EncodeDiskDepIx (NestedCtxt Header)) xs
      , All (DecodeDiskDepIx (NestedCtxt Header)) xs
        -- Required for 'getHfcBinaryBlockInfo'
      , All HasBinaryBlockInfo xs
      ) => SerialiseHFC xs where

  encodeDiskHfcBlock :: CodecConfig (HardForkBlock xs)
                     -> HardForkBlock xs -> Encoding
  encodeDiskHfcBlock cfg =
        encodeNS (hcmap pSHFC (fn . mapIK . encodeDisk) cfgs)
      . (getOneEraBlock . getHardForkBlock)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

  decodeDiskHfcBlock :: CodecConfig (HardForkBlock xs)
                     -> forall s. Decoder s (Lazy.ByteString -> HardForkBlock xs)
  decodeDiskHfcBlock cfg =
        fmap (\f -> HardForkBlock . OneEraBlock . f)
      $ decodeAnnNS (hcmap pSHFC aux cfgs)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

      aux :: SerialiseDiskConstraints blk
          => CodecConfig blk -> AnnDecoder I blk
      aux cfg' = AnnDecoder $ (\f -> I . f) <$> decodeDisk cfg'

  -- | Used as the implementation of 'reconstructPrefixLen' for
  -- 'HardForkBlock'.
  reconstructHfcPrefixLen :: proxy (Header (HardForkBlock xs)) -> PrefixLen
  reconstructHfcPrefixLen _ =
      -- We insert two bytes at the front
      2 `addPrefixLen` maximum (hcollapse perEra)
    where
      perEra :: NP (K PrefixLen) xs
      perEra = hcpure proxySingle reconstructOne

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K PrefixLen blk
      reconstructOne = K $ reconstructPrefixLen (Proxy @(Header blk))

  -- | Used as the implementation of 'reconstructNestedCtxt' for
  -- 'HardForkBlock'.
  reconstructHfcNestedCtxt ::
       proxy (Header (HardForkBlock xs))
    -> ShortByteString  -- ^ First bytes ('reconstructPrefixLen') of the block
    -> SizeInBytes      -- ^ Block size
    -> SomeSecond (NestedCtxt Header) (HardForkBlock xs)
  reconstructHfcNestedCtxt _ prefix blockSize =
     case nsFromIndex tag of
       Nothing -> error $ "invalid HardForkBlock with tag: " <> show tag
       Just ns -> injSomeSecond $ hcmap proxySingle reconstructOne ns
    where
      tag :: Word8
      tag = Short.index prefix 1

      prefixOne :: ShortByteString
      prefixOne = Short.pack . drop 2 . Short.unpack $ prefix

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K () blk -> SomeSecond (NestedCtxt Header) blk
      reconstructOne _ =
          reconstructNestedCtxt (Proxy @(Header blk)) prefixOne blockSize

      injSomeSecond :: NS (SomeSecond (NestedCtxt Header)) xs'
                   -> SomeSecond (NestedCtxt Header) (HardForkBlock xs')
      injSomeSecond (Z x) = case x of
          SomeSecond (NestedCtxt y) -> SomeSecond (NestedCtxt (NCZ y))
      injSomeSecond (S x) = case injSomeSecond x of
          SomeSecond (NestedCtxt y) -> SomeSecond (NestedCtxt (NCS y))

  -- | Used as the implementation of 'getBinaryBlockInfo' for
  -- 'HardForkBlock'.
  getHfcBinaryBlockInfo :: HardForkBlock xs -> BinaryBlockInfo
  getHfcBinaryBlockInfo (HardForkBlock (OneEraBlock bs)) =
      hcollapse $ hcmap (Proxy @HasBinaryBlockInfo) aux bs
    where
      -- The header is unchanged, but the whole block is offset by 2 bytes
      -- (list length and tag)
      aux :: HasBinaryBlockInfo blk => I blk -> K BinaryBlockInfo blk
      aux (I blk) = K $ BinaryBlockInfo {
            headerOffset = headerOffset underlyingBlockInfo + 2
          , headerSize   = headerSize   underlyingBlockInfo
          }
        where
          underlyingBlockInfo :: BinaryBlockInfo
          underlyingBlockInfo = getBinaryBlockInfo blk

  -- | Used as the implementation of 'estimateBlockSize' for 'HardForkBlock'.
  estimateHfcBlockSize :: Header (HardForkBlock xs) -> SizeInBytes
  estimateHfcBlockSize =
        (+ 2) -- Account for the era wrapper
      . hcollapse
      . hcmap (Proxy @SerialiseConstraintsHFC) (K . estimateBlockSize)
      . getOneEraHeader
      . getHardForkHeader

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Exception thrown in the HFC encoders
data HardForkEncoderException where
  -- | HFC disabled, but we saw a value from an era other than the first
  HardForkEncoderFutureEra :: SingleEraInfo blk -> HardForkEncoderException

  -- | HFC enabled, but we saw a value from a disabled era
  HardForkEncoderDisabledEra :: SingleEraInfo blk -> HardForkEncoderException

  -- | HFC disabled, but we saw a query that is only supported by the HFC
  HardForkEncoderQueryHfcDisabled :: HardForkEncoderException

  -- | HFC enabled, but we saw a HFC query that is not supported by the
  -- HFC-specific version used
  HardForkEncoderQueryWrongVersion :: HardForkEncoderException

deriving instance Show HardForkEncoderException
instance Exception HardForkEncoderException

futureEraException ::
     SListI xs
  => NS SingleEraInfo xs
  -> HardForkEncoderException
futureEraException = hcollapse . hmap (K . HardForkEncoderFutureEra)

disabledEraException ::
     forall blk. SingleEraBlock blk
  => Proxy blk
  -> HardForkEncoderException
disabledEraException = HardForkEncoderDisabledEra . singleEraInfo

{-------------------------------------------------------------------------------
  Dealing with annotations
-------------------------------------------------------------------------------}

data AnnDecoder f blk = AnnDecoder {
      annDecoder :: forall s. Decoder s (Lazy.ByteString -> f blk)
    }

{-------------------------------------------------------------------------------
  Serialisation of telescopes
-------------------------------------------------------------------------------}

encodeTelescope :: SListI xs
                => NP (f -.-> K Encoding) xs -> HardForkState f xs -> Encoding
encodeTelescope es (HardForkState st) = mconcat [
      Enc.encodeListLen (1 + fromIntegral ix)
    , mconcat $ hcollapse $ SimpleTelescope $
        (Telescope.bihzipWith (const encPast) encCurrent es st)
    ]
  where
    -- The tip of the telescope also tells us the length
    ix :: Word8
    ix = nsToIndex (Telescope.tip st)

    encPast :: K Past blk -> K Encoding blk
    encPast = K . encodePast . unK

    encCurrent :: (f -.-> K Encoding) blk -> Current f blk  -> K Encoding blk
    encCurrent enc = K . encodeCurrent (unK . apFn enc)

decodeTelescope :: NP (Decoder s :.: f) xs -> Decoder s (HardForkState f xs)
decodeTelescope = \ds -> do
    ix <- Dec.decodeListLen
    if ix < 1
      then fail $ "decodeTelescope: invalid telescope length " ++ show ix
      else HardForkState <$> go (ix - 1) ds
  where
    go :: Int
       -> NP (Decoder s :.: f) xs
       -> Decoder s (Telescope (K Past) (Current f) xs)
    go 0 (Comp d :* _)  = TZ <$> decodeCurrent d
    go i (Comp _ :* ds) = TS <$> (K <$> decodePast) <*> go (i - 1) ds
    go _ Nil            = error "decodeTelescope: invalid telescope length"

{-------------------------------------------------------------------------------
  Serialisation of sums
-------------------------------------------------------------------------------}

encodeNS :: SListI xs => NP (f -.-> K Encoding) xs -> NS f xs -> Encoding
encodeNS es ns = mconcat [
      Enc.encodeListLen 2
    , Enc.encodeWord8 $ nsToIndex ns
    , hcollapse $ hzipWith apFn es ns
    ]

decodeNS :: SListI xs => NP (Decoder s :.: f) xs -> Decoder s (NS f xs)
decodeNS ds = do
    enforceSize "decodeNS" 2
    i <- Dec.decodeWord8
    case nsFromIndex i of
      Nothing -> fail $ "decodeNS: invalid index " ++ show i
      Just ns -> hcollapse $ hizipWith aux ds ns
  where
    aux :: Index xs blk
        -> (Decoder s :.: f) blk
        -> K () blk
        -> K (Decoder s (NS f xs)) blk
    aux index (Comp dec) (K ()) = K $ injectNS index <$> dec

decodeAnnNS :: SListI xs
            => NP (AnnDecoder f) xs
            -> forall s. Decoder s (Lazy.ByteString -> NS f xs)
decodeAnnNS ds = do
    enforceSize "decodeDiskAnnNS" 2
    i <- Dec.decodeWord8
    case nsFromIndex i of
      Nothing -> fail $ "decodeAnnNS: invalid index " ++ show i
      Just ns -> hcollapse $ hizipWith aux ds ns
  where
    aux :: Index xs blk
        -> AnnDecoder f blk
        -> K () blk
        -> K (Decoder s (Lazy.ByteString -> NS f xs)) blk
    aux index (AnnDecoder dec) (K ()) = K $ (injectNS index .) <$> dec

{-------------------------------------------------------------------------------
  Dependent serialisation
-------------------------------------------------------------------------------}

encodeNested :: All (EncodeDiskDep (NestedCtxt f)) xs
             => CodecConfig (HardForkBlock xs)
             -> NestedCtxt f (HardForkBlock xs) a
             -> a
             -> Encoding
encodeNested = \ccfg (NestedCtxt ctxt) a ->
    go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ctxt a
  where
    go :: All (EncodeDiskDep (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NestedCtxt_ (HardForkBlock xs') f a
       -> a -> Encoding
    go Nil       ctxt       = case ctxt of {}
    go (c :* _)  (NCZ ctxt) = encodeDiskDep c (NestedCtxt ctxt)
    go (_ :* cs) (NCS ctxt) = go cs ctxt

decodeNested :: All (DecodeDiskDep (NestedCtxt f)) xs
             => CodecConfig (HardForkBlock xs)
             -> NestedCtxt f (HardForkBlock xs) a
             -> forall s. Decoder s (Lazy.ByteString -> a)
decodeNested = \ccfg (NestedCtxt ctxt) ->
    go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ctxt
  where
    go :: All (DecodeDiskDep (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NestedCtxt_ (HardForkBlock xs') f a
       -> Decoder s (Lazy.ByteString -> a)
    go Nil       ctxt       = case ctxt of {}
    go (c :* _)  (NCZ ctxt) = decodeDiskDep c (NestedCtxt ctxt)
    go (_ :* cs) (NCS ctxt) = go cs ctxt

encodeNestedCtxt :: All (EncodeDiskDepIx (NestedCtxt f)) xs
                 => CodecConfig (HardForkBlock xs)
                 -> SomeSecond (NestedCtxt f) (HardForkBlock xs)
                 -> Encoding
encodeNestedCtxt = \ccfg (SomeSecond ctxt) ->
    go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg))
       npWithIndices
       (flipNestedCtxt ctxt)
  where
    go :: All (EncodeDiskDepIx (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NP (K Word8) xs'
       -> NestedCtxt_ (HardForkBlock xs') f a
       -> Encoding
    go Nil       _           ctxt       = case ctxt of {}
    go (_ :* cs) (_   :* is) (NCS ctxt) = go cs is ctxt
    go (c :* _)  (K i :* _)  (NCZ ctxt) = mconcat [
          Enc.encodeListLen 2
        , Serialise.encode i
        , encodeDiskDepIx c (SomeSecond (NestedCtxt ctxt))
        ]

decodeNestedCtxt :: All (DecodeDiskDepIx (NestedCtxt f)) xs
                 => CodecConfig (HardForkBlock xs)
                 -> forall s. Decoder s (SomeSecond (NestedCtxt f) (HardForkBlock xs))
decodeNestedCtxt = \ccfg -> do
    enforceSize "decodeNestedCtxt" 2
    tag <- Serialise.decode
    case nsFromIndex tag of
      Nothing -> fail $ "decodeNestedCtxt: invalid tag " ++ show tag
      Just ns ->
        go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ns
  where
    go :: All (DecodeDiskDepIx (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NS (K ()) xs'
       -> forall s. Decoder s (SomeSecond (NestedCtxt f) (HardForkBlock xs'))
    go Nil       i     = case i of {}
    go (c :* _)  (Z _) = mapSomeNestedCtxt NCZ <$> decodeDiskDepIx c
    go (_ :* cs) (S i) = mapSomeNestedCtxt NCS <$> go cs i

{-------------------------------------------------------------------------------
  Serialisation of 'MismatchEraInfo'

  We have to be careful here not to introduce any additional wrapping when
  using 'HardForkNodeToClientDisabled'.
-------------------------------------------------------------------------------}

encodeEitherMismatch :: forall xs a. SListI xs
                     => BlockNodeToClientVersion (HardForkBlock xs)
                     -> (a -> Encoding)
                     -> (Either (MismatchEraInfo xs) a -> Encoding)
encodeEitherMismatch version enc ma =
    case (version, ma) of
      (HardForkNodeToClientDisabled {}, Right a) ->
          enc a
      (HardForkNodeToClientDisabled {}, Left err) ->
          throw $ futureEraException (mismatchFutureEra err)
      (HardForkNodeToClientEnabled {}, Right a) -> mconcat [
            Enc.encodeListLen 1
          , enc a
          ]
      (HardForkNodeToClientEnabled {}, Left (MismatchEraInfo err)) -> mconcat [
            Enc.encodeListLen 2
          , encodeNS (hpure (fn encodeName)) era1
          , encodeNS (hpure (fn (encodeName . getLedgerEraInfo))) era2
          ]
        where
          era1 :: NS SingleEraInfo xs
          era2 :: NS LedgerEraInfo xs
          (era1, era2) = Match.mismatchToNS err
  where
    encodeName :: SingleEraInfo blk -> K Encoding blk
    encodeName = K . Serialise.encode . singleEraName

decodeEitherMismatch :: SListI xs
                     => BlockNodeToClientVersion (HardForkBlock xs)
                     -> Decoder s a
                     -> Decoder s (Either (MismatchEraInfo xs) a)
decodeEitherMismatch version dec =
    case version of
      HardForkNodeToClientDisabled {} ->
        Right <$> dec
      HardForkNodeToClientEnabled {} -> do
        tag <- Dec.decodeListLen
        case tag of
          1 -> Right <$> dec
          2 -> do era1 <- decodeNS (hpure (Comp decodeName))
                  era2 <- decodeNS (hpure (Comp (LedgerEraInfo <$> decodeName)))
                  case Match.matchNS era1 era2 of
                    Left err -> return $ Left (MismatchEraInfo err)
                    Right _  -> fail "dispatchDecoderErr: unexpected match"
          _ -> fail $ "dispatchDecoderErr: invalid tag " ++ show tag
  where
    decodeName :: forall blk s. Decoder s (SingleEraInfo blk)
    decodeName = SingleEraInfo <$> Serialise.decode

{-------------------------------------------------------------------------------
  Distributive properties
-------------------------------------------------------------------------------}

distribSerialisedHeader :: SerialisedHeader (HardForkBlock xs)
                        -> NS SerialisedHeader xs
distribSerialisedHeader = \hdr ->
    case serialisedHeaderToDepPair hdr of
      GenDepPair (NestedCtxt ctxt) bs ->
        go ctxt bs
  where
    go :: NestedCtxt_ (HardForkBlock xs) Header a
       -> Serialised a
       -> NS SerialisedHeader xs
    go (NCZ c) = Z . SerialisedHeaderFromDepPair . GenDepPair (NestedCtxt c)
    go (NCS c) = S . go c

undistribSerialisedHeader :: NS SerialisedHeader xs
                          -> SerialisedHeader (HardForkBlock xs)
undistribSerialisedHeader =
    SerialisedHeaderFromDepPair . go
  where
    go :: NS SerialisedHeader xs
       -> GenDepPair Serialised (NestedCtxt Header (HardForkBlock xs))
    go (Z (SerialisedHeaderFromDepPair (GenDepPair (NestedCtxt c) bs))) =
        GenDepPair (NestedCtxt (NCZ c)) bs
    go (S bs) =
        depPairFirst (mapNestedCtxt NCS) $ go bs

distribQueryIfCurrent ::
     Some (QueryIfCurrent xs)
  -> NS (SomeSecond BlockQuery) xs
distribQueryIfCurrent = \(Some qry) -> go qry
  where
    go :: QueryIfCurrent xs result -> NS (SomeSecond BlockQuery) xs
    go (QZ qry) = Z (SomeSecond qry)
    go (QS qry) = S (go qry)

undistribQueryIfCurrent ::
     NS (SomeSecond BlockQuery) xs
  -> Some (QueryIfCurrent xs)
undistribQueryIfCurrent = go
  where
    go :: NS (SomeSecond BlockQuery) xs -> Some (QueryIfCurrent xs)
    go (Z qry) = case qry of
                   SomeSecond qry' ->
                     Some (QZ qry')
    go (S qry) = case go qry of
                   Some qry' ->
                     Some (QS qry')

{-------------------------------------------------------------------------------
  Deriving-via support

  This is primarily for the benefit of tests, and depends only on 'Serialise'
  (rather than 'SerialiseDisk'/'SerialiseNodeToNode'/'SerialiseNodeToClient').
-------------------------------------------------------------------------------}

-- | Used for deriving via
--
-- Example
--
-- > deriving via SerialiseNS Header SomeEras
-- >          instance Serialise (Header SomeSecond)
newtype SerialiseNS f xs = SerialiseNS {
      getSerialiseNS :: NS f xs
    }

instance All (Compose Serialise f) xs => Serialise (SerialiseNS f xs) where
  encode = encodeNS (hcpure (Proxy @(Compose Serialise f))
                            (fn (K . Serialise.encode)))
         . getSerialiseNS

  decode = fmap SerialiseNS
         $ decodeNS (hcpure (Proxy @(Compose Serialise f))
                            (Comp Serialise.decode))

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Abstract view over blocks
--
-- The network layer does not make any concrete assumptions about what blocks
-- look like.
module Ouroboros.Network.Block
  ( SlotNo (..)
  , BlockNo (..)
  , HeaderHash
  , HeaderFields (..)
  , castHeaderFields
  , HasHeader (..)
  , blockNo
  , blockSlot
  , blockHash
  , HasFullHeader (..)
  , StandardHash
  , ChainHash (..)
  , castHash
  , Point (..)
  , pointSlot
  , pointHash
  , castPoint
  , blockPoint
  , pattern GenesisPoint
  , pattern BlockPoint
  , atSlot
  , withHash
  , Tip (..)
  , castTip
  , getTipPoint
  , getTipBlockNo
  , getTipSlotNo
  , getLegacyTipBlockNo
  , tipFromHeader
  , legacyTip
  , toLegacyTip
  , encodeTip
  , encodedTipSize
  , decodeTip
  , ChainUpdate (..)
  , MaxSlotNo (..)
  , maxSlotNoFromMaybe
  , maxSlotNoToMaybe
  , maxSlotNoFromWithOrigin
  , genesisPoint
    -- * Serialisation
  , encodePoint
  , encodedPointSize
  , encodeChainHash
  , decodePoint
  , decodeChainHash
    -- * Serialised block/header
  , Serialised (..)
  , wrapCBORinCBOR
  , unwrapCBORinCBOR
  , mkSerialised
  , fromSerialised
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import qualified Codec.CBOR.Read as Read
import qualified Codec.CBOR.Write as Write
import           Codec.Serialise (Serialise (..))
import           Control.Monad (when)
import           Control.Tracer (contramap)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.Coerce (Coercible, coerce)
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (Case (..), Size, szCases, szGreedy)
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Network.Point (WithOrigin (..), block,
                     fromWithOrigin, origin, withOriginToMaybe)
import qualified Ouroboros.Network.Point as Point (Block (..))
import           Ouroboros.Network.Util.ShowProxy

genesisPoint :: Point block
genesisPoint = Point origin

-- | Header hash
type family HeaderHash b :: Type

-- | Header fields we expect to be present in a block
--
-- These fields are lazy because they are extracted from a block or block
-- header; this type is not intended for storage.
data HeaderFields b = HeaderFields {
      headerFieldSlot    :: SlotNo
    , headerFieldBlockNo :: BlockNo
    , headerFieldHash    :: HeaderHash b
      -- ^ NOTE: this field is last so that the derived 'Eq' and 'Ord'
      -- instances first compare the slot and block numbers, which is cheaper
      -- than comparing hashes.
    }
  deriving (Generic)

deriving instance StandardHash b => Show (HeaderFields b)
deriving instance StandardHash b => Eq   (HeaderFields b)
deriving instance StandardHash b => Ord  (HeaderFields b)

-- Serialise instance only for the benefit of tests
deriving instance Serialise (HeaderHash b) => Serialise (HeaderFields b)

type instance HeaderHash (HeaderFields b) = HeaderHash b

castHeaderFields :: HeaderHash b ~ HeaderHash b'
                 => HeaderFields b -> HeaderFields b'
castHeaderFields (HeaderFields h s b) = HeaderFields h s b

instance StandardHash b => StandardHash (HeaderFields b)

-- | Abstract over the shape of blocks (or indeed just block headers)
class (StandardHash b, Typeable b) => HasHeader b where
  getHeaderFields :: b -> HeaderFields b

instance (StandardHash b, Typeable b) => HasHeader (HeaderFields b) where
  getHeaderFields = castHeaderFields

blockHash :: HasHeader b => b -> HeaderHash b
blockHash = headerFieldHash . getHeaderFields

blockSlot :: HasHeader b => b -> SlotNo
blockSlot = headerFieldSlot . getHeaderFields

blockNo   :: HasHeader b => b -> BlockNo
blockNo = headerFieldBlockNo . getHeaderFields

-- | Extension of 'HasHeader' with some additional information
--
-- Used in tests and assertions only.
class HasHeader b => HasFullHeader b where
    blockPrevHash  :: b -> ChainHash b
    blockInvariant :: b -> Bool

-- | 'StandardHash' summarises the constraints we want header hashes to have
--
-- Without this class we would need to write
--
-- > deriving instance Eq (HeaderHash block) => Eq (ChainHash block)`
--
-- That requires @UndecidableInstances@; not a problem by itself, but it also
-- means that we can then not use @deriving Eq@ anywhere else for datatypes
-- that reference 'Hash', which is very frustrating; see
--
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#inferred-context-for-deriving-clauses>
--
-- Introducing the 'StandardHash' class avoids this problem.
--
-- Having these constraints directly as part of the 'HasHeader' class is
-- possible but libraries that /use/ the networking layer may wish to be able to
-- talk about 'StandardHash' independently of 'HasHeader' since the latter may
-- impose yet further constraints.
class ( Eq       (HeaderHash b)
      , Ord      (HeaderHash b)
      , Show     (HeaderHash b)
      , Typeable (HeaderHash b)
      , NoThunks (HeaderHash b)
      ) => StandardHash b

data ChainHash b = GenesisHash | BlockHash !(HeaderHash b)
  deriving (Generic)

deriving instance StandardHash block => Eq   (ChainHash block)
deriving instance StandardHash block => Ord  (ChainHash block)
deriving instance StandardHash block => Show (ChainHash block)

instance (StandardHash block, Typeable block) => NoThunks (ChainHash block)
  -- use generic instance

castHash :: Coercible (HeaderHash b) (HeaderHash b') => ChainHash b -> ChainHash b'
castHash GenesisHash   = GenesisHash
castHash (BlockHash h) = BlockHash (coerce h)

{-------------------------------------------------------------------------------
  Point on a chain
-------------------------------------------------------------------------------}

-- | A point on the chain is identified by its 'Slot' and 'HeaderHash'.
--
-- The 'Slot' tells us where to look and the 'HeaderHash' either simply serves
-- as a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
-- It's a newtype rather than a type synonym, because using a type synonym
-- would lead to ambiguity, since HeaderHash is a non-injective type family.
newtype Point block = Point
    { getPoint :: WithOrigin (Point.Block SlotNo (HeaderHash block))
    }
  deriving (Generic)

deriving newtype instance StandardHash block => Eq       (Point block)
deriving newtype instance StandardHash block => Ord      (Point block)
deriving newtype instance StandardHash block => Show     (Point block)
deriving newtype instance StandardHash block => NoThunks (Point block)

instance ShowProxy block => ShowProxy (Point block) where
    showProxy _ = "Point " ++ showProxy (Proxy :: Proxy block)

pattern GenesisPoint :: Point block
pattern GenesisPoint = Point Origin

pattern BlockPoint :: SlotNo -> HeaderHash block -> Point block
pattern BlockPoint { atSlot, withHash } = Point (At (Point.Block atSlot withHash))

{-# COMPLETE GenesisPoint, BlockPoint #-}

pointSlot :: Point block -> WithOrigin SlotNo
pointSlot (Point pt) = fmap Point.blockPointSlot pt

pointHash :: Point block -> ChainHash block
pointHash (Point pt) = case pt of
    Origin -> GenesisHash
    At blk -> BlockHash (Point.blockPointHash blk)

castPoint :: Coercible (HeaderHash b) (HeaderHash b') => Point b -> Point b'
castPoint GenesisPoint           = GenesisPoint
castPoint (BlockPoint slot hash) = BlockPoint slot (coerce hash)

blockPoint :: HasHeader block => block -> Point block
blockPoint b = Point (block s h)
  where
    HeaderFields { headerFieldSlot = s, headerFieldHash = h } = getHeaderFields b

{-------------------------------------------------------------------------------
  Tip of a chain
-------------------------------------------------------------------------------}

-- | Used in chain-sync protocol to advertise the tip of the server's chain.
--
data Tip b =
    -- | The tip is genesis
    TipGenesis

    -- | The tip is not genesis
  | Tip !SlotNo !(HeaderHash b) !BlockNo
  deriving (Generic)

deriving instance StandardHash b => Eq       (Tip b)
deriving instance StandardHash b => Show     (Tip b)
deriving instance StandardHash b => NoThunks (Tip b)
instance ShowProxy b => ShowProxy (Tip b) where
    showProxy _ = "Tip " ++ showProxy (Proxy :: Proxy b)

-- | The equivalent of 'castPoint' for 'Tip'
castTip :: (HeaderHash a ~ HeaderHash b) => Tip a -> Tip b
castTip TipGenesis  = TipGenesis
castTip (Tip s h b) = Tip s h b

getTipPoint :: Tip b -> Point b
getTipPoint TipGenesis  = GenesisPoint
getTipPoint (Tip s h _) = BlockPoint s h

getTipBlockNo :: Tip b -> WithOrigin BlockNo
getTipBlockNo TipGenesis  = Origin
getTipBlockNo (Tip _ _ b) = At b

getTipSlotNo :: Tip b -> WithOrigin SlotNo
getTipSlotNo TipGenesis  = Origin
getTipSlotNo (Tip s _ _) = At s

tipFromHeader ::  HasHeader a => a -> Tip a
tipFromHeader a = Tip headerFieldSlot headerFieldHash headerFieldBlockNo
  where
    HeaderFields { headerFieldSlot
                 , headerFieldBlockNo
                 , headerFieldHash
                 } = getHeaderFields a


-- | Get the block number associated with a 'Tip', or 'genesisBlockNo' otherwise
--
-- TODO: This is /wrong/. There /is/ no block number if we are at genesis
-- ('genesisBlockNo' is the block number of the first block on the chain).
-- Usage of this function should be phased out.
getLegacyTipBlockNo :: Tip b -> BlockNo
getLegacyTipBlockNo = fromWithOrigin genesisBlockNo . getTipBlockNo
  where
    genesisBlockNo = BlockNo 0
{-# DEPRECATED getLegacyTipBlockNo "Use getTipBlockNo" #-}

-- | Translate to the format it was before (to maintain binary compatibility)
toLegacyTip :: Tip b -> (Point b, BlockNo)
toLegacyTip tip = (getTipPoint tip, getLegacyTipBlockNo tip)
{-# DEPRECATED toLegacyTip "Use getTipPoint and getTipBlockNo" #-}

-- | Inverse of 'toLegacyTip'
--
-- TODO: This should be phased out, since it makes no sense to have a
-- 'BlockNo' for the genesis point.
legacyTip :: Point b -> BlockNo -> Tip b
legacyTip GenesisPoint     _ = TipGenesis -- Ignore block number
legacyTip (BlockPoint s h) b = Tip s h b
{-# DEPRECATED legacyTip "Use tipFromHeader instead" #-}

encodeTip :: (HeaderHash blk -> Encoding)
          -> (Tip        blk -> Encoding)
encodeTip encodeHeaderHash tip = mconcat
    [ Enc.encodeListLen 2
    , encodePoint encodeHeaderHash tipPoint
    , encode                       tipBlockNo
    ]
  where
    tipPoint   = getTipPoint tip
    -- note: 'encodePoint' would encode 'Origin' differently than @'Block' 0@,
    -- we keep the encoding backward compatible.
    tipBlockNo = fromWithOrigin (BlockNo 0)
                                (getTipBlockNo tip)

-- TODO: add a test, which should compare with 'encodedTip', including various
-- instantiations of 'blk', e.g. 'ByronBlock, etc.  Thus this test should live
-- in 'ourobors-consensus'.
encodedTipSize :: (Proxy (HeaderHash blk) -> Size)
               -> (Proxy (Tip        blk) -> Size)
encodedTipSize encodedHeaderHashSize tipProxy =
    1
  + encodedPointSize encodedHeaderHashSize (fst . toLegacyTip <$> tipProxy)
  -- TODO: remove 'unBlockNo' when 'BlockNo' 'ToCBOR' instance will implement
  -- 'encodedSizeExpr', also include a test in `cardano-ledger-byron`.
  + szGreedy (unBlockNo . snd . toLegacyTip <$> tipProxy)

decodeTip :: forall blk.
             (forall s. Decoder s (HeaderHash blk))
          -> (forall s. Decoder s (Tip        blk))
decodeTip decodeHeaderHash = do
    Dec.decodeListLenOf 2
    tipPoint    <- decodePoint decodeHeaderHash
    tipBlockNo  <- decode
    return $ case tipPoint :: Point blk of
      GenesisPoint   -> TipGenesis
      BlockPoint s h -> Tip s h tipBlockNo


{-------------------------------------------------------------------------------
  ChainUpdate type
-------------------------------------------------------------------------------}

-- | A representation of two actions to update a chain: add a block or roll
-- back to a previous point.
--
-- The type parameter @a@ is there to allow a 'Functor' instance. Typically,
-- it will be instantiated with @block@ itself.
data ChainUpdate block a = AddBlock a
                         | RollBack (Point block)
  deriving (Eq, Show, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  MaxSlotNo
-------------------------------------------------------------------------------}

-- | The highest slot number seen.
data MaxSlotNo
  = NoMaxSlotNo
    -- ^ No block/header has been seen yet, so we don't have a highest slot
    -- number.
  | MaxSlotNo !SlotNo
    -- ^ The highest slot number seen.
  deriving (Eq, Show, Generic, NoThunks)

-- The derived instances would do the same, but for clarity, we write it out
-- explicitly.
instance Ord MaxSlotNo where
  compare NoMaxSlotNo       (MaxSlotNo _) = LT
  compare NoMaxSlotNo       NoMaxSlotNo   = EQ
  compare (MaxSlotNo _)  NoMaxSlotNo      = GT
  compare (MaxSlotNo s1) (MaxSlotNo s2)   = compare s1 s2

maxSlotNoFromMaybe :: Maybe SlotNo -> MaxSlotNo
maxSlotNoFromMaybe = maybe NoMaxSlotNo MaxSlotNo

maxSlotNoToMaybe :: MaxSlotNo -> Maybe SlotNo
maxSlotNoToMaybe NoMaxSlotNo   = Nothing
maxSlotNoToMaybe (MaxSlotNo s) = Just s

maxSlotNoFromWithOrigin :: WithOrigin SlotNo -> MaxSlotNo
maxSlotNoFromWithOrigin = maxSlotNoFromMaybe . withOriginToMaybe

instance Semigroup MaxSlotNo where
  (<>) = max

instance Monoid MaxSlotNo where
  mempty  = NoMaxSlotNo
  mappend = (<>)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

--TODO: these two instances require UndecidableInstances
instance Serialise (HeaderHash b) => Serialise (ChainHash b) where
  encode = encodeChainHash encode
  decode = decodeChainHash decode

instance Serialise (HeaderHash block) => Serialise (Point block) where
  encode = encodePoint encode
  decode = decodePoint decode

encodeChainHash :: (HeaderHash block -> Encoding)
                -> (ChainHash  block -> Encoding)
encodeChainHash encodeHash chainHash =
    case chainHash of
      GenesisHash -> Enc.encodeListLen 0
      BlockHash h -> Enc.encodeListLen 1 <> encodeHash h

decodeChainHash :: (forall s. Decoder s (HeaderHash block))
                -> (forall s. Decoder s (ChainHash  block))
decodeChainHash decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return GenesisHash
      1 -> BlockHash <$> decodeHash
      _ -> fail "decodeChainHash: invalid tag"

-- TODO: remove 'unSlotNo', add a test.  This should be moved to
-- 'cardano-consensus' where similar tests exists (and all the infrastructure
-- to run them is in place).
encodedSlotNoSize :: Proxy SlotNo -> Size
encodedSlotNoSize = szGreedy . fmap unSlotNo

encodePoint :: (HeaderHash block -> Encoding)
            -> (Point      block -> Encoding)
encodePoint encodeHash (Point pt) = case pt of
    Origin -> Enc.encodeListLen 0
    At blk ->
           Enc.encodeListLen 2
        <> encode     (Point.blockPointSlot blk)
        <> encodeHash (Point.blockPointHash blk)

-- TODO: add a test (see 'encodedTipSize')
encodedPointSize :: (Proxy (HeaderHash block) -> Size)
                 -> (Proxy (Point      block) -> Size)
encodedPointSize encodedHeaderHashSize pointProxy =
    1
    + szCases
        [ Case "Origin" 1
        , Case "At" $
              1
            + encodedSlotNoSize
                (Point.blockPointSlot <$> blockProxy)
            + encodedHeaderHashSize
                (Point.blockPointHash <$> blockProxy)
        ]
  where
    blockProxy = At `contramap` (getPoint <$> pointProxy)

decodePoint :: (forall s. Decoder s (HeaderHash block))
            -> (forall s. Decoder s (Point      block))
decodePoint decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return (Point origin)
      2 -> do
        slot <- decode
        hash <- decodeHash
        return (Point (block slot hash))
      _ -> fail "decodePoint: invalid tag"

{-------------------------------------------------------------------------------
  Serialised block/header
-------------------------------------------------------------------------------}

-- | An already serialised value
--
-- When streaming blocks/header from disk to the network, there is often no
-- need to deserialise them, as we'll just end up serialising them again when
-- putting them on the wire.
newtype Serialised a = Serialised
  { unSerialised :: Lazy.ByteString }
  deriving (Eq)

instance Show (Serialised a) where
  show (Serialised bytes) = BSC.unpack (B16.encode bytes)

instance ShowProxy a => ShowProxy (Serialised a) where
    showProxy _ = "Serialised " ++ showProxy (Proxy :: Proxy a)

type instance HeaderHash (Serialised block) = HeaderHash block
instance StandardHash block => StandardHash (Serialised block)

-- | Wrap CBOR-in-CBOR
--
-- This is primarily useful for the /decoder/; see 'unwrapCBORinCBOR'
wrapCBORinCBOR :: (a -> Encoding) -> a -> Encoding
wrapCBORinCBOR enc = encode . mkSerialised enc

-- | Unwrap CBOR-in-CBOR
--
-- The CBOR-in-CBOR encoding gives us the 'ByteString' we need in order to
-- to construct annotations.
unwrapCBORinCBOR :: (forall s. Decoder s (Lazy.ByteString -> a))
                 -> (forall s. Decoder s a)
unwrapCBORinCBOR dec = fromSerialised dec =<< decode

-- | Construct 'Serialised' value from an unserialised value
mkSerialised :: (a -> Encoding) -> a -> Serialised a
mkSerialised enc = Serialised . Write.toLazyByteString . enc

-- | Decode a 'Serialised' value
--
-- Unlike a regular 'Decoder', which has an implicit input stream,
-- 'fromSerialised' takes the 'Serialised' value as an argument.
fromSerialised :: (forall s. Decoder s (Lazy.ByteString -> a))
               -> Serialised a -> (forall s. Decoder s a)
fromSerialised dec (Serialised payload) =
    case Read.deserialiseFromBytes dec payload of
      Left (Read.DeserialiseFailure _ reason) -> fail reason
      Right (trailing, mkA)
        | not (Lazy.null trailing) -> fail "trailing bytes in CBOR-in-CBOR"
        | otherwise                -> return (mkA payload)

-- | CBOR-in-CBOR
--
-- TODO: replace with encodeEmbeddedCBOR from cborg-0.2.4 once
-- it is available, since that will be faster.
--
-- TODO: Avoid converting to a strict ByteString, as that requires copying O(n)
-- in case the lazy ByteString consists of more than one chunks.
instance Serialise (Serialised a) where
  encode (Serialised bs) = mconcat [
        Enc.encodeTag 24
      , Enc.encodeBytes (Lazy.toStrict bs)
      ]

  decode = do
      tag <- Dec.decodeTag
      when (tag /= 24) $ fail "expected tag 24 (CBOR-in-CBOR)"
      Serialised . Lazy.fromStrict <$> Dec.decodeBytes

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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Abstract view over blocks
--
-- The network layer does not make any concrete assumptions about what blocks
-- look like.
module Ouroboros.Network.Block (
    SlotNo(..)
  , BlockNo(..)
  , HeaderHash
  , HasHeader(..)
  , StandardHash
  , ChainHash(..)
  , castHash
  , Point(..)
  , pointSlot
  , pointHash
  , castPoint
  , blockPoint
  , pattern GenesisPoint
  , pattern BlockPoint
  , atSlot
  , withHash
  , Tip(..)
  , castTip
  , getTipPoint
  , getTipBlockNo
  , getLegacyTipBlockNo
  , legacyTip
  , toLegacyTip
  , encodeTip
  , encodedTipSize
  , decodeTip
  , ChainUpdate(..)
  , MaxSlotNo (..)
  , maxSlotNoFromMaybe
  , maxSlotNoToMaybe
  , maxSlotNoFromWithOrigin
  , BlockMeasure(..)
  , blockMeasure
  , genesisPoint
    -- * Serialisation
  , encodePoint
  , encodedPointSize
  , encodeChainHash
  , decodePoint
  , decodeChainHash
    -- * Serialised block/header
  , Serialised(..)
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
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured)
import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

-- TODO: it should be exported from 'Cardano.Prelude'
import           Control.Tracer (contramap)

import           Cardano.Binary (Case (..), Size, szCases, szGreedy)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Network.Point (WithOrigin (..), block,
                     fromWithOrigin, origin, withOriginToMaybe)
import qualified Ouroboros.Network.Point as Point (Block (..))

genesisPoint :: Point block
genesisPoint = Point origin

-- | Header hash
type family HeaderHash b :: *

-- | Abstract over the shape of blocks (or indeed just block headers)
class (StandardHash b, Measured BlockMeasure b, Typeable b) => HasHeader b where
    blockHash      :: b -> HeaderHash b
    blockPrevHash  :: b -> ChainHash b
    blockSlot      :: b -> SlotNo
    blockNo        :: b -> BlockNo

    blockInvariant :: b -> Bool

-- | When implementing 'HasHeader', use this method to implement the 'measure'
-- method of the 'Measured' super class.
blockMeasure :: HasHeader b => b -> BlockMeasure
blockMeasure b = BlockMeasure (blockSlot b) (blockSlot b) 1

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
class ( Eq        (HeaderHash b)
      , Ord       (HeaderHash b)
      , Show      (HeaderHash b)
      , Typeable  (HeaderHash b)
      , NoUnexpectedThunks (HeaderHash b)
      ) => StandardHash b

data ChainHash b = GenesisHash | BlockHash !(HeaderHash b)
  deriving (Generic)

deriving instance StandardHash block => Eq   (ChainHash block)
deriving instance StandardHash block => Ord  (ChainHash block)
deriving instance StandardHash block => Show (ChainHash block)

instance (StandardHash block, Typeable block) => NoUnexpectedThunks (ChainHash block)
  -- use generic instance

castHash :: HeaderHash b ~ HeaderHash b' => ChainHash b -> ChainHash b'
castHash GenesisHash   = GenesisHash
castHash (BlockHash b) = BlockHash b

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

deriving newtype instance StandardHash block => Eq   (Point block)
deriving newtype instance StandardHash block => Ord  (Point block)
deriving newtype instance StandardHash block => Show (Point block)
deriving newtype instance (StandardHash block, Typeable block) => NoUnexpectedThunks (Point block)

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

castPoint :: (HeaderHash a ~ HeaderHash b) => Point a -> Point b
castPoint (Point Origin)                       = Point Origin
castPoint (Point (At (Point.Block slot hash))) = Point (block slot hash)

blockPoint :: HasHeader block => block -> Point block
blockPoint b = Point (block (blockSlot b) (blockHash b))

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

deriving instance StandardHash b => Eq                 (Tip b)
deriving instance StandardHash b => Show               (Tip b)
deriving instance StandardHash b => NoUnexpectedThunks (Tip b)

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

-- | Get the block number associated with a 'Tip', or 'genesisBlockNo' otherwise
--
-- TODO: This is /wrong/. There /is/ no block number if we are at genesis
-- ('genesisBlockNo' is the block number of the first block on the chain).
-- Usage of this function should be phased out.
getLegacyTipBlockNo :: Tip b -> BlockNo
getLegacyTipBlockNo = fromWithOrigin genesisBlockNo . getTipBlockNo
  where
    genesisBlockNo = BlockNo 0

-- | Translate to the format it was before (to maintain binary compatibility)
toLegacyTip :: Tip b -> (Point b, BlockNo)
toLegacyTip tip = (getTipPoint tip, getLegacyTipBlockNo tip)

-- | Inverse of 'toLegacyTip'
--
-- TODO: This should be phased out, since it makes no sense to have a
-- 'BlockNo' for the genesis point.
legacyTip :: Point b -> BlockNo -> Tip b
legacyTip GenesisPoint     _ = TipGenesis -- Ignore block number
legacyTip (BlockPoint s h) b = Tip s h b

encodeTip :: (HeaderHash blk -> Encoding)
          -> (Tip        blk -> Encoding)
encodeTip encodeHeaderHash tip = mconcat
    [ Enc.encodeListLen 2
    , encodePoint encodeHeaderHash tipPoint
    , encode                       tipBlockNo
    ]
  where
    (tipPoint, tipBlockNo) = toLegacyTip tip

-- TODO: add a test, which should compare with 'encodedTip', including various
-- instantiations of 'blk', e.g. 'ByronBlock, etc.  Thus this test should live
-- in 'ourobors-consensus'.
encodedTipSize :: (Proxy (HeaderHash blk) -> Size)
               -> (Proxy (Tip        blk) -> Size)
encodedTipSize encodedHeaderHashSize tipProxy =
    1
  + encodedPointSize encodedHeaderHashSize (fst . toLegacyTip <$> tipProxy)
  -- TODO: remove 'unBlockNo' when 'BlockNo' 'ToCBOR' instance will implement
  -- 'encodedSizeExpr', also include a test in `cardano-ledger`.
  + szGreedy (unBlockNo . snd . toLegacyTip <$> tipProxy)

decodeTip :: (forall s. Decoder s (HeaderHash blk))
          -> (forall s. Decoder s (Tip        blk))
decodeTip decodeHeaderHash = do
  Dec.decodeListLenOf 2
  tipPoint    <- decodePoint decodeHeaderHash
  tipBlockNo  <- decode
  return $ legacyTip tipPoint tipBlockNo

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
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

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
  Finger Tree Measure
-------------------------------------------------------------------------------}

-- | The measure used for 'Ouroboros.Network.ChainFragment.ChainFragment'.
data BlockMeasure = BlockMeasure {
       bmMinSlot :: !SlotNo,
       bmMaxSlot :: !SlotNo,
       bmSize    :: !Int
     }
  deriving Show


instance Semigroup BlockMeasure where
  vl <> vr =
    BlockMeasure (min (bmMinSlot vl) (bmMinSlot vr))
                 (max (bmMaxSlot vl) (bmMaxSlot vr))
                 (bmSize vl + bmSize vr)

instance Monoid BlockMeasure where
  mempty = BlockMeasure maxBound minBound 0
  mappend = (<>)

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
  deriving (Eq, Show)

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

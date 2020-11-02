{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
module Ouroboros.Chain.Point (
    Point (GenesisPoint, BlockPoint)
  , atSlot
  , withHash
  , getPoint
  , pointSlot
  , pointHash
  , castPoint
  , blockPoint
  , PointBlock (..)
    -- * Serialisation
  , encodePoint
  , decodePoint
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Coerce (Coercible, coerce)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import           Ouroboros.Chain.HasHeader
import           Ouroboros.Chain.Util.ShowProxy

-- | A point on the chain is identified by its 'Slot' and 'HeaderHash'.
--
-- The 'Slot' tells us where to look and the 'HeaderHash' either simply serves
-- as a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
-- It's a newtype rather than a type synonym, because using a type synonym
-- would lead to ambiguity, since HeaderHash is a non-injective type family.
newtype Point blk = Point {
      getPoint :: WithOrigin (PointBlock SlotNo (HeaderHash blk))
    }
  deriving (Generic)

deriving newtype instance StandardHash blk => Eq       (Point blk)
deriving newtype instance StandardHash blk => Ord      (Point blk)
deriving newtype instance StandardHash blk => Show     (Point blk)
deriving newtype instance StandardHash blk => NoThunks (Point blk)

instance ShowProxy blk => ShowProxy (Point blk) where
    showProxy _ = "Point " ++ showProxy (Proxy @blk)

pattern GenesisPoint :: Point blk
pattern GenesisPoint = Point Origin

pattern BlockPoint :: SlotNo -> HeaderHash blk -> Point blk
pattern BlockPoint { atSlot, withHash } = Point (At (PointBlock atSlot withHash))

{-# COMPLETE GenesisPoint, BlockPoint #-}

pointSlot :: Point block -> WithOrigin SlotNo
pointSlot (Point pt) = fmap pointBlockSlot pt

pointHash :: Point block -> ChainHash block
pointHash = \case
    GenesisPoint   -> GenesisHash
    BlockPoint _ h -> BlockHash h

castPoint :: Coercible (HeaderHash b) (HeaderHash b') => Point b -> Point b'
castPoint GenesisPoint           = GenesisPoint
castPoint (BlockPoint slot hash) = BlockPoint slot (coerce hash)

blockPoint :: HasHeader block => block -> Point block
blockPoint b = BlockPoint s h
  where
    HeaderFields { headerFieldSlot = s, headerFieldHash = h } = getHeaderFields b

data PointBlock slot hash = PointBlock {
       pointBlockSlot :: !slot
     , pointBlockHash :: !hash
     }
  deriving (Eq, Ord, Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise (HeaderHash block) => Serialise (Point block) where
  encode = encodePoint encode
  decode = decodePoint decode

encodePoint ::
     (HeaderHash block -> Encoding)
  -> (Point      block -> Encoding)
encodePoint encodeHash = \case
    GenesisPoint   -> Enc.encodeListLen 0
    BlockPoint s h -> mconcat [
        Enc.encodeListLen 2
      , encode     s
      , encodeHash h
      ]

decodePoint ::
     (forall s. Decoder s (HeaderHash block))
  -> (forall s. Decoder s (Point      block))
decodePoint decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return GenesisPoint
      2 -> do
        slot <- decode
        hash <- decodeHash
        return $ BlockPoint slot hash
      _ -> fail "decodePoint: invalid tag"

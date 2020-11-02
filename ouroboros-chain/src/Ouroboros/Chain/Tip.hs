{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Ouroboros.Chain.Tip (
    Tip (..)
  , castTip
  , getTipPoint
  , getTipBlockNo
  , getTipSlotNo
    -- * Legacy
  , getLegacyTipBlockNo
  , toLegacyTip
  , legacyTip
    -- * Serialisation
  , encodeTip
  , decodeTip
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..), fromWithOrigin)

import           Ouroboros.Chain.HasHeader
import           Ouroboros.Chain.Point
import           Ouroboros.Chain.Util.ShowProxy

-- | Used to represent the tip of a chain
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
    showProxy _ = "Tip " ++ showProxy (Proxy @b)

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

{-------------------------------------------------------------------------------
  Legacy
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeTip ::
     (HeaderHash blk -> Encoding)
  -> (Tip        blk -> Encoding)
encodeTip encodeHeaderHash tip = mconcat [
      Enc.encodeListLen 2
    , encodePoint encodeHeaderHash tipPoint
    , encode                       tipBlockNo
    ]
  where
    (tipPoint, tipBlockNo) = toLegacyTip tip

decodeTip ::
     (forall s. Decoder s (HeaderHash blk))
  -> (forall s. Decoder s (Tip        blk))
decodeTip decodeHeaderHash = do
    Dec.decodeListLenOf 2
    tipPoint    <- decodePoint decodeHeaderHash
    tipBlockNo  <- decode
    return $ legacyTip tipPoint tipBlockNo

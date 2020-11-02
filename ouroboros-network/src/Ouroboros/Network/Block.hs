{-# LANGUAGE OverloadedStrings #-}

-- | Abstract view over blocks
--
-- The network layer does not make any concrete assumptions about what blocks
-- look like.
module Ouroboros.Network.Block (
    SlotNo(..)
  , BlockNo(..)
  , HeaderHash
  , HeaderFields(..)
  , castHeaderFields
  , HasHeader(..)
  , blockNo
  , blockSlot
  , blockHash
  , HasFullHeader(..)
  , StandardHash
  , ChainHash(..)
  , castHash
  , Point(..)
  , pointSlot
  , pointHash
  , castPoint
  , blockPoint
  , atSlot
  , withHash
  , Tip(..)
  , castTip
  , getTipPoint
  , getTipBlockNo
  , getTipSlotNo
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

import           Data.Functor.Contravariant (contramap)
import           Data.Proxy (Proxy (..))

import           Cardano.Binary (Case (..), Size, szCases, szGreedy)

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Chain.ChainUpdate
import           Ouroboros.Chain.HasFullHeader
import           Ouroboros.Chain.HasHeader
import           Ouroboros.Chain.MaxSlotNo
import           Ouroboros.Chain.Point
import           Ouroboros.Chain.Serialised
import           Ouroboros.Chain.Tip

import           Ouroboros.Network.Point (WithOrigin (..))

genesisPoint :: Point block
genesisPoint = GenesisPoint

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

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- TODO: remove 'unSlotNo', add a test.  This should be moved to
-- 'cardano-consensus' where similar tests exists (and all the infrastructure
-- to run them is in place).
encodedSlotNoSize :: Proxy SlotNo -> Size
encodedSlotNoSize = szGreedy . fmap unSlotNo

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
                (pointBlockSlot <$> blockProxy)
            + encodedHeaderHashSize
                (pointBlockHash <$> blockProxy)
        ]
  where
    blockProxy = At `contramap` (getPoint <$> pointProxy)

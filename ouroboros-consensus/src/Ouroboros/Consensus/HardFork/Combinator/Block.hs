{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Block (
    -- * Type family instances
    BlockConfig(..)
  , CodecConfig(..)
  , Header(..)
  ) where

import           Data.FingerTree.Strict (Measured (..))
import           Data.Function (on)
import           Data.Functor.Product
import           Data.SOP.Strict

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

newtype instance CodecConfig (HardForkBlock xs) = HardForkCodecConfig {
      hardForkCodecConfigPerEra :: PerEraCodecConfig xs
    }

instance CanHardFork xs => BlockHasCodecConfig (HardForkBlock xs) where
  getCodecConfig =
        HardForkCodecConfig
      . PerEraCodecConfig
      . hcmap proxySingle getCodecConfig
      . getPerEraBlockConfig
      . hardForkBlockConfigPerEra

{-------------------------------------------------------------------------------
  GetHeader
-------------------------------------------------------------------------------}

instance CanHardFork xs => GetHeader (HardForkBlock xs) where
  newtype Header (HardForkBlock xs) = HardForkHeader {
        getHardForkHeader :: OneEraHeader xs
      }
    deriving (Show, NoUnexpectedThunks)

  getHeader = HardForkHeader . oneEraBlockHeader . getHardForkBlock

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (HardForkBlock xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (HardForkBlock xs)

instance CanHardFork xs => Measured BlockMeasure (HardForkBlock xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (HardForkBlock xs) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance CanHardFork xs => HasHeader (Header (HardForkBlock xs)) where
  blockHash      =            blockHash     . getHardForkHeader
  blockPrevHash  = castHash . blockPrevHash . getHardForkHeader
  blockSlot      =            blockSlot     . getHardForkHeader
  blockNo        =            blockNo       . getHardForkHeader
  blockInvariant = const True

{-------------------------------------------------------------------------------
  HasAnnTip
-------------------------------------------------------------------------------}

instance CanHardFork xs => HasAnnTip (HardForkBlock xs) where
  type TipInfo (HardForkBlock xs) = OneEraTipInfo xs

  getTipInfo =
        OneEraTipInfo
      . hcmap proxySingle (SingleEraTipInfo . getTipInfo)
      . getOneEraHeader
      . getHardForkHeader

  tipInfoHash _ =
        OneEraHash
      . hcmap proxySingle aux
      . getOneEraTipInfo
    where
      aux :: forall blk. SingleEraBlock blk
          => SingleEraTipInfo blk -> SingleEraHash blk
      aux = SingleEraHash . tipInfoHash (Proxy @blk) . getSingleEraTipInfo

{-------------------------------------------------------------------------------
  BasicEnvelopeValidation
-------------------------------------------------------------------------------}

instance CanHardFork xs => BasicEnvelopeValidation (HardForkBlock xs) where
  expectedFirstBlockNo _ =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty p -> expectedFirstBlockNo p

  minimumPossibleSlotNo _ =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty p -> minimumPossibleSlotNo p

  -- TODO: If the block is from a different era as the current tip, we just
  -- expect @succ b@. This may not be sufficient: if we ever transition /to/
  -- an era with EBBs, this is not correct.
  expectedNextBlockNo _ (OneEraTipInfo oldTip) (OneEraTipInfo newBlock) b =
      case Match.matchNS oldTip newBlock of
        Right matched  -> hcollapse $ hcmap proxySingle aux matched
        Left _mismatch -> succ b
    where
      aux :: forall blk. SingleEraBlock blk
          => Product SingleEraTipInfo SingleEraTipInfo blk
          -> K BlockNo blk
      aux (Pair (SingleEraTipInfo old) (SingleEraTipInfo new)) = K $
          expectedNextBlockNo (Proxy @blk) old new b

  -- TODO: If the block is from a different era as the current tip, we just
  -- expect @succ s@. This may not be sufficient: if we ever transition /to/
  -- an era with EBBs, this is not correct.
  minimumNextSlotNo _ (OneEraTipInfo oldTip) (OneEraTipInfo newBlock) s =
      case Match.matchNS oldTip newBlock of
        Right matched  -> hcollapse $ hcmap proxySingle aux matched
        Left _mismatch -> succ s
    where
      aux :: forall blk. SingleEraBlock blk
          => Product SingleEraTipInfo SingleEraTipInfo blk
          -> K SlotNo blk
      aux (Pair (SingleEraTipInfo old) (SingleEraTipInfo new)) = K $
          minimumNextSlotNo (Proxy @blk) old new s

  checkPrevHash HardForkBlockConfig{..} =
           go (getCheckEraTransition hardForkEraTransitionCheck) cfgs
      `on` getOneEraHash
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

      -- This is a pretty straight-forward aligning of two NS, except we allow
      -- the current tip to be /one/ era before the next block; in this case
      -- the 'hardForkEraTransitionCheck' will do the check for us.
      go :: All SingleEraBlock xs'
         => InPairs CheckTransition xs'
         -> NP BlockConfig xs'
         -> NS SingleEraHash xs' -> NS SingleEraHash xs' -> Bool
      go _            (c :* _)       (Z h) (Z h')     = aux c h h'
      go (PCons _ fs) (_ :* cs)      (S h) (S h')     = go fs cs h h'
      go (PCons f _)  (c :* c' :* _) (Z h) (S (Z h')) = checkOne f c c' h h'
      go _            _              _     _          = False

      checkOne :: CheckTransition blk blk'
               -> BlockConfig blk
               -> BlockConfig blk'
               -> SingleEraHash blk
               -> SingleEraHash blk'
               -> Bool
      checkOne f c c' h h' =
          checkTransitionWith f c c'
            (getSingleEraHash h)
            (BlockHash $ getSingleEraHash h')

      aux :: forall blk. SingleEraBlock blk
          => BlockConfig blk -> SingleEraHash blk -> SingleEraHash blk -> Bool
      aux cfg = checkPrevHash cfg `on` getSingleEraHash

{-------------------------------------------------------------------------------
  Other instances (primarily for the benefit of tests)
-------------------------------------------------------------------------------}

instance All Condense xs => Condense (HardForkBlock xs) where
  condense =
        hcollapse
      . hcmap (Proxy @Condense) (K . condense . unI)
      . getOneEraBlock
      . getHardForkBlock


instance All Eq xs => Eq (HardForkBlock xs) where
  (==) = (aux .: Match.matchNS) `on` (getOneEraBlock . getHardForkBlock)
    where
      aux :: Either (Match.Mismatch I I xs) (NS (Product I I) xs) -> Bool
      aux (Left  _) = False
      aux (Right m) = hcollapse $
                        hcmap (Proxy @Eq) (\(Pair x y) -> K $ x == y) m

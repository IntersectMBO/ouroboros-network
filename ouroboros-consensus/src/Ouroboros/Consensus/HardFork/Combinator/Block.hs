{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Block (
    -- * Type family instances
    Header (..)
  , NestedCtxt_ (..)
    -- * AnnTip
  , distribAnnTip
  , undistribAnnTip
  ) where

import           Data.Function (on)
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.SOP.Strict
import           Data.Typeable (Typeable)
import           Data.Word
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (ShowProxy, (.:))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  GetHeader
-------------------------------------------------------------------------------}

newtype instance Header (HardForkBlock xs) = HardForkHeader {
      getHardForkHeader :: OneEraHeader xs
    }
  deriving (Show, NoThunks)

instance Typeable xs => ShowProxy (Header (HardForkBlock xs)) where

instance CanHardFork xs => GetHeader (HardForkBlock xs) where
  getHeader = HardForkHeader . oneEraBlockHeader . getHardForkBlock

  blockMatchesHeader = \hdr blk ->
      case Match.matchNS
             (getOneEraHeader (getHardForkHeader hdr))
             (getOneEraBlock  (getHardForkBlock  blk)) of
        Left _          -> False
        Right hdrAndBlk ->
          hcollapse $ hcliftA proxySingle matchesSingle hdrAndBlk
    where
      matchesSingle :: GetHeader blk => Product Header I blk -> K Bool blk
      matchesSingle (Pair hdr (I blk)) = K (blockMatchesHeader hdr blk)

  headerIsEBB =
        hcollapse
      . hcmap proxySingle (K . headerIsEBB)
      . getOneEraHeader
      . getHardForkHeader

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

instance CanHardFork xs => StandardHash (HardForkBlock xs)

instance CanHardFork xs => HasHeader (HardForkBlock xs) where
  getHeaderFields = getBlockHeaderFields

instance CanHardFork xs => HasHeader (Header (HardForkBlock xs)) where
  getHeaderFields =
        hcollapse
      . hcmap proxySingle (K . getOne)
      . getOneEraHeader
      . getHardForkHeader
    where
      getOne :: forall blk. SingleEraBlock blk
             => Header blk -> HeaderFields (Header (HardForkBlock xs))
      getOne hdr = HeaderFields {
            headerFieldHash    = OneEraHash $
                                   toShortRawHash (Proxy @blk) headerFieldHash
          , headerFieldSlot    = headerFieldSlot
          , headerFieldBlockNo = headerFieldBlockNo
          }
        where
          HeaderFields{..} = getHeaderFields hdr

instance CanHardFork xs => GetPrevHash (HardForkBlock xs) where
  headerPrevHash =
        hcollapse
      . hcmap proxySingle (K . getOnePrev)
      . getOneEraHeader
      . getHardForkHeader
    where
      getOnePrev :: forall blk. SingleEraBlock blk
                 => Header blk -> ChainHash (HardForkBlock xs)
      getOnePrev hdr =
          case headerPrevHash hdr of
            GenesisHash -> GenesisHash
            BlockHash h -> BlockHash (OneEraHash $ toShortRawHash (Proxy @blk) h)

{-------------------------------------------------------------------------------
  NestedContent
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (HardForkBlock xs) :: (Type -> Type) -> (Type -> Type) where
    NCZ :: !(NestedCtxt_ x                  f a) -> NestedCtxt_ (HardForkBlock (x ': xs)) f a
    NCS :: !(NestedCtxt_ (HardForkBlock xs) f a) -> NestedCtxt_ (HardForkBlock (x ': xs)) f a

deriving instance All SingleEraBlock xs => Show (NestedCtxt_ (HardForkBlock xs) Header a)

instance CanHardFork xs => SameDepIndex (NestedCtxt_ (HardForkBlock xs) Header) where
  sameDepIndex = go
    where
      go :: All SingleEraBlock xs'
         => NestedCtxt_ (HardForkBlock xs') Header a
         -> NestedCtxt_ (HardForkBlock xs') Header b
         -> Maybe (a :~: b)
      go (NCZ ctxt) (NCZ ctxt') = sameDepIndex ctxt ctxt'
      go (NCS ctxt) (NCS ctxt') = go ctxt ctxt'
      go _          _           = Nothing

instance CanHardFork xs => HasNestedContent Header (HardForkBlock xs) where
  unnest =
      go . getOneEraHeader . getHardForkHeader
    where
      go :: All SingleEraBlock xs'
         => NS Header xs' -> DepPair (NestedCtxt Header (HardForkBlock xs'))
      go (Z x) = case unnest x of
                   DepPair (NestedCtxt ctxt) x' ->
                     DepPair (NestedCtxt (NCZ ctxt)) x'
      go (S x) = case go x of
                   DepPair (NestedCtxt ctxt) x' ->
                     DepPair (NestedCtxt (NCS ctxt)) x'

  nest = \(DepPair ctxt hdr) ->
      HardForkHeader . OneEraHeader $ go ctxt hdr
    where
      go :: All SingleEraBlock xs'
         => NestedCtxt Header (HardForkBlock xs') a -> a -> NS Header xs'
      go (NestedCtxt (NCZ ctxt)) x = Z (nest (DepPair (NestedCtxt ctxt) x))
      go (NestedCtxt (NCS ctxt)) x = S (go (NestedCtxt ctxt) x)

{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConvertRawHash (HardForkBlock xs) where
  toShortRawHash   _ = getOneEraHash
  fromShortRawHash _ = OneEraHash
  hashSize         _ = getSameValue hashSizes
    where
      hashSizes :: NP (K Word32) xs
      hashSizes = hcpure proxySingle hashSizeOne

      hashSizeOne :: forall blk. SingleEraBlock blk => K Word32 blk
      hashSizeOne = K $ hashSize (Proxy @blk)

{-------------------------------------------------------------------------------
  HasAnnTip
-------------------------------------------------------------------------------}

instance CanHardFork xs => HasAnnTip (HardForkBlock xs) where
  type TipInfo (HardForkBlock xs) = OneEraTipInfo xs

  getTipInfo =
        OneEraTipInfo
      . hcmap proxySingle (WrapTipInfo . getTipInfo)
      . getOneEraHeader
      . getHardForkHeader

  tipInfoHash _ =
        hcollapse
      . hcmap proxySingle (K . tipInfoOne)
      . getOneEraTipInfo
    where
      tipInfoOne :: forall blk. SingleEraBlock blk
                 => WrapTipInfo blk -> OneEraHash xs
      tipInfoOne = OneEraHash
                 . toShortRawHash (Proxy @blk)
                 . tipInfoHash    (Proxy @blk)
                 . unwrapTipInfo

distribAnnTip :: SListI xs => AnnTip (HardForkBlock xs) -> NS AnnTip xs
distribAnnTip AnnTip{..} =
    hmap distrib (getOneEraTipInfo annTipInfo)
  where
    distrib :: WrapTipInfo blk -> AnnTip blk
    distrib (WrapTipInfo info) =
        AnnTip annTipSlotNo annTipBlockNo info

undistribAnnTip :: SListI xs => NS AnnTip xs -> AnnTip (HardForkBlock xs)
undistribAnnTip = hcollapse . himap undistrib
  where
    undistrib :: Index xs blk
              -> AnnTip blk
              -> K (AnnTip (HardForkBlock xs)) blk
    undistrib index AnnTip{..} = K $
        AnnTip annTipSlotNo
               annTipBlockNo
               (OneEraTipInfo . injectNS index . WrapTipInfo $ annTipInfo)

{-------------------------------------------------------------------------------
  BasicEnvelopeValidation
-------------------------------------------------------------------------------}

instance CanHardFork xs => BasicEnvelopeValidation (HardForkBlock xs) where
  expectedFirstBlockNo _ =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty p _ -> expectedFirstBlockNo p

  minimumPossibleSlotNo _ =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty p _ -> minimumPossibleSlotNo p

  -- TODO: If the block is from a different era as the current tip, we just
  -- expect @succ b@. This may not be sufficient: if we ever transition /to/
  -- an era with EBBs, this is not correct.
  expectedNextBlockNo _ (OneEraTipInfo oldTip) (OneEraTipInfo newBlock) b =
      case Match.matchNS oldTip newBlock of
        Right matched  -> hcollapse $ hcmap proxySingle aux matched
        Left _mismatch -> succ b
    where
      aux :: forall blk. SingleEraBlock blk
          => Product WrapTipInfo WrapTipInfo blk
          -> K BlockNo blk
      aux (Pair (WrapTipInfo old) (WrapTipInfo new)) = K $
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
          => Product WrapTipInfo WrapTipInfo blk
          -> K SlotNo blk
      aux (Pair (WrapTipInfo old) (WrapTipInfo new)) = K $
          minimumNextSlotNo (Proxy @blk) old new s

{-------------------------------------------------------------------------------
  Other instances (primarily for the benefit of tests)
-------------------------------------------------------------------------------}

instance All Eq xs => Eq (HardForkBlock xs) where
  (==) = (aux .: Match.matchNS) `on` (getOneEraBlock . getHardForkBlock)
    where
      aux :: Either (Match.Mismatch I I xs) (NS (Product I I) xs) -> Bool
      aux (Left  _) = False
      aux (Right m) = hcollapse $
                        hcmap (Proxy @Eq) (\(Pair x y) -> K $ x == y) m

instance All (Compose Eq Header) xs => Eq (Header (HardForkBlock xs)) where
  (==) = (aux .: Match.matchNS) `on` (getOneEraHeader . getHardForkHeader)
    where
      aux :: Either (Match.Mismatch Header Header xs) (NS (Product Header Header) xs)
          -> Bool
      aux (Left  _) = False
      aux (Right m) = hcollapse $
                        hcmap
                          (Proxy @(Compose Eq Header))
                          (\(Pair x y) -> K $ x == y)
                          m

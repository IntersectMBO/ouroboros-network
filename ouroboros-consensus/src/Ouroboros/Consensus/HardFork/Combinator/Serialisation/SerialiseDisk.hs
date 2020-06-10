{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk (
      binaryBlockInfo
    ) where

import           Codec.CBOR.Encoding (Encoding)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.SOP.Dict
import           Data.SOP.Strict
import           Data.Word

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.Storage.ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB
                     (BinaryBlockInfo (..))
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

instance SerialiseHFC xs => SerialiseDiskConstraints  (HardForkBlock xs)
instance SerialiseHFC xs => ImmDbSerialiseConstraints (HardForkBlock xs)
instance SerialiseHFC xs => VolDbSerialiseConstraints (HardForkBlock xs)
instance SerialiseHFC xs => LgrDbSerialiseConstraints (HardForkBlock xs)

{-------------------------------------------------------------------------------
  'ReconstructNestedCtxt'
-------------------------------------------------------------------------------}

instance SerialiseHFC xs => ReconstructNestedCtxt Header (HardForkBlock xs) where
  reconstructPrefixLen _ =
      -- We insert two bytes at the front
      2 + maximum (hcollapse perEra)
    where
      perEra :: NP (K Word8) xs
      perEra = hcpure proxySingle reconstructOne

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K Word8 blk
      reconstructOne = K $ reconstructPrefixLen (Proxy @(Header blk))

  reconstructNestedCtxt _ prefix blockSize =
     case nsFromIndex tag of
       Nothing -> error $ "invalid HardForkBlock with tag: " <> show tag
       Just ns -> injSomeBlock $ hcmap proxySingle reconstructOne ns
    where
      tag :: Word8
      tag = Short.index prefix 1

      prefixOne :: ShortByteString
      prefixOne = Short.pack . drop 2 . Short.unpack $ prefix

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K () blk -> SomeBlock (NestedCtxt Header) blk
      reconstructOne _ =
          reconstructNestedCtxt (Proxy @(Header blk)) prefixOne blockSize

      injSomeBlock :: NS (SomeBlock (NestedCtxt Header)) xs'
                   -> SomeBlock (NestedCtxt Header) (HardForkBlock xs')
      injSomeBlock (Z x) = case x of
          SomeBlock (NestedCtxt y) -> SomeBlock (NestedCtxt (NCZ y))
      injSomeBlock (S x) = case injSomeBlock x of
          SomeBlock (NestedCtxt y) -> SomeBlock (NestedCtxt (NCS y))

-- | 'BinaryBlockInfo' compatible with the HFC defaults
--
-- This function should not be used when non-uniform encoding is used for
-- blocks.
binaryBlockInfo :: NP (I -.-> K BinaryBlockInfo) xs
                -> HardForkBlock xs -> BinaryBlockInfo
binaryBlockInfo fs (HardForkBlock (OneEraBlock bs)) =
    npToSListI fs $
      hcollapse $ hzipWith aux fs bs
  where
    -- The header is unchanged, but the whole block is offset by 2 bytes
    -- (list length and tag)
    aux :: (I -.-> K BinaryBlockInfo) blk -> I blk -> K BinaryBlockInfo blk
    aux (Fn f) blk = K $ BinaryBlockInfo {
          headerOffset = headerOffset underlyingBlockInfo + 2
        , headerSize   = headerSize   underlyingBlockInfo
        }
      where
        underlyingBlockInfo :: BinaryBlockInfo
        underlyingBlockInfo = unK $ f blk

{-------------------------------------------------------------------------------
  Blocks/headers
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => EncodeDisk (HardForkBlock xs) (HardForkBlock xs) where
  encodeDisk = encodeDiskHfcBlock

instance SerialiseHFC xs
      => DecodeDisk (HardForkBlock xs) (Lazy.ByteString -> HardForkBlock xs) where
  decodeDisk = decodeDiskHfcBlock

instance SerialiseHFC xs
      => EncodeDiskDepIx (NestedCtxt Header) (HardForkBlock xs) where
  encodeDiskDepIx = encodeNestedCtxt

instance SerialiseHFC xs
      => DecodeDiskDepIx (NestedCtxt Header) (HardForkBlock xs) where
  decodeDiskDepIx = decodeNestedCtxt

instance SerialiseHFC xs
      => EncodeDiskDep (NestedCtxt Header) (HardForkBlock xs) where
  encodeDiskDep =
      case dict of
        Dict -> encodeNested
    where
      dict :: Dict (All (EncodeDiskDep (NestedCtxt Header))) xs
      dict = all_NP (hcpure pSHFC Dict)

instance SerialiseHFC xs
      => DecodeDiskDep (NestedCtxt Header) (HardForkBlock xs) where
  decodeDiskDep =
      case dict of
        Dict -> decodeNested
    where
      dict :: Dict (All (DecodeDiskDep (NestedCtxt Header))) xs
      dict = all_NP (hcpure pSHFC Dict)

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => EncodeDisk (HardForkBlock xs) (AnnTip (HardForkBlock xs)) where
  encodeDisk cfg =
        encodeNS (hcmap pSHFC (fn . (K .: encodeDisk)) cfgs)
      . distribAnnTip
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

instance SerialiseHFC xs
      => DecodeDisk (HardForkBlock xs) (AnnTip (HardForkBlock xs)) where
  decodeDisk cfg =
        fmap undistribAnnTip
      $ decodeNS (hcmap pSHFC (Comp . decodeDisk) cfgs)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

instance SerialiseHFC xs
      => EncodeDisk (HardForkBlock xs) (HardForkConsensusState xs) where
  encodeDisk cfg =
      encodeTelescope (hcmap pSHFC (fn . aux) cfgs)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

      aux :: SerialiseDiskConstraints blk
          => CodecConfig blk -> WrapConsensusState blk -> K Encoding blk
      aux cfg' (WrapConsensusState st) = K $ encodeDisk cfg' st

instance SerialiseHFC xs
      => DecodeDisk (HardForkBlock xs) (HardForkConsensusState xs) where
  decodeDisk cfg =
      decodeTelescope (hcmap pSHFC (Comp . fmap WrapConsensusState . decodeDisk) cfgs)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

instance SerialiseHFC xs
      => EncodeDisk (HardForkBlock xs) (LedgerState (HardForkBlock xs) )where
  encodeDisk cfg =
        encodeTelescope (hcmap pSHFC (fn . (K .: encodeDisk)) cfgs)
      . getHardForkLedgerState
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

instance SerialiseHFC xs
      => DecodeDisk (HardForkBlock xs) (LedgerState (HardForkBlock xs)) where
  decodeDisk cfg =
        fmap HardForkLedgerState
      $ decodeTelescope (hcmap pSHFC (Comp . decodeDisk) cfgs)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

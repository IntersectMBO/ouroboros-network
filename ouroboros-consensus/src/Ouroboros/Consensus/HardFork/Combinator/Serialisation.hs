{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

-- | Serialisation support for the HFC
--
-- The HFC does not provide default instances for any serialisation class, since
-- serialisation often needs to take advantage of the concrete eras that it is
-- transitioning between. However, we /can/ provide some default implementations
-- here that implementors of specific chains can take advantage of if they wish.
--
-- Intended for qualified import
--
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation as Default
module Ouroboros.Consensus.HardFork.Combinator.Serialisation (
    -- * Nested contents
    encodeNested
  , decodeNested
  , encodeNestedCtxt
  , decodeNestedCtxt
    -- * ReconstructNestedCtxt
  , binaryBlockInfo
    -- * Deriving via
  , SerialiseOne(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.SOP.Strict
import           Data.Word

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Util (Some (..))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block

{-------------------------------------------------------------------------------
  NestedContext
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
                 -> SomeBlock (NestedCtxt f) (HardForkBlock xs)
                 -> Encoding
encodeNestedCtxt = \ccfg (SomeBlock ctxt) ->
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
        , encode i
        , encodeDiskDepIx c (SomeBlock (NestedCtxt ctxt))
        ]

decodeNestedCtxt :: All (DecodeDiskDepIx (NestedCtxt f)) xs
                 => CodecConfig (HardForkBlock xs)
                 -> forall s. Decoder s (SomeBlock (NestedCtxt f) (HardForkBlock xs))
decodeNestedCtxt = \ccfg -> do
    enforceSize "decodeNestedCtxt" 2
    tag <- decode
    case nsFromIndex tag of
      Nothing -> fail "decodeNestedCtxt: invalid tag"
      Just ns -> flipSomeNestedCtxt <$>
        go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ns
  where
    go :: All (DecodeDiskDepIx (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NS (K ()) xs'
       -> forall s. Decoder s (Some (NestedCtxt_ (HardForkBlock xs') f))
    go Nil       i     = case i of {}
    go (c :* _)  (Z _) = do Some ctxt <- unflipSomeNestedCtxt <$> decodeDiskDepIx c
                            return $ Some (NCZ ctxt)
    go (_ :* cs) (S i) = do Some ctxt <- go cs i
                            return $ Some (NCS ctxt)

{-------------------------------------------------------------------------------
  'ReconstructNestedCtxt'
-------------------------------------------------------------------------------}

instance CanHardFork xs => ReconstructNestedCtxt Header (HardForkBlock xs) where

  reconstructPrefixLen _ =
      -- We insert two bytes at the front
      2 + maximum (hcollapse perEra)
    where
      perEra :: NP (K Word8) xs
      perEra = hcpure (Proxy @SingleEraBlock) reconstructOne

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K Word8 blk
      reconstructOne = K $ reconstructPrefixLen (Proxy @(Header blk))

  reconstructNestedCtxt _ prefix blockSize =
     case nsFromIndex tag of
       Nothing -> error $ "invalid HardForkBlock with tag: " <> show tag
       Just ns -> injSomeBlock $ hcmap (Proxy @SingleEraBlock) reconstructOne ns
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

-- | 'BinaryBlockInfo' compatible with 'SerialiseOne'
binaryBlockInfo :: NP (I -.-> K BinaryBlockInfo) xs
                -> HardForkBlock xs -> BinaryBlockInfo
binaryBlockInfo fs (HardForkBlock (OneEraBlock bs)) =
    npToSListI fs $
      hcollapse $ hzipWith aux fs bs
  where
    -- The header is unchanged, but the whole block if offset by 2 bytes
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
  Deriving-via support
-------------------------------------------------------------------------------}

-- | Used for deriving via
--
-- Example
--
-- > deriving via SerialiseOne Header SomeEras
-- >          instance Serialise (Header SomeBlock)
newtype SerialiseOne f xs = SerialiseOne {
      getSerialiseOne :: NS f xs
    }

instance ( All SingleEraBlock xs
         , All (Compose Serialise f) xs
         ) => Serialise (SerialiseOne f xs) where
  encode (SerialiseOne ns) =
      hcollapse $ hczipWith (Proxy @(Compose Serialise f)) aux indices ns
    where
      aux :: Compose Serialise f blk => K Word8 blk -> f blk -> K Encoding blk
      aux (K i) x = K $ mconcat [
            Enc.encodeListLen 2
          , Enc.encodeWord8 i
          , encode x
          ]

      indices :: NP (K Word8) xs
      indices = go 0 sList
        where
          go :: Word8 -> SList xs' -> NP (K Word8) xs'
          go !_ SNil  = Nil
          go !i SCons = K i :* go (i + 1) sList

  decode = do
      enforceSize "SerialiseOne" 2
      i <- fromIntegral <$> Dec.decodeWord8
      if i < length decoders
        then SerialiseOne <$> decoders !! i
        else fail "decode: invalid index"
    where
      decoders :: [Decoder s (NS f xs)]
      decoders = hcollapse $ hcmap (Proxy @(Compose Serialise f)) aux injections

      aux :: Compose Serialise f blk
          => Injection f xs blk -> K (Decoder s (NS f xs)) blk
      aux inj = K (unK . apFn inj <$> decode)

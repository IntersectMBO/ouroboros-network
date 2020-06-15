{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Ouroboros.Consensus.Storage.ChainDB.Impl.BlockComponent
  ( BlockComponent (..)
  , translateToRawDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Proxy

import           Ouroboros.Network.Block (pattern BlockPoint, HasHeader,
                     HeaderHash, SlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockRef (..),
                     ChainDB, ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
                     (DecodeDisk (..), DecodeDiskDep (..),
                     ReconstructNestedCtxt (..), SizeInBytes)
import           Ouroboros.Consensus.Storage.Common

-- | Translate a ChainDB 'BlockComponent' into a 'BlockComponent' known by the
-- ImmutableDB and the VolatileDB.
translateToRawDB
  :: forall m blk b db.
     ( HasHeader blk
     , DBHeaderHash db ~ HeaderHash blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , ReconstructNestedCtxt Header blk
     , DecodeDiskDep (NestedCtxt Header) blk
     , MonadThrow m
     )
  => CodecConfig blk
  -> BlockComponent (ChainDB m blk) b
  -> BlockComponent db b
translateToRawDB ccfg = go
  where
    go :: forall b'. BlockComponent (ChainDB m blk) b' -> BlockComponent db b'
    go = \case
      GetBlock        -> parseBlock ccfg <$> getBlockRef <*> GetRawBlock
      GetRawBlock     -> GetRawBlock
      GetHeader       -> parseHeader ccfg
                           <$> getBlockRef
                           <*> GetNestedCtxt prefixLen
                           <*> GetBlockSize
                           <*> GetRawHeader
      GetRawHeader    -> GetRawHeader
      GetHash         -> GetHash
      GetSlot         -> GetSlot
      GetIsEBB        -> GetIsEBB
      GetBlockSize    -> GetBlockSize
      GetHeaderSize   -> GetHeaderSize
      GetNestedCtxt n -> GetNestedCtxt n
      GetPure a       -> GetPure a
      GetApply f bc   -> GetApply (go f) (go bc)

    prefixLen :: PrefixLen
    prefixLen = reconstructPrefixLen (Proxy @(Header blk))

getBlockRef :: DBHeaderHash db ~ HeaderHash blk
            => BlockComponent db (BlockRef blk)
getBlockRef = mkBlockRef <$> GetSlot <*> GetHash <*> GetIsEBB
  where
    mkBlockRef :: SlotNo -> HeaderHash blk -> IsEBB -> BlockRef blk
    mkBlockRef slot hash = BlockRef (BlockPoint slot hash)

parseBlock
  :: ( HasHeader blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , MonadThrow m
     )
  => CodecConfig blk
  -> BlockRef blk
  -> Lazy.ByteString
  -> m blk
parseBlock ccfg blockRef bytes = throwParseErrors blockRef bytes $
      CBOR.deserialiseFromBytes (decodeDisk ccfg) bytes

parseHeader
  :: forall m blk.
     ( HasHeader blk
     , ReconstructNestedCtxt Header blk
     , DecodeDiskDep (NestedCtxt Header) blk
     , MonadThrow m
     )
  => CodecConfig blk
  -> BlockRef blk
  -> ShortByteString
  -> SizeInBytes
  -> Lazy.ByteString
  -> m (Header blk)
parseHeader ccfg blockRef prefix blockSize bytes =
    case reconstructNestedCtxt (Proxy @(Header blk)) prefix blockSize of
      SomeBlock ctxt ->
        throwParseErrors blockRef bytes $
          CBOR.deserialiseFromBytes (parser ctxt) bytes
  where
    parser :: NestedCtxt Header blk a -> Decoder s (Lazy.ByteString -> Header blk)
    parser ctxt = (\f -> nest . DepPair ctxt . f) <$> decodeDiskDep ccfg ctxt

throwParseErrors
  :: (HasHeader blk, MonadThrow m)
  => BlockRef blk
  -> Lazy.ByteString
  -> Either CBOR.DeserialiseFailure (Lazy.ByteString, Lazy.ByteString -> b)
  -> m b
throwParseErrors blockRef fullBytes = \case
    Right (trailing, f)
      | Lazy.null trailing -> return $ f fullBytes
      | otherwise          -> throwM $ ChainDbTrailingData blockRef trailing
    Left err               -> throwM $ ChainDbParseFailure blockRef err

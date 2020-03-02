{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Consensus.Storage.Common (
    -- * Epochs
    EpochNo(..)
  , EpochSize(..)
    -- * Indexing
  , tipIsGenesis
    -- * BinaryInfo
  , BinaryInfo (..)
  , extractHeader
    -- * BlockComponent
  , DB (..)
  , BlockComponent (..)
  , castBlockComponent
    -- * Re-exports
  , SizeInBytes
  ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           GHC.Generics

import           Cardano.Slotting.Slot

import           Ouroboros.Network.DeltaQ (SizeInBytes)

import           Ouroboros.Consensus.Block

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

tipIsGenesis :: WithOrigin r -> Bool
tipIsGenesis Origin = True
tipIsGenesis (At _) = False

{-------------------------------------------------------------------------------
  BinaryInfo
-------------------------------------------------------------------------------}

-- | Information about the serialised block.
data BinaryInfo blob = BinaryInfo
  { binaryBlob   :: !blob
  , headerOffset :: !Word16
    -- ^ The offset within the 'binaryBlob' at which the header starts.
  , headerSize   :: !Word16
    -- ^ How many bytes the header is long. Extracting the 'headerSize' bytes
    -- from 'binaryBlob' starting from 'headerOffset' should yield the header.

    -- In the future, i.e. Shelley, we might want to extend this to include a
    -- field to tell where the transaction body ends and where the transaction
    -- witnesses begin so we can only extract the transaction body.
  } deriving (Eq, Show, Generic, Functor)

-- | Utility for BinaryInfo of ByteString which are quite common.
extractHeader :: BinaryInfo ByteString -> ByteString
extractHeader BinaryInfo { binaryBlob, headerOffset, headerSize } =
    BL.take (fromIntegral headerSize) $
    BL.drop (fromIntegral headerOffset) binaryBlob

{-------------------------------------------------------------------------------
  BlockComponent
-------------------------------------------------------------------------------}

-- | The type of a block, header, and header hash of a database. Used by
-- 'BlockComponent'.
class DB db where
  type DBBlock      db
  type DBHeader     db
  type DBHeaderHash db

-- | Which component of the block to read from a database: the whole block,
-- its header, its hash, the block size, ..., or combinations thereof.
--
-- NOTE: when requesting multiple components, we will not optimise/cache them.
data BlockComponent db a where
  GetBlock      :: BlockComponent db (DBBlock db)
  GetRawBlock   :: BlockComponent db ByteString
  GetHeader     :: BlockComponent db (DBHeader db)
  GetRawHeader  :: BlockComponent db ByteString
  GetHash       :: BlockComponent db (DBHeaderHash db)
  GetSlot       :: BlockComponent db SlotNo
  GetIsEBB      :: BlockComponent db IsEBB
  GetBlockSize  :: BlockComponent db Word32
  GetHeaderSize :: BlockComponent db Word16
  GetPure       :: a
                -> BlockComponent db a
  GetApply      :: BlockComponent db (a -> b)
                -> BlockComponent db a
                -> BlockComponent db b

instance Functor (BlockComponent db) where
  fmap f = (GetPure f <*>)

instance Applicative (BlockComponent db) where
  pure  = GetPure
  (<*>) = GetApply

-- | Cast one 'BlockComponent' to another when all associated types of the two
-- databases match.
castBlockComponent
  :: ( DBBlock      db1 ~ DBBlock      db2
     , DBHeader     db1 ~ DBHeader     db2
     , DBHeaderHash db1 ~ DBHeaderHash db2
     )
  => BlockComponent db1 b
  -> BlockComponent db2 b
castBlockComponent = \case
    GetBlock      -> GetBlock
    GetRawBlock   -> GetRawBlock
    GetHeader     -> GetHeader
    GetRawHeader  -> GetRawHeader
    GetHash       -> GetHash
    GetSlot       -> GetSlot
    GetIsEBB      -> GetIsEBB
    GetBlockSize  -> GetBlockSize
    GetHeaderSize -> GetHeaderSize
    GetPure a     -> GetPure a
    GetApply f bc -> GetApply (castBlockComponent f) (castBlockComponent bc)

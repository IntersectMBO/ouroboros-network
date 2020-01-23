{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Storage.Common (
    -- * Epochs
    EpochNo(..)
  , EpochSize(..)
    -- * Indexing
  , Tip(..)
  , tipIsGenesis
  , tipToPoint
  , tipFromPoint
    -- * Serialisation
  , encodeTip
  , decodeTip
    -- * BinaryInfo
  , BinaryInfo (..)
    -- * BlockComponent
  , DB (..)
  , BlockComponent (..)
  , castBlockComponent
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.ByteString.Lazy (ByteString)
import           Data.Word
import           GHC.Generics

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Ouroboros.Network.Block (Point (..), SlotNo, genesisPoint)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (IsEBB)
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | Tip of the chain
data Tip r = Tip !r | TipGen
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NoUnexpectedThunks)

-- | 'TipGen' is always smaller than 'Tip'
instance Ord r => Ord (Tip r) where
  compare x y = case (x, y) of
    (TipGen, TipGen) -> EQ
    (TipGen, Tip _)  -> LT
    (Tip _,  TipGen) -> GT
    (Tip a,  Tip b)  -> compare a b

instance Condense r => Condense (Tip r) where
  condense TipGen  = "genesis"
  condense (Tip r) = condense r

tipIsGenesis :: Tip r -> Bool
tipIsGenesis TipGen  = True
tipIsGenesis (Tip _) = False

tipToPoint :: Tip (Point blk) -> Point blk
tipToPoint TipGen  = genesisPoint
tipToPoint (Tip p) = p

-- | Tip from a point
--
-- NOTE: We really shouldn't instantate 'Tip' with 'Point'; see
-- <https://github.com/input-output-hk/ouroboros-network/issues/1155>
tipFromPoint :: Point blk -> Tip (Point blk)
tipFromPoint (Point Origin) = TipGen
tipFromPoint p              = Tip p

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance Serialise r => Serialise (Tip r) where
  encode = encodeTip encode
  decode = decodeTip decode

encodeTip :: (r     -> Encoding)
          -> (Tip r -> Encoding)
encodeTip encodeR tip =
    case tip of
      TipGen -> Enc.encodeListLen 0
      Tip r  -> Enc.encodeListLen 1 <> encodeR r

decodeTip :: (forall s. Decoder s r)
          -> (forall s. Decoder s (Tip r))
decodeTip decodeR = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return TipGen
      1 -> Tip <$> decodeR
      _ -> fail "decodeTip: invalid tag"

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

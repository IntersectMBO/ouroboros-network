{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Ouroboros.Chain.HasHeader (
    -- * Hash
    HeaderHash
  , StandardHash
  , ChainHash (..)
  , castHash
    -- * HasHeader
  , HasHeader (..)
  , HeaderFields (..)
  , castHeaderFields
  , blockHash
  , blockSlot
  , blockNo
    -- * Finger Tree Measure
  , BlockMeasure (..)
  , blockMeasure
    -- * Serialisation
  , encodeChainHash
  , decodeChainHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Coerce (Coercible, coerce)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (SlotNo (..))

{-------------------------------------------------------------------------------
  Hash
-------------------------------------------------------------------------------}

-- | Header hash
type family HeaderHash b :: Type

-- | 'StandardHash' summarises the constraints we want header hashes to have
--
-- Without this class we would need to write
--
-- > deriving instance Eq (HeaderHash block) => Eq (ChainHash block)`
--
-- That requires @UndecidableInstances@; not a problem by itself, but it also
-- means that we can then not use @deriving Eq@ anywhere else for datatypes that
-- reference 'Hash', which is very frustrating; see
--
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#inferred-context-for-deriving-clauses>
--
-- Introducing the 'StandardHash' class avoids this problem.
--
-- Having these constraints directly as part of the 'HasHeader' class is
-- possible but libraries that /use/ the networking layer may wish to be able to
-- talk about 'StandardHash' independently of 'HasHeader' since the latter may
-- impose yet further constraints.
class ( Eq       (HeaderHash b)
      , Ord      (HeaderHash b)
      , Show     (HeaderHash b)
      , Typeable (HeaderHash b)
      , NoThunks (HeaderHash b)
      ) => StandardHash b

data ChainHash b = GenesisHash | BlockHash !(HeaderHash b)
  deriving (Generic)

deriving instance StandardHash block => Eq   (ChainHash block)
deriving instance StandardHash block => Ord  (ChainHash block)
deriving instance StandardHash block => Show (ChainHash block)

instance (StandardHash block, Typeable block) => NoThunks (ChainHash block)
  -- use generic instance

castHash :: Coercible (HeaderHash b) (HeaderHash b') => ChainHash b -> ChainHash b'
castHash GenesisHash   = GenesisHash
castHash (BlockHash h) = BlockHash (coerce h)

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

-- | Header fields we expect to be present in a block (header).
--
-- These fields are lazy because they are extracted from a block or block
-- header; this type is not intended for storage.
data HeaderFields b = HeaderFields {
      headerFieldSlot    :: SlotNo
    , headerFieldBlockNo :: BlockNo
    , headerFieldHash    :: HeaderHash b
      -- ^ NOTE: this field is last so that the derived 'Eq' and 'Ord'
      -- instances first compare the slot and block numbers, which is cheaper
      -- than comparing hashes.
    }
  deriving (Generic)

deriving instance StandardHash b => Show (HeaderFields b)
deriving instance StandardHash b => Eq   (HeaderFields b)
deriving instance StandardHash b => Ord  (HeaderFields b)

-- Serialise instance only for the benefit of tests
deriving instance Serialise (HeaderHash b) => Serialise (HeaderFields b)

type instance HeaderHash (HeaderFields b) = HeaderHash b

instance StandardHash b => StandardHash (HeaderFields b)

instance (StandardHash b, Typeable b)
      => HasHeader (HeaderFields b) where
  getHeaderFields = castHeaderFields

instance (StandardHash b, Typeable b)
      => Measured BlockMeasure (HeaderFields b) where
  measure = blockMeasure

castHeaderFields ::
     HeaderHash b ~ HeaderHash b'
  => HeaderFields b -> HeaderFields b'
castHeaderFields (HeaderFields h s b) = HeaderFields h s b

-- | Abstract over the shape of blocks (or indeed just block headers)
class (StandardHash b, Measured BlockMeasure b, Typeable b) => HasHeader b where
  getHeaderFields :: b -> HeaderFields b

blockHash :: HasHeader b => b -> HeaderHash b
blockHash = headerFieldHash . getHeaderFields

blockSlot :: HasHeader b => b -> SlotNo
blockSlot = headerFieldSlot . getHeaderFields

blockNo :: HasHeader b => b -> BlockNo
blockNo = headerFieldBlockNo . getHeaderFields

{-------------------------------------------------------------------------------
  Finger Tree Measure
-------------------------------------------------------------------------------}

-- | The measure used for @ChainFragment@ and @AnchoredFragment@.
data BlockMeasure = BlockMeasure {
       bmMinSlot :: !SlotNo,
       bmMaxSlot :: !SlotNo,
       bmSize    :: !Int
     }
  deriving (Show)

instance Semigroup BlockMeasure where
  vl <> vr =
    BlockMeasure
      (min (bmMinSlot vl) (bmMinSlot vr))
      (max (bmMaxSlot vl) (bmMaxSlot vr))
      (bmSize vl + bmSize vr)

instance Monoid BlockMeasure where
  mempty  = BlockMeasure maxBound minBound 0
  mappend = (<>)

-- | When implementing 'HasHeader', use this method to implement the 'measure'
-- method of the 'Measured' super class.
blockMeasure :: HasHeader b => b -> BlockMeasure
blockMeasure b = BlockMeasure (blockSlot b) (blockSlot b) 1

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise (HeaderHash b) => Serialise (ChainHash b) where
  encode = encodeChainHash encode
  decode = decodeChainHash decode

encodeChainHash :: (HeaderHash block -> Encoding)
                -> (ChainHash  block -> Encoding)
encodeChainHash encodeHash chainHash =
    case chainHash of
      GenesisHash -> Enc.encodeListLen 0
      BlockHash h -> Enc.encodeListLen 1 <> encodeHash h

decodeChainHash :: (forall s. Decoder s (HeaderHash block))
                -> (forall s. Decoder s (ChainHash  block))
decodeChainHash decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return GenesisHash
      1 -> BlockHash <$> decodeHash
      _ -> fail "decodeChainHash: invalid tag"

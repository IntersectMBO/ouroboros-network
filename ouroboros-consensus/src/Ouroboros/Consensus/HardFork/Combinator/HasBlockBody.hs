{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.Combinator.HasBlockBody (
    -- * Generic infrastructure
    HasBlockBody(..)
  , splitBlock
    -- * HFC support
  , SerialiseHeaderBody(..)
  , Body(..)
  , hardForkBlockBinaryBlockInfo
  , toSerialiseOneHeader
  , toSerialiseOneBody
  , fromSerialiseOneHeader
  , fromSerialiseOneBody
  ) where

import           Codec.Serialise
import           Codec.Serialise.Encoding
import qualified Data.ByteString.Lazy as BS.L
import           Data.Functor.Product
import           Data.SOP.Strict

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

{-------------------------------------------------------------------------------
  HasBlockBody
-------------------------------------------------------------------------------}

-- | Blocks that can be split into a header and a body
--
-- The immutable DB needs to be able to read a header out of a block (see
-- 'BinaryBlockInfo'). When we are dealing with blocks from multiple eras,
-- this is a bit tricky: the encoder for a /header/ of a 'HardForkBlock' would
-- insert a tag to indicate which era the header is from, but the encoder for
-- a 'HardForkBlock' would instead insert a tag around the whole block, and
-- not have a nested tag around the header. This means that we cannot directly
-- apply the header decoder to the header nested inside a block, as it is
-- missing a tag.
--
-- An alternative approach, supported by this class, is to split the block
-- into a header and a body, and then encode the two separately. This is
-- primarily useful for tests, but might also be useful for real ledger
-- implementations, provided that the binary format is not dictated by
-- other means.
--
-- This class is not required by the hard fork combinator, merely provided as
-- an option.
class GetHeader blk => HasBlockBody blk where
  data family Body blk :: *

  -- | Get the block body
  --
  -- Getting the header comes from 'GetHeader'
  getBody :: blk -> Body blk

  -- | Reassemble the block from its header and its body
  --
  -- Can fail if this header and this body don't fit together.
  -- Should be 'Just' iff 'blockMatchesHeader'.
  assembleBlock :: Header blk -> Body blk -> Maybe blk

splitBlock :: HasBlockBody blk => blk -> (Header blk, Body blk)
splitBlock blk = (getHeader blk, getBody blk)

{-------------------------------------------------------------------------------
  Instance for 'HardForkBlock'
-------------------------------------------------------------------------------}

instance (CanHardFork xs, All HasBlockBody xs)
      => HasBlockBody (HardForkBlock xs) where
  newtype Body (HardForkBlock xs) = HardForkBody {
        getHardForkBody :: NS Body xs
      }

  getBody = HardForkBody
          . hcmap (Proxy @HasBlockBody) (getBody . unI)
          . getOneEraBlock
          . getHardForkBlock

  assembleBlock (HardForkHeader hdr) (HardForkBody body) =
      case Match.matchNS (getOneEraHeader hdr) body of
        Left _mismatch -> Nothing
        Right match    -> fmap (HardForkBlock . OneEraBlock)
                        . hsequence'
                        . hcmap (Proxy @HasBlockBody) assembleOne
                        $ match
    where
      assembleOne :: HasBlockBody blk
                  => Product Header Body blk -> (Maybe :.: I) blk
      assembleOne (Pair hdr' body') = Comp $ I <$> assembleBlock hdr' body'

{-------------------------------------------------------------------------------
  Serialisation support
-------------------------------------------------------------------------------}

-- | Serialise header and body separately
--
-- Can be used for deriving via.
newtype SerialiseHeaderBody xs = SerialiseHeaderBody {
      getSerialiseHeaderBody :: HardForkBlock xs
    }

toSerialiseOneHeader :: Header (HardForkBlock xs) -> SerialiseOne Header xs
toSerialiseOneHeader = SerialiseOne . getOneEraHeader . getHardForkHeader

fromSerialiseOneHeader :: SerialiseOne Header xs -> Header (HardForkBlock xs)
fromSerialiseOneHeader = HardForkHeader . OneEraHeader . getSerialiseOne

toSerialiseOneBody :: Body (HardForkBlock xs) -> SerialiseOne Body xs
toSerialiseOneBody = SerialiseOne . getHardForkBody

fromSerialiseOneBody :: SerialiseOne Body xs -> Body (HardForkBlock xs)
fromSerialiseOneBody = HardForkBody . getSerialiseOne

instance ( CanHardFork xs
         , All HasBlockBody xs
         , All (Compose Serialise Header) xs
         , All (Compose Serialise Body) xs
         ) => Serialise (SerialiseHeaderBody xs) where
  encode =
      uncurry aux . splitBlock . getSerialiseHeaderBody
    where
      aux :: Header (HardForkBlock xs)
          -> Body   (HardForkBlock xs)
          -> Encoding
      aux hdr body = mconcat [
          encodeListLen 2
        , encode (toSerialiseOneHeader hdr)
        , encode (toSerialiseOneBody body)
        ]

  decode = do
      enforceSize "SerialiseHeaderBody" 2
      hdr  <- fromSerialiseOneHeader <$> decode
      body <- fromSerialiseOneBody   <$> decode
      case assembleBlock hdr body of
        Nothing  -> fail "SerialiseHeaderBody: header/body mismatch"
        Just blk -> return $ SerialiseHeaderBody blk

-- | 'BinaryBlockInfo' for 'HardForkBlock'
--
-- This function is only useable if that serialisation strategy is used
-- consistently. See 'toSerialiseOneHeader' and friends. Typically used with
--
-- > deriving via SerialiseHeaderBody SomeEras
-- >          instance Serialise SomeBlock
-- >
-- > deriving via SerialiseOne Header SomeEras
-- >          instance Serialise (Header SomeBlock)
--
-- TODO: The way this is currently defined is not very efficient; this is not
-- a big deal right now as this is currently only used for tests.
hardForkBlockBinaryBlockInfo
  :: ( CanHardFork xs
     , All (Compose Serialise Header) xs
     )
  => HardForkBlock xs
  -> BinaryBlockInfo
hardForkBlockBinaryBlockInfo blk = BinaryBlockInfo {
      headerOffset = 1 -- Skip the list length
    , headerSize   = fromIntegral . BS.L.length . serialise $
                       toSerialiseOneHeader (getHeader blk)
    }

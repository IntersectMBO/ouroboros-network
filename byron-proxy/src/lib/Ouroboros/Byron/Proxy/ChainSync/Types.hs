module Ouroboros.Byron.Proxy.ChainSync.Types
  ( Block (..)
  , Point (..)
  ) where

import Control.Monad.Class.MonadST (MonadST)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Codec.Serialise (Serialise (..))
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Network.TypedProtocol.Codec (Codec)
import qualified Pos.Binary.Class as CSL (decode, encode)
import qualified Pos.Chain.Block as CSL (Block, HeaderHash)

import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Storage.ImmutableDB.API (SlotNo (..))

data Point = Point
  { pointSlot :: !SlotNo
  , pointHash :: !CSL.HeaderHash
  }
  deriving (Show, Eq)

-- | A newtype for the cardano-sl `Block`, for use in this chain sync codec.
-- Needed because supporting programs use `Serialise` instances, rather than
-- `Encoding` and `Decoder` terms, in order to make codecs.
newtype Block = Block
  { getBlock :: CSL.Block
  }

-- | Rip this from the cardano-sl `Bi` class.
instance Serialise Block where
  encode = CSL.encode . getBlock
  decode = Block <$> CSL.decode

instance Serialise Point where
  encode point =
       CBOR.encodeListLen 2
    <> encode (pointSlot point)
    <> CSL.encode (pointHash point)
  decode = do
    n <- CBOR.decodeListLen
    case n of
      2 -> Point <$> decode <*> CSL.decode
      _ -> fail "Point: invalid list length"

codec
  :: (MonadST m)
  => Codec (ChainSync Block Point) CBOR.DeserialiseFailure m Lazy.ByteString
codec = codecChainSync

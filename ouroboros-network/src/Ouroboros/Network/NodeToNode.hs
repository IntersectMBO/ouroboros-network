{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode (
    NodeToNodeProtocols(..)
  , NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DictVersion (..)
  ) where

import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Word
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..))
import           Codec.SerialiseTerm (SerialiseTerm (..))

import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Mux.Types (ProtocolEnum(..))


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-node protocol.
--
data NodeToNodeProtocols = ChainSyncWithHeaders
                         | BlockFetch
                         | TxSubmission
  deriving (Eq, Ord, Enum, Bounded, Show)

-- These protocol numbers end up in the wire format so it is vital that they
-- are stable, even as they are upgraded. So we use custom Enum instances here.
-- This allows us to retire old versions and add new, which may leave some
-- holes in the numbering space.

-- | These are the actual wire format protocol numbers.
--
-- The application specific protocol numbers start from 2 because of the two
-- mux built-in protocols.
--
-- These are chosen to not overlap with the node to client protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToNodeProtocols where

  fromProtocolEnum ChainSyncWithHeaders = 2
  fromProtocolEnum BlockFetch           = 3
  fromProtocolEnum TxSubmission         = 4

  toProtocolEnum 2 = Just ChainSyncWithHeaders
  toProtocolEnum 3 = Just BlockFetch
  toProtocolEnum 4 = Just TxSubmission
  toProtocolEnum _ = Nothing

-- |
-- Enumeration of node to node protocol versions.
--
data NodeToNodeVersion = NodeToNodeV_1
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Serialise NodeToNodeVersion where
    encode NodeToNodeV_1 = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return NodeToNodeV_1
        _ -> fail "decode NodeToNodeVersion: unknown tag"

-- |
-- Version data for NodeToNode protocol v1
--
newtype NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic :: Word16 }
  deriving (Eq, Show, Typeable)

instance SerialiseTerm NodeToNodeVersionData where
    encodeTerm NodeToNodeVersionData { networkMagic } =
      CBOR.TInt (fromIntegral networkMagic)

    decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffff = Right (NodeToNodeVersionData $ fromIntegral x)
                             | otherwise             = Left $ T.pack $ "networkMagic out of bound: " <> show x
    decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t

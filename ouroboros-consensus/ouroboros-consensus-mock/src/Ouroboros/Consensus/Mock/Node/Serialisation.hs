{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Serialisation (
    MockBlock
  ) where

import           Codec.Serialise (Serialise, decode, encode)
import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Crypto.Hash (Hash, ShortHash)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (AnnTip,
                     defaultDecodeAnnTip, defaultEncodeAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation

import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

-- | Local shorthand to make the instances more readable
type MockBlock ext = SimpleBlock SimpleMockCrypto ext

{-------------------------------------------------------------------------------
  Disk

  We use the default instances relying on 'Serialise' where possible.
-------------------------------------------------------------------------------}

instance Serialise ext => ImmDbSerialiseConstraints (MockBlock ext)
instance RunMockBlock SimpleMockCrypto ext => LgrDbSerialiseConstraints (MockBlock ext)
instance Serialise ext => VolDbSerialiseConstraints (MockBlock ext)
instance (Serialise ext, RunMockBlock SimpleMockCrypto ext)
      => SerialiseDiskConstraints  (MockBlock ext)

instance Serialise ext => EncodeDisk (MockBlock ext) (MockBlock ext)
instance Serialise ext => DecodeDisk (MockBlock ext) (Lazy.ByteString -> MockBlock ext) where
  decodeDisk _ = const <$> decode

instance Serialise ext => EncodeDisk (MockBlock ext) (Header (MockBlock ext))
instance Serialise ext => DecodeDisk (MockBlock ext) (Lazy.ByteString -> Header (MockBlock ext)) where
  decodeDisk _ = const <$> decode

instance EncodeDisk (MockBlock ext) (LedgerState (MockBlock ext))
instance DecodeDisk (MockBlock ext) (LedgerState (MockBlock ext))

instance EncodeDisk (MockBlock ext) (AnnTip (MockBlock ext)) where
  encodeDisk _ = defaultEncodeAnnTip encode
instance DecodeDisk (MockBlock ext) (AnnTip (MockBlock ext)) where
  decodeDisk _ = defaultDecodeAnnTip decode

{-------------------------------------------------------------------------------
  NodeToNode

  We use the default, unversioned instances relying on 'Serialise' where
  possible.
-------------------------------------------------------------------------------}

instance Serialise ext => SerialiseNodeToNodeConstraints (MockBlock ext)

instance SerialiseNodeToNode (MockBlock ext) (Hash ShortHash (Header (MockBlock ext)))

instance Serialise ext => SerialiseNodeToNode (MockBlock ext) (MockBlock ext) where
  encodeNodeToNode _ _ = defaultEncodeCBORinCBOR
  decodeNodeToNode _ _ = defaultDecodeCBORinCBOR

instance Serialise ext => SerialiseNodeToNode (MockBlock ext) (Header (MockBlock ext)) where
  encodeNodeToNode _ _ = defaultEncodeCBORinCBOR
  decodeNodeToNode _ _ = defaultDecodeCBORinCBOR

instance SerialiseNodeToNode (MockBlock ext) (Serialised (MockBlock ext))
instance SerialiseNodeToNode (MockBlock ext) (Serialised (Header (MockBlock ext)))
instance SerialiseNodeToNode (MockBlock ext) (GenTx (MockBlock ext))
instance SerialiseNodeToNode (MockBlock ext) (GenTxId (MockBlock ext))

{-------------------------------------------------------------------------------
  NodeToClient

  We use the default, unversioned instances relying on 'Serialise' where
  possible.
-------------------------------------------------------------------------------}

instance Serialise ext => SerialiseNodeToClientConstraints (MockBlock ext)

instance SerialiseNodeToClient (MockBlock ext) (Hash ShortHash (Header (MockBlock ext)))

instance Serialise ext => SerialiseNodeToClient (MockBlock ext) (MockBlock ext) where
  encodeNodeToClient _ _ = defaultEncodeCBORinCBOR
  decodeNodeToClient _ _ = defaultDecodeCBORinCBOR

instance SerialiseNodeToClient (MockBlock ext) (Serialised (MockBlock ext))
instance SerialiseNodeToClient (MockBlock ext) (GenTx (MockBlock ext))
instance SerialiseNodeToClient (MockBlock ext) (MockError (MockBlock ext))

instance SerialiseNodeToClient (MockBlock ext) (Some (Query (MockBlock ext))) where
  encodeNodeToClient _ _ (Some q) = case q of {}
  decodeNodeToClient _ _ = error "Mock no Query to decode"

instance SerialiseResult (MockBlock ext) (Query (MockBlock ext)) where
  encodeResult _ _ = \case {}
  decodeResult _ _ = \case {}

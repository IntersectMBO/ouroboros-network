{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Mock.Node.Serialisation (
    MockBlock
  , NestedCtxt_ (..)
  ) where

import           Codec.Serialise (Serialise, decode, encode, serialise)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Block (Serialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (AnnTip,
                     defaultDecodeAnnTip, defaultEncodeAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation

import           Ouroboros.Consensus.HardFork.Combinator (PartialLedgerConfig)
import           Ouroboros.Consensus.Storage.Serialisation

-- | Local shorthand to make the instances more readable
type MockBlock ext = SimpleBlock SimpleMockCrypto ext

{-------------------------------------------------------------------------------
  Disk

  We use the default instances relying on 'Serialise' where possible.
-------------------------------------------------------------------------------}

instance (Serialise ext, Typeable ext) => HasBinaryBlockInfo (MockBlock ext) where
  getBinaryBlockInfo = simpleBlockBinaryBlockInfo

instance (Serialise ext, RunMockBlock SimpleMockCrypto ext)
      => SerialiseDiskConstraints (MockBlock ext)

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

instance Serialise ext => SerialiseNodeToNodeConstraints (MockBlock ext) where
  estimateBlockSize hdr =
      7 {- CBOR-in-CBOR -} + 1 {- encodeListLen 2 -} + hdrSize + bodySize
    where
      hdrSize  = fromIntegral (Lazy.length (serialise hdr))
      bodySize = simpleBodySize (simpleHeaderStd hdr)

instance Serialise ext => SerialiseNodeToNode (MockBlock ext) (MockBlock ext) where
  encodeNodeToNode _ _ = defaultEncodeCBORinCBOR
  decodeNodeToNode _ _ = defaultDecodeCBORinCBOR

instance Serialise ext => SerialiseNodeToNode (MockBlock ext) (Header (MockBlock ext)) where
  encodeNodeToNode ccfg _ = encodeDisk ccfg . unnest
  decodeNodeToNode ccfg _ = nest <$> decodeDisk ccfg

instance SerialiseNodeToNode (MockBlock ext) (Serialised (MockBlock ext))
instance Serialise ext => SerialiseNodeToNode (MockBlock ext) (SerialisedHeader (MockBlock ext)) where
  encodeNodeToNode ccfg _ = encodeDisk ccfg
  decodeNodeToNode ccfg _ = decodeDisk ccfg
instance SerialiseNodeToNode (MockBlock ext) (GenTx (MockBlock ext))
instance SerialiseNodeToNode (MockBlock ext) (GenTxId (MockBlock ext))

{-------------------------------------------------------------------------------
  NodeToClient

  We use the default, unversioned instances relying on 'Serialise' where
  possible.
-------------------------------------------------------------------------------}

instance ( SerialiseNodeToClient (MockBlock ext) (PartialLedgerConfig (MockBlock ext))
         , Serialise ext
         ) => SerialiseNodeToClientConstraints (MockBlock ext)

instance Serialise ext => SerialiseNodeToClient (MockBlock ext) (MockBlock ext) where
  encodeNodeToClient _ _ = defaultEncodeCBORinCBOR
  decodeNodeToClient _ _ = defaultDecodeCBORinCBOR

instance SerialiseNodeToClient (MockBlock ext) (Serialised (MockBlock ext))
instance SerialiseNodeToClient (MockBlock ext) (GenTx (MockBlock ext))
instance SerialiseNodeToClient (MockBlock ext) (MockError (MockBlock ext))

instance SerialiseNodeToClient (MockBlock ext) (SomeSecond BlockQuery (MockBlock ext)) where
  encodeNodeToClient _ _ (SomeSecond QueryLedgerTip) = encode ()
  decodeNodeToClient _ _ = (\() -> SomeSecond QueryLedgerTip) <$> decode

instance SerialiseResult (MockBlock ext) (BlockQuery (MockBlock ext)) where
  encodeResult _ _ QueryLedgerTip = encode
  decodeResult _ _ QueryLedgerTip = decode

{-------------------------------------------------------------------------------
  Nested contents
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (SimpleBlock c ext) f a where
  CtxtMock :: NestedCtxt_ (SimpleBlock c ext) f (f (SimpleBlock c ext))

deriving instance Show (NestedCtxt_ (SimpleBlock c ext) f a)

instance TrivialDependency (NestedCtxt_ (SimpleBlock c ext) f) where
  type TrivialIndex (NestedCtxt_ (SimpleBlock c ext) f) = f (SimpleBlock c ext)

  hasSingleIndex CtxtMock CtxtMock = Refl
  indexIsTrivial = CtxtMock

instance SameDepIndex (NestedCtxt_ (SimpleBlock c ext) f)
instance HasNestedContent f (SimpleBlock c ext)

instance Serialise ext => ReconstructNestedCtxt Header        (MockBlock ext)
instance Serialise ext => EncodeDiskDepIx (NestedCtxt Header) (MockBlock ext)
instance Serialise ext => EncodeDiskDep   (NestedCtxt Header) (MockBlock ext)
instance Serialise ext => DecodeDiskDepIx (NestedCtxt Header) (MockBlock ext)
instance Serialise ext => DecodeDiskDep   (NestedCtxt Header) (MockBlock ext)

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Serialisation for on-disk storage.
--
-- We have separate classes for on-disk and on-the-wire serialisation, because
-- the encoding formats of the same type may differ, depending on the context.
--
-- We separate the encoder from the decoder, because sometimes the encoded
-- type will differ from the decoded one. For example, we encode a @blk@, but
-- decode an @'Lazy.ByteString' -> blk@ (when reading something from disk, we
-- have the precise bytestring that we can pass in as the annotation). If we
-- coupled the encoder to the decoder, we wouldn't be able to cleanly model
-- this use case. Moreover, sometimes we only need a single direction.
module Ouroboros.Consensus.Storage.ChainDB.Serialisation (
    -- * Serialisation to/from disk storage
    EncodeDisk (..)
  , DecodeDisk (..)
    -- * Support for dependent pairs
  , EncodeDiskDepIx(..)
  , EncodeDiskDep(..)
  , DecodeDiskDepIx(..)
  , DecodeDiskDep(..)
    -- * Manipulating raw bytestrings
  , addNestedCtxtEnvelope
  , dropNestedCtxtEnvelope
  , DropNestedCtxtFailure(..)
    -- * Reconstruct nested type
  , ReconstructNestedCtxt (..)
  , addReconstructedTypeEnvelope
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise
import           Control.Exception (Exception)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import           Data.SOP.BasicFunctors
import           Data.Word

import           Cardano.Binary (enforceSize)

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch (SizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.RedundantConstraints

{-------------------------------------------------------------------------------
  Serialisation to/from disk storage
-------------------------------------------------------------------------------}

-- | Encode a type @a@ so that it can be stored on disk, i.e., by the ChainDB.
--
-- There is no version negotiation for on disk serialisation. However,
-- instances can still decide to perform versioning internally to maintain
-- compatibility.
class EncodeDisk blk a where
  encodeDisk :: CodecConfig blk -> a -> Encoding

  -- When the config is not needed, we provide a default implementation using
  -- 'Serialise'
  default encodeDisk
    :: Serialise a
    => CodecConfig blk -> a -> Encoding
  encodeDisk _ccfg = encode

-- | Decode a type @a@ read from disk, i.e., by the ChainDB.
--
-- There is no version negotiation for on disk serialisation. However,
-- instances can still decide to perform versioning internally to maintain
-- compatibility.
class DecodeDisk blk a where
  decodeDisk :: CodecConfig blk -> forall s. Decoder s a

  -- When the config is not needed, we provide a default implementation using
  -- 'Serialise'
  default decodeDisk
    :: Serialise a
    => CodecConfig blk -> forall s. Decoder s a
  decodeDisk _ccfg = decode

{-------------------------------------------------------------------------------
  Dependent pairs
-------------------------------------------------------------------------------}

-- | Encode dependent index
class EncodeDiskDepIx f blk where
  encodeDiskDepIx :: CodecConfig blk -> SomeBlock f blk -> Encoding

  default encodeDiskDepIx
    :: TrivialDependency (f blk)
    => CodecConfig blk -> SomeBlock f blk -> Encoding
  encodeDiskDepIx _ _ = encode ()
    where
      _ = keepRedundantConstraint (Proxy @(TrivialDependency (f blk)))

-- | Encode a dependent value
class EncodeDiskDepIx f blk => EncodeDiskDep f blk where
  encodeDiskDep :: CodecConfig blk -> f blk a -> a -> Encoding

  default encodeDiskDep
    :: ( TrivialDependency (f blk)
       , EncodeDisk blk (TrivialIndex (f blk))
       )
    => CodecConfig blk -> f blk a -> a -> Encoding
  encodeDiskDep cfg ctxt = encodeDisk cfg . fromTrivialDependency ctxt

-- | Decode dependent index
class DecodeDiskDepIx f blk where
  decodeDiskDepIx :: CodecConfig blk -> Decoder s (SomeBlock f blk)

  default decodeDiskDepIx
    :: TrivialDependency (f blk)
    => CodecConfig blk -> Decoder s (SomeBlock f blk)
  decodeDiskDepIx _ = (\() -> SomeBlock indexIsTrivial) <$> decode

-- | Decode a dependent value
--
-- Typical usage: @f = NestedCtxt Header@.
class DecodeDiskDepIx f blk => DecodeDiskDep f blk where
  decodeDiskDep :: CodecConfig blk -> f blk a -> forall s. Decoder s (Lazy.ByteString -> a)

  default decodeDiskDep
    :: ( TrivialDependency (f blk)
       , DecodeDisk blk (Lazy.ByteString -> TrivialIndex (f blk))
       )
    => CodecConfig blk -> f blk a -> forall s. Decoder s (Lazy.ByteString -> a)
  decodeDiskDep cfg ctxt =
      (\f -> toTrivialDependency ctxt . f) <$> decodeDisk cfg

instance EncodeDiskDep f blk => EncodeDisk blk (DepPair (f blk)) where
  encodeDisk ccfg = encodeDisk ccfg . encodeSnd ccfg

instance DecodeDiskDep f blk => DecodeDisk blk (DepPair (f blk)) where
  decodeDisk ccfg = decodeDisk ccfg >>= decodeSnd ccfg

{-------------------------------------------------------------------------------
  Internal: support for serialisation of dependent pairs
-------------------------------------------------------------------------------}

encodeSnd :: EncodeDiskDep f blk
          => CodecConfig blk
          -> DepPair (f blk) -> GenDepPair Serialised (f blk)
encodeSnd ccfg (DepPair fa a) =
    GenDepPair fa (mkSerialised (encodeDiskDep ccfg fa) a)

decodeSnd :: DecodeDiskDep f blk
          => CodecConfig blk
          -> GenDepPair Serialised (f blk) -> Decoder s (DepPair (f blk))
decodeSnd ccfg (GenDepPair fa serialised) =
    DepPair fa <$> fromSerialised (decodeDiskDep ccfg fa) serialised

instance EncodeDiskDepIx f blk => EncodeDisk blk (GenDepPair Serialised (f blk)) where
  encodeDisk ccfg (GenDepPair fa serialised) = mconcat [
        CBOR.encodeListLen 2
      , encodeDiskDepIx ccfg (SomeBlock fa)
      , encode serialised
      ]

instance DecodeDiskDepIx f blk => DecodeDisk blk (GenDepPair Serialised (f blk)) where
  decodeDisk ccfg = do
      enforceSize "DecodeDisk GenDepPair" 2
      SomeBlock fa <- decodeDiskDepIx ccfg
      serialised   <- decode
      return $ GenDepPair fa serialised

{-------------------------------------------------------------------------------
  Add/remove envelope

  These functions manipulate raw bytestrings, adding/dropping an envelope that
  is compatible with the instances for

  > EncodeDisk blk (GenDepPair Serialised (f blk))
  > DecodeDisk blk (GenDepPair Serialised (f blk))

  defined above.
-------------------------------------------------------------------------------}

-- | Add envelope to raw contents as found inside the block
addNestedCtxtEnvelope
  :: EncodeDiskDep (NestedCtxt f) blk
  => CodecConfig blk
  -> (SomeBlock (NestedCtxt f) blk, Lazy.ByteString)
  -> Lazy.ByteString
addNestedCtxtEnvelope ccfg (SomeBlock ctxt, bs) =
    CBOR.toLazyByteString $
      encodeDisk ccfg (GenDepPair ctxt (Serialised bs))

-- | Drop envelope to get the raw contents as found inside the block
dropNestedCtxtEnvelope
  :: DecodeDiskDep (NestedCtxt f) blk
  => CodecConfig blk
  -> Lazy.ByteString
  -> Except DropNestedCtxtFailure (SomeBlock (NestedCtxt f) blk, Lazy.ByteString)
dropNestedCtxtEnvelope ccfg bs = do
    GenDepPair ctxt (Serialised bs') <-
      aux $ CBOR.deserialiseFromBytes (decodeDisk ccfg) bs
    return (SomeBlock ctxt, bs')
  where
    aux :: Either DeserialiseFailure (Lazy.ByteString, a)
        -> Except DropNestedCtxtFailure a
    aux (Left err) =
        throwError $ DropNestedCtxtFailure err
    aux (Right (trailing, a)) =
        if Lazy.null trailing then
          return a
        else
          throwError $ DropNestedCtxtTrailing trailing

data DropNestedCtxtFailure =
    DropNestedCtxtFailure DeserialiseFailure
  | DropNestedCtxtTrailing Lazy.ByteString
  deriving (Show)

instance Exception DropNestedCtxtFailure

{-------------------------------------------------------------------------------
  Reconstruct nested type
-------------------------------------------------------------------------------}

class EncodeDiskDep (NestedCtxt f) blk => ReconstructNestedCtxt f blk where
  -- | Number of bytes required to reconstruct the nested type
  --
  -- This will be the /minimum/ length of the 'ShortByteString' passed to
  -- 'reconstructNestedCtxt'.
  reconstructPrefixLen :: proxy (f blk) -> Word8

  -- | Reconstruct the context of nested contents
  --
  -- TODO: Right now we must be able to reconstruct the context  based on the
  -- raw representation /of that nested content/. In general, this will not be
  -- possible. We should instead pass the (first few) bytes of the raw /block/
  -- instead. Better yet, we should cache the 'NestedCtxt' inside the secondary
  -- index.
  reconstructNestedCtxt ::
       proxy (f blk)
    -> IsEBB
    -> SizeInBytes      -- ^ Block size
    -> Lazy.ByteString  -- ^ Corresponds to the raw contents
    -> SomeBlock (NestedCtxt f) blk

  -- Defaults if there is only one type

  default reconstructPrefixLen ::
        TrivialDependency (NestedCtxt_ blk f)
     => proxy (f blk) -> Word8
  reconstructPrefixLen _ = 0
    where
      _ = keepRedundantConstraint (Proxy @(TrivialDependency (NestedCtxt_ blk f)))

  default reconstructNestedCtxt ::
       TrivialDependency (NestedCtxt_ blk f)
    => proxy (f blk)
    -> IsEBB
    -> SizeInBytes      -- ^ Block size
    -> Lazy.ByteString  -- ^ Corresponds to the raw contents
    -> SomeBlock (NestedCtxt f) blk
  reconstructNestedCtxt _ _ _ _ = SomeBlock indexIsTrivial

-- | Reconstruct the nested type, then add it to the bytestring
--
-- This function is a suitable instantiation for 'cdbAddHdrEnv'.
addReconstructedTypeEnvelope
  :: ReconstructNestedCtxt f blk
  => proxy (f blk)
  -> CodecConfig blk
  -> IsEBB
  -> SizeInBytes
  -> Lazy.ByteString -> Lazy.ByteString
addReconstructedTypeEnvelope p ccfg isEBB size bs =
    addNestedCtxtEnvelope ccfg (reconstructNestedCtxt p isEBB size bs, bs)

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

instance EncodeDisk blk (ConsensusState (BlockProtocol blk))
      => EncodeDisk blk (WrapConsensusState blk) where
  encodeDisk cfg (WrapConsensusState st) = encodeDisk cfg st

instance DecodeDisk blk (ConsensusState (BlockProtocol blk))
      => DecodeDisk blk (WrapConsensusState blk) where
  decodeDisk cfg = WrapConsensusState <$> decodeDisk cfg

instance EncodeDisk blk blk
      => EncodeDisk blk (I blk) where
  encodeDisk cfg (I b) = encodeDisk cfg b

instance DecodeDisk blk blk
      => DecodeDisk blk (I blk) where
  decodeDisk cfg = I <$> decodeDisk cfg

instance DecodeDisk blk (a -> f blk)
      => DecodeDisk blk (((->) a :.: f) blk) where
  decodeDisk cfg = Comp <$> decodeDisk cfg

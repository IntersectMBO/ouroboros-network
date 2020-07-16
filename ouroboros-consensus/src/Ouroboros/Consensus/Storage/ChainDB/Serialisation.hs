{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
    -- * Serialised header
  , SerialisedHeader(..)
  , serialisedHeaderToPair
  , serialisedHeaderFromPair
  , castSerialisedHeader
  , encodeTrivialSerialisedHeader
  , decodeTrivialSerialisedHeader
    -- * Reconstruct nested type
  , ReconstructNestedCtxt (..)
  , PrefixLen (..)
  , addPrefixLen
  , takePrefix
    -- * Re-exported for convenience
  , SizeInBytes
    -- * Exported for the benefit of tests
  , encodeDepPair
  , decodeDepPair
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.SOP.BasicFunctors
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary (enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (Serialised (..), fromSerialised,
                     mkSerialised)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Util.ShowProxy

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
class EncodeDiskDep f blk where
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
class DecodeDiskDep f blk where
  decodeDiskDep :: CodecConfig blk -> f blk a -> forall s. Decoder s (Lazy.ByteString -> a)

  default decodeDiskDep
    :: ( TrivialDependency (f blk)
       , DecodeDisk blk (Lazy.ByteString -> TrivialIndex (f blk))
       )
    => CodecConfig blk -> f blk a -> forall s. Decoder s (Lazy.ByteString -> a)
  decodeDiskDep cfg ctxt =
      (\f -> toTrivialDependency ctxt . f) <$> decodeDisk cfg

instance (EncodeDiskDepIx f blk, EncodeDiskDep f blk)
       => EncodeDisk blk (DepPair (f blk)) where
  encodeDisk ccfg = encodeDisk ccfg . encodeDepPair ccfg

instance (DecodeDiskDepIx f blk, DecodeDiskDep f blk)
       => DecodeDisk blk (DepPair (f blk)) where
  decodeDisk ccfg = decodeDisk ccfg >>= decodeDepPair ccfg

{-------------------------------------------------------------------------------
  Internal: support for serialisation of dependent pairs
-------------------------------------------------------------------------------}

encodeDepPair :: EncodeDiskDep f blk
              => CodecConfig blk
              -> DepPair (f blk) -> GenDepPair Serialised (f blk)
encodeDepPair ccfg (DepPair fa a) =
    GenDepPair fa (mkSerialised (encodeDiskDep ccfg fa) a)

decodeDepPair :: DecodeDiskDep f blk
              => CodecConfig blk
              -> GenDepPair Serialised (f blk) -> Decoder s (DepPair (f blk))
decodeDepPair ccfg (GenDepPair fa serialised) =
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
  Serialised header

  TODO: Not entirely sure we /want/ default instances for EncodeDisk/DecodeDisk
  for 'SerialisedHeader'.
-------------------------------------------------------------------------------}

-- | A 'Serialised' header along with context identifying what kind of header
-- it is.
--
-- The 'SerialiseNodeToNodeDep' for 'Header' will decide how to actually
-- encode this.
newtype SerialisedHeader blk = SerialisedHeaderFromDepPair {
      serialisedHeaderToDepPair :: GenDepPair Serialised (NestedCtxt Header blk)
    }

deriving instance HasNestedContent Header blk => Show (SerialisedHeader blk)
instance ShowProxy blk => ShowProxy (SerialisedHeader blk) where
    showProxy _ = "SerialisedHeader " ++ showProxy (Proxy :: Proxy blk)

-- | Only needed for the 'ChainSyncServer'
type instance HeaderHash (SerialisedHeader blk) = HeaderHash blk
instance StandardHash blk => StandardHash (SerialisedHeader blk)

serialisedHeaderToPair ::
     SerialisedHeader blk
  -> (SomeBlock (NestedCtxt Header) blk, Lazy.ByteString)
serialisedHeaderToPair hdr =
    case serialisedHeaderToDepPair hdr of
      GenDepPair ctxt (Serialised bs) -> (SomeBlock ctxt, bs)

serialisedHeaderFromPair ::
     (SomeBlock (NestedCtxt Header) blk, Lazy.ByteString)
  -> SerialisedHeader blk
serialisedHeaderFromPair (SomeBlock ctxt, bs) =
    SerialisedHeaderFromDepPair $
      GenDepPair ctxt (Serialised bs)

castSerialisedHeader ::
     (forall a. NestedCtxt_ blk Header a -> NestedCtxt_ blk' Header a)
  -> SerialisedHeader blk -> SerialisedHeader blk'
castSerialisedHeader f =
      SerialisedHeaderFromDepPair
    . depPairFirst (castNestedCtxt f)
    . serialisedHeaderToDepPair

instance EncodeDiskDepIx (NestedCtxt Header) blk
      => EncodeDisk blk (SerialisedHeader blk) where
  encodeDisk ccfg = encodeDisk ccfg . serialisedHeaderToDepPair

instance DecodeDiskDepIx (NestedCtxt Header) blk
      => DecodeDisk blk (SerialisedHeader blk) where
  decodeDisk ccfg = SerialisedHeaderFromDepPair <$> decodeDisk ccfg

-- | Encode the header without the 'NestedCtxt'
--
-- Uses CBOR-in-CBOR
encodeTrivialSerialisedHeader ::
     forall blk. TrivialDependency (NestedCtxt_ blk Header)
  => SerialisedHeader blk -> Encoding
encodeTrivialSerialisedHeader =
      encode
    . Serialised
    . snd
    . serialisedHeaderToPair
  where
    _ = keepRedundantConstraint (Proxy @(TrivialDependency (NestedCtxt_ blk Header)))

-- | Inverse to 'encodeTrivialSerialisedHeader'
decodeTrivialSerialisedHeader ::
     forall blk. TrivialDependency (NestedCtxt_ blk Header)
  => forall s. Decoder s (SerialisedHeader blk)
decodeTrivialSerialisedHeader =
    ( serialisedHeaderFromPair
    . (SomeBlock (NestedCtxt indexIsTrivial), )
    . unSerialised
    ) <$> decode

{-------------------------------------------------------------------------------
  Reconstruct nested type
-------------------------------------------------------------------------------}

class HasNestedContent f blk => ReconstructNestedCtxt f blk where
  -- | Number of bytes required to reconstruct the nested context.
  --
  -- This will be the /minimum/ length of the 'ShortByteString' passed to
  -- 'reconstructNestedCtxt'.
  reconstructPrefixLen :: proxy (f blk) -> PrefixLen

  -- | Reconstruct the type of nested contents
  --
  -- TODO: Allow to fail.
  reconstructNestedCtxt ::
       proxy (f blk)
    -> ShortByteString  -- ^ First bytes ('reconstructPrefixLen') of the block
    -> SizeInBytes      -- ^ Block size
    -> SomeBlock (NestedCtxt f) blk

  -- Defaults if there is only one type

  default reconstructPrefixLen ::
        TrivialDependency (NestedCtxt_ blk f)
     => proxy (f blk) -> PrefixLen
  reconstructPrefixLen _ = PrefixLen 0
    where
      _ = keepRedundantConstraint (Proxy @(TrivialDependency (NestedCtxt_ blk f)))

  default reconstructNestedCtxt ::
       TrivialDependency (NestedCtxt_ blk f)
    => proxy (f blk)
    -> ShortByteString  -- ^ First bytes ('reconstructPrefixLen') of the block
    -> SizeInBytes      -- ^ Block size
    -> SomeBlock (NestedCtxt f) blk
  reconstructNestedCtxt _ _ _ = SomeBlock indexIsTrivial

-- | Number of bytes from the start of a block needed to reconstruct the
-- nested context.
--
-- See 'reconstructPrefixLen'.
newtype PrefixLen = PrefixLen {
      getPrefixLen :: Word8
    }
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks)

addPrefixLen :: Word8 -> PrefixLen -> PrefixLen
addPrefixLen m (PrefixLen n) = PrefixLen (m + n)

takePrefix :: PrefixLen -> Lazy.ByteString -> ShortByteString
takePrefix (PrefixLen n) =
    Short.toShort . Lazy.toStrict . Lazy.take (fromIntegral n)

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

instance EncodeDisk blk (ChainDepState (BlockProtocol blk))
      => EncodeDisk blk (WrapChainDepState blk) where
  encodeDisk cfg (WrapChainDepState st) = encodeDisk cfg st

instance DecodeDisk blk (ChainDepState (BlockProtocol blk))
      => DecodeDisk blk (WrapChainDepState blk) where
  decodeDisk cfg = WrapChainDepState <$> decodeDisk cfg

instance EncodeDisk blk blk
      => EncodeDisk blk (I blk) where
  encodeDisk cfg (I b) = encodeDisk cfg b

instance DecodeDisk blk blk
      => DecodeDisk blk (I blk) where
  decodeDisk cfg = I <$> decodeDisk cfg

instance DecodeDisk blk (a -> f blk)
      => DecodeDisk blk (((->) a :.: f) blk) where
  decodeDisk cfg = Comp <$> decodeDisk cfg

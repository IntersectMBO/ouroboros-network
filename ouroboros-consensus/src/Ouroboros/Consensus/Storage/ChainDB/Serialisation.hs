{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
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
    EncodeDisk (..)
  , DecodeDisk (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (decode, encode))
import           Data.SOP.BasicFunctors

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

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

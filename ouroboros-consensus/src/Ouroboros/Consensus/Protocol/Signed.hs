{-# LANGUAGE TypeFamilies #-}

-- | Support for protocols that include a signature
module Ouroboros.Consensus.Protocol.Signed (
    SignedBlock(..)
  ) where

import           Codec.CBOR.Encoding (Encoding)

-- | Blocks that contain a signed part
class SignedBlock b where
  -- | The part of the block that is signed
  type family Signed b :: *

  -- | Extract the part of the block that the signature should be computed over
  blockSigned :: b -> Signed b

  -- | Signatures are computed of the serialized form
  --
  -- NOTE: This encoding is important: it determines what the signature looks
  -- like. For backwards compatibility, the encoding of the parts of the block
  -- that are signed may in fact be different from the encoding of the block
  -- itself.
  encodeSigned :: proxy b -> Signed b -> Encoding

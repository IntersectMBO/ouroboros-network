{-# LANGUAGE TypeFamilies #-}

-- | Support for protocols that include a signature
module Ouroboros.Consensus.Protocol.Signed (
    SignedHeader(..)
  ) where

import           Codec.CBOR.Encoding (Encoding)

-- | Header that contain a signed part
--
-- This class enforces that signatures are computed over the header only
-- (without the block body). This is important: we must be able to verify
-- the signature in a header without having access to the block (which we
-- download separately). Typically of course the header will contain a hash
-- of the body, so the signature can include the body implicitly.
class SignedHeader hdr where
  -- | The part of the header that is signed
  type family Signed hdr :: *

  -- | Extract the part of the header that the signature should be computed over
  headerSigned :: hdr -> Signed hdr

  -- | Signatures are computed of the serialized form
  --
  -- NOTE: This encoding is important: it determines what the signature looks
  -- like. For backwards compatibility, the encoding of the parts of the
  -- header that are signed may in fact be different from the encoding of the
  -- header itself.
  encodeSigned :: proxy hdr -> Signed hdr -> Encoding

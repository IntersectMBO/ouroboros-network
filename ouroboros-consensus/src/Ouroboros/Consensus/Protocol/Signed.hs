{-# LANGUAGE TypeFamilies #-}

-- | Support for protocols that include a signature
module Ouroboros.Consensus.Protocol.Signed (
    Signed
  , SignedHeader (..)
  ) where

import           Data.Kind (Type)

-- | The part of the header that is signed
type family Signed hdr :: Type

-- | Header that contain a signed part
--
-- This class enforces that signatures are computed over the header only
-- (without the block body). This is important: we must be able to verify
-- the signature in a header without having access to the block (which we
-- download separately). Typically of course the header will contain a hash
-- of the body, so the signature can include the body implicitly.
class SignedHeader hdr where
  -- | Extract the part of the header that the signature should be computed over
  headerSigned :: hdr -> Signed hdr

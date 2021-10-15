module Ouroboros.Consensus.Util.CBOR.Simple (
    -- * Future-proofing
    versionZeroProductFromCBOR
  , versionZeroProductToCBOR
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad (unless)

{-------------------------------------------------------------------------------
  Serialization aliases
-------------------------------------------------------------------------------}

-- | Simple codec to use since there's no version history to deal with and
-- exactly one constructor
versionZeroProductFromCBOR :: String -> Int -> CBOR.Decoder s a -> CBOR.Decoder s a
versionZeroProductFromCBOR s arity dec = do
    CBOR.decodeListLenOf (1 + arity)
    tag <- CBOR.decodeTag
    unless (0 == tag) $ fail s
    dec

-- | Simple codec to use since there's no version history to deal with and
-- exactly one constructor
versionZeroProductToCBOR :: [CBOR.Encoding] -> CBOR.Encoding
versionZeroProductToCBOR xs =
       CBOR.encodeListLen (1 + toEnum (length xs)) <> CBOR.encodeTag 0
    <> mconcat xs

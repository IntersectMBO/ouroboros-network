-- |
-- = Protocol.Codec
--
-- Definition of codecs for typed transitions, permitting a representation in
-- a monomorphic type such as ByteString.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}

module Protocol.Codec where

import Data.Text (Text)
import Data.Kind (Type)

data Codec m concreteTo concreteFrom tr from = Codec
  { encode :: Encoder tr from (Encoded concreteTo (Codec m concreteTo concreteFrom tr))
  , decode :: Decoder m concreteFrom (Decoded tr from (Codec m concreteTo concreteFrom tr))
  }

data Encoded concrete codec to = Encoded
  { representation :: concrete
  , encCodec       :: codec to
  }

data Decoded tr from codec = forall to . Decoded
  { transition :: tr from to
  , decCodec   :: codec to
  }

newtype Encoder tr from encoded = Encoder
  { runEncoder :: forall to . tr from to -> encoded to
  }

newtype Decoder (m :: Type -> Type) (concrete :: Type) (decoded :: Type) = Decoder
  { runDecoder :: m (DecoderStep m concrete decoded)
  }

data DecoderStep (m :: Type -> Type) (concrete :: Type) (decoded :: Type) where
  -- | Finished with leftovers.
  Done    :: concrete -> decoded -> DecoderStep m concrete decoded
  -- | Failed to decode, with leftovers.
  Fail    :: concrete -> Text -> DecoderStep m concrete decoded
  -- | Partial decode.
  -- Giving 'Nothing' means no more input is available. The next decoder
  -- should therefore be either 'Done' or 'Fail'.
  Partial :: (Maybe concrete -> Decoder m concrete decoded) -> DecoderStep m concrete decoded

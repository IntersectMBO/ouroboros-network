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

data Codec m concrete tr from = Codec
  { encode :: Encoder tr from (Encoded m concrete)
  , decode :: Decoder m concrete (Decoded m tr from)
  }

data Encoded m concrete tr from = Encoded
  { representation :: concrete
  , encCodec       :: Codec m concrete tr from
  }

data Decoded m tr from concrete = forall to . Decoded
  { transition :: tr from to
  , decCodec   :: Codec m concrete tr to
  }

newtype Encoder tr from encoded = Encoder
  { runEncoder :: forall to . tr from to -> encoded tr to
  }

newtype Decoder m concrete decoded = Decoder
  { runDecoder :: m (DecoderStep m concrete decoded)
  }

data DecoderStep m concrete decoded where
  -- | Finished with leftovers.
  Done    :: concrete -> decoded concrete -> DecoderStep m concrete decoded
  -- | Failed to decode, with leftovers.
  Fail    :: concrete -> Text -> DecoderStep m concrete decoded
  -- | Partial decode.
  -- Giving 'Nothing' means no more input is available. The next decoder
  -- should therefore be either 'Done' or 'Fail'.
  Partial :: (Maybe concrete -> Decoder m concrete decoded) -> DecoderStep m concrete decoded

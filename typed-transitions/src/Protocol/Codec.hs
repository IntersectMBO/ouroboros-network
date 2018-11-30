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
  Done    :: Maybe concrete -> decoded -> DecoderStep m concrete decoded
  -- | Failed to decode, with leftovers.
  Fail    :: Maybe concrete -> Text -> DecoderStep m concrete decoded
  -- | Partial decode.
  -- Giving 'Nothing' means no more input is available. The next decoder
  -- should therefore be either 'Done' or 'Fail'.
  Partial :: (Maybe concrete -> Decoder m concrete decoded) -> DecoderStep m concrete decoded

hoistCodec
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Codec m cto cfrom tr st
  -> Codec n cto cfrom tr st
hoistCodec nat codec = codec
  { encode = hoistEncoder nat (encode codec)
  , decode = hoistDecoder nat (decode codec)
  }

hoistEncoder
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Encoder tr from (Encoded cto (Codec m cto cfrom tr))
  -> Encoder tr from (Encoded cto (Codec n cto cfrom tr))
hoistEncoder nat encoder = Encoder $ \tr ->
  let encoded = runEncoder encoder tr
  in  encoded { encCodec = hoistCodec nat (encCodec encoded) }

hoistDecoder
  :: forall m n cfrom cto tr from .
     ( Functor n )
  => (forall x . m x -> n x)
  -> Decoder m cfrom (Decoded tr from (Codec m cto cfrom tr))
  -> Decoder n cfrom (Decoded tr from (Codec n cto cfrom tr))
hoistDecoder nat decoder = Decoder $ flip fmap (nat (runDecoder decoder)) $ \step -> case step of
  Fail leftover txt -> Fail leftover txt
  Done leftover (Decoded tr (codec :: Codec m cto cfrom tr someState)) ->
    let hoisted :: Codec n cto cfrom tr someState
        hoisted = hoistCodec nat codec
    in  Done leftover (Decoded tr hoisted)
  Partial k -> Partial $ hoistDecoder nat . k

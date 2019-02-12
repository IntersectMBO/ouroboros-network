{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Network.TypedProtocol.Codec (
    -- * Defining and using Codecs
    Codec(..)
    -- ** Related types
  , PeerRole(..)
  , PeerHasAgency(..)
  , WeHaveAgency
  , TheyHaveAgency
  , SomeMessage(..)
    -- ** Incremental decoding
  , DecodeStep(..)
  , runDecoder
  , runDecoderPure
    -- ** Codec properties
  , AnyMessage(..)
  , AnyMessageAndAgency(..)
  , prop_codec
  , prop_codec_splits
  ) where

import           Network.TypedProtocol.Core
                   ( Protocol(..), PeerRole(..)
                   , PeerHasAgency(..), WeHaveAgency, TheyHaveAgency )


-- | A codec for a 'Protocol' handles the encoding and decoding of typed
-- protocol messages. This is typically used when sending protocol messages
-- over untyped channels. The codec chooses the exact encoding, for example
-- encoding in some text-based syntax, or some choice of binary format.
--
-- The codec is parametrised by:
--
-- * The protocol
-- * The peer role (client\/server)
-- * the type of decoding failures
-- * the monad in which the decoder runs
-- * the type of the encoded data (typically strings or bytes)
--
-- It is expected that typical codec implementations will be polymorphic in
-- the peer role. For example a codec for the ping\/pong protocol might have
-- type:
--
-- > codecPingPong :: forall m. Monad m => Codec PingPong String m String
--
-- A codec consists of a message encoder and a decoder.
--
-- The encoder is supplied both with the message to encode and the current
-- protocol state (matching the message). The protocol state can be either
-- a client or server state, but for either peer role it is a protocol state
-- in which the peer has agency, since those are the only states where a
-- peer needs to encode a message to be able to send it.
--
-- For example a simple text encoder for the ping\/pong protocol could be:
--
-- > encode :: WeHaveAgency pr st
-- >        -> Message PingPong st st'
-- >        -> String
-- >  encode (ClientAgency TokIdle) MsgPing = "ping\n"
-- >  encode (ClientAgency TokIdle) MsgDone = "done\n"
-- >  encode (ServerAgency TokBusy) MsgPong = "pong\n"
--
-- The decoder is also given the current protocol state and it is expected to
-- be able to decode /any/ message that is valid in that state, but /only/
-- messages that are valid in that state. Messages that are unexpected for the
-- current state should be treated like any other decoding format error.
--
-- While the current protocol state is known, the state that the message will
-- have the peer transition to is not known. For this reason the decoded
-- message is wrapped in the 'SomeMessage' constructor which hides the \"to\"
-- state.
--
-- The decoder uses an incremental decoding interface 'DecodeStep' so that
-- input can be supplied (e.g. from a Channel) bit by bit. This style of
-- decoder allows but does not require a format with message framing where the
-- decoder input matches exactly with the message boundaries.
--
-- > decode :: TheyHaveAgency pr st
-- >        -> m (DecodeStep String String m (SomeMessage st))
-- > decode stok =
-- >   decodeTerminatedFrame '\n' $ \str trailing ->
-- >     case (stok, str) of
-- >       (ServerAgency TokBusy, "pong") ->
-- >            DecodeDone (SomeMessage MsgPong) trailing
-- >       (ClientAgency TokIdle, "ping") ->
-- >            DecodeDone (SomeMessage MsgPing) trailing
-- >       (ClientAgency TokIdle, "done") ->
-- >            DecodeDone (SomeMessage MsgDone) trailing
-- >       _ -> DecodeFail ("unexpected message: " ++ str)
--
-- The main thing to note is the pattern matching on the combination of the
-- message string and the protocol state. This neatly fulfils the requirement
-- that we only return messages that are of the correct type for the given
-- protocol state.
--
-- This toy example format uses newlines @\n@ as a framing format. See
-- 'DecodeStep' for suggestions on how to use it for more realistic formats.
--
data Codec ps failure m bytes = Codec {
       encode :: forall (pr :: PeerRole) (st :: ps) (st' :: ps).
                 PeerHasAgency pr st
              -> Message ps st st'
              -> bytes,

       decode :: forall (pr :: PeerRole) (st :: ps).
                 PeerHasAgency pr st
              -> m (DecodeStep bytes failure m (SomeMessage st))
     }

-- The types here are pretty fancy. The decode is polymorphic in the protocol
-- state, but only for kinds that are the same kind as the protocol state.
-- The TheyHaveAgency is a type family that resolves to a singleton, and the
-- result uses existential types to hide the unknown type of the state we're
-- transitioning to.
--
-- Both the Message and TheyHaveAgency data families are indexed on the kind ps
-- which is why it has to be a parameter here, otherwise these type functions
-- are unusable.


-- | An incremental decoder with return a value of type @a@.
--
-- This interface is not designed to be used directly for implementing
-- decoders, only for running them. In real applications it is expected to use
-- libraries for text or binary decoding and to implement appropriate wrappers
-- to match up with this incremental decoder interface.
--
-- This style of interface already closely matches that provided by libraries
-- such as @attoparsec@ for text formats, and @binary@, @cereal@ and @cborg@
-- for binary formats.
--
data DecodeStep bytes failure m a =

    -- | The decoder has consumed the available input and needs more
    -- to continue. Provide @'Just'@ if more input is available and
    -- @'Nothing'@ otherwise, and you will get a new @'DecodeStep'@.
    DecodePartial (Maybe bytes -> m (DecodeStep bytes failure m a))

    -- | The decoder has successfully finished. This provides the decoded
    -- result value plus any unused input.
  | DecodeDone a (Maybe bytes)

    -- | The decoder ran into an error. The decoder either used
    -- @'fail'@ or was not provided enough input.
  | DecodeFail failure


-- | When decoding a 'Message' we only know the expected \"from\" state. We
-- cannot know the \"to\" state as this depends on the message we decode. To
-- resolve this we use the 'SomeMessage' wrapper which uses an existential
-- type to hide the \"to"\ state.
--
data SomeMessage (st :: ps) where
     SomeMessage :: Message ps st st' -> SomeMessage st


--
-- Running decoders
--

-- | Run a codec incremental decoder 'DecodeStep' against a list of input.
--
-- It ignores any unused trailing data. This is useful for demos, quick
-- experiments and tests.
--
-- See also 'Network.TypedProtocol.Driver.runDecoderWithChannel'
--
runDecoder :: Monad m
           => [bytes]
           -> DecodeStep bytes failure m a
           -> m (Either failure a)
runDecoder _      (DecodeDone x _trailing) = return (Right x)
runDecoder _      (DecodeFail failure)     = return (Left failure)
runDecoder []     (DecodePartial k)        = k Nothing  >>= runDecoder []
runDecoder (b:bs) (DecodePartial k)        = k (Just b) >>= runDecoder bs


-- | A variant of 'runDecoder' that is suitable for \"pure\" monads that have
-- a run function. This includes 'ST', using 'Control.Monad.ST.runST'.
--
runDecoderPure :: Monad m
               => (forall b. m b -> b)
               -> m (DecodeStep bytes failure m a)
               -> [bytes]
               -> Either failure a
runDecoderPure runM decoder bs = runM (runDecoder bs =<< decoder)


--
-- Codec properties
--

-- | Any message for a protocol, without knowing the protocol state.
--
-- Used at least for 'Eq' instances for messages.
--
data AnyMessage ps where
     AnyMessage :: Message ps st st' -> AnyMessage ps

-- | Used to hold the 'PeerHasAgency' state token and a corresponding 'Message'.
--
-- Used where we don't know statically what the state type is, but need the
-- agency and message to match each other.
--
data AnyMessageAndAgency ps where
  AnyMessageAndAgency :: PeerHasAgency pr (st :: ps)
                      -> Message ps (st :: ps) (st' :: ps)
                      -> AnyMessageAndAgency ps


-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codec
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps))
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> Bool
prop_codec runM Codec {encode, decode} (AnyMessageAndAgency stok msg) =
    case runDecoderPure runM (decode stok) [encode stok msg] of
      Right (SomeMessage msg') -> AnyMessage msg' == AnyMessage msg
      Left _                   -> False


-- | A variant on the codec round-trip property: given the encoding of a
-- message, check that decode always gives the same result irrespective
-- of how the chunks of input are fed to the incremental decoder.
--
-- This property guards against boundary errors in incremental decoders.
-- It is not necessary to check this for every message type, just for each
-- generic codec construction. For example given some binary serialisation
-- library one would write a generic adaptor to the codec interface. This
-- adaptor has to deal with the incremental decoding and this is what needs
-- to be checked.
--
prop_codec_splits
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps))
  => (bytes -> [[bytes]])   -- ^ alternative re-chunkings of serialised form
  -> (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> Bool
prop_codec_splits splits runM
                  Codec {encode, decode} (AnyMessageAndAgency stok msg) =
    and [ case runDecoderPure runM (decode stok) bytes' of
            Right (SomeMessage msg') -> AnyMessage msg' == AnyMessage msg
            Left _                   -> False

        | let bytes = encode stok msg
        , bytes' <- splits bytes ]


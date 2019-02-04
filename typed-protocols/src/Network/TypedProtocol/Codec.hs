{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeInType                 #-}

module Network.TypedProtocol.Codec (
    -- * Defining and using Codecs
    Codec(..)
    -- ** Related types
  , PeerKind(..)
  , PeerHasAgency(..)
  , WeHaveAgency
  , TheyHaveAgency
  , SomeMessage(..)
    -- ** Incremental decoding
  , DecodeStep(..)
    -- * Utilities
  , transformCodec
  , cborCodec
  , toLazyByteString
  ) where

import           Network.TypedProtocol.Core
                   ( Protocol(..), PeerKind(..)
                   , PeerHasAgency(..), WeHaveAgency, TheyHaveAgency )

import           Control.Monad.ST (ST)
import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Write    as CBOR

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import qualified Data.ByteString.Lazy          as LBS


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
-- > codecPingPong :: forall pk m. Monad m => Codec PingPong pk String m String
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
-- > encode :: WeHaveAgency pk st
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
-- > decode :: TheyHaveAgency pk st
-- >        -> m (DecodeStep String String m (SomeMessage st))
-- > decode stok =
-- >   decodeTerminatedFrame '\n' $ \str trailing ->
-- >     case (stok, str) of
-- >       (ServerAgency TokBusy, "pong\n") ->
-- >            DecodeDone (SomeMessage MsgPong) trailing
-- >       (ClientAgency TokIdle, "ping\n") ->
-- >            DecodeDone (SomeMessage MsgPing) trailing
-- >       (ClientAgency TokIdle, "done\n") ->
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
data Codec ps (pk :: PeerKind) failure m bytes = Codec {
       encode :: forall (st :: ps) (st' :: ps).
                 WeHaveAgency pk st
              -> Message ps st st'
              -> bytes,

       decode :: forall (st :: ps).
                 TheyHaveAgency pk st
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


-- | When decoding a 'Message' we only know the expected \"from\" state. We
-- cannot know the \"to\" state as this depends on the message we decode. To
-- resolve this we use the 'SomeMessage' wrapper which uses an existential
-- type to hide the \"to"\ state.
--
data SomeMessage (st :: ps) where
     SomeMessage :: Message ps st st' -> SomeMessage st


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


transformCodec
  :: Functor m
  => (bytes  -> bytes')
  -> (bytes' -> bytes)
  -> Codec ps pk failure m bytes
  -> Codec ps pk failure m bytes'
transformCodec to from Codec {encode, decode} = Codec {
    encode = \stok msg -> to (encode stok msg),
    decode = fmap (transformDecodeStep to from) . decode
  }

transformDecodeStep
  :: Functor m
  => (bytes -> bytes')
  -> (bytes' -> bytes)
  -> DecodeStep bytes  failure m a
  -> DecodeStep bytes' failure m a
transformDecodeStep to from (DecodePartial fn)   =
  DecodePartial $ fmap (transformDecodeStep to from) . fn . fmap from
transformDecodeStep to _    (DecodeDone a bs)    = DecodeDone a (fmap to bs)
transformDecodeStep _  _    (DecodeFail failure) = DecodeFail failure



{-
serialiseCodec :: (MonadST m, Serialise.Serialise a)
               => Codec ByteString CBOR.DeserialiseFailure m a
serialiseCodec = cborCodec Serialise.encode Serialise.decode 
-}


cborCodec :: forall pk ps m. MonadST m
          => (forall (st :: ps) (st' :: ps). WeHaveAgency   pk st -> Message ps st st' -> CBOR.Encoding)
          -> (forall (st :: ps) s.           TheyHaveAgency pk st -> CBOR.Decoder s (SomeMessage st))
          -> Codec ps pk CBOR.DeserialiseFailure m ByteString
cborCodec cborEncode cborDecode =
    Codec {
      encode = \stok msg -> convertCborEncoder  (cborEncode stok) msg,
      decode = \stok     -> convertCborDecoder' (cborDecode stok)
    }

convertCborEncoder :: (a -> CBOR.Encoding) -> a -> ByteString
convertCborEncoder cborEncode =
    CBOR.toStrictByteString
  . cborEncode

{-# NOINLINE toLazyByteString #-}
toLazyByteString :: BS.Builder -> LBS.ByteString
toLazyByteString = BS.toLazyByteStringWith strategy LBS.empty
  where
    strategy = BS.untrimmedStrategy 800 LBS.smallChunkSize

convertCborDecoder' :: MonadST m
                    => (forall s. CBOR.Decoder s a)
                    -> m (DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder' cborDecode =
    withLiftST (convertCborDecoder cborDecode)

convertCborDecoder :: forall s m a. Functor m
                   => (CBOR.Decoder s a)
                   -> (forall b. ST s b -> m b)
                   -> m (DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder cborDecode liftST =
    go <$> liftST (CBOR.deserialiseIncremental cborDecode)
  where
    go (CBOR.Done  trailing _ x)
      | BS.null trailing       = DecodeDone x Nothing
      | otherwise              = DecodeDone x (Just trailing)
    go (CBOR.Fail _ _ failure) = DecodeFail failure
    go (CBOR.Partial k)        = DecodePartial (fmap go . liftST . k)

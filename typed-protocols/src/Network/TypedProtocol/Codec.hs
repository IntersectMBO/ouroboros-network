{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
-- @UndecidableInstances@ extension is required for defining @Show@ instance of
-- @'AnyMessage'@ and @'AnyMessageAndAgency'@.
{-# LANGUAGE UndecidableInstances  #-}

module Network.TypedProtocol.Codec
  ( -- * Defining and using Codecs
    Codec (..)
  , hoistCodec
  , isoCodec
  , mapFailureCodec
    -- ** Related types
  , PeerRole (..)
  , PeerHasAgency (..)
  , WeHaveAgency
  , TheyHaveAgency
  , SomeMessage (..)
  , CodecFailure (..)
    -- ** Incremental decoding
  , DecodeStep (..)
  , runDecoder
  , runDecoderPure
    -- ** Codec properties
  , AnyMessage (..)
  , AnyMessageAndAgency (..)
  , prop_codecM
  , prop_codec
  , prop_codec_splitsM
  , prop_codec_splits
  , prop_codec_binary_compatM
  , prop_codec_binary_compat
  , prop_codecs_compatM
  , prop_codecs_compat
  , SamePeerHasAgency (..)
  ) where

import           Control.Exception (Exception)
import           Data.Kind (Type)
import           Data.Monoid (All (..))

import           Network.TypedProtocol.Core (PeerHasAgency (..), PeerRole (..),
                     Protocol (..), TheyHaveAgency, WeHaveAgency)
import           Network.TypedProtocol.Driver (SomeMessage (..))

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

hoistCodec
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Codec ps failure m bytes
  -> Codec ps failure n bytes
hoistCodec nat codec = codec
  { decode = fmap (hoistDecodeStep nat) . nat . decode codec
  }

isoCodec :: Functor m
         => (bytes -> bytes')
         -> (bytes' -> bytes)
         -> Codec ps failure m bytes
         -> Codec ps failure m bytes'
isoCodec f finv Codec {encode, decode} = Codec {
      encode = \tok msg -> f $ encode tok msg,
      decode = \tok -> isoDecodeStep f finv <$> decode tok
    }

mapFailureCodec
  :: Functor m
  => (failure -> failure')
  -> Codec ps failure  m bytes
  -> Codec ps failure' m bytes
mapFailureCodec f Codec {encode, decode} = Codec {
    encode = encode,
    decode = \tok -> mapFailureDecodeStep f <$> decode tok
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

isoDecodeStep
  :: Functor m
  => (bytes -> bytes')
  -> (bytes' -> bytes)
  -> DecodeStep bytes failure m a
  -> DecodeStep bytes' failure m a
isoDecodeStep f  finv  (DecodePartial g)    = DecodePartial (fmap (isoDecodeStep f finv) . g . fmap finv)
isoDecodeStep f  _finv (DecodeDone a bytes) = DecodeDone a (fmap f bytes)
isoDecodeStep _f _finv (DecodeFail failure) = DecodeFail failure

hoistDecodeStep
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> DecodeStep bytes failure m a
  -> DecodeStep bytes failure n a
hoistDecodeStep nat step = case step of
  DecodeDone a mb -> DecodeDone a mb
  DecodeFail fail_AvoidNameShadow -> DecodeFail fail_AvoidNameShadow
  DecodePartial k -> DecodePartial (fmap (hoistDecodeStep nat) . nat . k)

mapFailureDecodeStep
  :: Functor m
  => (failure -> failure')
  -> DecodeStep bytes failure  m a
  -> DecodeStep bytes failure' m a
mapFailureDecodeStep f step = case step of
  DecodeDone a mb    -> DecodeDone a mb
  DecodeFail failure -> DecodeFail (f failure)
  DecodePartial k    -> DecodePartial (fmap (mapFailureDecodeStep f) . k)


-- | Each 'Codec' can use whatever @failure@ type is appropriate. This simple
-- exception type is provided for use by simple codecs (e.g. \"identity\") when
-- nothing more than a 'String' is needed. It is an instance of 'Exception'.
--
data CodecFailure = CodecFailureOutOfInput
                  | CodecFailure String
  deriving (Eq, Show)

-- safe instance with @UndecidableInstances@ in scope
instance Exception CodecFailure


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
-- Used at least for 'Eq' instances for messages, but also as a target for an
-- identity codec `Codec ps failure m (AnyMessage ps)` .
--
data AnyMessage ps where
     AnyMessage :: Message ps st st' -> AnyMessage ps

-- requires @UndecidableInstances@ and @QuantifiedConstraints@.
instance (forall st st'. Show (Message ps st st')) => Show (AnyMessage ps) where
    show (AnyMessage msg) = show msg

-- | Used to hold the 'PeerHasAgency' state token and a corresponding 'Message'.
--
-- Used where we don't know statically what the state type is, but need the
-- agency and message to match each other.
--
data AnyMessageAndAgency ps where
  AnyMessageAndAgency :: PeerHasAgency pr (st :: ps)
                      -> Message ps (st :: ps) (st' :: ps)
                      -> AnyMessageAndAgency ps

-- requires @UndecidableInstances@ and @QuantifiedConstraints@.
instance
    ( forall (st :: ps).             Show (ClientHasAgency st)
    , forall (st :: ps).             Show (ServerHasAgency st)
    , forall (st :: ps) (st' :: ps). Show (Message ps st st')
    ) => Show (AnyMessageAndAgency ps) where
  show (AnyMessageAndAgency agency msg) = show (agency, msg)

-- | The 'Codec' round-trip property: decode after encode gives the same
-- message. Every codec must satisfy this property.
--
prop_codecM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     )
  => Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> m Bool
prop_codecM Codec {encode, decode} (AnyMessageAndAgency stok msg) = do
    r <- decode stok >>= runDecoder [encode stok msg]
    case r of
      Right (SomeMessage msg') -> return $ AnyMessage msg' == AnyMessage msg
      Left _                   -> return False

-- | The 'Codec' round-trip property in a pure monad.
--
prop_codec
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps))
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> Bool
prop_codec runM codec msg =
    runM (prop_codecM codec msg)


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
prop_codec_splitsM
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps))
  => (bytes -> [[bytes]])   -- ^ alternative re-chunkings of serialised form
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> m Bool
prop_codec_splitsM splits
                  Codec {encode, decode} (AnyMessageAndAgency stok msg) = do
    and <$> sequence
      [ do r <- decode stok >>= runDecoder bytes'
           case r of
             Right (SomeMessage msg') -> return $ AnyMessage msg' == AnyMessage msg
             Left _                   -> return False

      | let bytes = encode stok msg
      , bytes' <- splits bytes ]


-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codec_splits
  :: forall ps failure m bytes.
     (Monad m, Eq (AnyMessage ps))
  => (bytes -> [[bytes]])
  -> (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> Bool
prop_codec_splits splits runM codec msg =
    runM $ prop_codec_splitsM splits codec msg


-- | Auxiliary definition for 'prop_codec_binary_compatM'.
--
-- Used for the existential @st :: ps@ parameter when expressing that for each
-- value of 'PeerHasAgency' for protocol A, there is a corresponding
-- 'PeerHasAgency' for protocol B of some @st :: ps@.
data SamePeerHasAgency (pr :: PeerRole) (ps :: Type) where
  SamePeerHasAgency
    :: forall (pr :: PeerRole) ps (st :: ps).
       PeerHasAgency pr st
    -> SamePeerHasAgency pr ps

-- | Binary compatibility of two protocols
--
-- We check the following property:
--
-- 1. Using codec A, we encode a message of protocol @psA@ to @bytes@.
--
-- 2. When we decode those @bytes@ using codec B, we get a message of protocol
-- @ps@B.
--
-- 3. When we encode that message again using codec B, we get @bytes@.
--
-- 4. When we decode those @bytes@ using codec A, we get the original message
-- again.
prop_codec_binary_compatM
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => Codec psA failure m bytes
  -> Codec psB failure m bytes
  -> (forall pr (stA :: psA). PeerHasAgency pr stA -> SamePeerHasAgency pr psB)
     -- ^ The states of A map directly of states of B.
  -> AnyMessageAndAgency psA
  -> m Bool
prop_codec_binary_compatM
    codecA codecB stokEq
    (AnyMessageAndAgency (stokA :: PeerHasAgency pr stA) msgA) =
  case stokEq stokA of
    SamePeerHasAgency stokB -> do
      -- 1.
      let bytesA = encode codecA stokA msgA
      -- 2.
      r1 <- decode codecB stokB >>= runDecoder [bytesA]
      case r1 of
        Left _     -> return False
        Right (SomeMessage msgB) -> do
          -- 3.
          let bytesB = encode codecB stokB msgB
          -- 4.
          r2 <- decode codecA stokA >>= runDecoder [bytesB]
          case r2 of
            Left _                    -> return False
            Right (SomeMessage msgA') -> return $ AnyMessage msgA' == AnyMessage msgA

-- | Like @'prop_codec_splitsM'@ but run in a pure monad @m@, e.g. @Identity@.
prop_codec_binary_compat
  :: forall psA psB failure m bytes.
     ( Monad m
     , Eq (AnyMessage psA)
     )
  => (forall a. m a -> a)
  -> Codec psA failure m bytes
  -> Codec psB failure m bytes
  -> (forall pr (stA :: psA). PeerHasAgency pr stA -> SamePeerHasAgency pr psB)
  -> AnyMessageAndAgency psA
  -> Bool
prop_codec_binary_compat runM codecA codecB stokEq msgA =
     runM $ prop_codec_binary_compatM codecA codecB stokEq msgA


-- | Compatibility between two codecs of the same protocol.  Encode a message
-- with one codec and decode it with the other one, then compare if the result
-- is the same as initial message.
--
prop_codecs_compatM
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => Codec ps failure m bytes
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> m Bool
prop_codecs_compatM codecA codecB
                    (AnyMessageAndAgency stok msg) =
    getAll <$> do r <- decode codecB stok >>= runDecoder [encode codecA stok msg]
                  case r of
                    Right (SomeMessage msg') -> return $ All $ AnyMessage msg' == AnyMessage msg
                    Left _                   -> return $ All False
            <> do r <- decode codecA stok >>= runDecoder [encode codecB stok msg]
                  case r of
                    Right (SomeMessage msg') -> return $ All $ AnyMessage msg' == AnyMessage msg
                    Left _                   -> return $ All False

-- | Like @'prop_codecs_compatM'@ but run in a pure monad @m@, e.g. @Identity@.
--
prop_codecs_compat
  :: forall ps failure m bytes.
     ( Monad m
     , Eq (AnyMessage ps)
     , forall a. Monoid a => Monoid (m a)
     )
  => (forall a. m a -> a)
  -> Codec ps failure m bytes
  -> Codec ps failure m bytes
  -> AnyMessageAndAgency ps
  -> Bool
prop_codecs_compat run codecA codecB msg =
    run $ prop_codecs_compatM codecA codecB msg

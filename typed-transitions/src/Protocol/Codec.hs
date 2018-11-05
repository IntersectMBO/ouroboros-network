-- |
-- = Protocol.Codec
--
-- Defition of encode/decode for transitions and its compatibility with a
-- 'Channel'.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}

module Protocol.Codec where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Protocol.Channel

data Codec fail t m piece = Codec
  { encode :: t -> m piece
  , decode :: Decoder fail piece m t
  }

newtype Decoder fail piece m t = Decoder
  { runDecoder :: piece -> m (DecoderStep fail piece m t)
  }

instance Functor m => Functor (Decoder fail piece m) where
  fmap f = Decoder . (fmap . fmap . fmap) f . runDecoder

data DecoderStep fail piece m t where
  -- | Finished with leftovers.
  Done    :: t -> piece -> DecoderStep fail piece m t
  -- | Failed to decode, with leftovers.
  Fail    :: fail -> piece -> DecoderStep fail piece m t
  -- | Partial decode.
  Partial :: Decoder fail piece m t -> DecoderStep fail piece m t

instance Functor m => Functor (DecoderStep fail piece m) where
  fmap f term = case term of
    Done t rest     -> Done (f t) rest
    Fail fail piece -> Fail fail piece
    Partial decoder -> Partial (fmap f decoder)

data DecodeFailure fail where
  UnexpectedEndOfInput :: DecodeFailure fail
  DecodeFailure        :: fail -> DecodeFailure fail

-- | Use a 'Codec' to transform a 'Channel'.
codecChannel
  :: forall fail t m piece .
     ( Monad m )
  => Codec fail t m piece
  -> Channel m piece
  -> Channel (ExceptT (DecodeFailure fail) m) t
codecChannel codec = codecChannel' Nothing
  where
  codecChannel'
    :: Maybe piece -- ^ Leftovers from prior 'recv'
    -> Channel m piece
    -> Channel (ExceptT (DecodeFailure fail) m) t
  codecChannel' leftover channel = Channel
    { send = codecSend channel
    , recv = codecRecv leftover channel
    }

  codecSend
    :: Channel m piece
    -> t
    -> ExceptT (DecodeFailure fail) m (Channel (ExceptT (DecodeFailure fail) m) t)
  codecSend channel t = lift (encode codec t) >>= fmap (codecChannel' Nothing) . lift . send channel

  codecRecv
    :: Maybe piece
    -> Channel m piece
    -> ExceptT (DecodeFailure fail) m (Maybe (t, Channel (ExceptT (DecodeFailure fail) m) t))
  codecRecv leftover channel = case leftover of
    Just piece -> codecRecvLeftover piece channel (decode codec)
    Nothing -> codecRecvNoLeftover channel (decode codec)

  -- Receive using leftovers.
  codecRecvLeftover
    :: piece
    -> Channel m piece
    -> Decoder fail piece m t
    -> ExceptT (DecodeFailure fail) m (Maybe (t, Channel (ExceptT (DecodeFailure fail) m) t))
  codecRecvLeftover leftover channel decoder =
    lift (runDecoder decoder leftover) >>= \it -> case it of
      -- Done without even using the channel.
      Done t leftover' -> pure $ Just (t, codecChannel' (Just leftover') channel)
      Fail fail _      -> throwE $ DecodeFailure fail
      Partial decoder' -> codecRecvNoLeftover channel decoder'

  -- Receive without any leftovers: go straight to the channel 'recv'.
  codecRecvNoLeftover
    :: Channel m piece
    -> Decoder fail piece m t
    -> ExceptT (DecodeFailure fail) m (Maybe (t, Channel (ExceptT (DecodeFailure fail) m) t))
  codecRecvNoLeftover channel decoder = lift (recv channel) >>= \it -> case it of
    Nothing -> throwE $ UnexpectedEndOfInput
    Just (piece, channel') -> lift (runDecoder decoder piece) >>= \it -> case it of
      Done t leftover  -> pure $ Just (t, codecChannel' (Just leftover) channel')
      Fail fail _      -> throwE $ DecodeFailure fail
      Partial decoder' -> codecRecvNoLeftover channel' decoder'

eliminateDecodeFailure
  :: ( Monad m )
  => (forall x . DecodeFailure fail -> m x)
  -> ExceptT (DecodeFailure fail) m t
  -> m t
eliminateDecodeFailure elim (ExceptT term) = term >>= either elim pure

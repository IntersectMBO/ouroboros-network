-- |
-- = Protocol.Codec
--
-- Defition of encode/decode for transitions and its compatibility with a
-- 'Duplex'.

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

-- | Use a 'Codec' to transform a 'Duplex'.
codecDuplex
  :: forall fail t m piece .
     ( Monad m )
  => Codec fail t m piece
  -> Duplex m m piece piece
  -> Duplex m (ExceptT (DecodeFailure fail) m) t t
codecDuplex codec = codecDuplex' Nothing
  where
  codecDuplex'
    :: Maybe piece -- ^ Leftovers from prior 'recv'
    -> Duplex m m piece piece
    -> Duplex m (ExceptT (DecodeFailure fail) m) t t
  codecDuplex' leftover channel = Duplex
    { send = codecSend channel
    , recv = codecRecv leftover channel
    }

  codecSend
    :: Duplex m m piece piece
    -> t
    -> m (Duplex m (ExceptT (DecodeFailure fail) m) t t)
  codecSend channel t = encode codec t >>= fmap (codecDuplex' Nothing) . send channel

  codecRecv
    :: Maybe piece
    -> Duplex m m piece piece
    -> ExceptT (DecodeFailure fail) m (Maybe (t, Duplex m (ExceptT (DecodeFailure fail) m) t t))
  codecRecv leftover channel = case leftover of
    Just piece -> codecRecvLeftover piece channel (decode codec)
    -- If there's no input, give 'Nothing' rather than 'UnexpectedEndOfInput',
    -- since in this case the end of input is normal.
    Nothing -> codecRecvNoLeftover (pure Nothing) channel (decode codec)

  -- Receive using leftovers.
  codecRecvLeftover
    :: piece
    -> Duplex m m piece piece
    -> Decoder fail piece m t
    -> ExceptT (DecodeFailure fail) m (Maybe (t, Duplex m (ExceptT (DecodeFailure fail) m) t t))
  codecRecvLeftover leftover channel decoder =
    lift (runDecoder decoder leftover) >>= \it -> case it of
      -- Done without even using the channel.
      Done t leftover' -> pure $ Just (t, codecDuplex' (Just leftover') channel)
      Fail fail _      -> throwE $ DecodeFailure fail
      Partial decoder' -> codecRecvNoLeftover (throwE UnexpectedEndOfInput) channel decoder'

  -- Receive without any leftovers: go straight to the channel 'recv'.
  codecRecvNoLeftover
    :: (forall x . ExceptT (DecodeFailure fail) m (Maybe x)) -- ^ End of input.
    -> Duplex m m piece piece
    -> Decoder fail piece m t
    -> ExceptT (DecodeFailure fail) m (Maybe (t, Duplex m (ExceptT (DecodeFailure fail) m) t t))
  codecRecvNoLeftover onEnd channel decoder = lift (recv channel) >>= \it -> case it of
    Nothing -> onEnd
    Just (piece, channel') -> lift (runDecoder decoder piece) >>= \it -> case it of
      Done t leftover  -> pure $ Just (t, codecDuplex' (Just leftover) channel')
      Fail fail _      -> throwE $ DecodeFailure fail
      Partial decoder' -> codecRecvNoLeftover (throwE UnexpectedEndOfInput) channel' decoder'

eliminateDecodeFailure
  :: ( Monad m )
  => (forall x . DecodeFailure fail -> m x)
  -> ExceptT (DecodeFailure fail) m t
  -> m t
eliminateDecodeFailure elim (ExceptT term) = term >>= either elim pure

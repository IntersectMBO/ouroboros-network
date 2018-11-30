{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Protocol.Driver where

import Data.Text (Text)

import Protocol.Channel
import Protocol.Core
import Protocol.Codec

data Result t where
  Normal     :: t -> Result t
  -- | Unexpected data was given. This includes the case of an EOF.
  Unexpected :: Text -> Result t
  deriving (Show)

-- | Drive a 'Peer' using a 'Channel', by way of a 'Codec' which describes
-- the relationship between the concrete representation understood by the
-- 'Channel', and the typed transitions understood by the 'Peer'.
--
-- A failure to decode arises as an 'Unexpected :: Result t'.
useCodecWithChannel
 :: forall m concrete p tr status init end t .
    ( Monad m )
 => Channel m concrete
 -> Codec m concrete tr init
 -> Peer p tr (status init) end m t
 -> m (Result t)
useCodecWithChannel = go Nothing
  where
  -- Tracks leftovers from the channel.
  go :: forall status state .
        Maybe concrete
     -> Channel m concrete
     -> Codec m concrete tr state
     -> Peer p tr (status state) end m t
     -> m (Result t)
  go leftovers channel codec peer = case peer of
    PeerDone t -> pure $ Normal t
    PeerLift m -> m >>= go leftovers channel codec
    PeerYield exc next -> do
      let enc = runEncoder (encode codec) (exchangeTransition exc)
          codec' = encCodec enc
      channel' <- send channel (representation enc)
      go leftovers channel' codec' next
    PeerAwait k -> runDecoder (decode codec) >>= startDecoding leftovers channel k

  startDecoding
    :: forall state .
       Maybe concrete
    -> Channel m concrete
    -> (forall inter . tr state inter -> Peer p tr (ControlNext (TrControl p state inter) Awaiting Yielding Finished inter) end m t)
    -> DecoderStep m concrete (Decoded m tr state)
    -> m (Result t)
  startDecoding leftovers channel k step = case step of
    Fail _ txt -> pure $ Unexpected txt
    -- We just started decoding. We haven't fed any input yet. And still, the
    -- decoder is done. That's bizarre but not necessarily wrong. It _must_ be
    -- the case that `leftovers'` is empty, otherwise the decoder conjured
    -- some leftovers from nothing.
    Done leftovers' (Decoded tr codec') -> go leftovers channel codec' (k tr)
    -- Typically, the decoder will be partial. We start by passing the
    -- leftovers if any. NB: giving `Nothing` to `l` means there's no more
    -- input.
    Partial l -> case leftovers of
      Just piece -> runDecoder (l (Just piece)) >>= decodeFromChannel channel k
      Nothing -> decodeFromChannel channel k step

  decodeFromChannel
    :: forall state .
       Channel m concrete
    -> (forall inter . tr state inter -> Peer p tr (ControlNext (TrControl p state inter) Awaiting Yielding Finished inter) end m t)
    -> DecoderStep m concrete (Decoded m tr state)
    -> m (Result t)
  decodeFromChannel channel k step = case step of
    Fail _ txt -> pure $ Unexpected txt
    -- Leftovers are given as 'Just' even if they are empty.
    -- Not ideal but shouldn't be a problem in practice.
    Done leftovers (Decoded tr codec') -> go (Just leftovers) channel codec' (k tr)
    -- Read from the channel and carry on. A premature end-of-input will
    -- result in a Fail and therefore an Unexpected.
    Partial l -> recv channel >>= \next -> case next of
      (mPiece, channel') ->
        runDecoder (l mPiece) >>= decodeFromChannel channel' k

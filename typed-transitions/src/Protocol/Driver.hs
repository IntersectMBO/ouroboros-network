{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Protocol.Driver where

import Protocol.Channel
import Protocol.Core
import Protocol.Codec

-- |
-- = Driving a Peer by was of a Duplex and Channel
--
-- A 'Duplex' allows for sending and receiving pieces of some concrete type.
-- In applications, this will probably be some sort of socket. In order to
-- use it to drive a typed protocol application (represented by a 'Peer'),
-- there must be a way to encode typed transitions of that protocol to the
-- concrete type, and to parse pieces of that concrete type incrementally into
-- a typed transition. This is defined by a 'Codec'.
--
-- A 'Codec' and a 'Duplex' alone is not enough to do encoding and decoding,
-- because the 'Codec' does not make any _decisions_ about the way in which
-- the protocol application progresses. It defines encodings for _all_ possible
-- transitions from a state, and an inverse for that encoder. It's the 'Peer'
-- term which decides which transitions to encode, thereby leading the 'Codec'
-- through a path in the protocol type.
--
-- Driving a 'Peer' in this way may give rise to an exception, given by
-- 'Unexpected :: Result t'.

-- | The outcome of a 'Peer' when driven by a 'Duplex' and 'Codec'.
-- It's possible that an unexpected transition was given, either because the
-- other end made a protocol error, or because the 'Codec' is not coherent
-- (decode is not inverse to encode). It's also possible that a decoder
-- fails because a transition was expected, but the 'Duplex' closed. This also
-- gives rise to an 'Unexpected' value, because the decoder will fail.
data Result fail t where
  Normal     :: t -> Result fail t
  -- | Unexpected data was given. This includes the case of an EOF.
  Unexpected :: fail -> Result fail t
  deriving (Show)

-- | Feed a 'Fold' using the receive side of a 'Duplex'. Leftovers of the
-- fold will be prepended onto the remaining duplex.
--
-- Like 'Protocol.Channel.foldOverInput', but for a 'Duplex'.
foldOverDuplex
  :: ( Functor n, Monad m )
  => Fold [recv] m r
  -> Duplex n m send recv
  -> m (r, Duplex n m send recv)
foldOverDuplex fold duplex = runFold fold >>= \choice -> case choice of
  Complete leftovers it -> flip (,) (prependDuplexRecv leftovers duplex) <$> it
  Partial step -> recv duplex >>= \(inp, duplex') -> case inp of
    Nothing -> flip (,) duplex' <$> end step
    Just i  -> foldOverDuplex (more step [i]) duplex'

-- | Drive a 'Peer' using a 'Duplex', by way of a 'Codec' which describes
-- the relationship between the concrete representation understood by the
-- 'Duplex', and the typed transitions understood by the 'Peer'.
--
-- A failure to decode arises as an 'Unexpected :: Result t'.
useCodecWithDuplex
 :: forall m fail concreteSend concreteRecv p tr status init end t .
    ( Monad m )
 => Duplex m m concreteSend concreteRecv
 -> Codec m fail concreteSend concreteRecv tr init
 -> Peer p tr (status init) end m t
 -> m (Result fail t)
useCodecWithDuplex duplex codec peer = case peer of
  PeerDone t -> pure $ Normal t
  PeerLift m -> m >>= useCodecWithDuplex duplex codec
  -- Encode the transition, dump it to the duplex, and continue with the
  -- new codec and duplex.
  PeerYield exc next -> do
    let enc = runEncoder (encode codec) (exchangeTransition exc)
        codec' = encCodec enc
    duplex' <- send duplex (representation enc)
    useCodecWithDuplex duplex' codec' next
  PeerAwait k -> do
    (result, duplex') <- foldOverDuplex (decode codec) duplex
    case result of
      Left fail -> pure $ Unexpected fail
      Right (Decoded tr codec') -> useCodecWithDuplex duplex' codec' (k tr)

{-# LANGUAGE NamedFieldPuns #-}

-- | The integration point of 'Network.TypedProtocol.Channel' and
-- 'Network.Mux.Channel'.
--
module Ouroboros.Network.Channel
  ( fromChannel
  , toChannel

  , createPipeConnectedChannels
  ) where

import qualified Data.ByteString.Lazy as LBS

import qualified Network.TypedProtocol.Channel as TP
import qualified Network.Mux.Channel as Mx

fromChannel :: Mx.Channel m
            -> TP.Channel m LBS.ByteString
fromChannel Mx.Channel { Mx.send, Mx.recv } = TP.Channel {
    TP.send = send,
    TP.recv = recv
  }

toChannel :: TP.Channel m LBS.ByteString
          -> Mx.Channel m
toChannel TP.Channel { TP.send, TP.recv } = Mx.Channel {
    Mx.send = send,
    Mx.recv = recv
  }

-- | Create a local pipe, with both ends in this process, and expose that as
-- a pair of 'Channel's, one for each end.
--
-- This is primarily for testing purposes since it does not allow actual IPC.
--
createPipeConnectedChannels :: IO (TP.Channel IO LBS.ByteString,
                                   TP.Channel IO LBS.ByteString)
createPipeConnectedChannels =
    (\(a, b) -> (fromChannel a, fromChannel b))
    <$> Mx.createPipeConnectedChannels

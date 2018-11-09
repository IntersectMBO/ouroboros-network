{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.MsgChannel (
      MsgChannel(..)
    , RecvResult(..)
    ) where


-- | One end of a bidirectional message channel. It is a reliable, ordered
-- channel with message boundaries.
--
data MsgChannel failure m msg = MsgChannel {

       recv :: m (RecvResult failure msg (MsgChannel failure m msg)),

       send :: msg -> m (MsgChannel failure m msg)
     }

data RecvResult failure msg channel = RecvMsg msg channel
                                    | RecvFailure failure
                                    | ChannelClosed



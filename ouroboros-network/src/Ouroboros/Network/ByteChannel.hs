{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ByteChannel where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Network.Socket (Socket)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)


import           Network.TypedProtocol.ByteChannel


socketAsByteChannel :: Socket -> ByteChannel IO ByteString
socketAsByteChannel sd = ByteChannel {
    sendL = Socket.sendAll sd,
    recvL = \l -> Socket.recv sd (fromIntegral l) >>= \bs -> if BL.null bs
                                              then return Nothing
                                              else return (Just bs),
    byteLength = fromIntegral . BL.length
  }

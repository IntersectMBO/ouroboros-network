
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Socket (
      SocketBearer (..)
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import           Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Ouroboros.Network.Mux as Mx
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString.Lazy (sendAll, recv)


newtype SocketBearer m = SocketBearer {
    runSocketBearer :: IO m
    } deriving (Functor, Applicative)

instance Monad SocketBearer where
    return = pure

    {-# INLINE (>>=) #-}
    SocketBearer m >>= f = SocketBearer (m >>= runSocketBearer . f)

instance MonadIO SocketBearer where
    liftIO action =  liftIO action

data SocketCtx = SocketCtx {
      scSocket :: Socket
    }

instance Mx.MuxBearer SocketBearer where
    type AssociationDetails SocketBearer = AddrInfo
    type MuxBearerHandle SocketBearer = SocketCtx
    type LocalClockModel SocketBearer = Int -- microseconds

    open addr = liftIO $ do
        sd <- socket (addrFamily addr) Stream defaultProtocol
        setSocketOption sd ReuseAddr 1
        bind sd (addrAddress addr)
        listen sd 2
        return $ SocketCtx sd

    sduSize _ = return 1480 -- XXX query socket for PMTU/MSS

    write ctx fn = do
        ts <- liftIO $ getMonotonicTime
        let sdu = fn $ Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff
        liftIO $ sendAll (scSocket ctx) $ Mx.encodeMuxSDU sdu
        return ts

    read ctx = do
        hbuf <- liftIO $ recvLen' (scSocket ctx) 8 []
        case Mx.decodeMuxSDUHeader hbuf of
             Nothing     -> error "failed to decode header" -- XXX
             Just header -> do
                 blob <- liftIO $ recvLen' (scSocket ctx)
                                           (fromIntegral $ Mx.msLength header) []
                 ts <- liftIO $ getMonotonicTime
                 return (header {Mx.msBlob = blob}, ts)

    close ctx = liftIO $ close (scSocket ctx)
    abandon ctx = liftIO $ close (scSocket ctx)

recvLen' :: Socket -> Int64 -> [BL.ByteString] -> IO BL.ByteString
recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
recvLen' sd l bufs = do
    buf <- recv sd l
    if BL.null buf
          then error "socket closed" -- XXX throw exception
          else recvLen' sd (l - fromIntegral (BL.length buf)) (buf : bufs)



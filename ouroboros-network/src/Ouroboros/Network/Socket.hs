
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Socket (
      SocketBearer (..)
    , demo
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadFork
import           Data.Bits
import           Data.Int
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Ouroboros.Network.Mux as Mx
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString.Lazy (sendAll, recv)

import           Text.Printf

newtype SocketBearer m = SocketBearer {
    runSocketBearer :: IO m
    } deriving (Functor, Applicative)

instance Monad SocketBearer where
    return = pure

    {-# INLINE (>>=) #-}
    SocketBearer m >>= f = SocketBearer (m >>= runSocketBearer . f)

instance MonadIO SocketBearer where
    liftIO action =  SocketBearer $ liftIO action

data SocketCtx = SocketCtx {
      scSocket :: Socket
    }

instance Mx.MuxBearer SocketBearer where
    type AssociationDetails SocketBearer = AddrInfo
    type MuxBearerHandle SocketBearer = SocketCtx
    type LocalClockModel SocketBearer = Int -- microseconds

    open addr = liftIO $ do
        sd <- socket (addrFamily addr) Stream defaultProtocol
        connect sd (addrAddress addr)
        return $ SocketCtx sd

    server addr fn = do
        sd <- liftIO $ socket (addrFamily addr) Stream defaultProtocol
        liftIO $ setSocketOption sd ReuseAddr 1
        liftIO $ bind sd (addrAddress addr)
        liftIO $ listen sd 2
        forever $ do
            (client, _) <- liftIO $ accept sd
            fn $ SocketCtx client

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


demo :: IO ()
demo = do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6060")
    fork $ server addr
    threadDelay 1000000 -- give the server time to bind to the port
    runSocketBearer $ client addr
    return ()

  where
    server addr = do
        forever $ do
            runSocketBearer $ Mx.server addr serverAccept

    serverAccept :: Mx.MuxBearerHandle SocketBearer -> SocketBearer ()
    serverAccept ctx = do
        (_, _) <- Mx.read ctx
        -- liftIO $ printf "read '%s'\n" (show $ Mx.msBlob sdu0)
        let msg = pack "Pong"
        let sdu1 = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol 0 msg
        _ <- Mx.write ctx (cb sdu1)

        Mx.close ctx

    client :: Mx.AssociationDetails SocketBearer -> SocketBearer ()
    client addr = do
        ctx <- Mx.open addr
        let msg = pack "Ping"
        let sdu0 = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol 0 msg
        ts0 <- Mx.write ctx (cb sdu0)
        (_, ts1) <- Mx.read ctx
        liftIO $ printf "rtt %d\n" $ ts1 - ts0
        return ()

    cb sdu ts = sdu {Mx.msTimestamp = ts}


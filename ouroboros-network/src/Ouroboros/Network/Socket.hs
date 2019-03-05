

module Ouroboros.Network.Socket (
      killResponder
    , startInitiator
    , startInitiatorT
    , startResponder
    , startResponderT
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           GHC.Stack
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import qualified Ouroboros.Network.Mux as Mx

import           Text.Printf

newtype SocketCtx = SocketCtx { scSocket :: Socket }

setupMux :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl) => (ptcl -> Mx.MiniProtocolDescription ptcl IO)
         -> SocketCtx
         -> Maybe (Maybe SomeException -> IO ())
         -> IO ()
setupMux mpds ctx rescb_m =
    void $ Mx.muxStart mpds (writeSocket ctx) (readSocket ctx) (sduSize ctx) (closeSocket ctx) rescb_m

closeSocket :: SocketCtx -> IO ()
closeSocket ctx = close (scSocket ctx)

sduSize :: SocketCtx -> IO Word16
sduSize ctx = do
    -- XXX it is really not acceptable to call getSocketOption for every SDU we want to send
    mss <- getSocketOption (scSocket ctx) MaxSegment
    -- 1260 = IPv6 min MTU minus TCP header, 8 = mux header size
    return $ fromIntegral $ max (1260 - 8) (min 0xffff (15 * mss - 8))

writeSocket :: Mx.ProtocolEnum ptcl => SocketCtx -> Mx.MuxSDU ptcl -> IO (Time IO)
writeSocket ctx sdu = do
    --say "write"
    ts <- getMonotonicTime
    let sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff }
        buf = Mx.encodeMuxSDU sdu'
    --hexDump buf ""
    Socket.sendAll (scSocket ctx) buf
    return ts

readSocket :: (HasCallStack , Mx.ProtocolEnum ptcl) => SocketCtx -> IO (Mx.MuxSDU ptcl, Time IO)
readSocket ctx = do
        hbuf <- recvLen' (scSocket ctx) 8 []
        --say "read"
        --hexDump hbuf ""
        case Mx.decodeMuxSDUHeader hbuf of
             Left  e      -> throwM e
             Right header -> do
                 --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                 blob <- recvLen' (scSocket ctx)
                                  (fromIntegral $ Mx.msLength header) []
                 ts <- getMonotonicTime
                 --say $ (scName ctx) ++ " read blob"
                 --hexDump blob ""
                 return (header {Mx.msBlob = blob}, ts)
  where
    recvLen' :: Socket -> Int64 -> [BL.ByteString] -> IO BL.ByteString
    recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
    recvLen' sd l bufs = do
        buf <- Socket.recv sd l
        if BL.null buf
            then throwM $ Mx.MuxError Mx.MuxBearerClosed "Socket closed when reading data" callStack
            else recvLen' sd (l - fromIntegral (BL.length buf)) (buf : bufs)

startResponder :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
               => Mx.MiniProtocolDescriptions ptcl IO
               -> AddrInfo
               -> IO (Socket, Async ())
startResponder mpds addr = startResponderT mpds addr Nothing

startResponderT :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
                => Mx.MiniProtocolDescriptions ptcl IO
                -> AddrInfo
                -> Maybe (Maybe SomeException -> IO ())
                -> IO (Socket, Async ())
startResponderT mpds addr rescb_m =
    bracketOnError
        (socket (addrFamily addr) Stream defaultProtocol)
        close
        (\sd -> do
            setSocketOption sd ReuseAddr 1
            setSocketOption sd ReusePort 1
            bind sd (addrAddress addr)
            listen sd 2
            rh <- async (server sd)
            return (sd, rh)
        )
  where
    server sd = forever $ do
        (client, _) <- accept sd
        setupMux mpds (SocketCtx client) rescb_m

killResponder :: (Socket, Async ()) -> IO ()
killResponder (sd, hdl) = do
    cancel hdl
    close sd

startInitiator :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
               => Mx.MiniProtocolDescriptions ptcl IO
               -> AddrInfo
               -> AddrInfo
               -> IO ()
startInitiator mpds local remote = startInitiatorT mpds local remote Nothing

startInitiatorT :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
                => Mx.MiniProtocolDescriptions ptcl IO
                -> AddrInfo
                -> AddrInfo
                -> Maybe (Maybe SomeException -> IO ())
                -> IO ()
startInitiatorT mpds local remote rescb_m = do
    bracketOnError
        (socket (addrFamily local) Stream defaultProtocol)
        close
        (\sd -> do
            setSocketOption sd ReuseAddr 1
            setSocketOption sd ReusePort 1
            bind sd (addrAddress local)
            connect sd (addrAddress remote)

            setupMux mpds (SocketCtx sd) rescb_m
        )
    return ()

hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))



{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds                  #-}

module Ouroboros.Network.Socket (
      SocketBearer (..)
    , demo
    , demo2
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadST
import           Control.Monad.ST
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TVar as STM
import           Data.Bits
import           Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import           Data.Text (Text, unpack)
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString.Lazy (sendAll, recv)
import qualified Say as S

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.ChainSyncExamples
import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Serialise

import           Protocol.Channel (Duplex)
import           Protocol.Codec
import           Protocol.Driver
import qualified Codec.CBOR.Encoding as CBOR (Encoding)

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

instance MonadST SocketBearer where
    withLiftST = \f -> f stToSocketBearer

stToSocketBearer ::  ST RealWorld a -> SocketBearer a
stToSocketBearer x = SocketBearer $ stToIO x

data SocketCtx = SocketCtx {
      scSocket :: Socket
    }

newtype SocketBearerSTM a = SocketBearerSTM {
    runSocketBearerSTM :: STM.STM a
  } deriving (Functor, Applicative, Monad)

instance MonadFork SocketBearer where
  fork (SocketBearer io) = SocketBearer (fork io)

instance MonadSay SocketBearer where
  say x = S.sayString x

instance MonadSTM SocketBearer where
    type Tr   SocketBearer = SocketBearerSTM
    type TVar SocketBearer = STM.TVar

    atomically  =  SocketBearer . STM.atomically . runSocketBearerSTM
    newTVar     = SocketBearerSTM . STM.newTVar
    readTVar    = SocketBearerSTM . STM.readTVar
    writeTVar   = fmap SocketBearerSTM . STM.writeTVar
    retry       = SocketBearerSTM STM.retry

    newTVarIO   = SocketBearer . STM.newTVarIO
    modifyTVar  = fmap SocketBearerSTM . STM.modifyTVar
    modifyTVar' = fmap SocketBearerSTM . STM.modifyTVar'
    check       = SocketBearerSTM . STM.check

    type TMVar SocketBearer = STM.TMVar

    newTMVar        = SocketBearerSTM . STM.newTMVar
    newTMVarIO      = SocketBearer . STM.newTMVarIO
    newEmptyTMVar   = SocketBearerSTM STM.newEmptyTMVar
    newEmptyTMVarIO = SocketBearer STM.newEmptyTMVarIO
    takeTMVar       = SocketBearerSTM . STM.takeTMVar
    tryTakeTMVar    = SocketBearerSTM . STM.tryTakeTMVar
    putTMVar        = fmap SocketBearerSTM . STM.putTMVar
    tryPutTMVar     = fmap SocketBearerSTM . STM.tryPutTMVar
    readTMVar       = SocketBearerSTM . STM.readTMVar
    tryReadTMVar    = SocketBearerSTM . STM.tryReadTMVar
    swapTMVar       = fmap SocketBearerSTM . STM.swapTMVar
    isEmptyTMVar    = SocketBearerSTM . STM.isEmptyTMVar

    type TBQueue SocketBearer = STM.TBQueue

#if MIN_VERSION_stm(2,5,0)
    newTBQueue     = SocketBearerSTM . STM.newTBQueue
#else
    -- STM prior to 2.5.0 takes an Int
    newTBQueue     = SocketBearerSTM . STM.newTBQueue . fromEnum
#endif
    readTBQueue    = SocketBearerSTM . STM.readTBQueue
    writeTBQueue   = fmap SocketBearerSTM . STM.writeTBQueue


instance Mx.MuxBearer SocketBearer where
    type AssociationDetails SocketBearer = AddrInfo
    type MuxBearerHandle SocketBearer = SocketCtx
    type LocalClockModel SocketBearer = Int -- microseconds

    initiator local remote = liftIO $ do
        sd <- socket (addrFamily local) Stream defaultProtocol
        setSocketOption sd ReuseAddr 1
        setSocketOption sd ReusePort 1
        bind sd (addrAddress local)
        connect sd (addrAddress remote)
        return $ SocketCtx sd

    responder addr fn = do
        sd <- liftIO $ socket (addrFamily addr) Stream defaultProtocol
        liftIO $ do
            setSocketOption sd ReuseAddr 1
            setSocketOption sd ReusePort 1
            bind sd (addrAddress addr)
            listen sd 2
        forever $ do
            (client, _) <- liftIO $ accept sd
            fn $ SocketCtx client

    sduSize _ = return 1480 -- XXX query socket for PMTU/MSS

    write ctx fn = do
        say "write"
        ts <- liftIO $ getMonotonicTime
        let sdu = fn $ Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff
            buf = Mx.encodeMuxSDU sdu
        hexDump buf ""
        liftIO $ sendAll (scSocket ctx) buf
        return ts

    read ctx = do
        hbuf <- liftIO $ recvLen' (scSocket ctx) 8 []
        say "read"
        hexDump hbuf ""
        case Mx.decodeMuxSDUHeader hbuf of
             Nothing     -> error "failed to decode header" -- XXX
             Just header -> do
                 say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                 blob <- liftIO $ recvLen' (scSocket ctx)
                                           (fromIntegral $ Mx.msLength header) []
                 ts <- liftIO $ getMonotonicTime
                 say "read blob"
                 hexDump blob ""
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

hexDump :: BL.ByteString -> String -> SocketBearer ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = do
    hexDump (BL.tail buf) (out ++ (printf "0x%02x " (BL.head buf)))

demo2 :: forall block .
        (Chain.HasHeader block, Serialise block, Eq block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo2 chain0 updates = do
    a:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6060")
    b:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    producerVar <- newTVarIO (CPS.initChainProducerState chain0)
    consumerVar <- newTVarIO chain0
    consumerDone <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain
        a_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [(Mx.ChainSync, Mx.MiniProtocolDescription Mx.ChainSync
                        (consumerInit consumerDone target consumerVar)
                         consumerRsp)
                    ]
        b_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [(Mx.ChainSync, Mx.MiniProtocolDescription Mx.ChainSync
                        producerInit
                        (producerRsp producerVar))
                    ]

    runSocketBearer $ Mx.startResponder b_mps b
    say "started producer"
    threadDelay 1000000 -- give the producer time to start
    runSocketBearer $ Mx.startResponder a_mps a
    runSocketBearer $ Mx.startInitiator a_mps a b
    say "started consumer"

    sequence_
        [ do threadDelay 1000000 -- just to provide interest
             atomically $ do
                      p <- readTVar producerVar
                      let Just p' = CPS.applyChainUpdate update p
                      writeTVar producerVar p'
               | update <- updates ]

    r <- atomically $ takeTMVar consumerDone

    return r
  where

    checkTip target consumerVar = atomically $ do
          chain <- readTVar consumerVar
          return (Chain.headPoint chain == target)

    consumerClient :: TMVar SocketBearer Bool -> Point block -> (TVar SocketBearer (Chain block)) -> Client block SocketBearer ()
    consumerClient done target consChain = Client
        { rollforward = \_ -> checkTip target consChain >>= \b -> case b of
            True -> do
                pure $ Left ()
            False -> pure $ Right $ consumerClient done target consChain
        , rollbackward = \_ _ -> checkTip target consChain >>= \b -> case b of
            True -> do
                pure $ Left ()
            False -> pure $ Right $ consumerClient done target consChain
        , points = \_ -> pure $ consumerClient done target consChain
        }

    throwOnUnexpected :: String -> Result Text t -> IO t
    throwOnUnexpected str (Unexpected txt) = error $ str ++ " " ++ unpack txt
    throwOnUnexpected _   (Normal t) = pure t

    codec :: Codec SocketBearer Text CBOR.Encoding BS.ByteString (ChainSyncMessage block (Point block)) 'StIdle
    codec = hoistCodec stToSocketBearer codecChainSync

    consumerInit :: TMVar SocketBearer Bool -> Point block -> (TVar IO (Chain block)) -> Duplex SocketBearer SocketBearer CBOR.Encoding BS.ByteString -> SocketBearer ()
    consumerInit done target consChain channel = do
       let consumerPeer = chainSyncClientPeer (chainSyncClientExample consChain
                                               (consumerClient done target consChain))

       r <- useCodecWithDuplex channel codec consumerPeer
       liftIO $ throwOnUnexpected "consumer" r
       say "consumer done"
       atomically $ putTMVar done True

       return ()
    consumerRsp _ = return ()

    producerRsp ::  TVar SocketBearer (CPS.ChainProducerState block) -> Duplex SocketBearer SocketBearer CBOR.Encoding BS.ByteString -> SocketBearer ()
    producerRsp prodChain channel = forever $ do
        let producerPeer = chainSyncServerPeer (chainSyncServerExample () prodChain)

        r <- useCodecWithDuplex channel codec producerPeer
        liftIO $ throwOnUnexpected "producer" r
        say "producer done"


    producerInit _ = return ()


demo :: IO ()
demo = do
    serverAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6060")
    clientAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    fork $ server serverAddr
    threadDelay 1000000 -- give the server time to bind to the port
    runSocketBearer $ client clientAddr serverAddr
    return ()

  where
    server addr = do
        forever $ do
            runSocketBearer $ Mx.responder addr serverAccept

    serverAccept :: Mx.MuxBearerHandle SocketBearer -> SocketBearer ()
    serverAccept ctx = do
        (_, _) <- Mx.read ctx
        -- liftIO $ printf "read '%s'\n" (show $ Mx.msBlob sdu0)
        let msg = pack "Pong"
        let sdu1 = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol Mx.ModeResponder 0 msg
        _ <- Mx.write ctx (cb sdu1)

        Mx.close ctx

    client :: Mx.AssociationDetails SocketBearer -> Mx.AssociationDetails SocketBearer ->
              SocketBearer ()
    client local remote = do
        ctx <- Mx.initiator local remote
        let msg = pack "Ping"
        let sdu0 = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol Mx.ModeInitiator 0 msg
        ts0 <- Mx.write ctx (cb sdu0)
        (_, ts1) <- Mx.read ctx
        liftIO $ printf "rtt %d\n" $ ts1 - ts0
        return ()

    cb sdu ts = sdu {Mx.msTimestamp = ts}


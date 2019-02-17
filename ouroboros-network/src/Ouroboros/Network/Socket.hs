
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.Socket (
      SocketBearer (..)
    --, demo
    , demo2
    , runInitiator
    , runResponder
    , killResponder
    , hexDump
    ) where

import           Control.Concurrent.Async
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import           Control.Monad
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IO.Class
import           Control.Monad.ST
import qualified Control.Monad.STM as STM
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Int
--import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import           Data.Text (Text, unpack)
import           Data.Word
import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Say as S

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Serialise

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import           Protocol.Channel (Duplex)
import           Protocol.Codec
import           Protocol.Driver

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
    withLiftST f = f stToSocketBearer

stToSocketBearer ::  ST RealWorld a -> SocketBearer a
stToSocketBearer x = SocketBearer $ stToIO x

newtype SocketCtx = SocketCtx { scSocket :: Socket }

newtype SocketBearerSTM a = SocketBearerSTM {
    runSocketBearerSTM :: STM.STM a
  } deriving (Functor, Applicative, Monad)

instance MonadFork SocketBearer where
  fork (SocketBearer io) = SocketBearer (fork io)

instance MonadSay SocketBearer where
  say = S.sayString

instance MonadTime SocketBearer where
  type Time SocketBearer = Int -- microseconds
  getMonotonicTime = liftIO $ fmap (fromIntegral . (`div` 1000)) getMonotonicNSec

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64

instance MonadTimer SocketBearer where
    data Timeout SocketBearer = TimeoutSocketbearer !(TVar SocketBearer TimeoutState) !GHC.TimeoutKey

    readTimeout (TimeoutSocketbearer var _key) = readTVar var

    newTimeout = \usec -> do
        var <- newTVarM TimeoutPending
        mgr <- liftIO GHC.getSystemTimerManager
        key <- liftIO $ GHC.registerTimeout mgr usec (STM.atomically (timeoutAction var))
        return (TimeoutSocketbearer var key)
      where
        timeoutAction var = do
            x <- readTVar var
            case x of
                 TimeoutPending   -> writeTVar var TimeoutFired
                 TimeoutFired     -> error "MonadTimer(Socketbearer): invariant violation"
                 TimeoutCancelled -> return ()

    updateTimeout (TimeoutSocketbearer _var key) usec = do
      mgr <- liftIO GHC.getSystemTimerManager
      liftIO $ GHC.updateTimeout mgr key usec

    cancelTimeout (TimeoutSocketbearer var key) = do
        atomically $ do
            x <- readTVar var
            case x of
                 TimeoutPending   -> writeTVar var TimeoutCancelled
                 TimeoutFired     -> return ()
                 TimeoutCancelled -> return ()
        mgr <- liftIO GHC.getSystemTimerManager
        liftIO $ GHC.unregisterTimeout mgr key

    threadDelay d = liftIO $ threadDelay d

    registerDelay = undefined -- XXX

instance MonadSTM SocketBearer where
    type Tr   SocketBearer = SocketBearerSTM
    type TVar SocketBearer = STM.TVar

    atomically  =  SocketBearer . STM.atomically . runSocketBearerSTM
    newTVar     = SocketBearerSTM . STM.newTVar
    readTVar    = SocketBearerSTM . STM.readTVar
    writeTVar   = fmap SocketBearerSTM . STM.writeTVar
    retry       = SocketBearerSTM STM.retry

    newTVarM    = SocketBearer . STM.newTVarIO
    modifyTVar  = fmap SocketBearerSTM . STM.modifyTVar
    modifyTVar' = fmap SocketBearerSTM . STM.modifyTVar'
    check       = SocketBearerSTM . STM.check

    type TMVar SocketBearer = STM.TMVar

    newTMVar        = SocketBearerSTM . STM.newTMVar
    newTMVarM       = SocketBearer . STM.newTMVarIO
    newEmptyTMVar   = SocketBearerSTM STM.newEmptyTMVar
    newEmptyTMVarM  = SocketBearer STM.newEmptyTMVarIO
    takeTMVar       = SocketBearerSTM . STM.takeTMVar
    tryTakeTMVar    = SocketBearerSTM . STM.tryTakeTMVar
    putTMVar        = fmap SocketBearerSTM . STM.putTMVar
    tryPutTMVar     = fmap SocketBearerSTM . STM.tryPutTMVar
    readTMVar       = SocketBearerSTM . STM.readTMVar
    tryReadTMVar    = SocketBearerSTM . STM.tryReadTMVar
    swapTMVar       = fmap SocketBearerSTM . STM.swapTMVar
    isEmptyTMVar    = SocketBearerSTM . STM.isEmptyTMVar

    type TQueue SocketBearer = STM.TQueue
    newTQueue      = SocketBearerSTM   STM.newTQueue
    readTQueue     = SocketBearerSTM . STM.readTQueue
    tryReadTQueue  = SocketBearerSTM . STM.tryReadTQueue
    writeTQueue    = fmap SocketBearerSTM . STM.writeTQueue
    isEmptyTQueue  = SocketBearerSTM . STM.isEmptyTQueue

    type TBQueue SocketBearer = STM.TBQueue

#if MIN_VERSION_stm(2,5,0)
    newTBQueue     = SocketBearerSTM . STM.newTBQueue
#else
    -- STM prior to 2.5.0 takes an Int
    newTBQueue     = SocketBearerSTM . STM.newTBQueue . fromEnum
#endif
    readTBQueue    = SocketBearerSTM . STM.readTBQueue
    tryReadTBQueue = SocketBearerSTM . STM.tryReadTBQueue
    writeTBQueue   = fmap SocketBearerSTM . STM.writeTBQueue
    isEmptyTBQueue = SocketBearerSTM . STM.isEmptyTBQueue
    isFullTBQueue  = SocketBearerSTM . STM.isFullTBQueue


setupMux :: Mx.MiniProtocolDescriptions SocketBearer -> SocketCtx -> SocketBearer ()
setupMux mpds bearer = do
    jobs <- Mx.muxJobs mpds bearer
    aids <- liftIO $ mapM spawn jobs
    fork (watcher aids)

    return ()
  where
    watcher as = do
        (_,r) <- liftIO $ waitAnyCatchCancel as
        case r of
             Left  e -> say $ "SocketBearer died due to " ++ show e
             Right _ -> liftIO $ close (scSocket bearer)

    spawn job = async $ runSocketBearer job

instance Mx.MuxBearer SocketBearer where
    type AssociationDetails SocketBearer = AddrInfo
    type MuxBearerHandle SocketBearer = SocketCtx
    type LocalClockModel SocketBearer = Int -- microseconds
    type ResponderHandle SocketBearer = (Socket, Async ())

    initiator mpds local remote = do
        ctx <- liftIO $ do
            sd <- socket (addrFamily local) Stream defaultProtocol
            setSocketOption sd ReuseAddr 1
            setSocketOption sd ReusePort 1
            bind sd (addrAddress local)
            connect sd (addrAddress remote)
            return $ SocketCtx sd
        setupMux mpds ctx

    responder mpds addr = liftIO $ do
        sd <- socket (addrFamily addr) Stream defaultProtocol
        setSocketOption sd ReuseAddr 1
        setSocketOption sd ReusePort 1
        bind sd (addrAddress addr)
        listen sd 2
        rh <- async (runSocketBearer $ server sd)
        return (sd, rh)
      where
        server sd = forever $ do
                (client, _) <- liftIO $ accept sd
                setupMux mpds $ SocketCtx client

    killResponder (sd, hdl) = liftIO $ do
        cancel hdl
        close sd

    sduSize ctx = do
        mss <- liftIO $ getSocketOption (scSocket ctx) MaxSegment
        return $ fromIntegral $ max 0xffff (15 * mss)

    write ctx fn = do
        --say "write"
        ts <- liftIO getMonotonicTime
        let sdu = fn $ Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff
            buf = Mx.encodeMuxSDU sdu
        --hexDump buf ""
        liftIO $ sendAll (scSocket ctx) buf
        return ts

    read ctx = do
        hbuf <- liftIO $ recvLen' (scSocket ctx) 8 []
        --say "read"
        --hexDump hbuf ""
        case Mx.decodeMuxSDUHeader hbuf of
             Nothing     -> error "failed to decode header" -- XXX
             Just header -> do
                 --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                 --when ((Mx.msLength header) == 2) $ liftIO $ close (scSocket ctx)
                 blob <- liftIO $ recvLen' (scSocket ctx)
                                           (fromIntegral $ Mx.msLength header) []
                 ts <- liftIO getMonotonicTime
                 --say $ (scName ctx) ++ " read blob"
                 --hexDump blob ""
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

runResponder :: Mx.MiniProtocolDescriptions SocketBearer
             -> AddrInfo
             -> IO (Mx.ResponderHandle SocketBearer)
runResponder mps addr = runSocketBearer $ Mx.responder mps addr

killResponder :: Mx.ResponderHandle SocketBearer -> IO ()
killResponder h = runSocketBearer $ Mx.killResponder h

runInitiator :: Mx.MiniProtocolDescriptions SocketBearer
             -> AddrInfo
             -> AddrInfo
             -> IO ()
runInitiator mps local remote = runSocketBearer $ Mx.initiator mps local remote

hexDump :: BL.ByteString -> String -> SocketBearer ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))

demo2 :: forall block .
        (Chain.HasHeader block, Serialise block, Eq block, Show block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo2 chain0 updates = do
    a:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    b:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    {-printf $ "\nStart Chain:\n"
    printf $ (Chain.prettyPrintChain "\n" show chain0)
    printf "\n"-}

    producerVar <- newTVarM (CPS.initChainProducerState chain0)
    consumerVar <- newTVarM chain0
    consumerDone <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain
        a_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [(Mx.ChainSync, Mx.MiniProtocolDescription Mx.ChainSync
                        (consumerInit consumerDone target consumerVar)
                        dummyCallback)
                    ]
        b_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [(Mx.ChainSync, Mx.MiniProtocolDescription Mx.ChainSync
                        dummyCallback
                        (producerRsp producerVar))
                    ]

    {-printf $ "Expected Chain:\n"
    printf $ Chain.prettyPrintChain "\n" show expectedChain
    printf "\n"-}

    b_h <- runSocketBearer $ Mx.responder b_mps b
    --say "started producer"
    --threadDelay 1000000 -- give the producer time to start
    a_h <- runSocketBearer $ Mx.responder a_mps a
    runSocketBearer $ Mx.initiator a_mps a b
    --say "started consumer"

    wd <- async $ clientWatchDog consumerVar

    fork $ sequence_
        [ do threadDelay 10000 -- just to provide interest
             atomically $ do
                 p <- readTVar producerVar
                 let Just p' = CPS.applyChainUpdate update p
                 writeTVar producerVar p'
             | update <- updates
        ]

    r <- atomically $ takeTMVar consumerDone
    cancel wd
    runSocketBearer $ Mx.killResponder b_h
    runSocketBearer $ Mx.killResponder a_h

    {-printf $ "\nResult Chain:\n"
    chain1 <- atomically $ readTVar consumerVar
    printf $ (Chain.prettyPrintChain "\n" show chain1)
    printf "\n"-}

    return r
  where

    clientWatchDog consumerVar = forever $ do
        threadDelay 10000000
        say "\nClient WatchDog:\n"
        x <- atomically $ readTVar consumerVar
        printf (Chain.prettyPrintChain "\n" show x) :: IO ()
        printf "\n"

    checkTip target consumerVar = atomically $ do
          chain <- readTVar consumerVar
          return (Chain.headPoint chain == target)

    consumerClient :: Point block -> TVar SocketBearer (Chain block) -> Client block SocketBearer ()
    consumerClient target consChain =
      Client
        { rollforward = \_ -> checkTip target consChain >>= \b ->
            if b then pure $ Left ()
                 else pure $ Right $ consumerClient target consChain
        , rollbackward = \_ _ -> checkTip target consChain >>= \b ->
            if b then pure $ Left ()
                 else pure $ Right $ consumerClient target consChain
        , points = \_ -> pure $ consumerClient target consChain
        }

    throwOnUnexpected :: String -> Result Text t -> IO t
    throwOnUnexpected str (Unexpected txt) = error $ str ++ " " ++ unpack txt
    throwOnUnexpected _   (Normal t)       = pure t

    codec :: Codec SocketBearer Text CBOR.Encoding BS.ByteString (ChainSyncMessage block (Point block)) 'StIdle
    codec = hoistCodec stToSocketBearer codecChainSync

    consumerInit :: TMVar SocketBearer Bool -> Point block -> TVar IO (Chain block) -> Duplex SocketBearer SocketBearer CBOR.Encoding BS.ByteString -> SocketBearer ()
    consumerInit done target consChain channel = do
       let consumerPeer = chainSyncClientPeer (chainSyncClientExample consChain
                                               (consumerClient target consChain))

       r <- useCodecWithDuplex channel codec consumerPeer
       liftIO $ throwOnUnexpected "consumer" r
       --say "consumer done"
       atomically $ putTMVar done True

       return ()

    dummyCallback _ = forever $
        liftIO $ threadDelay 1000000

    producerRsp ::  TVar SocketBearer (CPS.ChainProducerState block) -> Duplex SocketBearer SocketBearer CBOR.Encoding BS.ByteString -> SocketBearer ()
    producerRsp prodChain channel = do
        let producerPeer = chainSyncServerPeer (chainSyncServerExample () prodChain)

        r <- useCodecWithDuplex channel codec producerPeer
        liftIO $ throwOnUnexpected "producer" r
        --say "producer done"


{-
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
-}


{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Socket (
    --, demo
      demo2
    , killResponder
    , startInitiator
    , startInitiatorT
    , startResponder
    , startResponderT
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           GHC.Stack
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.Channel
import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Serialise

import           Text.Printf

newtype SocketCtx = SocketCtx { scSocket :: Socket }

setupMux :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl) => (ptcl -> Mx.MiniProtocolDescription ptcl IO)
         -> SocketCtx
         -> Maybe (TQueue IO (Maybe SomeException))
         -> IO ()
setupMux mpds ctx resq_m = do
    jobs <- Mx.muxJobs mpds (writeSocket ctx) (readSocket ctx) (sduSize ctx)
    aids <- mapM async jobs
    void $ fork (watcher aids)
  where
    watcher as = do
        (_,r) <- waitAnyCatchCancel as
        close (scSocket ctx)
        case resq_m of
             Nothing ->
                 case r of
                      Left  e -> say $ "Socket Bearer died due to " ++ show e
                      Right _ -> return ()
             Just resq ->
                 case r of
                      Left  e -> atomically $ writeTQueue resq $ Just e
                      Right _ -> atomically $ writeTQueue resq Nothing

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

readSocket :: Mx.ProtocolEnum ptcl => SocketCtx -> IO (Mx.MuxSDU ptcl, Time IO)
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
                -> Maybe (TQueue IO (Maybe SomeException))
                -> IO (Socket, Async ())
startResponderT mpds addr resq_m = do
    sd <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption sd ReuseAddr 1
    setSocketOption sd ReusePort 1
    bind sd (addrAddress addr)
    listen sd 2
    rh <- async (server sd)
    return (sd, rh)
  where
    server sd = forever $ do
        (client, _) <- accept sd
        setupMux mpds (SocketCtx client) resq_m

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
                -> Maybe (TQueue IO (Maybe SomeException))
                -> IO ()
startInitiatorT mpds local remote resq_m = do
    sd <- socket (addrFamily local) Stream defaultProtocol
    setSocketOption sd ReuseAddr 1
    setSocketOption sd ReusePort 1
    bind sd (addrAddress local)
    connect sd (addrAddress remote)

    setupMux mpds (SocketCtx sd) resq_m

hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))

-- | The enumeration of all mini-protocols in our demo protocol.
data DemoProtocols = ChainSync
  deriving (Eq, Ord, Enum, Bounded)

instance Mx.ProtocolEnum DemoProtocols where
  fromProtocolEnum ChainSync = 2

  toProtocolEnum 2 = Just ChainSync
  toProtocolEnum _ = Nothing


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
        a_mps ChainSync = Mx.MiniProtocolDescription
                            (Mx.AppProtocolId ChainSync)
                            (consumerInit consumerDone target consumerVar)
                            dummyCallback
        b_mps ChainSync = Mx.MiniProtocolDescription
                            (Mx.AppProtocolId ChainSync)
                            dummyCallback
                            (producerRsp producerVar)

    {-printf $ "Expected Chain:\n"
    printf $ Chain.prettyPrintChain "\n" show expectedChain
    printf "\n"-}

    b_h <- startResponder b_mps b
    --say "started producer"
    --threadDelay 1000000 -- give the producer time to start
    a_h <- startResponder a_mps a
    startInitiator a_mps a b
    --say "started consumer"

    wd <- async $ clientWatchDog consumerVar

    void $ fork $ sequence_
        [ do threadDelay 10000 -- just to provide interest
             atomically $ do
                 p <- readTVar producerVar
                 let Just p' = CPS.applyChainUpdate update p
                 writeTVar producerVar p'
             | update <- updates
        ]

    r <- atomically $ takeTMVar consumerDone
    cancel wd
    killResponder b_h
    killResponder a_h

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

    consumerClient :: Point block -> TVar IO (Chain block) -> Client block IO ()
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

    consumerInit :: TMVar IO Bool -> Point block -> TVar IO (Chain block)
                 -> Channel IO BL.ByteString -> IO ()
    consumerInit done target consChain channel = do
       let consumerPeer = chainSyncClientPeer (chainSyncClientExample consChain
                                               (consumerClient target consChain))

       runPeer codecChainSync channel consumerPeer
       --say "consumer done"
       atomically $ putTMVar done True

       return ()

    dummyCallback _ = forever $
        threadDelay 1000000

    producerRsp ::  TVar IO (CPS.ChainProducerState block)
                -> Channel IO BL.ByteString -> IO ()
    producerRsp prodChain channel = do
        let producerPeer = chainSyncServerPeer (chainSyncServerExample () prodChain)

        runPeer codecChainSync channel producerPeer
        --say "producer done"


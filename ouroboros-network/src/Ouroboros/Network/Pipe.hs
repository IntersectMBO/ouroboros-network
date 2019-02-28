{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Pipe (
    demo
  ) where

import           Control.Monad
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import           Data.Word
import           System.IO (Handle, hClose, hFlush)
import           System.Process (createPipe)

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Serialise

import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Channel

import qualified Data.ByteString.Lazy as BL

data PipeCtx = PipeCtx {
      pcRead  :: Handle
    , pcWrite :: Handle
    }

setupMux :: Mx.MiniProtocolDescriptions DemoProtocols IO -> PipeCtx -> IO ()
setupMux mpds ctx =
    void $ Mx.muxStart mpds (writePipe ctx) (readPipe ctx) (sduSize ctx) (closePipe ctx) Nothing

closePipe :: PipeCtx -> IO ()
closePipe ctx = do
    hClose (pcRead ctx)
    hClose (pcWrite ctx)

sduSize :: PipeCtx -> IO Word16
sduSize _ = return 32768

writePipe :: PipeCtx -> Mx.MuxSDU DemoProtocols -> IO (Time IO)
writePipe ctx sdu = do
    ts <- getMonotonicTime
    let sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff }
        buf = Mx.encodeMuxSDU sdu'
    BL.hPut (pcWrite ctx) buf
    hFlush (pcWrite ctx)
    return ts

readPipe :: PipeCtx -> IO (Mx.MuxSDU DemoProtocols, Time IO)
readPipe ctx = do
        hbuf <- recvLen' (pcRead ctx) 8 []
        case Mx.decodeMuxSDUHeader hbuf of
             Left e     -> throwM e
             Right header -> do
                 --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                 blob <- recvLen' (pcRead ctx)
                                  (fromIntegral $ Mx.msLength header) []
                 ts <- getMonotonicTime
                 --say $ (scName ctx) ++ " read blob"
                 --hexDump blob ""
                 return (header {Mx.msBlob = blob}, ts)
  where
    recvLen' :: Handle -> Int -> [BL.ByteString] -> IO BL.ByteString
    recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
    recvLen' pd l bufs = do
        buf <- BL.hGet pd l
        if BL.null buf
            then error "pipe closed" -- XXX throw exception
            else recvLen' pd (l - fromIntegral (BL.length buf)) (buf : bufs)

startPipe :: Mx.MiniProtocolDescriptions DemoProtocols IO -> (Handle, Handle) -> IO ()
startPipe mpds (r, w) = do
    let ctx = PipeCtx r w
    setupMux mpds ctx

-- | The enumeration of all mini-protocols in our demo protocol.
data DemoProtocols = ChainSync
  deriving (Eq, Ord, Enum, Bounded)

instance Mx.ProtocolEnum DemoProtocols where
  fromProtocolEnum ChainSync = 2

  toProtocolEnum 2 = Just ChainSync
  toProtocolEnum _ = Nothing


-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo :: forall block .
        (Chain.HasHeader block, Serialise block, Eq block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo chain0 updates = do

    (hndRead1, hndWrite1) <- createPipe
    (hndRead2, hndWrite2) <- createPipe

    producerVar <- atomically $ newTVar (CPS.initChainProducerState chain0)
    consumerVar <- atomically $ newTVar chain0
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

    startPipe b_mps (hndRead1, hndWrite2)
    startPipe a_mps (hndRead2, hndWrite1)

    void $ fork $ sequence_
        [ do threadDelay 10000 -- just to provide interest
             atomically $ do
                 p <- readTVar producerVar
                 let Just p' = CPS.applyChainUpdate update p
                 writeTVar producerVar p'
             | update <- updates
        ]

    atomically $ takeTMVar consumerDone

  where
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
    consumerInit done_ target consChain channel = do
       let consumerPeer = chainSyncClientPeer (chainSyncClientExample consChain
                                               (consumerClient target consChain))

       runPeer codecChainSync channel consumerPeer
       atomically $ putTMVar done_ True

       return ()

    dummyCallback _ = forever $
        threadDelay 1000000

    producerRsp ::  TVar IO (CPS.ChainProducerState block)
                -> Channel IO BL.ByteString -> IO ()
    producerRsp prodChain channel = do
        let producerPeer = chainSyncServerPeer (chainSyncServerExample () prodChain)

        runPeer codecChainSync channel producerPeer


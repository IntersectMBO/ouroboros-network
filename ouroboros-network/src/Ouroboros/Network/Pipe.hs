{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Pipe (
    pipeDuplex
  , demo
  ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTimer
import           Control.Monad.ST (stToIO)
import           Data.Bits
import qualified Data.Map.Strict as M
import           Data.Text (Text, unpack)
import           Data.Word
import           System.IO (Handle, hClose, hFlush, hIsEOF)
import           System.Process (createPipe)

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

import           Protocol.Channel
import           Protocol.Codec
import           Protocol.Driver

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR (toBuilder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)

data PipeCtx = PipeCtx {
      pcRead  :: Handle
    , pcWrite :: Handle
    }

setupMux :: Mx.MiniProtocolDescriptions IO -> PipeCtx -> IO ()
setupMux mpds ctx = do
    jobs <- Mx.muxJobs mpds (writePipe ctx) (readPipe ctx) (sduSize ctx)
    aids <- mapM async jobs
    void $ fork (watcher aids)

  where
    watcher as = do
        (_,r) <- waitAnyCatchCancel as
        case r of
             Left  e -> print $ "Pipe Bearer died due to " ++ show e
             Right _ -> do
               hClose (pcRead ctx)
               hClose (pcWrite ctx)

sduSize :: PipeCtx -> IO Word16
sduSize _ = return 32768

writePipe :: PipeCtx -> Mx.MuxSDU -> IO (Time IO)
writePipe ctx sdu = do
    ts <- getMonotonicTime
    let sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff }
        buf = Mx.encodeMuxSDU sdu'
    BL.hPut (pcWrite ctx) buf
    hFlush (pcWrite ctx)
    return ts

readPipe :: PipeCtx -> IO (Mx.MuxSDU, Time IO)
readPipe ctx = do
        hbuf <- recvLen' (pcRead ctx) 8 []
        case Mx.decodeMuxSDUHeader hbuf of
             Nothing     -> error "failed to decode header" -- XXX
             Just header -> do
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

startPipe :: Mx.MiniProtocolDescriptions IO -> (Handle, Handle) -> IO ()
startPipe mpds (r, w) = do
    let ctx = PipeCtx r w
    setupMux mpds ctx

pipeDuplex
  :: Handle -- ^ Read
  -> Handle -- ^ Write
  -> Duplex IO IO CBOR.Encoding BS.ByteString
pipeDuplex hndRead hndWrite = uniformDuplex snd_ rcv
  where
    snd_ encoding = do
      BS.hPutBuilder hndWrite (CBOR.toBuilder encoding)
      hFlush hndWrite
    rcv = hIsEOF hndRead >>= \eof ->
      if eof
      then pure Nothing
      else fmap Just (BS.hGetSome hndRead LBS.smallChunkSize)

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

    throwOnUnexpected :: String -> Result Text t -> IO t
    throwOnUnexpected str (Unexpected txt) = error $ str ++ " " ++ unpack txt
    throwOnUnexpected _   (Normal t)       = pure t

    codec :: Codec IO Text CBOR.Encoding BS.ByteString (ChainSyncMessage block (Point block)) 'StIdle
    codec = hoistCodec stToIO codecChainSync

    consumerInit :: TMVar IO Bool -> Point block -> TVar IO (Chain block) -> Duplex IO IO CBOR.Encoding BS.ByteString -> IO ()
    consumerInit done_ target consChain channel = do
       let consumerPeer = chainSyncClientPeer (chainSyncClientExample consChain
                                               (consumerClient target consChain))

       r <- useCodecWithDuplex channel codec consumerPeer
       throwOnUnexpected "consumer" r
       atomically $ putTMVar done_ True

       return ()

    dummyCallback _ = forever $
        threadDelay 1000000

    producerRsp ::  TVar IO (CPS.ChainProducerState block) -> Duplex IO IO CBOR.Encoding BS.ByteString -> IO ()
    producerRsp prodChain channel = do
        let producerPeer = chainSyncServerPeer (chainSyncServerExample () prodChain)

        r <- useCodecWithDuplex channel codec producerPeer
        throwOnUnexpected "producer" r


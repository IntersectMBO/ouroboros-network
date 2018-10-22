{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipe where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST (RealWorld, stToIO)

import           Chain (Chain, ChainUpdate)
import qualified Chain
import           ChainProducerState as ChainProducer (ChainProducerState,
                     ReaderId, applyChainUpdate, initChainProducerState)
import           ConsumersAndProducers
import           Protocol
import           ProtocolInterfaces
import           Serialise

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)

import           System.IO (Handle, hFlush)
import           System.Process (createPipe)



data ProtocolAction s r a
  = Send s (IO (ProtocolAction s r a))
  | Recv (r -> IO (ProtocolAction s r a))
  | Fail ProtocolFailure

data ProtocolFailure = ProtocolStopped
                     | ProtocolFailure String
  deriving Show

newtype Protocol s r a = Protocol {
       unwrapProtocol ::
         forall x. (a -> IO (ProtocolAction s r x)) -> IO (ProtocolAction s r x)
     }

instance Functor (Protocol s r) where
    fmap f a = a >>= return . f

instance Applicative (Protocol s r) where
    pure x = Protocol $ \k -> k x
    (<*>) = ap

instance Monad (Protocol s r) where
    return = pure

    {-# INLINE (>>=) #-}
    m >>= f = Protocol $ \k ->
                unwrapProtocol m $ \x ->
                  unwrapProtocol (f x) k

instance MonadIO (Protocol s r) where
    liftIO action = Protocol (\k -> action >>= k)

unProtocol :: Protocol s r a -> IO (ProtocolAction s r a)
unProtocol (Protocol k) = k (\_ -> return (Fail ProtocolStopped))

recvMsg :: Protocol s r r
recvMsg = Protocol (\k -> return (Recv (\msg -> k msg)))

sendMsg :: s -> Protocol s r ()
sendMsg msg = Protocol (\k -> return (Send msg (k ())))

protocolFailure :: ProtocolFailure -> Protocol s r a
protocolFailure failure = Protocol (\_k -> return (Fail failure))

----------------------------------------

example1 :: Protocol String Int ()
example1 = do
    sendMsg "hello"
    x <- recvMsg
    liftIO $ print x
    return ()

consoleProtocolAction :: (Show s, Show r, Read r)
                      => Protocol s r a -> IO ()
consoleProtocolAction a = unProtocol a >>= go
  where
    go (Send msg k) = do
      print ("Send", msg)
      k >>= go
    go (Recv k)     = do
      print "Recv"
      x <- readLn
      print ("Recv", x)
      k x >>= go
    go (Fail err) =
      print ("Fail", err)

demo1 :: IO ()
demo1 = consoleProtocolAction example1

-------------------------------------------

-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo2 :: (Chain.HasHeader block, Serialise (block p), Eq (block p))
      => Chain (block p) -> [ChainUpdate (block p)] -> IO Bool
demo2 chain0 updates = do

    -- Create two pipes (each one is unidirectional) to connect up the
    -- producer and consumer ends of the protocol
    (hndRead1, hndWrite1) <- createPipe
    (hndRead2, hndWrite2) <- createPipe

    -- Initialise the producer and consumer state to be the same
    producerVar <- newTVarIO (initChainProducerState chain0)
    consumerVar <- newTVarIO chain0

    -- Fork the producer and consumer
    ptid <- forkIO $ producer hndRead2 hndWrite1 producerVar
    ctid <- forkIO $ consumer hndRead1 hndWrite2 consumerVar

    -- Apply updates to the producer's chain and let them sync
    _ <- forkIO $ sequence_
           [ do threadDelay 1000 -- just to provide interest
                atomically $ do
                  p <- readTVar producerVar
                  let Just p' = ChainProducer.applyChainUpdate update p
                  writeTVar producerVar p'
           | update <- updates ]

    -- Wait until the consumer's chain syncs with the producer's chain
    let Just expectedChain = Chain.applyChainUpdates updates chain0
    chain' <- atomically $ do
                chain' <- readTVar consumerVar
                check (Chain.headPoint expectedChain == Chain.headPoint chain')
                return chain'

    killThread ptid
    killThread ctid

    return (expectedChain == chain')

type ConsumerSideProtocol block = Protocol MsgConsumer (MsgProducer block)
type ProducerSideProtocol block = Protocol (MsgProducer block) MsgConsumer

producer :: forall p block. (Chain.HasHeader block, Serialise (block p))
         => Handle -> Handle -> TVar (ChainProducerState (block p)) -> IO ()
producer hndRead hndWrite producerVar = do
    runProtocolWithPipe
      hndRead hndWrite
      producerSideProtocol
  where
    -- Reuse the generic 'producerSideProtocol1'
    -- but interpret it in our Protocol free monad.
    producerSideProtocol :: ProducerSideProtocol (block p) ()
    producerSideProtocol =
      producerSideProtocol1
        producerHandlers
        sendMsg
        recvMsg

    -- Reuse the generic 'exampleProducer'
    -- and lift it from IO to the Protocol monad
    producerHandlers :: ProducerHandlers (block p) (ProducerSideProtocol (block p)) ReaderId
    producerHandlers =
      liftProducerHandlers liftIO (exampleProducer producerVar)

consumer :: forall p block. (Chain.HasHeader block, Serialise (block p))
         => Handle -> Handle -> TVar (Chain (block p)) -> IO ()
consumer hndRead hndWrite chainVar =
    runProtocolWithPipe
      hndRead hndWrite
      consumerSideProtocol
  where
    -- Reuse the generic 'consumerSideProtocol1'
    -- but interpret it in our Protocol free monad.
    consumerSideProtocol :: ConsumerSideProtocol (block p) ()
    consumerSideProtocol =
      consumerSideProtocol1
        consumerHandlers
        sendMsg
        recvMsg

    -- Reuse the generic 'exampleProducer'
    -- and lift it from IO to the Protocol monad
    consumerHandlers :: ConsumerHandlers (block p) (ConsumerSideProtocol (block p))
    consumerHandlers =
      liftConsumerHandlers liftIO (exampleConsumer chainVar)


------------------------------------------------

runProtocolWithPipe :: forall smsg rmsg.
                       (Serialise smsg, Serialise rmsg)
                    => Handle
                    -> Handle
                    -> Protocol smsg rmsg ()
                    -> IO ()
runProtocolWithPipe hndRead hndWrite p =
    unProtocol p >>= go mempty
  where
    go trailing (Send msg k) = do
      -- print ("Send", msg)
      BS.hPutBuilder hndWrite (CBOR.toBuilder (encode msg))
      hFlush hndWrite
      k >>= go trailing

    go trailing (Recv k) = do
      mmsg <- decodeFromHandle trailing
                =<< stToIO (CBOR.deserialiseIncremental decode)
      case mmsg of
        Left failure -> fail (show failure)
        Right (trailing', msg) -> do
          -- print ("Recv", msg)
          k msg >>= go trailing'

    go _trailing (Fail failure) = fail (show failure)

    decodeFromHandle :: BS.ByteString
                     -> CBOR.IDecode RealWorld rmsg
                     -> IO (Either CBOR.DeserialiseFailure
                                   (BS.ByteString, rmsg))

    decodeFromHandle _trailing (CBOR.Done trailing' _off msg) =
      return (Right (trailing', msg))

    decodeFromHandle _trailing (CBOR.Fail _trailing' _off failure) =
      return (Left failure)

    decodeFromHandle trailing (CBOR.Partial k) | not (BS.null trailing) =
      stToIO (k (Just trailing)) >>= decodeFromHandle mempty

    decodeFromHandle _ (CBOR.Partial k) = do
      chunk <- BS.hGetSome hndRead LBS.smallChunkSize
      stToIO (k (if BS.null chunk then Nothing else Just chunk))
        >>= decodeFromHandle mempty

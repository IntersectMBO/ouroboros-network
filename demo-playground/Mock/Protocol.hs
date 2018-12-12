{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- NOT TO BE CONFUSED with the notion of protocol from typed-transitions.
-- This one is just a free monad over IO with sending, receiving, and failing.
-- It's used in the demo-playground so it was preserved.

module Protocol (
    Protocol
  , ProtocolFailure(..)
  , recvMsg
  , sendMsg
  , runProtocolWithPipe
    -- * Demos
  , demo1
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Cont (ContT (..))
import           Control.Monad.IO.Class
import           Control.Monad.ST (RealWorld, stToIO)
import           System.IO (Handle, hFlush)

import           Ouroboros.Network.Serialise

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)

data ProtocolAction s r a
  = Send s (IO (ProtocolAction s r a))
  | Recv (r -> IO (ProtocolAction s r a))
  | Fail ProtocolFailure

data ProtocolFailure = ProtocolStopped
                     | ProtocolFailure String
  deriving Show

instance Exception ProtocolFailure

newtype Protocol s r a = Protocol {
       unwrapProtocol ::
         forall x. ContT (ProtocolAction s r x) IO a
     }
    deriving Functor

instance Applicative (Protocol s r) where
    pure x = Protocol $ pure x
    (<*>)  = ap

instance Monad (Protocol s r) where
    return = pure

    {-# INLINE (>>=) #-}
    Protocol m >>= f = Protocol (m >>= unwrapProtocol . f)

instance MonadIO (Protocol s r) where
    liftIO action = Protocol $ liftIO action

unProtocol :: Protocol s r a -> IO (ProtocolAction s r a)
unProtocol (Protocol (ContT k)) = k (\_ -> return (Fail ProtocolStopped))

recvMsg :: Protocol s r r
recvMsg = Protocol $ ContT (\k -> return (Recv (\msg -> k msg)))

sendMsg :: s -> Protocol s r ()
sendMsg msg = Protocol $ ContT (\k -> return (Send msg (k ())))

_protocolFailure :: ProtocolFailure -> Protocol s r a
_protocolFailure failure = Protocol $ ContT (\_k -> return (Fail failure))

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

    go _trailing (Fail ex) = throwIO ex

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

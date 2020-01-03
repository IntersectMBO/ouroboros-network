{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Async.PingPong where

import           Control.Concurrent
import           Control.Exception

import           Test.QuickCheck


-- if 'Bool' param is 'False', then the channel (or the other end) will block
-- on read.  This spoofs the size of data by one.
--
data BinaryChannel a = BinaryChannel {
    readChannel  :: Bool -> IO (Maybe a),
    writeChannel :: Bool -> a -> IO (),
    closeChannel :: IO ()
  }

data ChannelError = ReceivedNullBytes
  deriving Show

instance Exception ChannelError

--
-- PingPong Client API
--

data PingPongClient a
    = SendPing !a (PingPongClient a)
    | Done
    -- ^ for the sake of simplicity we never send a terminating message

-- Send n request and the reply the last message to the server
listPingPongClient :: [a] -> PingPongClient a
listPingPongClient = go
  where
    go []       = Done
    go (a : as) = SendPing a (go as)

constPingPongClient :: Int -> a -> PingPongClient a
constPingPongClient n a = listPingPongClient (replicate n a)

data Blocking = BlockOnRead | BlockOnWrite | NonBlocking
  deriving Show

instance Arbitrary Blocking where
    arbitrary = frequency [ (3, pure BlockOnRead)
                          , (2, pure BlockOnWrite)
                          , (1, pure NonBlocking)
                          ]

runPingPongClient :: BinaryChannel a
                  -> Blocking
                  -> ThreadId  -- producer thread
                  -> PingPongClient a
                  -> IO [a]
runPingPongClient channel blocking tid = go []
    where
      go !res (SendPing a Done) = do
        -- send the message, but report more bytes to the server; this makes
        -- sure that it will blocked on reading when we kill it
        case blocking of
          BlockOnRead -> do
            writeChannel channel False a
            -- run the server thread now, so it blocks on reading
            yield
            killThread tid
            pure $ reverse res
          BlockOnWrite -> do
            writeChannel channel True a
            mr <- readChannel channel False
            -- run the server thread now, so it block on writing
            yield
            killThread tid
            case mr of
              Nothing -> pure (reverse res)
              Just r  -> pure (reverse (r : res))
          NonBlocking -> do
            writeChannel channel True a
            mr <- readChannel channel True
            killThread tid
            case mr of
              Just r  -> pure $ reverse (r : res)
              Nothing -> pure $ reverse res
      go !res (SendPing a next) = do
        writeChannel channel True a
        mr <- readChannel channel True
        case mr of
          Just r  -> go (r : res) next
          Nothing -> do
            killThread tid
            pure $ reverse res
      go !res Done = do
        killThread tid
        pure $ reverse res


-- Do pipelining, the the client will never read, instead it will kill the
-- server.
runPingPongClientPipelined :: BinaryChannel a
                           -> Blocking
                           -> ThreadId
                           -> [a]
                           -> IO (Maybe [a])
runPingPongClientPipelined channel blocking tid as0 = goSend as0
    where
      goSend []  = error "runPingPongClientPipelined: expected non empty list"
      goSend [a] = do
        -- send the message, but report more bytes to the server; this makes
        -- sure that it will blocked on reading when we kill it
        case blocking of
          BlockOnRead -> do
            writeChannel channel False a
            -- run the server thread now, so it blocks on reading
            yield
            killThread tid
            pure Nothing
          BlockOnWrite -> do
            writeChannel channel True a
            _ <- readChannel channel False
            -- run the server thread now, so it block on writing
            yield
            killThread tid
            pure Nothing
          NonBlocking -> do
            writeChannel channel True a
            goRecv [] as0
      goSend (a : as) = do
        writeChannel channel True a
        goSend as

      goRecv res [] = do
        killThread tid
        pure (Just $ reverse res)
      goRecv res (_ : as) = do
        Just r <- readChannel channel True
        goRecv (r : res) as


--
-- PingPong Server API
--


data PingPongServer a = PingPongServer {
    recvPing :: a -> IO (a, PingPongServer a)
  }

constPingPongServer :: PingPongServer a
constPingPongServer = PingPongServer {
    recvPing = \a -> pure (a, constPingPongServer)
  }

runPingPongServer :: BinaryChannel a
                  -> PingPongServer a
                  -> IO ()
runPingPongServer channel PingPongServer { recvPing } = do
    Just a <- readChannel channel True
    (a', server') <- recvPing a
    writeChannel channel True a'
    runPingPongServer channel server'

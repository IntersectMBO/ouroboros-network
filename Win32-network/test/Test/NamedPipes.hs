{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Test.NamedPipes
  ( tests
  , test_PingPong
  , test_PingPongPipelined
  , Blocking (..)
  , handleToBinaryChannel
  -- generators
  , NonEmptyBS (..)
  , LargeNonEmptyBS (..)
  ) where

import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Exception (SomeException, AsyncException (..), Exception, catch, bracket, finally, throwIO, mask)
import           Control.Monad (when)
import           Data.Functor (void)
import           Data.Foldable (foldl', traverse_)
import           Data.Binary (Binary (..), encode, decode)
import           Data.Bits
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import           System.Win32
import           System.Win32.NamedPipes
import           System.Win32.File.Interruptible

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

pipeName :: String
pipeName = "\\\\.\\pipe\\test-Win32-network"

tests :: TestTree
tests =
  testGroup "Win32.NamedPipes"
  [ testCase "interruptible connectNamedPipe"
      test_interruptible_connectNamedPipe
  , testCase "interruptible readHandle"
      test_interruptible_readHandle
  , testCase "interruptible readHandle twice (synchronous)"
      test_interruptible_readHandle_sync
  , testCase "concurrent read and write"
      test_concurrent_read_and_write
  , testProperty "writeHandle & readHandle"         prop_WriteRead
  , testProperty "writeHandle & readHandle (large)" prop_WriteReadLarge
  , testProperty "interruptible writeHandle"        prop_interruptible_writeHandle

  , testProperty "PingPong test"                    prop_PingPong
  , testProperty "PingPongPipelined test"           prop_PingPongPipelined

  , testGroup "generators"
    [ testProperty "NonEmptyBS" prop_NonEmptyBS
    , testProperty "NonEmptyBS" prop_shrink_NonEmptyBS
    ]
  ]


-- | This test would fail (deadlock) if the blocking call `connectNamedPipe`,
-- would not be interruptible.
--
test_interruptible_connectNamedPipe :: IO ()
test_interruptible_connectNamedPipe =
    bracket (createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing)
            closeHandle
            $ \hpipe -> do
                tid <- forkIO (connectNamedPipe hpipe)
                threadDelay 100
                killThread tid

-- | Check if 'readHandle'`is interruptible
--
test_interruptible_readHandle :: IO ()
test_interruptible_readHandle =
    bracket ((,) <$> createNamedPipe pipeName
                                     pIPE_ACCESS_DUPLEX
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                gENERIC_READ
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_ATTRIBUTE_NORMAL
                                Nothing)
            -- 'IO ()' is a monoid!
            (foldMap closeHandle)
            $ \(_,     hpipe') -> do
                tid <- forkIO (void $ readHandle hpipe' 1 Nothing)
                threadDelay 100
                killThread tid

-- | Interrupt two consecutive reads.
--
test_interruptible_readHandle_sync :: IO ()
test_interruptible_readHandle_sync =
    bracket ((,) <$> createNamedPipe pipeName
                                     pIPE_ACCESS_DUPLEX
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                (gENERIC_READ .|. gENERIC_WRITE)
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_ATTRIBUTE_NORMAL
                                Nothing)
             (foldMap closeHandle)
             $ \(_,     hpipe') -> do
                tid <- forkIO (void $ readHandle hpipe' 1 Nothing)
                threadDelay 100
                killThread tid
                tid' <- forkIO (void $ readHandle hpipe' 1 Nothing)
                threadDelay 100
                killThread tid'


test_concurrent_read_and_write :: IO ()
test_concurrent_read_and_write =
    bracket ((,) <$> createNamedPipe pipeName
                                     pIPE_ACCESS_DUPLEX
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                (gENERIC_READ .|. gENERIC_WRITE)
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_ATTRIBUTE_NORMAL
                                Nothing)
            (foldMap closeHandle)
            $ \(hServer, hClient) -> do
              -- connectNamedPipe hServer Nothing
              let x = BSC.pack (replicate 2_500_000 'x')
                  y = BSC.pack (replicate 2_500_000 'y')
              clientVar <- newEmptyMVar
              _ <- forkIO $ do
                y' <- readHandle hClient (BS.length y) Nothing
                putMVar clientVar y'
              _ <- forkIO $ do
                threadDelay 100
                writeHandle hClient x Nothing
              threadDelay 200
              writeHandle hServer y Nothing
              x' <- readHandle hServer (BS.length x) Nothing
              y' <- takeMVar clientVar
              assertBool "concurrent read and write" $ x == x' && y == y'


-- | Small non-empty 'ByteString's.
--
data NonEmptyBS = NonEmptyBS { getNonEmptyBS :: ByteString }
  deriving (Eq, Show)

instance Arbitrary NonEmptyBS where
    arbitrary = do
      bs <- arbitrary
      if BS.null bs
        then do
          -- generate a non empty string
          NonEmpty s <- arbitrary
          pure (NonEmptyBS $ BSC.pack s)
        else pure (NonEmptyBS bs)

    shrink (NonEmptyBS bs) =
      [ NonEmptyBS bs'
      | bs' <- shrink bs
      , not (BS.null bs')
      ]

prop_NonEmptyBS :: NonEmptyBS -> Bool
prop_NonEmptyBS (NonEmptyBS bs) = not (BS.null bs)

prop_shrink_NonEmptyBS :: NonEmptyBS -> Bool
prop_shrink_NonEmptyBS = all (\(NonEmptyBS bs) -> not (BS.null bs)) . shrink

-- | Large non-empty 'ByteString's, up to 2.5MB.
--
data LargeNonEmptyBS = LargeNonEmptyBS
    { getLargeNonEmptyBS :: ByteString
    , getSize            :: Word
      -- ^ arbitrary size which is less than length of the bytestring; Useful
      -- for setting buffer size of a named pipe
    }
  deriving (Show, Eq)

instance Arbitrary LargeNonEmptyBS where
    arbitrary = do
        bs <- getNonEmptyBS <$> resize 2_500_000 arbitrary
        bufSize <- fromIntegral <$> choose (64, BS.length bs)
        pure $ LargeNonEmptyBS bs bufSize

    shrink (LargeNonEmptyBS bs bufSize) =
      [ (LargeNonEmptyBS bs' (min bufSize (fromIntegral $ BS.length bs')))
      | NonEmptyBS bs' <- shrink (NonEmptyBS bs)
      ]

test_WriteRead :: ByteString -> IO Bool
test_WriteRead bs =
    bracket ((,) <$> createNamedPipe pipeName
                                     pIPE_ACCESS_DUPLEX
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     maxBound
                                     maxBound
                                     0
                                     Nothing
                 <*> createFile pipeName
                                (gENERIC_READ .|. gENERIC_WRITE)
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_ATTRIBUTE_NORMAL
                                Nothing)
            (foldMap closeHandle)
            $ \(r,w) -> do
              bs' <- writeHandle w bs Nothing >> readHandle r (BS.length bs) Nothing
              pure (bs == bs')

prop_WriteRead :: NonEmptyBS -> Property
prop_WriteRead = ioProperty . test_WriteRead . getNonEmptyBS

prop_WriteReadLarge :: LargeNonEmptyBS -> Property
prop_WriteReadLarge = ioProperty . test_WriteRead . getLargeNonEmptyBS


prop_interruptible_writeHandle :: Property
prop_interruptible_writeHandle = ioProperty $ do
    let bs = BSC.pack $ replicate 100 'a'
    v <- newEmptyMVar

    bracket
      ((,) <$> createNamedPipe pipeName
                               pIPE_ACCESS_DUPLEX
                               (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                               pIPE_UNLIMITED_INSTANCES
                               1
                               1
                               0
                               Nothing
           <*> createFile pipeName
                          gENERIC_WRITE
                          fILE_SHARE_NONE
                          Nothing
                          oPEN_EXISTING
                          fILE_ATTRIBUTE_NORMAL
                          Nothing)
      (foldMap closeHandle)
      $ \(_,w) -> do

        tid <- mask $ \unmask -> forkIO $ void $
          unmask (writeHandle w bs Nothing)
            `catch` \(e :: AsyncException) -> putMVar v e >> throwIO e

        killThread tid

        (Just ThreadKilled ==) <$> tryReadMVar v


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

-- | Send & receive from a pipe using a simple framing (header is the size of
-- the payload).  Note that the pipe does buffering of ingress and egress
-- bytes, and thus both operation can block.
--
handleToBinaryChannel :: Binary a
                      => (Int -> IO ByteString)
                      -> (ByteString -> IO ())
                      -> HANDLE
                      -> BinaryChannel a
handleToBinaryChannel readH writeH h = BinaryChannel { readChannel, writeChannel, closeChannel }
    where
      -- send all chunks through the pipe
      writeChannel b a = do
        let chunks :: [ByteString]
            chunks = BSL.toChunks (encode a)
            size   :: Int
            size   = bool (+1) id b $ foldl' (\x y -> x + BS.length y) 0 chunks
        -- send header
        _ <- writeH (BSL.toStrict $ encode size) -- just a single chunk
        -- send payload
        traverse_ (\chunk -> writeH chunk) chunks

      readChannel b = do
        bs <- readLen [] 8
        if b
          then do
            let s = decode (BSL.fromStrict bs)
            bs' <- readLen [] s
            pure $ Just $ decode $ BSL.fromStrict $ bs'
          else pure Nothing

      readLen !bufs 0 = pure $ BS.concat (reverse bufs)
      readLen !bufs s = do
        bs <- readH s
        when (BS.null bs)
          $ throwIO ReceivedNullBytes
        readLen (bs : bufs) (s - fromIntegral (BS.length bs))
        
      closeChannel = closeHandle h

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

forkPingPongServer :: forall a.
                      Binary a
                   => HANDLE
                   -> (HANDLE -> IO ()) -- ^ connectNamedPipe
                   -> BinaryChannel a
                   -> PingPongServer a
                   -> IO (ThreadId, MVar ())
forkPingPongServer h connect channel server = do
      var <- newEmptyMVar
      tid <- forkIOWithUnmask $ \unmask ->
          unmask
            (do
              -- the connectNamedPipe call is blocking, but we call it with
              -- async exceptions masked
              --
              -- TODO: once in a while this errors with `resource vanished`,
              -- in this case the test passess.
              connect h
                -- useful for debugging
                `catch` (\(e :: SomeException) -> do
                          putStrLn $ "forkPingPongServer: connect error: " ++ show e
                          throwIO e)
              runPingPongServer channel server)
          `finally` putMVar var ()
      pure (tid, var)


-- 
-- PingPong tests
--

-- | Stress test for named pipe ffi calls.
--
-- For each entry in @NonEmptyList (NonNegative Int)@ we run a single instance
-- of a ping pong protocol which will exchange that many messages.  Each
-- instance runs on it's own named pipe.  When the client sends its last message, it
-- misinforms the server about the size of the message, and kills the server
-- thread.  This ensure that when we kill the server it is blocked on reading.
--
test_PingPong :: (HANDLE -> BinaryChannel ByteString)
              -> (HANDLE -> IO ())
              -> (String -> DWORD -> IO HANDLE)
              -> (String          -> IO HANDLE)
              -> Int
              -- ^ the number of messages exchanged in the ping pong protocol
              -> Blocking
              -> LargeNonEmptyBS
              -> IO Bool
test_PingPong handleToBinaryChannelK
              connect
              createNamedPipeK
              createFileK
              n
              blocking
              (LargeNonEmptyBS bs bufSize) = do

    let pname = pipeName ++ "-ping-pong"

    -- fork the PingPong server
    h <- createNamedPipeK pname (fromIntegral bufSize)
    let channel = handleToBinaryChannelK h
    (tid, lock) <- forkPingPongServer
                      h connect channel
                      (constPingPongServer @ByteString)

    -- run the PingPong client
    channel' <- handleToBinaryChannelK <$> createFileK pname
    res <- runPingPongClient channel' blocking tid (constPingPongClient n bs)

    -- await until server is killed
    takeMVar lock
    closeChannel channel
    closeChannel channel'

    pure $ case blocking of
      NonBlocking -> res == replicate n bs
      _           -> res == replicate (pred n) bs


prop_PingPong :: NonNegative Int
              -- ^ number of messages exchanged in the ping pong protocol
              -> Blocking
              -> LargeNonEmptyBS
              -> Property
prop_PingPong (NonNegative n) blocking bss = ioProperty $
    test_PingPong
      (\h -> handleToBinaryChannel
               (\s  -> readHandle  h s Nothing)
               (\bs -> writeHandle h bs Nothing)
               h)
      connectNamedPipe
      (\name  bufSize -> createNamedPipe name
                                pIPE_ACCESS_DUPLEX
                                (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                pIPE_UNLIMITED_INSTANCES
                                (fromIntegral bufSize)
                                (fromIntegral bufSize)
                                0
                                Nothing)
      (\name -> createFile name
                           (gENERIC_READ .|. gENERIC_WRITE)
                           fILE_SHARE_NONE
                           Nothing
                           oPEN_EXISTING
                           fILE_ATTRIBUTE_NORMAL
                           Nothing)
      n blocking bss


--
-- Pipelined PingPong test
--


test_PingPongPipelined :: (HANDLE -> BinaryChannel ByteString)
                       -> (HANDLE -> IO ())
                       -> (String -> IO HANDLE)
                       -> (String -> IO HANDLE)
                       -> Blocking
                       -> [ByteString]
                       -- non empty list of requests
                       -> IO Bool
test_PingPongPipelined handleToBinaryChannelK
              connect
              createNamedPipeK
              createFileK
              blocking
              bss = do

    let pname = pipeName ++ "-ping-pong-pipelined"

    -- fork the PingPong server
    h <- createNamedPipeK pname
    let channel = handleToBinaryChannelK h
    (tid, lock) <-
        forkPingPongServer
          h connect channel
          (constPingPongServer @ByteString)

    -- run the PingPong client
    channel' <- handleToBinaryChannelK <$> createFileK pname
    res <- runPingPongClientPipelined channel' blocking tid bss

    takeMVar lock
    closeChannel channel
    closeChannel channel'
    case blocking of
      NonBlocking -> case res of
        Just bss' -> pure $ bss == bss'
        Nothing   -> pure False
      _           -> pure True -- if we evalute this case branch, it means that
                               -- killing blocked thread did not deadlock.


prop_PingPongPipelined :: Blocking
                       -> Positive Int
                       -- ^ in-bound buffer size
                       -> NonEmptyList LargeNonEmptyBS
                       -> Property
prop_PingPongPipelined blocking (Positive bufSize) (NonEmpty bss) = ioProperty $
    test_PingPongPipelined
      (\h -> handleToBinaryChannel
               (\s  -> readHandle  h s Nothing)
               (\bs -> writeHandle h bs Nothing)
               h)
      connectNamedPipe
      (\name -> createNamedPipe name
                                pIPE_ACCESS_DUPLEX
                                (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                pIPE_UNLIMITED_INSTANCES
                                (fromIntegral bufSize)
                                maxBound -- outbout queue must be atleast n *
                                         -- size of the bytestring, so that the
                                         -- client will not bclok.
                                0
                                Nothing)
      (\name -> createFile name
                           (gENERIC_READ .|. gENERIC_WRITE)
                           fILE_SHARE_NONE
                           Nothing
                           oPEN_EXISTING
                           fILE_ATTRIBUTE_NORMAL
                           Nothing)
      blocking (map getLargeNonEmptyBS bss)

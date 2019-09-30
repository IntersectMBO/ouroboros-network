{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Test.NamedPipes (tests) where

import           Control.Concurrent.MVar
import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Exception (AsyncException (..), Exception, catch, finally, throwIO, mask)
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

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

pipeName :: String
pipeName = "\\\\.\\pipe\\nWin32-named-pipes-test"

tests :: TestTree
tests =
  testGroup "NamedPipes"
  [ testCase "interruptible connectNamedPipe"
      test_interruptible_connectNamedPipe
  , testCase "interruptible readPipe"
      test_interruptible_readPipe
  , testCase "interruptible readPipe twice (synchronous)"
      test_interruptible_readPipe_sync
  , testCase "interruptible readPipe twice (concurrent)"
      test_interruptible_readPipe_conc
  , testProperty "writePipe & readPipe"         prop_WriteRead
  , testProperty "writePipe & readPipe (large)" prop_WriteReadLarge
  , testProperty "interruptible writePipe"      prop_interruptible_Write

  , testProperty "PingPong Stress test"         (withMaxSuccess 75 prop_PingPong)

  , testGroup "generators"
    [ testProperty "NonEmptyBS" prop_NonEmptyBS
    , testProperty "NonEmptyBS" prop_shrink_NonEmptyBS
    ]
  ]


-- | This test would fail (deadlock) if the blocking call `connectNamedPipe`,
-- would not be interruptible.
--
test_interruptible_connectNamedPipe :: IO ()
test_interruptible_connectNamedPipe = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    tid <- forkIO (connectNamedPipe hpipe Nothing)
    threadDelay 100
    killThread tid
    closePipe hpipe

-- | Check if 'readPipe'`is interruptible
--
test_interruptible_readPipe :: IO ()
test_interruptible_readPipe = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                         gENERIC_READ
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    tid <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    closePipe hpipe'
    closePipe hpipe

-- | Interrupt two consecutive reads.
--
test_interruptible_readPipe_sync :: IO ()
test_interruptible_readPipe_sync = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                        gENERIC_READ
                        fILE_SHARE_NONE
                        Nothing
                        oPEN_EXISTING
                        fILE_ATTRIBUTE_NORMAL
                        Nothing
    tid <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    tid' <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid'
    closePipe hpipe'
    closePipe hpipe


-- | Interrupt two simultanous reads.
--
test_interruptible_readPipe_conc :: IO ()
test_interruptible_readPipe_conc = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                         gENERIC_READ
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    tid  <- forkIO (void $ readPipe hpipe' 1)
    tid' <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    killThread tid'
    closePipe hpipe'
    closePipe hpipe


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

test_WriteRead :: ByteString -> Property
test_WriteRead bs = ioProperty (do
    hRead <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             maxBound
                             maxBound
                             0
                             Nothing
    hWrite <- createFile pipeName
                         (gENERIC_READ .|. gENERIC_WRITE)
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    _ <- writePipe hWrite bs
    bs' <- readPipe hRead (BS.length bs)
    closePipe hRead
    closePipe hWrite
    pure (bs === bs'))

prop_WriteRead :: NonEmptyBS -> Property
prop_WriteRead = test_WriteRead . getNonEmptyBS

prop_WriteReadLarge :: LargeNonEmptyBS -> Property
prop_WriteReadLarge = test_WriteRead . getLargeNonEmptyBS


prop_interruptible_Write :: Property
prop_interruptible_Write = ioProperty $ do
    let bs = BSC.pack $ replicate 100 'a'
    v <- newEmptyMVar

    -- create a pipe which will block after writing 1 byte
    hRead <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             1
                             1
                             0
                             Nothing
    hWrite <- createFile pipeName
                         gENERIC_WRITE
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing

    tid <- mask $ \unmask -> forkIO $ void $
      unmask (writePipe hWrite bs)
        `catch` \(e :: AsyncException) -> putMVar v e >> throwIO e

    killThread tid
    closePipe hRead
    closePipe hWrite

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
handleToBinaryChannel :: Binary a => HANDLE -> BinaryChannel a
handleToBinaryChannel h = BinaryChannel { readChannel, writeChannel, closeChannel }
    where
      -- send all chunks through the pipe
      writeChannel b a = do
        let chunks :: [ByteString]
            chunks = BSL.toChunks (encode a)
            size   :: Int
            size   = bool (+1) id b $ foldl' (\x y -> x + BS.length y) 0 chunks
        -- send header
        _ <- writePipe h (BSL.toStrict $ encode size) -- just a single chunk
        -- send payload
        traverse_ (writePipe h) chunks

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
        bs <- readPipe h s
        when (BS.null bs)
          $ throwIO ReceivedNullBytes
        readLen (bs : bufs) (s - fromIntegral (BS.length bs))
        


      closeChannel = closePipe h

--
-- PingPong Client API
--

data PingPongClient a
    = SendPing !a (a -> IO (PingPongClient a))

constPingPongClient :: a -> PingPongClient a
constPingPongClient !a = SendPing a ((\b -> pure $ constPingPongClient b))

data Blocking = BlockOnRead | BlockOnWrite
  deriving Show

instance Arbitrary Blocking where
    arbitrary = frequency [ (2, pure BlockOnRead)
                          , (1, pure BlockOnWrite)
                          ]

runPingPongClient :: BinaryChannel a
                  -> Blocking
                  -> ThreadId  -- producer thread
                  -> Int       -- kill producer after that many messages
                  -> PingPongClient a
                  -> IO ()
runPingPongClient channel blocking tid = go
    where
      go 0 (SendPing a _) = do
        -- send the message, but report more bytes to the server; this makes
        -- sure that it will blocked on reading when we kill it
        case blocking of
          BlockOnRead -> do
            writeChannel channel False a
            killThread tid
          BlockOnWrite -> do
            writeChannel channel True a
            _ <- readChannel channel False
            killThread tid
      go n (SendPing a k) = do
        writeChannel channel True a
        Just b <- readChannel channel True
        k b >>= go (pred n)


forkPingPongClient :: forall a.
                      Binary a
                   => String
                   -> Blocking
                   -> ThreadId
                   -> Int
                   -> PingPongClient a
                   -> IO ThreadId
forkPingPongClient pname blocking tid n client = do
  channel <-
    handleToBinaryChannel
      <$> createFile pname
                     (gENERIC_READ .|. gENERIC_WRITE)
                     fILE_SHARE_NONE
                     Nothing
                     oPEN_EXISTING
                     fILE_ATTRIBUTE_NORMAL
                     Nothing
  forkIO
    (runPingPongClient channel blocking tid n client
      `finally` closeChannel channel)


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
                   => String
                   -> DWORD  -- input & output buffer size of named pipe
                   -> PingPongServer a
                   -> IO (ThreadId, MVar ())
forkPingPongServer pname bufSize server = do
      var <- newEmptyMVar
      hpipe <- createNamedPipe pname
                               pIPE_ACCESS_DUPLEX
                               (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                               pIPE_UNLIMITED_INSTANCES
                               bufSize
                               bufSize
                               0
                               Nothing
      let channel = handleToBinaryChannel hpipe
      tid <- mask $ \unmask -> forkIO $
          unmask
            (do
              -- the connectNamedPipe call is blocking, but we call it with async exceptions masked
              connectNamedPipe hpipe Nothing
              runPingPongServer channel server)
        `finally` do
            closeChannel channel
            putMVar var ()
      pure (tid, var)


-- | Stress test for named pipe ffi calls.
--
-- For each entry in @NonEmptyList (NonNegative Int)@ we run a single instance
-- of a ping pong protocol which will exchange that many messages.  When the
-- client sends its last message, it misinforms the server about the size of
-- the message, and kills the server thread.  This ensure that when we kill the
-- server it is blocked on reading.
--
prop_PingPong :: NonEmptyList (NonNegative Int, Blocking)
              -- ^ each list element denotes the number of messages exchanged
              -- in the ping pong protocol
              -> LargeNonEmptyBS
              -> Property
prop_PingPong (NonEmpty ns) (LargeNonEmptyBS bs bufSize) =
    ioProperty $ do
      -- fork servers
      tidsAndLocks <-
        traverse
          (\pname ->
            forkPingPongServer
              pname
              (fromIntegral bufSize)
              (constPingPongServer @ByteString))
          pnames

      -- fork clients
      traverse_
        (\(tid, pname, (NonNegative k, blocking)) -> forkPingPongClient pname blocking tid k (constPingPongClient bs))
        (zip3 (map fst tidsAndLocks)
              pnames
              ns)

      -- await until all servers are killed
      traverse_ (\(_, lock) -> takeMVar lock) tidsAndLocks

      pure True
  where
    pnames = map (\x -> "\\\\.\\pipe\\nWin32-named-pipes-test-" ++ show x) [1..(length ns)]

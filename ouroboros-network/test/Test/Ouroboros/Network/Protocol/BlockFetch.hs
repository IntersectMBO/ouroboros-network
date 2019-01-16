{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.BlockFetch where

import qualified Pipes

import           Control.Monad.ST.Lazy (runST)
import           Control.Monad (void)
import           Control.Monad.Free (Free)
import           Data.Functor (($>))
import           Data.Functor.Identity (Identity (..))

import           Protocol.Core (Those (..), connect)

import Control.Monad.Class.MonadFork (MonadFork (..))
import Control.Monad.Class.MonadProbe (MonadProbe (..))
import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Control.Monad.Class.MonadTimer (MonadTimer (..))
import Control.Monad.IOSim (SimF)

import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server
import Ouroboros.Network.Protocol.BlockFetch.Direct

import Ouroboros.Network.Testing.ConcreteBlock (BlockHeader)
import Test.Ouroboros.Network.Testing.Arbitrary
import Test.Ouroboros.Network.Testing.Utils (runExperiment)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testGroup "BlockRequestProtocol"
    [ testProperty "Test directBlockRequest using testing server"
        prop_directBlockRequestProtocol_acc
    , testProperty "Test connect using testing server"
        prop_connectBlockRequestProtocol_acc
    , testProperty "Run blockRequestProtocol_experiment using connect in ST"
        prop_connectBlockRequestProtocol_ST
    , testProperty "Run blockRequestProtocol_experiment using connect in IO"
        prop_connectBlockRequestProtocol_IO
    , testProperty "Run blockRequestProtocol_experiment using directBlockRequest in ST"
        prop_directBlockRequestProtocol_ST
    , testProperty "Run blockRequestProtocol_experiment using directBlockRequest in IO"
        prop_directBlockRequestProtocol_IO
    ]
  , testGroup "BlockFetchProtocol"
    -- These two tests cover the same scope as the @'BlockRequestProtocol'@
    -- tests above, this is because here we can have a receiver that
    -- accumulates all received values.
    [ testProperty "blockFetchServerProtocol_ST"
        prop_blockFetchProtocol_ST
    , testProperty "blockFetchServerProtocol_IO"
        prop_blockFetchProtocol_IO
    ]
  , testGroup "BlockFetchServer: round trip tests"
    [ testProperty "direct: round trip in ST" prop_directRoundTripST
    , testProperty "direct: round trip in IO" prop_directRoundTripIO
    , testProperty "connect: round trip in ST" prop_connectRoundTripST
    , testProperty "connect: round trip in IO" prop_connectRoundTripIO
    ]
  ]

{-------------------------------------------------------------------------------
-- @'BlockRequestProtocol' tests
-------------------------------------------------------------------------------}

-- | Testing server which accumulates received value in its return value.
--
accumulatingBlockRequestReceiver
  :: Monad m
  => BlockRequestReceiver range m [range]
accumulatingBlockRequestReceiver = go []
 where
  go acc =
    BlockRequestReceiver {
      recvMessageRequestRange = \range -> return $ go (range : acc),
      recvMessageDone         = return (reverse acc)
    }

-- | @'directBlockRequest'@ is an identity
--
prop_directBlockRequestProtocol_acc
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_directBlockRequestProtocol_acc as =
  let ranges = map (\(ArbitraryPoint p, ArbitraryPoint p') -> ChainRange p p') as
  in
    fst (runIdentity
      $ directBlockRequest
          accumulatingBlockRequestReceiver
          (blockRequestSenderFromProducer (Pipes.each ranges >> return ())))
    === ranges

-- | Test @'blockRequestReceiverStream'@ against
-- @'blockRequestSenderStream'@ using @'connect'@.
--
prop_connectBlockRequestProtocol_acc
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_connectBlockRequestProtocol_acc as =
  let ranges = map (\(ArbitraryPoint p, ArbitraryPoint p') -> ChainRange p p') as
      client = blockRequestSenderFromProducer (Pipes.each ranges >> return ())
  in case  runIdentity $ connect
            (blockRequestReceiverStream accumulatingBlockRequestReceiver)
            (blockRequestSenderStream client) of
        These res _ -> res === ranges
        This res    -> res === ranges
        That _      -> property False

-- | Test @'constantReceiver'@ against @'blockRequestSenderFromProducer'@ using either
-- @'directBlockRequest'@ or @'connect'@.
--
blockRequestProtocol_experiment
  :: forall m.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     )
  => (forall a b. BlockRequestReceiver (ChainRange BlockHeader) m a -> BlockRequestSender (ChainRange BlockHeader) m b -> m ())
  -- ^ either 'directBlockRequest' or @'connect'@
  -> [(ArbitraryPoint, ArbitraryPoint)]
  -> Probe m Property
  -> m ()
blockRequestProtocol_experiment run as probe = do
  let ranges = map (\(ArbitraryPoint p, ArbitraryPoint p') -> ChainRange p p') as
  var <- atomically $ newTVar []
  let server = constantReceiver (\a -> atomically $ modifyTVar var (a:)) (return ())
      client = blockRequestSenderFromProducer (void $ Pipes.each ranges)

  _ <- run server client

  res <- atomically $ readTVar var
  probeOutput probe $ reverse res === ranges

prop_directBlockRequestProtocol_ST
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_directBlockRequestProtocol_ST as = runST $ runExperiment $
  blockRequestProtocol_experiment @(Free (SimF _))
    (\ser cli -> void $ directBlockRequest ser cli) as

prop_directBlockRequestProtocol_IO
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_directBlockRequestProtocol_IO as = ioProperty $ runExperiment $
  blockRequestProtocol_experiment
    (\ser cli -> void $ directBlockRequest ser cli)  as

prop_connectBlockRequestProtocol_ST
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_connectBlockRequestProtocol_ST as = runST $ runExperiment $
  blockRequestProtocol_experiment @(Free (SimF _))
    (\ser cli -> void $ connect
      (blockRequestReceiverStream ser)
      (blockRequestSenderStream cli)) as

prop_connectBlockRequestProtocol_IO
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_connectBlockRequestProtocol_IO as = ioProperty $ runExperiment $
  blockRequestProtocol_experiment
    (\ser cli -> void $ connect
      (blockRequestReceiverStream ser)
      (blockRequestSenderStream cli)) as

-- | @'BlockFetchReceiver'@ which accumulates received blocks.
--
blockFetchClientReceiver
  :: Applicative m
  => BlockFetchReceiver block m [block]
blockFetchClientReceiver = receiver []
 where
  receiver acc = BlockFetchReceiver {
      recvMsgStartBatch = pure (blockReceiver acc),
      recvMsgNoBlocks   = pure (receiver acc),
      recvMsgDoneClient = acc
    }
  blockReceiver acc = BlockFetchReceiveBlocks {
      recvMsgBlock       = \b -> pure (blockReceiver (b : acc)),
      recvMsgBatchDone   = pure (receiver acc),
      recvMsgServerError = pure (receiver acc)
    }

{-------------------------------------------------------------------------------
-- @'BlockFetchProtocol' tests
-------------------------------------------------------------------------------}

-- | Test @'BlockFetchProtocol'@ using both @'directBlockFetch'@
-- and @'connect\''@.  The test is requesting ranges of integers @(n, m) ::
-- (Int, In)@.  If ranges for which @x > y@ will be treated as there are no
-- corresponding blocks (@'Int'@s), otherwise it will stream the list of all
-- @[x .. y]@.
--
blockFetchProtocol_experiment
  :: forall m.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     )
  => [(Int, Int)]
  -> Probe m Property
  -> m ()
blockFetchProtocol_experiment ranges probe = do
  var  <- atomically $ newTVar (map Element ranges ++ [End])
  var' <- atomically $ newTVar (map Element ranges ++ [End])

  let sender = blockFetchServerSender () (readRequest var) blockStream
  (_, resDirect) <- directBlockFetch sender blockFetchClientReceiver

  let sender' = blockFetchServerSender () (readRequest var') blockStream
  resConn <- connect
    (blockFetchServerStream sender')
    (blockFetchReceiverStream blockFetchClientReceiver)

  let res = reverse $ concatMap (\(x, y) -> [x..y]) ranges
  case resConn of
    This _       -> probeOutput probe $ property False
    That res'    -> probeOutput probe $ res' === res .&&. resDirect === res
    These _ res' -> probeOutput probe $ res' === res .&&. resDirect === res

 where
  blockStream (x, y) = return (Just (Pipes.each [x..y] >> return ()))

  readRequest v = atomically $ do
    rs <- readTVar v
    case rs of
      []        -> retry
      (r : rs') -> writeTVar v rs' $> r

prop_blockFetchProtocol_ST
  :: NonEmptyList (Int, Int)
  -> Property
prop_blockFetchProtocol_ST (NonEmpty as) = runST $ runExperiment $
  blockFetchProtocol_experiment as

prop_blockFetchProtocol_IO
  :: NonEmptyList (Int, Int)
  -> Property
prop_blockFetchProtocol_IO (NonEmpty as) = ioProperty $ runExperiment $
  blockFetchProtocol_experiment as

{-------------------------------------------------------------------------------
-- Round trip tests
-------------------------------------------------------------------------------}

-- | Round trip test in either @'IO'@ or in @Free ('SimF' s)@ monad.  It
-- assures that there can be multiple outstanding range requests, and that the
-- server returns the exepcted ranges.
--
roundTrip_experiment
  :: forall m.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     )
  => (forall a b. BlockRequestReceiver (Maybe (Int, Int)) m a
        -> BlockRequestSender (Maybe (Int, Int)) m b
        -> m ())
        -- ^ run @'BlockRequestProtocol'@
  -> (forall a. BlockFetchServerSender Int m a
        -> BlockFetchReceiver Int m [Int]
        -> m (Maybe [Int]))
        -- ^ run @'BlockFetchProtocol'@
  -> [(Int, Int)] -- ^ ranges to send
  -> Positive Int -- ^ size of queue connecting both servers
  -> Probe m Property
  -> m ()
roundTrip_experiment runClient runServer ranges (Positive queueSize) probe = do
  (serverReceiver, serverSender) <- connectThroughQueue (fromIntegral queueSize) blockStream
  let clientSender = blockRequestSenderFromProducer
        (Pipes.each (map Just ranges ++ [Nothing]) >> return ())
  fork $ runClient serverReceiver clientSender
  fork $ do
    res <- runServer serverSender blockFetchClientReceiver
    let expected = concatMap (\(x, y) -> [x..y]) ranges
    probeOutput probe ((reverse <$> res) === Just expected)
 where
  blockStream :: Maybe (Int, Int) -> m (Maybe (Pipes.Producer Int m ()))
  blockStream (Just (x, y)) = return (Just (Pipes.each [x..y] >> return ()))
  blockStream Nothing       = return Nothing

prop_directRoundTripST
  :: [(Int, Int)]
  -> Positive Int
  -> Property
prop_directRoundTripST ranges queueSize = runST $ runExperiment $
  roundTrip_experiment @(Free (SimF _))
    (\ser cli -> void $ directBlockRequest ser cli)
    (\ser cli -> Just . snd <$> directBlockFetch ser cli)
    ranges
    queueSize

prop_directRoundTripIO
  :: [(Int, Int)]
  -> Positive Int
  -> Property
prop_directRoundTripIO ranges queueSize = ioProperty $ runExperiment $
  roundTrip_experiment
    (\ser cli -> void $ directBlockRequest ser cli)
    (\ser cli -> Just . snd <$> directBlockFetch ser cli)
    ranges
    queueSize

that :: Those a b -> Maybe b
that (These _ b) = Just b
that (That b)    = Just b
that (This _)    = Nothing

prop_connectRoundTripST
  :: [(Int, Int)]
  -> Positive Int
  -> Property
prop_connectRoundTripST ranges queueSize = runST $ runExperiment $
  roundTrip_experiment
    (\ser cli -> void $ connect
      (blockRequestReceiverStream ser)
      (blockRequestSenderStream cli))
    (\ser cli -> that <$> connect
      (blockFetchServerStream ser)
      (blockFetchReceiverStream cli))
    ranges
    queueSize

prop_connectRoundTripIO
  :: [(Int, Int)]
  -> Positive Int
  -> Property
prop_connectRoundTripIO ranges queueSize = ioProperty $ runExperiment $
  roundTrip_experiment
    (\ser cli -> void $ connect
      (blockRequestReceiverStream ser)
      (blockRequestSenderStream cli))
    (\ser cli -> that <$> connect
      (blockFetchServerStream ser)
      (blockFetchReceiverStream cli))
    ranges
    queueSize

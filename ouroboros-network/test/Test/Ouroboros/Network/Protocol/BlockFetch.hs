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
import           Data.List (foldl')

import           Protocol.Core (Those (..), connect)

import Control.Monad.Class.MonadFork (MonadFork (..))
import Control.Monad.Class.MonadProbe (MonadProbe (..), MonadRunProbe (..), withProbe)
import Control.Monad.Class.MonadSTM (MonadSTM (..))
import Control.Monad.Class.MonadTimer (MonadTime (..), MonadTimer (..))
import Control.Monad.IOSim (SimF)

import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server
import Ouroboros.Network.Protocol.BlockFetch.Direct

import Ouroboros.Network.Testing.ConcreteBlock (BlockHeader)
import Test.Ouroboros.Network.Testing.Arbitrary

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testGroup "BlockFetchClientProtocol"
    [ testProperty "Test directBlockFetchClient using testing server"
        prop_directBlockFetchClientProtocol_acc
    , testProperty "Test connect using testing server"
        prop_connectBlockFetchClientProtocol_acc
    , testProperty "Run blockFetchClientProtocol_experiment using connect in ST"
        prop_connectBlockFetchClientProtocol_ST
    , testProperty "Run blockFetchClientProtocol_experiment using connect in IO"
        prop_connectBlockFetchClientProtocol_IO
    , testProperty "Run blockFetchClientProtocol_experiment using directBlockFetchClient in ST"
        prop_directBlockFetchClientProtocol_ST
    , testProperty "Run blockFetchClientProtocol_experiment using directBlockFetchClient in IO"
        prop_directBlockFetchClientProtocol_IO
    ]
  , testGroup "BlockFetchServerProtocol"
    -- These two tests cover the same scope as the @'BlockFetchClientProtocol'@
    -- tests above, this is because here we can have a receiver that
    -- accumulates all received values.
    [ testProperty "blockFetchServerProtocol_ST"
        prop_blockFetchServerProtocol_ST
    , testProperty "blockFetchServerProtocol_IO"
        prop_blockFetchServerProtocol_IO
    ]
  , testGroup "BlockFetchServer: round trip tests"
    [ testProperty "direct: round trip in ST" prop_directRoundTripST
    , testProperty "direct: round trip in IO" prop_directRoundTripIO
    , testProperty "connect: round trip in ST" prop_connectRoundTripST
    , testProperty "connect: round trip in IO" prop_connectRoundTripIO
    ]
  ]

-- | FIXME: find a better place for it, this might be useful elsewhere too.
runExperiment
  :: forall m n.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     , MonadRunProbe m n
     )
  => (Probe m Property -> m ())
  -> n Property
runExperiment exp_ = isValid <$> withProbe exp_
 where
  isValid :: [(Time m, Property)] -> Property
  isValid = foldl' (\acu (_,p) -> acu .&&. p) (property True)

{-------------------------------------------------------------------------------
-- @'BlockFetchClientProtocol' tests
-------------------------------------------------------------------------------}

-- | Testing server which accumulates received value in its return value.
--
accumulatingBlockFetchServerReceiver
  :: Monad m
  => BlockFetchServerReceiver range m [range]
accumulatingBlockFetchServerReceiver = go []
 where
  go acc =
    BlockFetchServerReceiver {
      recvMessageRequestRange = \range -> return $ go (range : acc),
      recvMessageDone         = return (reverse acc)
    }

-- | @'directBlockFetchClient'@ is an identity
--
prop_directBlockFetchClientProtocol_acc
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_directBlockFetchClientProtocol_acc as =
  let ranges = map (\(ArbitraryPoint p, ArbitraryPoint p') -> ChainRange p p') as
  in
    fst (runIdentity
      $ directBlockFetchClient
          accumulatingBlockFetchServerReceiver
          (blockFetchClientSenderFromProducer (Pipes.each ranges >> return ())))
    === ranges

-- | Test @'blockFetchServerReceiverStream'@ against
-- @'blockFetchClientSenderStream'@ using @'connect'@.
--
prop_connectBlockFetchClientProtocol_acc
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_connectBlockFetchClientProtocol_acc as =
  let ranges = map (\(ArbitraryPoint p, ArbitraryPoint p') -> ChainRange p p') as
      client = blockFetchClientSenderFromProducer (Pipes.each ranges >> return ())
  in case  runIdentity $ connect
            (blockFetchServerReceiverStream accumulatingBlockFetchServerReceiver)
            (blockFetchClientSenderStream client) of
        These res _ -> res === ranges
        This res    -> res === ranges
        That _      -> property False

-- | Test @'constantReceiver'@ against @'blockFetchClientSenderFromProducer'@ using either
-- @'directBlockFetchClient'@ or @'connect'@.
--
blockFetchClientProtocol_experiment
  :: forall m.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     )
  => (forall a b. BlockFetchServerReceiver (ChainRange BlockHeader) m a -> BlockFetchClientSender (ChainRange BlockHeader) m b -> m ())
  -- ^ either 'directBlockFetchClient' or @'connect'@
  -> [(ArbitraryPoint, ArbitraryPoint)]
  -> Probe m Property
  -> m ()
blockFetchClientProtocol_experiment run as probe = do
  let ranges = map (\(ArbitraryPoint p, ArbitraryPoint p') -> ChainRange p p') as
  var <- atomically $ newTVar []
  let server = constantReceiver (\a -> atomically $ modifyTVar var (a:)) (return ())
      client = blockFetchClientSenderFromProducer (void $ Pipes.each ranges)

  _ <- run server client

  res <- atomically $ readTVar var
  probeOutput probe $ reverse res === ranges

prop_directBlockFetchClientProtocol_ST
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_directBlockFetchClientProtocol_ST as = runST $ runExperiment $
  blockFetchClientProtocol_experiment @(Free (SimF _))
    (\ser cli -> void $ directBlockFetchClient ser cli) as

prop_directBlockFetchClientProtocol_IO
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_directBlockFetchClientProtocol_IO as = ioProperty $ runExperiment $
  blockFetchClientProtocol_experiment
    (\ser cli -> void $ directBlockFetchClient ser cli)  as

prop_connectBlockFetchClientProtocol_ST
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_connectBlockFetchClientProtocol_ST as = runST $ runExperiment $
  blockFetchClientProtocol_experiment @(Free (SimF _))
    (\ser cli -> void $ connect
      (blockFetchServerReceiverStream ser)
      (blockFetchClientSenderStream cli)) as

prop_connectBlockFetchClientProtocol_IO
  :: [(ArbitraryPoint, ArbitraryPoint)]
  -> Property
prop_connectBlockFetchClientProtocol_IO as = ioProperty $ runExperiment $
  blockFetchClientProtocol_experiment
    (\ser cli -> void $ connect
      (blockFetchServerReceiverStream ser)
      (blockFetchClientSenderStream cli)) as

-- | @'BlockFetchClientReceiver'@ which accumulates received blocks.
--
blockFetchClientReceiver
  :: Applicative m
  => BlockFetchClientReceiver block m [block]
blockFetchClientReceiver = receiver []
 where
  receiver acc = BlockFetchClientReceiver {
      recvMsgStartBatch = pure (blockReceiver acc),
      recvMsgNoBlocks   = pure (receiver acc),
      recvMsgDoneClient = acc
    }
  blockReceiver acc = BlockFetchClientReceiveBlocks {
      recvMsgBlock       = \b -> pure (blockReceiver (b : acc)),
      recvMsgBatchDone   = pure (receiver acc),
      recvMsgServerError = pure (receiver acc)
    }

{-------------------------------------------------------------------------------
-- @'BlockFetchServerProtocol' tests
-------------------------------------------------------------------------------}

-- | Test @'BlockFetchServerProtocol'@ using both @'directBlockFetchServer'@
-- and @'connect\''@.  The test is requesting ranges of integers @(n, m) ::
-- (Int, In)@.  If ranges for which @x > y@ will be treated as there are no
-- corresponding blocks (@'Int'@s), otherwise it will stream the list of all
-- @[x .. y]@.
--
blockFetchServerProtocol_experiment
  :: forall m.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     )
  => [(Int, Int)]
  -> Probe m Property
  -> m ()
blockFetchServerProtocol_experiment ranges probe = do
  var  <- atomically $ newTVar (map Element ranges ++ [End])
  var' <- atomically $ newTVar (map Element ranges ++ [End])

  let sender = blockFetchServerSender () (readRequest var) blockStream
  (_, resDirect) <- directBlockFetchServer sender blockFetchClientReceiver

  let sender' = blockFetchServerSender () (readRequest var') blockStream
  resConn <- connect
    (blockFetchServerStream sender')
    (blockFetchClientReceiverStream blockFetchClientReceiver)

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

prop_blockFetchServerProtocol_ST
  :: NonEmptyList (Int, Int)
  -> Property
prop_blockFetchServerProtocol_ST (NonEmpty as) = runST $ runExperiment $
  blockFetchServerProtocol_experiment as

prop_blockFetchServerProtocol_IO
  :: NonEmptyList (Int, Int)
  -> Property
prop_blockFetchServerProtocol_IO (NonEmpty as) = ioProperty $ runExperiment $
  blockFetchServerProtocol_experiment as

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
  => (forall a b. BlockFetchServerReceiver (Maybe (Int, Int)) m a
        -> BlockFetchClientSender (Maybe (Int, Int)) m b
        -> m ())
        -- ^ run @'BlockFetchClientProtocol'@
  -> (forall a. BlockFetchServerSender Int m a
        -> BlockFetchClientReceiver Int m [Int]
        -> m (Maybe [Int]))
        -- ^ run @'BlockFetchServerProtocol'@
  -> [(Int, Int)] -- ^ ranges to send
  -> Positive Int -- ^ size of queue connecting both servers
  -> Probe m Property
  -> m ()
roundTrip_experiment runClient runServer ranges (Positive queueSize) probe = do
  (serverReceiver, serverSender) <- connectThroughQueue (fromIntegral queueSize) blockStream
  let clientSender = blockFetchClientSenderFromProducer
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
    (\ser cli -> void $ directBlockFetchClient ser cli)
    (\ser cli -> (Just . snd) <$> directBlockFetchServer ser cli)
    ranges
    queueSize

prop_directRoundTripIO
  :: [(Int, Int)]
  -> Positive Int
  -> Property
prop_directRoundTripIO ranges queueSize = ioProperty $ runExperiment $
  roundTrip_experiment
    (\ser cli -> void $ directBlockFetchClient ser cli)
    (\ser cli -> (Just . snd) <$> directBlockFetchServer ser cli)
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
      (blockFetchServerReceiverStream ser)
      (blockFetchClientSenderStream cli))
    (\ser cli -> that <$> connect
      (blockFetchServerStream ser)
      (blockFetchClientReceiverStream cli))
    ranges
    queueSize

prop_connectRoundTripIO
  :: [(Int, Int)]
  -> Positive Int
  -> Property
prop_connectRoundTripIO ranges queueSize = ioProperty $ runExperiment $
  roundTrip_experiment
    (\ser cli -> void $ connect
      (blockFetchServerReceiverStream ser)
      (blockFetchClientSenderStream cli))
    (\ser cli -> that <$> connect
      (blockFetchServerStream ser)
      (blockFetchClientReceiverStream cli))
    ranges
    queueSize

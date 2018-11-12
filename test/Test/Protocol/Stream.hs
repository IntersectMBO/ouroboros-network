{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Protocol.Stream
  ( tests
  ) where

import           Control.Monad.Free (Free)
import           Control.Monad.ST.Lazy (runST)
import           Numeric.Natural (Natural)
import qualified Streaming.Prelude as S
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Protocol.Core (Those(..), connect')
import           Ouroboros.Network.Protocol.Stream.Client
import           Ouroboros.Network.Protocol.Stream.Server
import           Ouroboros.Network.Sim (SimF)
import           Ouroboros.Network.MonadClass

tests :: TestTree
tests = testGroup "Protocol.Stream"
  [ testProperty "stream ST" (prop_stream_ST @Int)
  , testProperty "stream IO" (prop_stream_IO @Int)
  ]

stream_experiment
  :: ( MonadSTM m
     , MonadTBQueue m
     , MonadProbe m
     , Eq a
     , Show a
     ) 
  => [a]
  -> Natural
  -> Probe m Property
  -> m ()
stream_experiment as window p = do
  server <- newServer (const $ S.unfoldr go as)
  (client, queue) <- atomically $ newClientSTM window
  fork $ do
    r <- connect' (streamClient client (error "impossible happend: const is strict")) (streamServer server)
    case r of
      These{}   -> probeOutput p (property True)
      That _    -> probeOutput p (counterexample "client has not finished" False)
      This _    -> probeOutput p (counterexample "server has not finished" False)
  as' <- consumeQueue queue
  probeOutput p ((reverse as') === as)
 where
  consumeQueue queue = fn id
   where
    fn k = do
      s <- atomically $ readTBQueue queue
      case s of
        StreamElement a -> fn ((a :) . k)
        EndOfStream     -> return (k [])

  go [] = return $ Left ()
  go (x : xs) = return $ Right (x, xs)

test_stream
  :: forall m n a.
    ( Eq a
    , Show a
    , MonadSTM m
    , MonadTBQueue m
    , MonadProbe m
    , MonadRunProbe m n
    )
  => [a]
  -> Natural
  -> n Property
test_stream as window = isValid <$> withProbe (stream_experiment as window)
 where
  isValid :: [(Time m, Property)] -> Property
  isValid [] = counterexample "probe empty" False
  isValid xs = isValid' xs

  isValid' :: [(Time m, Property)] -> Property
  isValid' ((_, x) : xs) = x .&&. isValid' xs
  isValid' []            = property True

prop_stream_ST
  :: forall a.
    ( Arbitrary a
    , Eq a
    , Show a
    )
  => [a]
  -> Positive Int
  -> Property
prop_stream_ST as (Positive window) = runST (test_stream @(Free (SimF _)) as (fromIntegral window))

prop_stream_IO
  :: forall a.
     ( Arbitrary a
     , Eq a
     , Show a
     )
  => [a]
  -> Positive Int
  -> Property
prop_stream_IO as (Positive window) = ioProperty $ test_stream as (fromIntegral window)

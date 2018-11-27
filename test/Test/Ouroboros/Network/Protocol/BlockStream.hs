{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.BlockStream where

import           Control.Monad (void)
import           Data.Word (Word32)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Foldable (traverse_)
import           Pipes (yield)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Protocol.Channel
import           Protocol.Transition (SomeTransition)

import Ouroboros.Network.Protocol.BlockStream.Client
import Ouroboros.Network.Protocol.Stream.Server
import Ouroboros.Network.Protocol.Stream.Type
import Ouroboros.Network.MonadClass (MonadFork (..), MonadSTM (..))

import Test.Ouroboros.Network.Protocol.Stream (Window (..), readQueue)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockStream"
  [ testProperty "blockStream" $ test_blockStream
  -- TODO: include a test for @'runBlockStreamClientConstant'@ over a channel
  -- with framing
  ]

blockStreamServer :: Monad m => StreamServer m (Int, Int) (Int, Int) [(Int, Int)]
blockStreamServer = streamServer (go [])
 where
  go acc (x, y) | x >= y    = yield (x, x) >> return ((x, x):acc)
                | otherwise = yield (x, x) >> go ((x, x):acc) (succ x, y)

data BlockPoints = BlockPoints (NonEmpty (Int, Int)) Threshold Window
  deriving Show

instance Arbitrary BlockPoints where
  arbitrary = do
    NonEmpty points <- fmap (\(x, y) -> if x <= y then (x, y) else (y, x)) <$> (arbitrary @(NonEmptyList (Int, Int)))
    let maxThreshold :: Word32
        maxThreshold = max 1 (fromIntegral $ minimum ((\(x, y) -> y - x) `map` points))
    threshold <- choose (1, maxThreshold)
    window    <- fromIntegral <$> choose (threshold, 2 * threshold)
    return $ BlockPoints (NE.fromList points) (Threshold threshold) (Window window)

test_blockStream
  :: BlockPoints
  -> Property
test_blockStream (BlockPoints points threshold (Window window)) = ioProperty $ withChannels points k
 where
  k :: NonEmpty (Channel IO (SomeTransition (StreamMessage (Int, Int) (Int, Int))))
    -> NonEmpty (Channel IO (SomeTransition (StreamMessage (Int, Int) (Int, Int))))
    -> IO Property 
  k ccs scs = do
    traverse_ (\sc -> fork $ void $ useChannelHomogeneous sc (streamServerPeer blockStreamServer)) scs
    queue <- atomically $ newTBQueue window
    let pointsWithChannels = points `NE.zip` ccs
    fork $ runBlockStreamClient queue pointsWithChannels threshold
    results <- readQueue queue
    let expected = concatMap (\(x,y) -> zip [x..y] [x..y]) $ NE.toList points
    -- reverse results, since the first received result is at the bottom of the
    -- list
    return $ reverse results === expected

  withChannels (_ :| ranges) fn = withMVarChannels $ \cc sc -> do
    go (cc :| []) (sc :| []) ranges
   where
    go ccs scs []           = fn ccs scs
    go ccs scs (_ : ranges') = 
      withMVarChannels $ \cc sc -> go (cc `NE.cons` ccs) (sc `NE.cons` scs) ranges'




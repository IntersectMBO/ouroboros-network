{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Ouroboros.Network.Protocol.BlockStream.Client
  ( Threshold (..)
  , runBlockStreamClient
  , runBlockStreamClientConstant
  ) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32)
import Protocol.Channel (Channel, useChannelHomogeneous)
import Protocol.Transition (SomeTransition)

import Ouroboros.Network.Protocol.Stream.Type
import Ouroboros.Network.Protocol.Stream.Client
import Ouroboros.Network.MonadClass (MonadSTM (..), fork)

newtype Threshold = Threshold { runThreshold :: Word32 }
  deriving (Eq, Show, Ord, Enum, Num)

deriving instance Real Threshold
deriving instance Integral Threshold

-- | A simple STM based lock.
--
data Lock m = Lock
  { lockAwait   :: Tr m () -- ^ block until lock is released
  , lockRelease :: Tr m () -- ^ release lock
  }

-- | @'Lock'@ smart constructor.
--
newLock :: MonadSTM m => m (Lock m)
newLock = do
  var <- atomically $ newTVar False
  return Lock
    { lockAwait   = lockAwait var
    , lockRelease = lockRelease var
    }
 where
  lockAwait var =
      readTVar var >>= \case
        True  -> return ()
        False -> retry
  lockRelease var = writeTVar var True

-- | @'StreamClient'@ which fetches a list of ranges of blocks.
-- @'blockStreamClient'@ runs a request for each range in the input list of
-- ranges.  Each request is spanned after @'Threshold'@ responses are received
-- (to the current request).  It also takes care that responses from each
-- requests are written in the right order to the queue, and only the final
-- request writes @'EndOfStream'@ when it recieves @'MsgStreamEnd'@.
--
blockStreamClient
  :: forall m point body.  MonadSTM m
  => Int
  -> TBQueue m (StreamElement (point, body))
  -> Maybe (Lock m)
  -> (point, point)
  -> [((point, point), Channel m (SomeTransition (StreamMessage (point, point) (point, body))))]
  -- ^ Non empty list of continous range of block bodies (and channels).  The
  -- server is supposed to respond including the range boundaries.
  -> Threshold
  -- ^ after receiving than many messages, request next range of bodies.
  -- @'Threshold'@ has to be smaller than the minimal number of blocks is
  -- a given requests (otherwise subsequent requests will never be sent).
  -> StreamClient m (point, point) (point, body) ()
blockStreamClient clientId queue lock range ranges threshold = SendMsgRequest range (client 1 Nothing)
 where
  client
    :: Word32
    -> Maybe (Lock m) -- ^ lock for a next request
    -> ClientHandleData m (point, body) ()
  client count nextLock = ClientHandleData
    { recvMsgData = \a -> do
          -- This might block in two scenarios
          --   * lock is not released
          --   * queue received too many writes
          atomically $ do
            traverse_ lockAwait lock
            writeTBQueue queue (StreamElement a)
          nextLock' <- if count >= runThreshold threshold
            then nextRequest nextLock ranges
            else return      nextLock
          return $ client (succ count) nextLock'

      , recvMsgStreamEnd =
          case nextLock of
            -- next lock was never assigned, i.e. this is the last request, write
            -- @'EndOfStream'@.
            Nothing -> do
              atomically $ writeTBQueue queue EndOfStream
            -- next lock was assigned: when we finish writting responses to this
            -- request, release the next lock.
            Just lock' -> do
              atomically $ lockRelease lock'
    }

  -- run next request if there is any, also returns the lock that needs to be
  -- released when the current client is finishes receiving data.
  nextRequest
    :: Maybe (Lock m)
    -> [((point, point), Channel m (SomeTransition (StreamMessage (point, point) (point, body))))]
    -> m (Maybe (Lock m))
  nextRequest Nothing (range' : ranges') = do
      nextLock' <- newLock
      fork $ runBlockStreamClient' (succ clientId) (Just nextLock') queue (range' :| ranges') threshold
      return (Just nextLock')
  nextRequest nextLock _  = return nextLock

runBlockStreamClient'
  :: MonadSTM m
  => Int
  -> Maybe (Lock m)
  -> TBQueue m (StreamElement (point, body))
  -> NonEmpty ((point, point), Channel m (SomeTransition (StreamMessage (point, point) (point, body))))
  -> Threshold
  -> m ()
runBlockStreamClient' clientId lock queue ((range, channel) :| ranges) threshold =
  void $ useChannelHomogeneous channel (streamClientPeer $ blockStreamClient clientId queue lock range ranges threshold)

-- | Interface to run block stream client.  For each range run a new request
-- using the @'StreamProtocol'@ protocol.  Each new request is run after
-- @'Threshold'@ of responses of the current request were collected.  The
-- constructed @'StreamClient'@ takes care that all the responses are written
-- in the right order.
--
-- Below x represents a block that the server has, - a missing block,
-- parenthesis represent the split into ranges:
--
--  (x x x x) (x x - -) (- - - -) (- - - -)
--
-- If the @'Threshold'@ is greater than 2, the second request will receive
-- only two responses which will finish the conversation (third request will
-- not be sent).  If the @'Threshold'@ is 2 (i.e. a next request is sent after
-- recieving two responses, the client will send three requests, and it might
-- receive blocks from the third range, but the fourth request will not be sent.
--
-- Hence there is no guarantee that a server, which follows this protocol will
-- respond with a contiguous range of blocks.
--
-- If @'Threshold'@ is larger than a number of blocks in one of the ranges, the
-- cunstructed @'StreamClient'@ will never request ranges past that particular
-- range. It's up to the caller to assure that the @'Threshold'@ is small
-- enought.
--
runBlockStreamClient
  :: MonadSTM m
  => TBQueue m (StreamElement (point, body))
  -- ^ queue which accumulates responses
  -> NonEmpty ((point, point), Channel m (SomeTransition (StreamMessage (point, point) (point, body))))
  -- ^ non empty list of ranges, together witch a channel over which each range
  -- will be requested.
  -> Threshold
  -- ^ threshold after which next request is run
  -> m ()
runBlockStreamClient = runBlockStreamClient' 0 Nothing

-- | A version of @'runBlockStreamClient'@ which runs on a single channel.  This
-- requires that the channel handles framing, since it will be reused for all
-- requests requests.
--
runBlockStreamClientConstant
  :: ( MonadSTM m
     )
  => TBQueue m (StreamElement (point, body))
  -> NonEmpty (point, point)
  -> Threshold
  -> Channel m (SomeTransition (StreamMessage (point, point) (point, body)))
  -> m ()
runBlockStreamClientConstant queue points threshold channel
  = runBlockStreamClient queue ((,channel) <$> points) threshold

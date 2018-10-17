{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.Sim.Consumer where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Protocol.Chain.StreamConsumer
import Protocol.Chain.Type

import Block
import Chain
import MonadClass.MonadSTM

-- | Drop the suffix of the chain beginning at and not including the 'Point'.
dropAfter :: Point -> Seq Block -> Seq Block
dropAfter point chain = case Seq.viewr chain of
  Seq.EmptyR  -> mempty
  bs Seq.:> b ->
    if blockPoint b == point
    then chain
    else dropAfter point bs

extendWith :: Block -> Seq Block -> Seq Block
extendWith block chain = chain Seq.|> block

-- | A 'ConsumerStream' which naively downloads the whole chain, never
-- attempting to improve the read pointer, and puts all of the blocks into a
-- 'TVar' forming one chain (tines abandoned by a fork are released).
--
-- It will continue to request blocks until it gets one which has the same hash
-- as the read pointer.
simpleConsumerStream
  :: forall m stm x .
     ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> ConsumerStream m x
simpleConsumerStream chainVar = ConsumerStream $ \readPointer header -> atomically $ do
  modifyTVar' chainVar (dropAfter readPointer)
  pure (simpleConsumerStreamStep chainVar readPointer header)

simpleConsumerStreamStep
  :: forall m stm x .
     ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> Point  -- ^ Read pointer
  -> Header -- ^ Tip
  -> ConsumerStreamStep m x
simpleConsumerStreamStep chainVar readPointer tip =
  -- If we're at the tip we'll just wait.
  --
  -- Alternatively, we could DownloadBlocks, but that would mean we would not
  -- get the fast relay. The producer would respond when it either has the next
  -- (complete) block, or has changed to a new chain.
  --
  -- It's important to point out that this is the way in which the consumer can
  -- opt to receive a fast relay from only particular peers. It can send the
  -- "next tip" request to those from whom it wishes to receive fast relay, and
  -- no request to the others.
  if pointHash readPointer == headerHash tip
  then NextTip (simpleConsumerStream chainVar) (simpleConsumerNext chainVar readPointer tip)
  else DownloadBlocks maxBound (simpleConsumerDownload chainVar readPointer tip)

simpleConsumerDownload
  :: forall m stm x .
     ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> Point
  -> Header
  -> ConsumerDownload m x
simpleConsumerDownload chainVar readPointer tip = ConsumerDownload
  { downloadBlock       = \block mNewTip -> do
      atomically $ modifyTVar' chainVar (extendWith block)
      let readPointer' = blockPoint block
      case mNewTip of
        Nothing   -> pure $ simpleConsumerDownload chainVar readPointer' tip
        Just tip' -> pure $ simpleConsumerDownload chainVar readPointer' tip'
  , downloadOver        = \mNewTip -> case mNewTip of
      Nothing   -> pure $ simpleConsumerStreamStep chainVar readPointer tip
      Just tip' -> pure $ simpleConsumerStreamStep chainVar readPointer tip'
  , downloadInterrupted = simpleConsumerStream chainVar
  }

simpleConsumerNext
  :: forall m stm x .
     ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> Point
  -> Header
  -> ConsumerNext m x
simpleConsumerNext chainVar readPointer tip = ConsumerNext
  { -- We now have a new tip, but the read pointer remains the same.
    headerNoRelay = \header -> pure (simpleConsumerStreamStep chainVar readPointer header)
  , headerRelay   = \header -> pure (simpleConsumerRelay chainVar readPointer header)
  }

simpleConsumerRelay
  :: forall m stm x .
     ( MonadSTM m stm )
  => TVar m (Seq Block)
  -> Point
  -> Header
  -> ConsumerRelay m x
simpleConsumerRelay chainVar readPointer header = ConsumerRelay
  { bodyRelay = \body -> do
      let !block = Block header body
          readPointer' = blockPoint block
      atomically $ modifyTVar' chainVar (extendWith (Block header body))
      pure $ simpleConsumerStreamStep chainVar readPointer' header
  , headerNewRelay = \header' -> pure (simpleConsumerRelay chainVar readPointer header')
  , headerNew      = \header' -> pure (simpleConsumerStreamStep chainVar readPointer header')
  , abandonRelay   = simpleConsumerStream chainVar
  }

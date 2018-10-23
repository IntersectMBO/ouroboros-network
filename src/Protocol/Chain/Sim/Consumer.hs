{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.Sim.Consumer where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Protocol.Chain.StreamConsumer

import MonadClass.MonadSTM

-- | Drop the suffix of the chain beginning at and not including the 'Point'.
dropAfter
  :: (header -> point)
  -> (point -> point -> Bool) -- ^ Point equality
  -> point
  -> Seq header
  -> Seq header
dropAfter mkPoint eqPoint point chain = case Seq.viewr chain of
  Seq.EmptyR  -> mempty
  bs Seq.:> b ->
    if mkPoint b `eqPoint` point
    then chain
    else dropAfter mkPoint eqPoint point bs

extendWith :: header -> Seq header -> Seq header
extendWith block chain = chain Seq.|> block

-- | A 'ConsumerStream' which naively downloads the whole chain, never
-- attempting to improve the read pointer, and puts all of the blocks into a
-- 'TVar' forming one chain (tines abandoned by a fork are released).
--
-- It will continue to request blocks until it gets one which has the same hash
-- as the read pointer.
simpleConsumerStream
  :: forall point header m stm .
     ( MonadSTM m stm )
  => (header -> point)
  -> (point -> point -> Bool)
  -- FIXME FIXME use a blockchain representation which is never empty! The
  -- consumer invariant is that the end of the chain in this 'TVar' is the
  -- read pointer, which breaks down if there is no end of the chain because
  -- it's empty.
  -> TVar m (Seq header)
  -> ConsumerStream point header m ()
simpleConsumerStream mkPoint eqPoint chainVar = ConsumerStream $ \readPointer header -> atomically $ do
  modifyTVar' chainVar (dropAfter mkPoint eqPoint readPointer)
  pure (simpleConsumerStreamStep mkPoint eqPoint chainVar readPointer header)

simpleConsumerStreamStep
  :: forall point header m stm .
     ( MonadSTM m stm )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> point  -- ^ Read pointer
  -> header -- ^ Tip
  -> ConsumerStreamStep point header m ()
simpleConsumerStreamStep mkPoint eqPoint chainVar readPointer tip =
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
  if readPointer `eqPoint` mkPoint tip
  then NextTip (simpleConsumerStream mkPoint eqPoint chainVar) (simpleConsumerNext mkPoint eqPoint chainVar readPointer) ()
  else DownloadHeaders maxBound (simpleConsumerDownload mkPoint eqPoint chainVar readPointer tip)

simpleConsumerDownload
  :: forall point header m stm .
     ( MonadSTM m stm )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> point
  -> header
  -> ConsumerDownload point header m ()
simpleConsumerDownload mkPoint eqPoint chainVar readPointer tip = ConsumerDownload
  { downloadHeader     = \header mNewTip -> do
      atomically $ modifyTVar' chainVar (extendWith header)
      let readPointer' = mkPoint header
      case mNewTip of
        Nothing   -> pure $ simpleConsumerDownload mkPoint eqPoint chainVar readPointer' tip
        Just tip' -> pure $ simpleConsumerDownload mkPoint eqPoint chainVar readPointer' tip'
  , downloadOver        = \mNewTip -> case mNewTip of
      Nothing   -> pure $ simpleConsumerStreamStep mkPoint eqPoint chainVar readPointer tip
      Just tip' -> pure $ simpleConsumerStreamStep mkPoint eqPoint chainVar readPointer tip'
  , downloadInterrupted = simpleConsumerStream mkPoint eqPoint chainVar
  }

simpleConsumerNext
  :: forall point header m stm .
     ( MonadSTM m stm )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> point
  -> ConsumerNext point header m ()
simpleConsumerNext mkPoint eqPoint chainVar readPointer = ConsumerNext
  { -- We now have a new tip, but the read pointer remains the same.
    runConsumerNext = \header -> pure (simpleConsumerStreamStep mkPoint eqPoint chainVar readPointer header)
  }

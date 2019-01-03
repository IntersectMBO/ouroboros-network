{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.Chain.Consumer where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.Class.MonadSTM

import Ouroboros.Network.Protocol.Chain.ConsumerStream


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
  :: forall point header m .
     ( MonadSTM m )
  => (header -> point)
  -> (point -> point -> Bool)
  -- FIXME FIXME use a blockchain representation which is never empty! The
  -- consumer invariant is that the end of the chain in this 'TVar' is the
  -- read pointer, which breaks down if there is no end of the chain because
  -- it's empty.
  -> TVar m (Seq header)
  -> ConsumerStream point header m ()
simpleConsumerStream mkPoint eqPoint chainVar = ConsumerStream $
  -- It's the same as a fork: set the new read pointer and tip, then go
  -- to 'simpleConsumerChoice'.
  simpleConsumerForked mkPoint eqPoint chainVar

simpleConsumerForked
  :: forall point header m .
     ( MonadSTM m )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> (point, point) -- ^ Read pointer, followed by tip
  -> m (ConsumerChoice point header m ())
simpleConsumerForked mkPoint eqPoint chainVar points = atomically $ do
  modifyTVar' chainVar (dropAfter mkPoint eqPoint (fst points))
  pure (simpleConsumerChoice mkPoint eqPoint chainVar points)

simpleConsumerChoice
  :: forall point header m .
     ( MonadSTM m )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> (point, point) -- ^ Read pointer, followed by tip
  -> ConsumerChoice point header m ()
simpleConsumerChoice mkPoint eqPoint chainVar points =
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
  if fst points `eqPoint` snd points
  then Next () (simpleConsumerNext mkPoint eqPoint chainVar (fst points))
  else Download maxBound (simpleConsumerDownload mkPoint eqPoint chainVar points)

simpleConsumerNext
  :: forall point header m .
     ( MonadSTM m )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> point  -- ^ Read pointer
  -> ConsumerNext point header m ()
simpleConsumerNext mkPoint eqPoint chainVar readPointer = ConsumerNext
  { nextForked    = \points ->
      simpleConsumerForked mkPoint eqPoint chainVar points
  , nextExtended  = \tip    ->
      pure $ simpleConsumerChoice mkPoint eqPoint chainVar (readPointer, tip)
  , nextExhausted = ()
  }

simpleConsumerDownload
  :: forall point header m .
     ( MonadSTM m )
  => (header -> point)
  -> (point -> point -> Bool)
  -> TVar m (Seq header)
  -> (point, point) -- ^ Read pointer, followed by tip
  -> ConsumerDownload point header m ()
simpleConsumerDownload mkPoint eqPoint chainVar points = ConsumerDownload
  { downloadHeader = \datum   -> do
      atomically $ modifyTVar' chainVar (extendWith (fst datum))
      let readPointer' = mkPoint (fst datum)
          tip' = fromMaybe (snd points) (snd datum)
      pure $ simpleConsumerDownload mkPoint eqPoint chainVar (readPointer', tip')
  , downloadOver   = \mNewTip ->
      pure $ simpleConsumerChoice mkPoint eqPoint chainVar (fst points, fromMaybe (snd points) mNewTip)
  , downloadForked = \points'  ->
      simpleConsumerForked mkPoint eqPoint chainVar points'
  }

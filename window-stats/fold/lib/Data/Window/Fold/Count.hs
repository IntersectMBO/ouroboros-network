{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Window.Fold.Count
-- Description : @foldl@ combinators driving a count-based sliding window.
-- Stability   : experimental
--
-- 'F.Fold' combinators that maintain a sliding count window
-- ('Data.Window.Count.Window') over the input stream and surface its
-- rolling measure to a downstream fold.
--
-- All combinators are polymorphic in the measure @v@ and the sample
-- type @a@ behind a 'FT.Measured' constraint, so they work unchanged
-- with any of the built-in sample wrappers
-- ('Data.Window.Count.SumSample', 'Data.Window.Count.MomentSample',
-- 'Data.Window.Count.MinMaxSample') and with user-defined measures.
--
-- = Integration with @with-tdigest@
--
-- These combinators also work directly with t-digest-backed windows
-- from the @with-tdigest@ sublibrary. Importing
-- "Data.Window.DigestCount" brings the
-- @'FT.Measured' ('Data.TDigest.TDigest' comp) ('Data.Window.DigestCount.DigestSample' comp)@
-- instance into scope, after which the combinators here apply
-- directly. For example, a rolling 95th-percentile fold over
-- 1000-sample windows:
--
-- @
-- import Control.Foldl qualified as F
-- import Data.Window.Fold.Count  (windowMeasureFinal)
-- import Data.Window.DigestCount (DigestSample)
-- import Data.TDigest qualified as TD
--
-- rollingP95 :: F.Fold (DigestSample 100) (Maybe Double)
-- rollingP95 = TD.quantile 0.95 \<$\> windowMeasureFinal 1000
-- @
--
module Data.Window.Fold.Count
  ( windowScan
  , windowFoldWith
  , windowFoldRolling
  , windowFoldRollingFull
  , windowFoldRollingM
  , windowFoldRolling2
  , windowFoldRolling2Full
  , windowFinal
  , windowMeasureFinal
  ) where

import Control.Foldl qualified as F
import Data.FingerTree qualified as FT
import Data.Sequence qualified as S

import Data.Window.Internal.Count qualified as W

-- | Compute the window measure at each step as the window moves across the
-- input stream, collecting all intermediate measures into a sequence.
-- The sequence has the same length as the input stream, with the i-th element
-- being the measure of the window ending at the i-th input element.
--
windowScan :: forall v a. FT.Measured v a
           => Int
           -> F.Fold a (S.Seq v)
windowScan n = F.Fold step initial extract
  where
    step :: (W.Window v a, S.Seq v) -> a -> (W.Window v a, S.Seq v)
    step (w, ms) x =
      let !w'  = W.insert x w
          !ms' = ms S.|> W.windowMeasure w'
      in  (w', ms')
    initial = (W.empty n, S.empty)
    extract = snd

-- | Apply an extraction function to the window measure at each step,
-- producing a rolling sequence of statistics as the window moves across
-- the input stream. A specialisation of 'windowScan' for when you want
-- to post-process the measure at each step.
--
windowFoldWith :: FT.Measured v a
               => Int
               -> (v -> b)
               -> F.Fold a (S.Seq b)
windowFoldWith n f = fmap f <$> windowScan n


-- | Stream the rolling sequence of window measures into an inner
-- 'F.Fold'. Unlike 'windowScan' / 'windowFoldWith', the intermediate
-- measures are __not__ retained — each measure is fed to the inner
-- fold as soon as it is produced. This keeps the memory footprint
-- bounded by the inner fold's own state, independent of the input
-- length.
--
-- Use this when you want to summarise the rolling statistics
-- (e.g. average them, take their max, fold them into a histogram)
-- rather than materialise every intermediate value.
--
windowFoldRolling :: forall v a r. FT.Measured v a
                  => Int
                  -> F.Fold v r
                  -> F.Fold a r
windowFoldRolling n = F.purely $ \innerStep innerInit innerExtract ->
  let step (w, s) x =
        let !w'  = W.insert x w
            !s'  = innerStep s (W.windowMeasure w')
        in  (w', s')
  in  F.Fold step (W.empty n :: W.Window v a, innerInit) (innerExtract . snd)


-- | Like 'windowFoldRolling' but only feeds a measure to the inner
-- fold once the window has reached its configured size. The first
-- @n - 1@ measures, which would otherwise be over a partially filled
-- window, are dropped.
--
-- This is usually what you want for statistical measures (rolling
-- mean, variance, quantiles) where partial-window readings are
-- misleading.
--
windowFoldRollingFull :: forall v a r. FT.Measured v a
                      => Int
                      -> F.Fold v r
                      -> F.Fold a r
windowFoldRollingFull n = F.purely $ \innerStep innerInit innerExtract ->
  let step (w, s) x =
        let !w'  = W.insert x w
            !s'  = if W.isFull w'
                     then innerStep s (W.windowMeasure w')
                     else s
        in  (w', s')
  in  F.Fold step (W.empty n :: W.Window v a, innerInit) (innerExtract . snd)


-- | Monadic counterpart of 'windowFoldRolling', for combining a sliding
-- window with a 'F.FoldM' (e.g. one that emits each measure to an
-- effectful sink).
--
windowFoldRollingM :: forall m v a r. (Monad m, FT.Measured v a)
                   => Int
                   -> F.FoldM m v r
                   -> F.FoldM m a r
windowFoldRollingM n = F.impurely $ \innerStep innerInit innerExtract ->
  let step (w, s) x = do
        let !w' = W.insert x w
        !s' <- innerStep s (W.windowMeasure w')
        return (w', s')
      init' = do
        s <- innerInit
        return (W.empty n :: W.Window v a, s)
      extract' (_, s) = innerExtract s
  in  F.FoldM step init' extract'


-- | Maintain two windows of different sizes over the same input
-- stream, feeding the pair of measures @(short, long)@ to the inner
-- fold at each step. Convenient for cross-window comparisons such as
-- short- versus long-period moving averages.
--
-- A pair is emitted on every input, starting from one-sample windows.
-- The first @max short long - 1@ pairs therefore include at least one
-- partially-filled window; only from reading @max short long@ onward
-- are both windows full. If those partial readings are unwanted, use
-- 'windowFoldRolling2Full', which suppresses emission until both
-- windows are full.
--
windowFoldRolling2 :: forall v a r. FT.Measured v a
                   => Int -- ^ short window size
                   -> Int -- ^ long window size
                   -> F.Fold (v, v) r
                   -> F.Fold a r
windowFoldRolling2 short long = F.purely $ \innerStep innerInit innerExtract ->
  let step (ws, wl, s) x =
        let !ws' = W.insert x ws
            !wl' = W.insert x wl
            !s'  = innerStep s (W.windowMeasure ws', W.windowMeasure wl')
        in  (ws', wl', s')
      init' = ( W.empty short :: W.Window v a
              , W.empty long  :: W.Window v a
              , innerInit
              )
  in  F.Fold step init' (\(_, _, s) -> innerExtract s)


-- | Like 'windowFoldRolling2' but only emits a pair of measures once
-- __both__ windows are full. Use this when partial-window readings
-- would distort the comparison (typically for any statistical
-- measure).
--
windowFoldRolling2Full :: forall v a r. FT.Measured v a
                       => Int -- ^ short window size
                       -> Int -- ^ long window size
                       -> F.Fold (v, v) r
                       -> F.Fold a r
windowFoldRolling2Full short long = F.purely $ \innerStep innerInit innerExtract ->
  let step (ws, wl, s) x =
        let !ws' = W.insert x ws
            !wl' = W.insert x wl
            !s'  = if W.isFull ws' && W.isFull wl'
                     then innerStep s (W.windowMeasure ws', W.windowMeasure wl')
                     else s
        in  (ws', wl', s')
      init' = ( W.empty short :: W.Window v a
              , W.empty long  :: W.Window v a
              , innerInit
              )
  in  F.Fold step init' (\(_, _, s) -> innerExtract s)


-- | Return the window itself at the end of the input stream. Useful
-- when you want to query multiple statistics over the trailing window
-- after consuming a stream.
--
windowFinal :: forall v a. FT.Measured v a
            => Int
            -> F.Fold a (W.Window v a)
windowFinal n = F.Fold (\w x -> W.insert x w) (W.empty n) id


-- | Return only the measure of the window at the end of the input
-- stream. The most common shorthand when you want a single rolling
-- statistic over the final @n@ samples.
--
windowMeasureFinal :: forall v a. FT.Measured v a
                   => Int
                   -> F.Fold a v
windowMeasureFinal n = W.windowMeasure <$> windowFinal n

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Window.Fold.Timed
-- Description : @foldl@ combinators driving a time-based sliding window.
-- Stability   : experimental
--
-- 'F.Fold' combinators that maintain a sliding time-based window
-- ('Data.Window.Timed.TimedWindow') over the input stream and surface
-- its rolling measure to a downstream fold.
--
-- All combinators are polymorphic in the time type @t@ (behind a
-- 'TimeLike' constraint), the measure @v@, and the sample type @a@
-- (behind a 'FT.Measured' constraint), so they work unchanged with
-- the wall-clock ('UTCTime') and monotonic
-- ('Control.Monad.Class.MonadTime.SI.Time') stock instances, with any
-- user-defined 'TimeLike' instance, and with any of the built-in
-- sample wrappers ('Data.Window.Timed.SumSample',
-- 'Data.Window.Timed.MomentSample', 'Data.Window.Timed.MinMaxSample').
--
-- = Integration with @with-tdigest@
--
-- These combinators also work directly with t-digest-backed timed
-- windows from the @with-tdigest@ sublibrary. Importing
-- "Data.Window.DigestTimed" (or "Data.Window.DigestCount", which
-- exposes the same instance) brings the
-- @'FT.Measured' ('Data.TDigest.TDigest' comp) ('Data.Window.DigestCount.DigestSample' comp)@
-- instance into scope, after which the combinators here apply
-- directly. For example, a rolling 50th-percentile fold over a
-- 60-second window:
--
-- @
-- import Control.Foldl qualified as F
-- import Data.Time (NominalDiffTime, UTCTime)
-- import Data.Window.Fold.Timed   (windowMeasureFinal)
-- import Data.Window.DigestCount  (DigestSample)
-- import Data.TDigest qualified as TD
--
-- rollingP50 :: NominalDiffTime
--            -> F.Fold (UTCTime, DigestSample 100) (Maybe Double)
-- rollingP50 d = TD.quantile 0.5 \<$\> windowMeasureFinal d
-- @
--
module Data.Window.Fold.Timed
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

import Data.Window.Internal.Timed qualified as W
import Data.Window.TimeLike


-- | Compute the window measure at each step as the window moves across the
-- input stream, collecting all intermediate measures into a sequence.
-- The sequence has the same length as the input stream, with the i-th element
-- being the measure of the window ending at the i-th input element.
--
windowScan :: forall t v a. (TimeLike t, FT.Measured v a)
           => Dur t
           -> F.Fold (t, a) (S.Seq v)
windowScan timedWindowDuration = F.Fold step initial extract
  where
    step :: (W.TimedWindow t v a, S.Seq v) -> (t, a) -> (W.TimedWindow t v a, S.Seq v)
    step (w, ms) (t, a) =
      let !w'  = W.insert (t, a) w
          !ms' = ms S.|> W.windowMeasure w'
      in  (w', ms')
    initial = (W.empty timedWindowDuration, S.empty)
    extract = snd

-- | Apply an extraction function to the window measure at each step,
-- producing a rolling sequence of statistics as the window moves across
-- the input stream. A specialisation of 'windowScan' for when you want
-- to post-process the measure at each step.
--
windowFoldWith :: (TimeLike t, FT.Measured v a)
               => Dur t
               -> (v -> b)
               -> F.Fold (t, a) (S.Seq b)
windowFoldWith timedWindowDuration f = fmap f <$> windowScan timedWindowDuration


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
windowFoldRolling :: forall t v a r. (TimeLike t, FT.Measured v a)
                  => Dur t
                  -> F.Fold v r
                  -> F.Fold (t, a) r
windowFoldRolling timedWindowDuration = F.purely $ \innerStep innerInit innerExtract ->
  let step (!w, !s) (t, a) =
        let !w' = W.insert (t, a) w
            !s' = innerStep s (W.windowMeasure w')
        in  (w', s')
  in  F.Fold step (W.empty timedWindowDuration :: W.TimedWindow t v a, innerInit) (innerExtract . snd)


-- | Like 'windowFoldRolling' but only feeds a measure to the inner
-- fold once the timestamps span at least the configured duration.
-- Measures over a window that has not yet been "filled" by the
-- duration are dropped.
--
-- This is usually what you want for statistical measures (rolling
-- mean, variance, quantiles) where partial-window readings are
-- misleading.
--
windowFoldRollingFull :: forall t v a r. (TimeLike t, FT.Measured v a)
                      => Dur t
                      -> F.Fold v r
                      -> F.Fold (t, a) r
windowFoldRollingFull timedWindowDuration = F.purely $ \innerStep innerInit innerExtract ->
  let step (!w, !s) (t, a) =
        let !w' = W.insert (t, a) w
            !s' = if maybe False (>= timedWindowDuration) (W.windowDuration w')
                    then innerStep s (W.windowMeasure w')
                    else s
        in  (w', s')
  in  F.Fold step (W.empty timedWindowDuration :: W.TimedWindow t v a, innerInit) (innerExtract . snd)


-- | Monadic counterpart of 'windowFoldRolling', for combining a sliding
-- window with a 'F.FoldM' (e.g. one that emits each measure to an
-- effectful sink).
--
windowFoldRollingM :: forall m t v a r. (Monad m, TimeLike t, FT.Measured v a)
                   => Dur t
                   -> F.FoldM m v r
                   -> F.FoldM m (t, a) r
windowFoldRollingM timedWindowDuration = F.impurely $ \innerStep innerInit innerExtract ->
  let step (w, s) (t, a) = do
        let !w' = W.insert (t, a) w
        !s' <- innerStep s (W.windowMeasure w')
        return (w', s')
      init' = do
        s <- innerInit
        return (W.empty timedWindowDuration :: W.TimedWindow t v a, s)
      extract' (_, s) = innerExtract s
  in  F.FoldM step init' extract'


-- | Maintain two windows of different durations over the same input
-- stream, feeding the pair of measures @(short, long)@ to the inner
-- fold at each step. Convenient for cross-window comparisons such as
-- short- versus long-period moving averages.
--
-- A pair is emitted on every input, starting from a one-sample
-- window (zero duration). Until the input timestamps span at least
-- @max short long@, every emitted pair includes at least one
-- not-yet-spanned window. If those partial readings are unwanted, use
-- 'windowFoldRolling2Full', which suppresses emission until both
-- windows have been spanned by their configured duration.
--
windowFoldRolling2 :: forall t v a r. (TimeLike t, FT.Measured v a)
                   => Dur t -- ^ short window duration
                   -> Dur t -- ^ long window duration
                   -> F.Fold (v, v) r
                   -> F.Fold (t, a) r
windowFoldRolling2 short long = F.purely $ \innerStep innerInit innerExtract ->
  let step (ws, wl, s) (t, a) =
        let !ws' = W.insert (t, a) ws
            !wl' = W.insert (t, a) wl
            !s'  = innerStep s (W.windowMeasure ws', W.windowMeasure wl')
        in  (ws', wl', s')
      init' = ( W.empty short :: W.TimedWindow t v a
              , W.empty long  :: W.TimedWindow t v a
              , innerInit
              )
  in  F.Fold step init' (\(_, _, s) -> innerExtract s)


-- | Like 'windowFoldRolling2' but only emits a pair of measures once
-- __both__ windows have been spanned by their configured duration
-- (i.e. @windowDuration w >= configured duration@ for each window).
-- Use this when partial-window readings would distort the comparison
-- (typically for any statistical measure).
--
windowFoldRolling2Full :: forall t v a r. (TimeLike t, FT.Measured v a)
                       => Dur t -- ^ short window duration
                       -> Dur t -- ^ long window duration
                       -> F.Fold (v, v) r
                       -> F.Fold (t, a) r
windowFoldRolling2Full short long = F.purely $ \innerStep innerInit innerExtract ->
  let step (ws, wl, s) (t, a) =
        let !ws' = W.insert (t, a) ws
            !wl' = W.insert (t, a) wl
            !s'  = if maybe False (>= short) (W.windowDuration ws')
                      && maybe False (>= long) (W.windowDuration wl')
                     then innerStep s (W.windowMeasure ws', W.windowMeasure wl')
                     else s
        in  (ws', wl', s')
      init' = ( W.empty short :: W.TimedWindow t v a
              , W.empty long  :: W.TimedWindow t v a
              , innerInit
              )
  in  F.Fold step init' (\(_, _, s) -> innerExtract s)


-- | Return the window itself at the end of the input stream. Useful
-- when you want to query multiple statistics over the trailing window
-- after consuming a stream.
--
windowFinal :: forall t v a. (TimeLike t, FT.Measured v a)
            => Dur t
            -> F.Fold (t, a) (W.TimedWindow t v a)
windowFinal timedWindowDuration =
  F.Fold (\w ta -> W.insert ta w) (W.empty timedWindowDuration) id


-- | Return only the measure of the window at the end of the input
-- stream. The most common shorthand when you want a single rolling
-- statistic over the trailing duration.
--
windowMeasureFinal :: forall t v a. (TimeLike t, FT.Measured v a)
                   => Dur t
                   -> F.Fold (t, a) v
windowMeasureFinal d = W.windowMeasure <$> windowFinal d

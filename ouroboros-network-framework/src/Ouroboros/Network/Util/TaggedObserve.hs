{-# LANGUAGE LambdaCase #-}
module Ouroboros.Network.Util.TaggedObserve (
      TaggedObservable (..)
    , matchTaggedObservations

      -- reexports
    , ObserveIndicator (..)
    , Observable (..)
    , matchObservations
    ) where

import           Control.Tracer
import           Control.Tracer.Observe

-- Like Observable but with a t tag type field too.
data TaggedObservable t s e d = TOStart t s
                              | TOEnd t e (Maybe d)
                              --           ^^ holds the difference between start and end
matchTaggedObservations
    :: Monad m
    => (t -> m (Maybe s))
    -> (t -> s -> m ())
    -> (s -> e -> d)
    -> Tracer m (TaggedObservable t s e d)
    -> Tracer m (TaggedObservable t s e d)
matchTaggedObservations getStart putStart f tr = Tracer $ \case
    obs@(TOStart t s) -> do
        putStart t s
        traceWith tr obs
    (TOEnd t e _) -> do
        before <- getStart t
        traceWith tr $ TOEnd t e $ fmap (flip f e) before

instance (Show t, Show s, Show e, Show d) => Show (TaggedObservable t s e d) where
    show (TOStart t time)     = "TOStart " ++ show t ++ " " ++ show time
    show (TOEnd t time mTime) = "TOEnd " ++ show t ++ " " ++ show time ++ ", ODiff " ++ show mTime


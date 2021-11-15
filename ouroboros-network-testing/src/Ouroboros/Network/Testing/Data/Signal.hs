{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Testing.Data.Signal (
  -- * Events
  Events,
  eventsFromList,
  eventsFromListUpToTime,
  eventsToList,
  selectEvents,

  -- * Low level access
  primitiveTransformEvents,
  TS(..),
  E(..),

  -- * Signals
  Signal,
  -- ** Construction and conversion
  fromChangeEvents,
  toChangeEvents,
  fromEvents,

  -- ** QuickCheck
  signalProperty,

  -- * Simple signal transformations
  truncateAt,
  stable,
  nub,
  nubBy,

  -- * Temporal operations
  linger,
  timeout,
  until,
  difference,
  scanl,

  -- * Set-based temporal operations
  keyedTimeout,
  keyedLinger,
  keyedUntil,

  ) where

import           Prelude hiding (until, scanl)

import           Data.Maybe (maybeToList)
import           Data.List (groupBy)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.OrdPSQ as PSQ
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.Foldable as Deque (toList)
import qualified Deque.Lazy as Deque
import           Deque.Lazy (Deque)

import           Control.Monad.Class.MonadTime (Time(..), DiffTime, addTime)


import           Test.QuickCheck




--
-- Time stamps and events
--

-- The instance Applicative Signal relies on merging event streams.
-- The IO simulator's treatment of time means that we can have many
-- events that occur at the same virtual time, though they are stil
-- causually ordered.
--
-- We need these compound time stamps to be able to resolve the order
-- of the events that have the same Time when merging event streams.
-- The compound time stamp records the event number from the original
-- trace, for events derivied from the original trace. For artificially
-- constructed events, they can use small or big counters to be ordered
-- before or after other events at the same time. Negative counters are
-- permitted for this purpose.

data TS = TS !Time !Int
  deriving (Eq, Ord, Show)

-- A single event or entry in a time series, annotated with its timestamp.
--
data E a = E {-# UNPACK #-} !TS a
  deriving (Show, Functor)


--
-- Events
--

-- | A time-ordered trace of discrete events that occur at specific times.
--
-- This corresponds for example to a trace of events or observations from a
-- simulation.
--
newtype Events a = Events [E a]
  deriving (Show, Functor)

-- | Construct 'Events' from a time series.
--
eventsFromList :: [(Time, a)] -> Events a
eventsFromList txs =
    Events [ E (TS t i) x
           | ((t, x), i) <- zip txs [100, 102..] ]


-- | Construct 'Events' from a time series.
--
-- The time series is truncated at (but not including) the given time. This is
-- necessary to check properties over finite prefixes of infinite time series.
--
eventsFromListUpToTime :: Time -> [(Time, a)] -> Events a
eventsFromListUpToTime horizon txs =
    Events [ E (TS t i) x
           | let txs' = takeWhile (\(t,_) -> t < horizon) txs
           , ((t, x), i) <- zip txs' [100, 102..] ]


eventsToList :: Events a -> [(Time, a)]
eventsToList (Events txs) = [ (t, x) | E (TS t _i) x <- txs ]

selectEvents :: (a -> Maybe b) -> Events a -> Events b
selectEvents select (Events txs) =
    Events [ E t y | E t x <- txs, y <- maybeToList (select x) ]

primitiveTransformEvents :: ([E a] -> [E b]) -> Events a -> Events b
primitiveTransformEvents f (Events txs) = Events (f txs)


--
-- Signals
--

-- | A signal is a time-varying value. It has a value at all times. It changes
-- value at discrete times, i.e. it is not continuous.
--
data Signal a = Signal a [E a]
  deriving (Show, Functor)

instance Applicative Signal where
    pure  x = Signal x []
    f <*> x = mergeSignals f x

mergeSignals :: Signal (a -> b) -> Signal a -> Signal b
mergeSignals (Signal f0 fs0) (Signal x0 xs0) =
    Signal (f0 x0) (go f0 x0 (mergeBy compareTimestamp fs0 xs0))
  where
    go :: (a -> b) -> a -> [MergeResult (E (a -> b)) (E a)] -> [E b]
    go _ _ [] = []
    go _ x (OnlyInLeft   (E t f)         : rs) = E t (f x) : go f x rs
    go f _ (OnlyInRight          (E t x) : rs) = E t (f x) : go f x rs
    go _ _ (InBoth       (E t f) (E _ x) : rs) = E t (f x) : go f x rs

compareTimestamp :: E a -> E b -> Ordering
compareTimestamp (E ts _) (E ts' _) = compare ts ts'


-- | Construct a 'Signal' from an initial value and a time series of events
-- that represent new values of the signal.
--
-- This only makes sense for events that sample a single time-varying value.
--
fromChangeEvents :: a -> Events a -> Signal a
fromChangeEvents x (Events xs) = Signal x xs


-- | Convert a 'Signal' into a time series of events when the signal value
-- changes.
--
toChangeEvents :: Signal a -> Events a
toChangeEvents = Events . toTimeSeries

toTimeSeries :: Signal a -> [E a]
toTimeSeries (Signal x xs) = E (TS (Time 0) 0) x : xs


-- | Construct a 'Signal' that represents a time series of discrete events. The
-- signal is @Just@ the event value at the time of the event, and is @Nothing@
-- at all other times.
--
-- Note that this signal \"instantaneously\" takes the event value and reverts
-- to @Nothing@ before time moves on. Therefore this kind of signal is not
-- \"stable\" in the sense of 'stableSignal'.
--
fromEvents :: Events a -> Signal (Maybe a)
fromEvents (Events txs) =
    Signal Nothing
           [ E (TS t i') s
           | E (TS t i) x <- txs
           , (i', s) <- [(i, Just x), (i+1, Nothing)]
           ]


-- | A signal can change value more than once at a single point of time.
--
-- Sometimes we are interested only in the final \"stable\" value of the signal
-- before time moves on. This function discards the other values, keeping only
-- the final value at each time.
--
stable :: Signal a -> Signal a
stable (Signal x xs) =
    Signal x ((map last . groupBy sameTime) xs)
  where
    sameTime (E (TS t _) _) (E (TS t' _) _) = t == t'

-- Truncate a 'Signal' after a given time. This is typically necessary to
-- check properties over finite prefixes of infinite signals.
--
truncateAt :: Time -> Signal a -> Signal a
truncateAt horizon (Signal x txs) =
    Signal x (takeWhile (\(E (TS t _) _) -> t < horizon) txs)


-- | Sometimes the way a signal is constructed leads to duplicate signal values
-- which can slow down signal processing. This tidies up the signal by
-- eliminating the duplicates. This does not change the meaning (provided the
-- 'Eq' instance is true equality).
--
nub :: Eq a => Signal a -> Signal a
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> Signal a -> Signal a
nubBy eq (Signal x0 xs0) =
    Signal x0 (go x0 xs0)
  where
    go _ [] = []
    go x (E t x' : xs)
      | x `eq` x' = go x xs
      | otherwise = E t x' : go x' xs


-- | A linger signal remains @True@ for the given time after the underlying
-- signal is @True@.
--
linger :: DiffTime
       -> (a -> Bool)
       -> Signal a
       -> Signal Bool
linger = error "TODO: Signal.linger"


-- | Make a timeout signal, based on observing an underlying signal.
--
-- The timeout signal takes the value @True@ when the timeout has occurred, and
-- @False@ otherwise.
--
-- The timeout is controlled by an \"arming\" function on the underlying signal.
-- The arming function should return @True@ when the timeout should be started,
-- and it returns the time to wait before the timeout fires. The arming function
-- should return @False@ when the timeout should be cancelled or not started.
--
-- The output signal becomes @True@ when the arming function has been
-- continuously active (i.e. returning @True@) for the given duration.
--
timeout :: forall a.
           DiffTime    -- ^ timeout duration
        -> (a -> Bool) -- ^ the arming function
        -> Signal a
        -> Signal Bool
timeout d arm =
    Signal False
  . disarmed
  . toTimeSeries
  where
    disarmed :: [E a] -> [E Bool]
    disarmed []          = []
    disarmed (E ts@(TS t _) x : txs)
      | arm x     = armed (d `addTime` t) (E ts x : txs)
      | otherwise = E ts False : disarmed txs

    armed :: Time -> [E a] -> [E Bool]
    armed !expiry [] = [E expiryTS True] where expiryTS = TS expiry 0

    armed !expiry (E ts@(TS t _) x : txs)
      | t > expiry  = E expiryTS True  : expired (E ts x : txs)
      | not (arm x) = E ts       False : disarmed txs
      | t < expiry  = E ts       False : armed expiry txs
      | otherwise   = E expiryTS True  : expired txs
      where
        expiryTS = TS expiry 0

    expired :: [E a] -> [E Bool]
    expired [] = []
    expired (E t x : txs)
      | arm x      = E t True  : expired  txs
      | otherwise  = E t False : disarmed txs


until :: (a -> Bool) -- ^ Start
      -> (a -> Bool) -- ^ Stop
      -> Signal a
      -> Signal Bool
until _ = error "TODO: Signal.until"


-- | Make a signal that keeps track of recent activity, based on observing an
-- underlying signal.
--
-- The underlying signal is scrutinised with the provided \"activity interest\"
-- function that tells us if the signal value is activity of interest to track.
-- If it is, the given key is entered into the result signal set for the given
-- time duration. If the same activity occurs again before the duration expires
-- then the expiry will be extended to the new deadline (it is not cumulative).
-- The key will be removed from the result signal set when it expires.
--
keyedLinger :: forall a b. Ord b
            => DiffTime
            -> (a -> Set b)  -- ^ The activity set signal
            -> Signal a
            -> Signal (Set b)
keyedLinger d activity =
    Signal Set.empty
  . go Set.empty PSQ.empty
  . toTimeSeries
  where
    go :: Set b
       -> OrdPSQ b Time ()
       -> [E a]
       -> [E (Set b)]
    go _ _ [] = []

    go lingerSet lingerPSQ (E ts@(TS t _) xs : txs)
      | Just (x, t', _, lingerPSQ') <- PSQ.minView lingerPSQ
      , t' < t
      , let lingerSet' = Set.delete x lingerSet
      = E (TS t' 0) lingerSet' : go lingerSet' lingerPSQ' (E ts xs : txs)

    go lingerSet lingerPSQ (E ts@(TS t _) x : txs) =
      let ys         = activity x
          lingerSet' = lingerSet <> ys
          lingerPSQ' = Set.foldl' (\s y -> PSQ.insert y t' () s) lingerPSQ ys
          t'         = addTime d t
       in if lingerSet' /= lingerSet
            then E ts lingerSet' : go lingerSet' lingerPSQ' txs
            else                   go lingerSet' lingerPSQ' txs


keyedTimeout :: forall a b. Ord b
             => DiffTime
             -> (a -> Set b)  -- ^ The timeout arming set signal
             -> Signal a
             -> Signal (Set b)
keyedTimeout d arm =
    Signal Set.empty
  . go Set.empty PSQ.empty Set.empty
  . toTimeSeries
  where
    go :: Set b
       -> OrdPSQ b Time ()
       -> Set b
       -> [E a]
       -> [E (Set b)]
    go _ _ _ [] = []

    go armedSet armedPSQ timedout (E ts@(TS t _) x : txs)
      | Just (y, t', _, armedPSQ') <- PSQ.minView armedPSQ
      , t' < t
      , let armedSet' = Set.delete y armedSet
            timedout' = Set.insert y timedout
      = E (TS t' 0) timedout' : go armedSet' armedPSQ' timedout' (E ts x : txs)

    go armedSet armedPSQ timedout (E ts@(TS t _) x : txs) =
      let armedSet' = arm x
          armedAdd  = armedSet' Set.\\ armedSet
          armedDel  = armedSet  Set.\\ armedSet'
          armedPSQ' = flip (Set.foldl' (\s y -> PSQ.insert y t' () s)) armedAdd
                    . flip (Set.foldl' (\s y -> PSQ.delete y       s)) armedDel
                    $ armedPSQ
          t'        = addTime d t
          timedout' = timedout `Set.intersection` armedSet'
       in if timedout' /= timedout
            then E ts timedout' : go armedSet' armedPSQ' timedout' txs
            else                  go armedSet' armedPSQ' timedout' txs


keyedUntil :: forall a b. Ord b
           => (a -> Set b)   -- ^ Start set signal
           -> (a -> Set b)   -- ^ Stop set signal
           -> (a -> Bool)    -- ^ Stop all signal
           -> Signal a
           -> Signal (Set b)
keyedUntil start stop stopAll =
    Signal Set.empty
  . go Set.empty
  . toTimeSeries
  where

    go :: Set b
       -> [E a]
       -> [E (Set b)]
    go _ [] = []
    go active (E t x : txs)
       | active' /= active = E t active' : go active' txs
       | otherwise         =               go active' txs
      where
        active'
          | stopAll x = Set.empty
          | otherwise = (active <> start x) Set.\\ stop x


difference :: (a -> a -> b)
           -> Signal a
           -> Signal (Maybe b)
difference diff (Signal x0 txs0) =
    Signal Nothing (go x0 txs0)
  where
    go _ []                    = []
    go x (E (TS t i) x' : txs) = E (TS t i)    (Just (diff x x'))
                               : E (TS t (i+1)) Nothing
                               : go x' txs


scanl :: (b -> a -> b) -> b -> Signal a -> Signal b
scanl f z (Signal x0 txs0) =
    let a0 = f z x0 in
    Signal a0 (go a0 txs0)
  where
    go !_ []             = []
    go !a (E ts x : txs) = E ts a' : go a' txs
                          where
                            a' = f a x

--
-- QuickCheck
--

-- | Check a property over a 'Signal'. The property should be true at all times.
--
-- On failure it shows the @n@ most recent signal values.
--
signalProperty :: forall a. Int -> (a -> String)
               -> (a -> Bool) -> Signal a -> Property
signalProperty atMost showSignalValue p =
    go 0 mempty . eventsToList . toChangeEvents
  where
    go :: Int -> Deque (Time, a) -> [(Time, a)] -> Property
    go !_ !_ []                   = property True
    go !n !q ((t, x) : txs) | p x = next
      where
        next
          | n < atMost = go (n+1) (              Deque.snoc (t,x)  q) txs
          | otherwise  = go n     ((Deque.tail . Deque.snoc (t,x)) q) txs

    go !_ !recent ((t, x) : _) = counterexample details (property False)
      where
        details =
          unlines [ "Last " ++ show atMost ++ " signal values:"
                  , unlines [ show t' ++ "\t: " ++ showSignalValue x'
                            | (t',x') <- Deque.toList recent ]
                  , "Property violated at: " ++ show t
                  , "Invalid signal value:"
                  , showSignalValue x
                  ]

--
-- Utils
--

-- | Generic merging utility. For sorted input lists this is a full outer join.
--
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b
  deriving (Eq, Show)



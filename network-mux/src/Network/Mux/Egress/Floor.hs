{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The byte floor's pure core.
--
-- Scheduled egress is strict priority: while a lower tier can fill the
-- budget, the tiers above it are not served at all. The floor is a small
-- allowance for the credited bearers among them, shared fairly in bytes
-- across three classes and first come first served within a class. Bytes
-- across classes, because a class sending large batches would otherwise take
-- the floor from one sending single-SDU votes by the ratio of their batch
-- sizes; turns within a class, because what a starved bearer needs is for its
-- one small message to go. A bearer whose allowance is spent has no class: it
-- is served when the budget idles, and under saturation its keep-alive lapses
-- and the peer leaves.
--
-- The sharing is start-time fair queuing over the classes: every class has a
-- virtual finish time in bytes, a grant starts at the later of that and the
-- floor's virtual time, the start of the last grant, and advances the class
-- by its bytes; the next grant goes to the waiting head with the earliest
-- start. Two backlogged classes never differ by more than a batch of each,
-- a class that was idle starts again at the present and banks nothing, and
-- the gap between a bearer being granted and asking again, during which its
-- class is momentarily empty, changes nothing. Deficit round-robin was tried
-- first and degenerated to turns on exactly that gap.
--
-- This module decides who is next and keeps the virtual times; it knows
-- nothing of real time, tokens or threads. "Network.Mux.Egress.Bucket"
-- drives it.
--
module Network.Mux.Egress.Floor
  ( -- * Classes
    FloorClass (..)
  , floorClass
  , floorClasses
  , classHeads
    -- * Fair queuing
  , FloorState (..)
  , newFloorState
  , startTag
  , floorPick
  , floorGranted
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word8)


-- | The classes the floor serves: partners, pool relays, strangers with
-- credit. Local roots have no class, they head the cascade; neither has
-- anything at or below rest, the exhausted and the unranked.
newtype FloorClass = FloorClass Word8
  deriving (Eq, Ord, Show)

-- | The class of a tier, if the tier is served by the floor: 1 partners,
-- 2 pool relays, 3 strangers; tier 0 and anything from 4 up, none.
floorClass :: Word8 -> Maybe FloorClass
floorClass tier =
  case tier of
       1 -> Just (FloorClass 0)
       2 -> Just (FloorClass 1)
       3 -> Just (FloorClass 2)
       _ -> Nothing

floorClasses :: [FloorClass]
floorClasses = map FloorClass [0 .. 2]

-- | Each class's first waiter in a queue keyed by class and then by ticket:
-- one lookup per class, so the cost is in the classes, not the waiters.
classHeads :: Map (FloorClass, t) a -> Map FloorClass (t, a)
classHeads q =
  Map.fromList
    [ (c, (t, a))
    | c <- floorClasses
    , Just ((c', t), a) <- [Map.lookupMin (Map.dropWhileAntitone ((< c) . fst) q)]
    , c' == c ]

-- | The virtual clock, in bytes: the start of the last grant, and each
-- class's finish, the start of its last grant plus its bytes.
data FloorState = FloorState {
    fsVirtual :: !Integer,
    fsFinish  :: !(Map FloorClass Integer)
  }
  deriving (Eq, Show)

newFloorState :: FloorState
newFloorState = FloorState { fsVirtual = 0, fsFinish = Map.empty }

-- | When a grant to the class would start: after its last one, and not
-- before the present, so an idle class returns with nothing banked.
startTag :: FloorState -> FloorClass -> Integer
startTag FloorState { fsVirtual, fsFinish } c =
  max fsVirtual (Map.findWithDefault 0 c fsFinish)

-- | The bearer the floor serves next, given each non-empty class's head: its
-- ticket and the bytes it needs. The class with the earliest start, the
-- lower class on a tie, and that class's head. Nothing when no class has a
-- head. A pure choice: asking again with the same state and heads gives the
-- same answer.
--
floorPick :: forall t. FloorState -> Map FloorClass (t, Int) -> Maybe (FloorClass, t)
floorPick st heads =
  case Map.toAscList heads of
       []       -> Nothing
       (h : hs) -> Just (snd (foldl' earlier (tag h) (map tag hs)))
  where
    tag (c, (t, _)) = (startTag st c, (c, t))
    earlier best@(s, _) cand@(s', _) = if s' < s then cand else best

-- | A bearer of the class was granted its batch: the grant started at the
-- class's start tag, which becomes the present, and the class finishes its
-- bytes later.
floorGranted :: FloorClass -> Int -> FloorState -> FloorState
floorGranted c need st =
  let s = startTag st c
  in st { fsVirtual = s, fsFinish = Map.insert c (s + fromIntegral need) (fsFinish st) }

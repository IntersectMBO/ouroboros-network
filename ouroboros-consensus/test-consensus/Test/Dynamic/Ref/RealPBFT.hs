{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A reference simulator of the RealPBFT protocol under \"ideal
-- circumstances\"
--
-- See 'step'.
--
module Test.Dynamic.Ref.RealPBFT (
  Outcome (..),
  State (..),
  advanceUpTo,
  emptyState,
  nullState,
  step,
  viable,
  ) where

import           Data.Foldable (Foldable, foldl', toList)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Protocol.PBFT (PBftParams (..))

import           Test.Dynamic.Util.NodeJoinPlan

oneK :: Num a => PBftParams -> a
oneK PBftParams{pbftSecurityParam} =
    fromIntegral (maxRollbacks pbftSecurityParam)

twoK :: Num a => PBftParams -> a
twoK PBftParams{pbftSecurityParam} =
    2 * fromIntegral (maxRollbacks pbftSecurityParam)

oneN :: Num a => PBftParams -> a
oneN PBftParams{pbftNumNodes} = fromIntegral pbftNumNodes

{-------------------------------------------------------------------------------
  PBFT state
-------------------------------------------------------------------------------}

-- | Outcome of a node leading a slot
--
data Outcome

  = Absent
    -- ^ the leader hadn't yet joined

  | Nominal
    -- ^ the leader extended the networks' common chain with a valid block

  | Unable
    -- ^ the leader could not forge a valid block

  | Wasted
    -- ^ the leader forged a valid-but-irrelevant block, because it forged
    -- before sufficiently synchronizing
    --
    -- As of commit 4eb23cfe \"Merge #1260\", this only happens if a node leads
    -- in the slot it joins. In that slot, it forges before the mini protocols
    -- are able to pull in any of the existing nodes' chain, and so it
    -- re-forges the epoch 0 boundary block (EBB).

  deriving (Eq, Show)

-- | The state of the RealPBFT net
--
data State = State
  { ids      :: !(Seq CoreNodeId)
    -- ^ who forged each of the last @k@ blocks

  , nomCount :: !NumNominals
    -- ^ cache for optimization: number of 'Nominal's in 'outs'

  , nextSlot :: !SlotNo

  , outs     :: !(Seq Outcome)
    -- ^ the outcome of each the last @2k@ slots
  }
  deriving (Eq, Show)

newtype NumNominals = NumNominals Int
  deriving (Eq, Ord, Show)

emptyState :: State
emptyState = State Seq.empty (NumNominals 0) 0 Seq.empty

-- | There are no recorded forgings
--
nullState :: State -> Bool
nullState State{ids} = Seq.null ids

-- | Record the latest forging in the state
--
-- Should be followed by a call to 'extendOut'.
--
extendId :: PBftParams -> State -> CoreNodeId -> State
extendId params st@State{ids} i = st
  { ids      = snd $ prune (oneK params) $ ids Seq.|> i
  }

count :: (Eq a, Foldable t) => a -> t a -> Int
count a bs = length [ () | b <- toList bs, a == b ]

prune :: Int -> Seq a -> (Seq a, Seq a)
prune lim x = Seq.splitAt excess x
  where
    excess = Seq.length x - fromIntegral lim

-- | Record the latest outcome in the state
--
extendOut :: PBftParams -> State -> Outcome -> State
extendOut params State{ids, nomCount, nextSlot, outs} out = State
  { ids
  , nomCount = nomCount'
  , outs     = outs'
  , nextSlot = succ nextSlot
  }
  where
    (dropped, outs') = prune (twoK params) $ outs Seq.|> out

    NumNominals nc = nomCount
    nomCount'      = NumNominals $ nc
      + (if Nominal == out then 1 else 0)
      - count Nominal dropped

-- | @True@ if the state would violate 'pbftSignatureThreshold'
--
tooMany :: PBftParams -> State -> CoreNodeId -> Bool
tooMany params State{ids} i =
    not $
    Seq.length ids < k || count i ids <= most
  where
    PBftParams{pbftSignatureThreshold} = params

    k :: forall a. Num a => a
    k = oneK params

    -- how many blocks in the latest k-blocks that a single core node is
    -- allowed to have forged
    most = floor $ k * pbftSignatureThreshold

-- | @True@ if the state resulted from a sequence of @2k@ slots that had less
-- than @k@ 'Nominal' outcomes
--
tooSparse :: PBftParams -> State -> Bool
tooSparse params State{nomCount, outs} =
    Seq.length outs - nc > oneK params
  where
    NumNominals nc = nomCount

-- | @True@ if the state has a suffix of @n@ sequential 'Nominal' outcomes
--
-- Once this happens, by the assumption list on 'step', all remaining slots
-- will be 'Nominal'.
--
saturated :: PBftParams -> State -> Bool
saturated params State{outs} =
    Seq.length outs >= oneN params && all (== Nominal) nouts
  where
    (_, nouts) = prune (oneN params) outs

{-------------------------------------------------------------------------------
  PBFT reference implementation
-------------------------------------------------------------------------------}

-- | Advance the state by one slot
--
-- ASSUMPTION Any new valid block is immediately selected by all nodes.
--
-- This assumption seems mild because of PBFT's fixed round-robin schedule; it
-- primarily constrains the network topology, network outages, node restarts,
-- clock skew, etc to be trivial. In other words, \"ideal circumstances\".
--
-- The assumption is useful in this reference implementation because it lets us
-- maintain a single \"global\" 'State' common to all nodes.
--
step :: PBftParams -> NodeJoinPlan -> State -> State
step params nodeJoinPlan st
  | maybe True (s <) mbJ       = stuck Absent
  | Just s == mbJ, not isFirst = stuck Wasted
  | tooMany params st' i       = stuck Unable
  | otherwise                  = extendOut params st' Nominal
  where
    s = nextSlot st

    -- @s@'s scheduled leader
    i = CoreNodeId $ fromIntegral $ unSlotNo s `mod` oneN params

    -- when @i@ joins the network
    mbJ = Map.lookup i m where NodeJoinPlan m = nodeJoinPlan

    -- whether @i@ would be the first to lead
    isFirst = nullState st

    -- the state that @i@ would make
    st' = extendId params st i

    stuck o = extendOut params st o

-- | Iterate 'step'
--
-- POST 'nextSlot' is @>=@ the given slot
--
advanceUpTo :: PBftParams -> NodeJoinPlan -> State -> SlotNo -> State
advanceUpTo params nodeJoinPlan = go
  where
    go st s
      | nextSlot st >= s = st
      | otherwise        = go (step params nodeJoinPlan st) s

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Finish an incomplete 'NodeJoinPlan': all remaining nodes join ASAP
--
-- Specifically, \"ASAP\" is either when the last already-scheduled node joins
-- or \"now\" (the given 'SlotNo'), whichever is latest.
--
fillOut :: PBftParams -> NodeJoinPlan -> SlotNo -> NodeJoinPlan
fillOut params (NodeJoinPlan m) s =
    NodeJoinPlan $
    foldl' (\acc i -> Map.insert i j acc) m $
    CoreNodeId <$> [i0 .. iN]
  where
    iN       = oneN params - 1
    j        = max s j0
    (i0, j0) = case Map.lookupMax m of
        Nothing                -> (0,      0)
        Just (CoreNodeId h, x) -> (succ h, x)

-- | @True@ if there will be no necessary violations of \"@k@-blocks in
-- @2k@-slots\" assuming all remaining nodes join ASAP
--
-- PRE the given parameters and state are not @tooSparse params st@
--
viable :: PBftParams -> SlotNo -> NodeJoinPlan -> State -> Bool
viable params lastSlot nodeJoinPlan st0 = go st0
  where
    nodeJoinPlan' = fillOut params nodeJoinPlan (nextSlot st0)

    go st
      | lastSlot < nextSlot st = True
      | saturated params st    = True   -- an optimization
      | tooSparse params st'   = False
      | otherwise              = go st'
      where
        st' = step params nodeJoinPlan' st

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | A reference simulator of the RealPBFT protocol under \"ideal
-- circumstances\"
--
-- See 'step'.
--
module Test.Dynamic.Ref.RealPBFT (
  Outcome (..),
  State (..),
  advanceUpTo,
  deterministicPlan,
  emptyState,
  nullState,
  step,
  viable,
  ) where

import           Data.Foldable (Foldable, foldl', toList)
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Word (Word64)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime.Mock (NumSlots (..))
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
    --
    -- (We're using /nominal/ in the engineer's sense of \"according to
    -- plan\".)

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
  { forgers  :: !(Seq CoreNodeId)
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
nullState State{forgers} = Seq.null forgers

-- | Record the latest forging in the state
--
-- Should be followed by a call to 'extendOutcome'.
--
extendForger :: PBftParams -> State -> CoreNodeId -> State
extendForger params st@State{forgers} i = st
  { forgers = snd $ prune (oneK params) $ forgers Seq.|> i
  }

count :: (Eq a, Foldable t) => a -> t a -> Int
count a bs = length [ () | b <- toList bs, a == b ]

prune :: Int -> Seq a -> (Seq a, Seq a)
prune lim x = Seq.splitAt excess x
  where
    excess = Seq.length x - fromIntegral lim

-- | Record the latest outcome in the state
--
extendOutcome :: PBftParams -> State -> Outcome -> State
extendOutcome params State{forgers, nomCount, nextSlot, outs} out = State
  { forgers
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
tooMany :: PBftParams -> State -> Bool
tooMany params st@State{forgers} =
    not $
    Seq.length forgers < k || count i forgers <= pbftLimit params
  where
    k :: forall a. Num a => a
    k = oneK params

    i = nextLeader params st

-- | How many blocks in the latest @k@-blocks that a single core node is
-- allowed to have signed
pbftLimit :: Integral a => PBftParams -> a
pbftLimit params = floor $ oneK params * pbftSignatureThreshold
  where
    PBftParams{pbftSignatureThreshold} = params

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
  | maybe True (s <) mbJ  = stuck Absent
  | joinLead, not isFirst = stuck Wasted
  | tooMany params st'    = stuck Unable
  | otherwise             = extendOutcome params st' Nominal
  where
    s = nextSlot st

    -- @s@'s scheduled leader
    i = nextLeader params st

    -- when @i@ joins the network
    mbJ = Map.lookup i m where NodeJoinPlan m = nodeJoinPlan

    -- @i@ is joining and also leading
    joinLead = Just s == mbJ

    -- whether @i@ would be the first to lead
    isFirst = nullState st

    -- the state that @i@ would make
    st' = extendForger params st i

    stuck o = extendOutcome params st o

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

-- | The scheduled leader of 'nextSlot'
nextLeader :: PBftParams -> State -> CoreNodeId
nextLeader params State{nextSlot} =
  CoreNodeId $ fromIntegral $ unSlotNo nextSlot `mod` oneN params

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

-- | This node join plan does not introduce any ambiguity
--
-- There is currently only one source of ambiguity. Suppose only one slot has
-- been lead, so the net has a single one-block chain. If the second lead slot
-- is lead by a node that also joins in that same slot, then that node will
-- forge its own one-block chain before it synchronizes with the net. This
-- process may continue for the third lead slot, and so on. It necessarily
-- stops in one of two ways:
--
--   * One of the competing nodes leads for a second time. Because of
--     round-robin, this will be the first leader again.
--
--   * A new node leads in a slot after its join slot. It will have
--     synchronized with the net and chosen one of the one-block chains. It
--     will forge a block atop that, and then the whole network will choose
--     this new longest chain. This reference simulator, in general, cannot
--     anticipate which of the competing one-block chains this node will have
--     selected, so this case is considered non-deterministic.
--
-- Once the net contains a chain with more than one-block, there will never be
-- anymore contention. Nodes might still join and immediately forge their own
-- one-block chain, but it will not be competitive with the net's necessarily
-- longer chain.
deterministicPlan :: NumSlots -> NodeJoinPlan -> Bool
deterministicPlan (NumSlots t) (NodeJoinPlan m) =
    (t < 1 ||) $
    (checkFromGenesis $ sortOn (\(_, _, l1, _) -> l1) $ map mk (Map.toList m))
  where
    n = fromIntegral $ Map.size m :: Word64

    mk (i, joinSlot) = (i, joinSlot, l1, l1 + SlotNo n)
      where
        l1 = lead1 (i, joinSlot)

    -- the first slot this node will lead
    lead1 (CoreNodeId (fromIntegral -> i), SlotNo joinSlot) =
        SlotNo $ joinSlot + d'
      where
        l = joinSlot `mod` n
        d' = (if l > i then n else 0) + i - l

    -- the net has all empty chains
    checkFromGenesis = \case
      []               -> True
      (_, _, _, l2):xs -> checkFromSingle l2 xs

    -- the net has a single one-block chain
    checkFromSingle l2 = \case
      []                      -> True
      (_, joinSlot, l1', _):xs
        | l2 < l1'            -> True
        | joinSlot < l1'      -> True
        | otherwise           -> checkFromAtRisk l2 xs

    lastSlot = SlotNo $ t - 1

    -- the net has multiple one-block chains
    checkFromAtRisk l2 = \case
      []                       -> lastSlot >= l2
      (_, joinSlot, l1', _):xs
        | l2 < l1'             -> True
        | joinSlot < l1'       -> False
        | otherwise            -> checkFromAtRisk l2 xs

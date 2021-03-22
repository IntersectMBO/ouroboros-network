{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A reference simulator of the PBFT protocol under \"ideal circumstances\"
--
-- See 'step'.
--
module Test.ThreadNet.Ref.PBFT (
    Outcome (..)
  , Result (..)
  , State (..)
  , advanceUpTo
  , definitelyEnoughBlocks
  , emptyState
  , mkLeaderOf
  , nullState
  , pbftLimit
  , resultConstrName
  , simulate
  , simulateShort
  , step
  , viable
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import           Data.Foldable (foldl', toList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT (PBftParams (..),
                     PBftSignatureThreshold (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Test.ThreadNet.Util.NodeJoinPlan

import           Test.Util.InvertedMap (InvertedMap)
import qualified Test.Util.InvertedMap as InvertedMap
import           Test.Util.Slots (NumSlots (..))

oneK :: Num a => PBftParams -> a
oneK PBftParams{pbftSecurityParam} =
    fromIntegral (maxRollbacks pbftSecurityParam)

twoK :: Num a => PBftParams -> a
twoK PBftParams{pbftSecurityParam} =
    2 * fromIntegral (maxRollbacks pbftSecurityParam)

oneN :: Num a => PBftParams -> a
oneN PBftParams{pbftNumNodes = NumCoreNodes n} = fromIntegral n

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

instance Condense Outcome where
  condense = show

-- | The state of a PBFT net with only one longest chain
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
    excess = Seq.length x - lim

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
    count i forgers > pbftLimit params
  where
    i = nextLeader params st

-- | How many blocks in the latest @k@-blocks that a single core node is
-- allowed to have signed
--
pbftLimit :: Integral a => PBftParams -> a
pbftLimit params =
    floor $ oneK params * getPBftSignatureThreshold pbftSignatureThreshold
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
step :: PBftParams -> NodeJoinPlan -> State -> (Outcome, State)
step params nodeJoinPlan st
  | maybe True (s <) mbJ  = (id &&& stuck) Absent
  | joinLead, not isFirst = (id &&& stuck) Wasted
  | tooMany params st'    = (id &&& stuck) Unable
  | otherwise             = (id &&& extendOutcome params st') Nominal
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
      | otherwise        = go (snd $ step params nodeJoinPlan st) s

-- | The result of a reference simulation
--
-- There is only one way that multiple competing chains arise given the PBFT
-- protocol under \"ideal circumstances\". When a node leads the slot in which
-- it joins, it will forge before syncing. The fresh one-block chain it creates
-- this way will be competitive if the other chains in the net are also only
-- one block long. Once there is a two-block chain in the net, it will be the
-- prefix of the net's one common chain going forward.
--
-- It is possible that the simulator is unable to statically determine which
-- one-block chain will be the prefix of the common prefix going forward. In
-- such a 'Nondeterministic' scenario, the simulator inherently can not predict
-- up to that slot. Moreover, it cannot predict past that slot, since the
-- choice of chain affects who may be able to lead next. Therefore, the
-- simulator offers no information beyond identifying this case.
--
data Result
  = Forked !NumSlots !(Map CoreNodeId (Set SlotNo))
    -- ^ All the chains in the net consist of 1 block. The map contains the
    -- deterministic block selections made by nodes, identifying each one-block
    -- chain by the slot in which its block was forged.
  | Nondeterministic
    -- ^ The outcomes cannot be determined statically.
  | Outcomes ![Outcome]
    -- ^ The expected outcomes of each slot.
  deriving (Show)

resultConstrName :: Result -> String
resultConstrName = \case
  Outcomes{}       -> "Outcomes"
  Forked{}         -> "Forked"
  Nondeterministic -> "Nondeterministic"

-- | Run 'simulateStep', switching to iterating 'step' once there is a 2-block
-- chain
--
-- See 'Result'.
--
simulate
  :: HasCallStack => PBftParams -> NodeJoinPlan -> NumSlots -> Result
simulate params nodeJoinPlan numSlots =
    case simulateShort params nodeJoinPlan numSlots of
      Left (Det st@State{outs}) -> Outcomes $ toList outs <> go st
      Left Nondet               -> Nondeterministic
      Right st                  -> Forked numSlots $ selected <> forged
        where
          ShortState{ssChains, ssJoined} = st

          -- nodes that forged a chain definitely selected it
          forged :: Map CoreNodeId (Set SlotNo)
          forged = Set.singleton <$> ssChains

          -- each chain is identified by the slot of the only block in the
          -- chain
          chains :: Set SlotNo
          chains = foldMap Set.singleton ssChains

          -- nodes that did not forge their own chain select among the first
          -- available
          selected :: Map CoreNodeId (Set SlotNo)
          selected = flip Map.map ssJoined $ \joinSlot ->
            case Set.lookupMin chains of
              -- if no chains /ever/ exist, the node selects none
              Nothing           -> Set.empty
              -- the first chain was forged in this slot
              Just s
                -- if the node joined before or during the forging of the first
                -- chain, it selects it
                | joinSlot <= s -> Set.singleton s
                -- the node selects from among the chains that existed when it
                -- joined
                | otherwise     ->
                  (fst . Set.split (succ joinSlot)) $
                  chains
  where
    NumSlots t = numSlots

    go st
      | nextSlot st >= SlotNo t = []
      | otherwise               = o : go st'
      where
        (o, st') = step params nodeJoinPlan st

{-------------------------------------------------------------------------------
  Prior to a 2-block chain arising
-------------------------------------------------------------------------------}

data DetOrNondet
  = Det !State
    -- ^ The plan is /deterministic/ and results in the given 'State'.
  | Nondet
    -- ^ The plan is /nondeterministic/; we cannot predict which 'State' it
    -- yields.
    --
    -- See 'Result'.

simulateShort
  :: HasCallStack
  => PBftParams -> NodeJoinPlan -> NumSlots -> Either DetOrNondet ShortState
simulateShort params nodeJoinPlan (NumSlots t)
  -- no one has time to forge
  | 0 == t    = Left $ Det emptyState
  | otherwise = go $ emptyShortState nodeJoinPlan
  where
    go :: ShortState -> Either DetOrNondet ShortState
    go st = do
      st'@ShortState{ssNextSlot = s'} <- stepShort params st
      (if s' < SlotNo t then go else Right) st'

-- | The state of a PBFT net with no chains or any number of one-blocks chains
--
-- See 'Result'.
--
data ShortState = ShortState
  { ssAbsent   :: !(InvertedMap SlotNo CoreNodeId)
    -- ^ The nodes that haven't yet joined
    --
    -- INVARIANT @all (>= 'ssNextSlot') 'ssAbsent'@
  , ssChains   :: !(Map CoreNodeId SlotNo)
    -- ^ The nodes that have forged a one-block chain and when
  , ssJoined   :: !(Map CoreNodeId SlotNo)
    -- ^ The nodes that have joined in previous slots
  , ssNextSlot :: !SlotNo
  }
  deriving Show

-- | Origin: no joined nodes and no chains
--
emptyShortState :: NodeJoinPlan -> ShortState
emptyShortState (NodeJoinPlan m) = ShortState
  { ssAbsent   = InvertedMap.fromMap m
  , ssChains   = Map.empty
  , ssJoined   = Map.empty
  , ssNextSlot = 0
  }

stepShort
  :: HasCallStack
  => PBftParams -> ShortState -> Either DetOrNondet ShortState
stepShort params st
  | Just don <- mbDon = Left don
  | otherwise         = Right ShortState
    { ssAbsent   = ssAbsent'
    , ssChains   = ssChains'
    , ssJoined   = ssJoined'
    , ssNextSlot = ssNextSlot'
    }
  where
    ShortState{ssAbsent, ssChains, ssJoined, ssNextSlot} = st

    leaderOf :: SlotNo -> CoreNodeId
    leaderOf = mkLeaderOf params

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf =
      coreNodeIdJoinSlot $ NodeJoinPlan $ ssJoined' <> InvertedMap.toMap ssAbsent'

    (ssAbsent', ssJoined') =
        (later, ssJoined <> InvertedMap.toMap now)
      where
        (now, later) = assertInvariant $
                       InvertedMap.spanAntitone (== ssNextSlot) ssAbsent
    ssChains'              =
      maybe id (\l -> Map.insert l ssNextSlot) mbLeader $
      ssChains
    ssNextSlot'            = succ ssNextSlot

    assertInvariant :: forall a. a -> a
    assertInvariant x = case InvertedMap.minViewWithKey ssAbsent of
      Just ((s, _), _) | s < ssNextSlot -> error $
        "ShortState invariant violation: " <> show (ssNextSlot, ssAbsent)
      _                                 -> x

    -- 'Just' if we can decide 'DetOrNondet'
    mbDon :: Maybe DetOrNondet
    mbDon =
      degenerateLim <|>
      detSecondLead <|>
      choiceMade <|>
      detAllHaveJoinedWithoutMultipleChains

    -- each node is 'Unable' to lead even once
    degenerateLim :: Maybe DetOrNondet
    degenerateLim = do
      guard $ (0 :: Word64) == pbftLimit params
      Just $ Det State
        { forgers  = Seq.empty
        , nomCount = NumNominals 0
        , nextSlot = ssNextSlot'
        , outs     = Seq.fromList $
            [ if s < joinSlotOf (leaderOf s) then Absent else Unable
            | s <- [0 .. ssNextSlot]
            ]
        }

    -- a node is leading for the second time; it has won the race
    detSecondLead :: Maybe DetOrNondet
    detSecondLead = do
      winner   <- mbLeader
      leadSlot <- Map.lookup winner ssChains
      Just $ Det State
        { forgers  = Seq.fromList [winner, winner]
        , nomCount = NumNominals 2
        , nextSlot = ssNextSlot'
        , outs     = Seq.fromList $
          [ let l = leaderOf s in
            if | s == leadSlot || s == ssNextSlot -> Nominal
               | s < joinSlotOf l                 -> Absent
               | Just s == Map.lookup l ssChains  -> Wasted
               | otherwise                        -> Unable
          | s <- [0 .. ssNextSlot]
          ]
        }

    -- old node is leading for the first time in a slot later than its join
    -- slot
    choiceMade :: Maybe DetOrNondet
    choiceMade = do
      leader <- mbLeader
      guard $ leader `Map.notMember` ssChains
      guard $ leader `Map.member` ssJoined
      case Map.toList ssChains of
        -- it's merely forging the first 1-block chain
        []                   -> Nothing

        [(winner, leadSlot)] -> Just $ Det State
          { forgers  = Seq.fromList [winner, leader]
          , nomCount = NumNominals 2
          , nextSlot = ssNextSlot'
          , outs     = Seq.fromList
            [ let l = leaderOf s in
              if | s == leadSlot || s == ssNextSlot -> Nominal
                 | s < joinSlotOf l                 -> Absent
                 | Just s == Map.lookup l ssChains  -> Wasted
                 | otherwise                        -> Unable
            | s <- [0 .. ssNextSlot]
            ]
          }

        _:_:_                -> Just Nondet

    -- all nodes have joined and there are not multiple chains
    detAllHaveJoinedWithoutMultipleChains :: Maybe DetOrNondet
    detAllHaveJoinedWithoutMultipleChains = do
      guard $ InvertedMap.null ssAbsent'
      case Map.toList ssChains' of
        -- the degenerateLim guard should have prevented this evaluation; we
        -- know no other way for this state to be reachable
        []                   -> error "impossible!"

        [(winner, leadSlot)] -> Just $ Det State
          { forgers  = Seq.singleton winner
          , nomCount = NumNominals 1
          , nextSlot = ssNextSlot'
          , outs     = Seq.fromList
            [ let l = leaderOf s in
              if | s == leadSlot                   -> Nominal
                 | s < joinSlotOf l                -> Absent
                 | Just s == Map.lookup l ssChains -> Wasted
                 | otherwise                       -> Unable
            | s <- [0 .. ssNextSlot]
            ]
          }

        _:_:_                -> Nothing

    -- the node that successfully leads in ssNextSlot
    mbLeader :: Maybe CoreNodeId
    mbLeader
      -- node is 'Absent'
      | not $ cid `Map.member` ssJoined'
      = Nothing

      -- each node is 'Unable' to lead twice before all lead once
      --
      -- Note: This whole function will reach a 'DetOrNondet' before any node
      -- leads twice.
      | (1 :: Word64) < oneK params
      , (1 :: Word64) == pbftLimit params
      , cid `Map.member` ssChains
      = Nothing

      | otherwise = Just cid
      where
        -- the node scheduled to lead in ssNextSlot
        cid = leaderOf ssNextSlot

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | @True@ if there will be no necessary violations of \"@k@-blocks in
-- @2k@-slots\" before the given slot, assuming all remaining nodes join ASAP
--
-- PRE the given parameters and state are not already @tooSparse params st@
--
-- NOTE this function does not consider the competition between 1-block
-- multichains, so 'definitelyEnoughBlocks' must still be checked to confirm
--
viable :: PBftParams -> SlotNo -> NodeJoinPlan -> State -> Bool
viable params sentinel nodeJoinPlan st0 = go st0
  where
    nodeJoinPlan' = fillOut params nodeJoinPlan (nextSlot st0)

    go st
      | sentinel <= nextSlot st = True
      | saturated params st     = True   -- an optimization
      | tooSparse params st'    = False
      | otherwise               = go st'
      where
        (_o, st') = step params nodeJoinPlan' st

-- | Confirm that the simulated chain includes at least @k@ blocks within every
-- @2k@-slot window
--
definitelyEnoughBlocks :: PBftParams -> Result -> Bool
definitelyEnoughBlocks params = \case
    Forked numSlots m ->
      let NumSlots t = numSlots
          nominals   = if Map.null m then 0 else 1
          badCount   = max 0 $ t - nominals
      in badCount < k
    Nondeterministic  -> False
    Outcomes outcomes ->
      let enters = map tick outcomes
          exits  = replicate (2 * fromIntegral k) 0 ++ enters
      in go 0 $ zip exits enters
  where
    PBftParams{pbftSecurityParam} = params
    k = maxRollbacks pbftSecurityParam

    tick :: Outcome -> Word64
    tick Nominal = 0
    tick _       = 1

    go :: Word64 -> [(Word64, Word64)] -> Bool
    go badCount exens
      | badCount > k = False
      | otherwise    = case exens of
          []                     -> True
          (exit, enter) : exens' -> go (badCount - exit + enter) exens'

{-------------------------------------------------------------------------------
  Auxiliaries
-------------------------------------------------------------------------------}

mkLeaderOf :: PBftParams -> SlotNo -> CoreNodeId
mkLeaderOf params (SlotNo s) =
    CoreNodeId $ s `mod` n
  where
    PBftParams{pbftNumNodes} = params
    NumCoreNodes n           = pbftNumNodes

-- | The scheduled leader of 'nextSlot'
--
nextLeader :: PBftParams -> State -> CoreNodeId
nextLeader params State{nextSlot} = mkLeaderOf params nextSlot

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

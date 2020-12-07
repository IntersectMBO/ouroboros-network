{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Scratch.ProtocolCombinator (
  Moderator,
  initialModeratorChainDepState,
  ) where

import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Maybe (isJust)
import           Data.Proxy (Proxy (..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (SlotNo)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked

import           Test.Util.Slots (NumSlots (..))

{-------------------------------------------------------------------------------
  The protocol combinator
-------------------------------------------------------------------------------}

-- | Extends protocol @p@ such that the leader schedule is partially overrided
-- to incur a /reset/, which ensures that there is a single longest chain in
-- the net and all nodes (have the opportunity to) select it, and does so "
-- often enough ".
data Moderator p

{-------------------------------------------------------------------------------
  The moderator's history
-------------------------------------------------------------------------------}

data History = History
  { nextSlot          :: !SlotNo
  , recentActiveSlots :: !(Seq SlotNo)
  }
  deriving (Eq, Generic, NoThunks, Show)

-- | Update the underlying chain state without affecting the history
--
-- Compare this to a type-constrained lens for 'moderatorCdstUnderlying' \/
-- 'tickedModeratorCdstUnderlying'.
updateUnderlyingCdstVia ::
     Functor f
  => Ticked (ModeratorChainDepState p)
  -> (Ticked (ChainDepState p) -> f (ChainDepState p))
  -> f (ModeratorChainDepState p)
updateUnderlyingCdstVia tickedCdst f =
    f x <&> \y -> ModeratorChainDepState
      { moderatorCdstCache      = tickedModeratorCdstCache tickedCdst
      , moderatorCdstCache2     = Just x
      , moderatorCdstHistory    =
          -- adding a header does not advance time, and so does not affect
          -- the moderator history
          tickedModeratorCdstHistory tickedCdst
      , moderatorCdstUnderlying = y
      }
  where
    x = tickedModeratorCdstUnderlying tickedCdst

-- | Tick the 'History'
--
-- Suppose the last header applied to the selected chain was in slot U.
--
-- Suppose the current slot is slot V. (TODO because of EBBs we might have U =
-- V.)
--
-- Given the moderator chain state in slot U {- as well as the result of
-- ticking its underlying chain state forward to slot V -}, this function
-- determines which slots from U to V exclusive were /active/.
--
-- PREREQUISITE Slot V is either in the same epoch as U or else in the
-- subsequent epoch.
--
-- TODO How to handle PBFT for Byron? The Permissive part is dependent on the
-- actual chains, which we might not have yet due to delays, etc. Current idea:
-- be conservative and assume that every node was always able to lead. The
-- current implementation cannot do that, since it merely opaquely uses
-- whatever the chain dep state happens to be (same other node may see a more
-- recent chain, whose PBFT signatures window no longer prevents it from
-- leading, etc etc). One (disappointing) option: we could override
-- 'checkIsLeader' via an extra field in the 'ConsensusConfig (Moderator p)'.
tickHistory :: forall p.
     ConsensusProtocol p
  => ConsensusConfig (Moderator p)
  -> Ticked (ChainDepState p)
     -- ^ the state as of the requested slot
  -> SlotNo
     -- ^ the requested slot
  -> ModeratorChainDepState p
     -- ^ the latest state from some slot before the requested slot
  -> History
tickHistory cfg nextTickedCdst slot cdst =
    go moderatorCdstHistory
  where
    ModeratorConfig
      { moderatorConfigCanBeLeaders
      , moderatorConfigSameEpoch
      , moderatorConfigUnderlying
      } = cfg

    ModeratorChainDepState
      { moderatorCdstCache
      , moderatorCdstCache2
      , moderatorCdstHistory
      } = cdst

    go :: History -> History
    go history =
        if nextSlot > slot then history else
        go History
          { nextSlot          = succ nextSlot
          , recentActiveSlots =
              if not isActive then recentActiveSlots else
              recentActiveSlots Seq.|> nextSlot
          }
      where
        History
          { nextSlot
          , recentActiveSlots
          } = history

        -- The state suitable for use in the nextSlot leader check
        --
        -- For Shelley, this logic is sufficient because it ensures the used
        -- chain state is from the same epoch as the slot being checked, hence
        -- it uses the correct nonce etc.
        tickedCdst :: Ticked (ChainDepState p)
        tickedCdst =
            if   moderatorConfigSameEpoch moderatorCdstCache nextSlot
            then maybe (error "todo") id moderatorCdstCache2
            else nextTickedCdst

        isActive :: Bool
        isActive =
            any isJust $
            [ checkIsLeader
                moderatorConfigUnderlying
                canBeLeader
                nextSlot
                tickedCdst
            | canBeLeader <- moderatorConfigCanBeLeaders
            ]

{-------------------------------------------------------------------------------
  ConsensusProtocol instance
-------------------------------------------------------------------------------}

-- | Chain selection is unchanged
instance ChainSelection p => ChainSelection (Moderator p) where
  type ChainSelConfig (Moderator p) = ChainSelConfig p
  type SelectView     (Moderator p) = SelectView     p

  compareChains _ = compareChains (Proxy @p)

data instance ConsensusConfig (Moderator p) = ModeratorConfig
  { moderatorConfigCanBeLeaders :: ![CanBeLeader p]
  , moderatorConfigDelta        :: !NumSlots
  , moderatorConfigMaxDepth     :: !Word64
  , moderatorConfigSameEpoch    :: !(Maybe SlotNo -> SlotNo -> Bool)
  , moderatorConfigUnderlying   :: !(ConsensusConfig p)
  }
  deriving (Generic)

instance (ConsensusProtocol p, NoThunks (CanBeLeader p))
      => NoThunks (ConsensusConfig (Moderator p))

data ModeratorChainDepState p = ModeratorChainDepState
  { moderatorCdstCache      :: !(Maybe SlotNo)
  , moderatorCdstCache2     :: !(Maybe (Ticked (ChainDepState p)))
  , moderatorCdstHistory    :: !History
    -- ^ includes the slot of the last header included in
    -- 'moderatorCdstUnderlying'
  , moderatorCdstUnderlying :: !(ChainDepState p)
  }
  deriving (Generic)

initialModeratorChainDepState :: ChainDepState p -> SlotNo -> ModeratorChainDepState p
initialModeratorChainDepState x nextSlot = ModeratorChainDepState
  { moderatorCdstCache      = Nothing
  , moderatorCdstCache2     = Nothing
  , moderatorCdstHistory    = History {..}
  , moderatorCdstUnderlying = x
  }
  where
    recentActiveSlots = Seq.empty

-- undecidable
deriving instance (Eq   (Ticked (ChainDepState p)), Eq   (ChainDepState p)) => Eq   (ModeratorChainDepState p)
deriving instance (Show (Ticked (ChainDepState p)), Show (ChainDepState p)) => Show (ModeratorChainDepState p)

instance (ConsensusProtocol p, NoThunks (Ticked (ChainDepState p)))
      => NoThunks (ModeratorChainDepState p)

data instance Ticked (ModeratorChainDepState p) = TickedModeratorChainDepState
  { tickedModeratorCdstCache      :: !(Maybe SlotNo)
  , tickedModeratorCdstHistory    :: !History
    -- ^ excludes the slot to which 'tickedModeratorCdstUnderlying' was ticked
  , tickedModeratorCdstUnderlying :: !(Ticked (ChainDepState p))
  }
  deriving (Generic)

instance (ConsensusProtocol p, Eq (Ticked (ChainDepState p)), Show (Ticked (ChainDepState p)), NoThunks (Ticked (ChainDepState p)), NoThunks (CanBeLeader p))
      => ConsensusProtocol (Moderator p) where
  type CanBeLeader   (Moderator p) = CanBeLeader            p
  type ChainDepState (Moderator p) = ModeratorChainDepState p
  type IsLeader      (Moderator p) = IsLeader               p
  type LedgerView    (Moderator p) = LedgerView             p
  type ValidateView  (Moderator p) = ValidateView           p
  type ValidationErr (Moderator p) = ValidationErr          p

  protocolSecurityParam = protocolSecurityParam . moderatorConfigUnderlying
  chainSelConfig        = chainSelConfig        . moderatorConfigUnderlying

  checkIsLeader cfg canBeLeader slot tickedCdst =
      -- TODO override this according to 'tickedModeratorCdstHistory'
      checkIsLeader
        (moderatorConfigUnderlying cfg)
        canBeLeader
        slot
        (tickedModeratorCdstUnderlying tickedCdst)

  tickChainDepState cfg tickedLedgerView slot cdst =
      TickedModeratorChainDepState
        { tickedModeratorCdstCache      =
            Just slot
        , tickedModeratorCdstHistory    =
            tickHistory cfg tickedModeratorCdstUnderlying slot cdst
        , tickedModeratorCdstUnderlying
        }
    where
      tickedModeratorCdstUnderlying =
        tickChainDepState
          (moderatorConfigUnderlying cfg)
          tickedLedgerView
          slot
          (moderatorCdstUnderlying cdst)

  updateChainDepState cfg validateView slot tickedCdst =
      updateUnderlyingCdstVia tickedCdst $
      updateChainDepState
        (moderatorConfigUnderlying cfg)
        validateView
        slot

  reupdateChainDepState cfg validateView slot tickedCdst =
      runIdentity $
      updateUnderlyingCdstVia tickedCdst $
      pure @Identity .
      reupdateChainDepState
        (moderatorConfigUnderlying cfg)
        validateView
        slot

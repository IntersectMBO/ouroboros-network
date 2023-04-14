{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- | Working with the Byron spec chain state
module Ouroboros.Consensus.ByronSpec.Ledger.Accessors (
    -- * ChainState getters
    GetChainState
  , getChainStateDIState
  , getChainStateHash
  , getChainStateSlot
  , getChainStateUPIState
  , getChainStateUtxoState
    -- * ChainState modifiers
  , ModChainState
  , modChainStateDIState
  , modChainStateSlot
  , modChainStateUPIState
  , modChainStateUtxoState
    -- * Auxiliary
  , getDIStateDSState
  , modDIStateDSState
  ) where

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.STS.UTXO as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

{-------------------------------------------------------------------------------
  Accessors
-------------------------------------------------------------------------------}

type GetChainState    a = Spec.State Spec.CHAIN -> a
type ModChainState a = forall m. Applicative m => (a -> m a)
                       -> Spec.State Spec.CHAIN -> m (Spec.State Spec.CHAIN)

getChainStateSlot :: GetChainState Spec.Slot
getChainStateSlot (a, _, _, _, _, _) = a

modChainStateSlot :: ModChainState Spec.Slot
modChainStateSlot fn (a, b, c, d, e, f) = (, b, c, d, e, f) <$> fn a

getChainStateHash :: GetChainState Spec.Hash
getChainStateHash (_, _, c, _, _, _) = c

getChainStateUtxoState :: GetChainState Spec.UTxOState
getChainStateUtxoState (_, _, _, d, _, _) = d

modChainStateUtxoState :: ModChainState Spec.UTxOState
modChainStateUtxoState fn (a, b, c, d, e, f) = (a, b, c, , e, f) <$> fn d

getChainStateDIState :: GetChainState Spec.DIState
getChainStateDIState (_, _, _, _, e, _) = e

modChainStateDIState :: ModChainState Spec.DIState
modChainStateDIState fn (a, b, c, d, e, f) = (a, b, c, d, , f) <$> fn e

getChainStateUPIState :: GetChainState Spec.UPIState
getChainStateUPIState (_, _, _, _, _, f) = f

modChainStateUPIState :: ModChainState Spec.UPIState
modChainStateUPIState fn (a, b, c, d, e, f) = (a, b, c, d, e, ) <$> fn f

{-------------------------------------------------------------------------------
  'Spec.DSState' is a sub-state of 'Spec.DIState'

  There is a lens in Ledger.Delegation to do this but we are phasing out @lens@
  across all repos, so don't want to depend on it here
-------------------------------------------------------------------------------}

-- | Extract 'Spec.DSState' from 'Spec.DIState'
getDIStateDSState :: Spec.DIState -> Spec.DSState
getDIStateDSState Spec.DIState{..} = Spec.DSState {
      _dSStateScheduledDelegations = _dIStateScheduledDelegations
    , _dSStateKeyEpochDelegations  = _dIStateKeyEpochDelegations
    }

-- | Update 'Spec.DIState' from 'Spec.DSState'
modDIStateDSState :: Applicative m
                  => (Spec.DSState -> m Spec.DSState)
                  -> Spec.DIState -> m Spec.DIState
modDIStateDSState f diState@Spec.DIState{..} =
    update <$> f (getDIStateDSState diState)
  where
    update :: Spec.DSState -> Spec.DIState
    update Spec.DSState{..} = Spec.DIState{
          _dIStateScheduledDelegations = _dSStateScheduledDelegations
        , _dIStateKeyEpochDelegations  = _dSStateKeyEpochDelegations
          -- The rest stays the same
        , _dIStateDelegationMap        = _dIStateDelegationMap
        , _dIStateLastDelegation       = _dIStateLastDelegation
        }

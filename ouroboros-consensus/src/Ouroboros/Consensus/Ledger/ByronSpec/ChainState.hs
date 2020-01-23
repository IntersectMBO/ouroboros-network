{-# LANGUAGE RecordWildCards #-}

-- | Working with the Byron spec chain state
--
-- Intended for qualified import
--
-- > import qualified Ouroboros.Consensus.Ledger.ByronSpec.ChainState as ChainState
module Ouroboros.Consensus.Ledger.ByronSpec.ChainState (
    -- * Getters
    getSlot
  , getHash
  , getUtxoState
  , getDIState
  , getDSState
  , getUPIState
    -- * Setters
  , setSlot
  , setUtxoState
  , setDIState
  , setDSState
  , setUPIState
  ) where

import qualified Cardano.Ledger.Spec.STS.UTXO as Spec
import qualified Cardano.Spec.Chain.STS.Rule.Chain as Spec
import qualified Control.State.Transition as Spec
import qualified Ledger.Core as Spec
import qualified Ledger.Delegation as Spec
import qualified Ledger.Update as Spec

{-------------------------------------------------------------------------------
  Accessors
-------------------------------------------------------------------------------}

type Getter a = Spec.State Spec.CHAIN -> a
type Setter a = a -> Spec.State Spec.CHAIN -> Spec.State Spec.CHAIN

getSlot :: Getter Spec.Slot
getSlot (a, _, _, _, _, _) = a

setSlot :: Setter Spec.Slot
setSlot a (_, b, c, d, e, f) = (a, b, c, d, e, f)

getHash :: Getter Spec.Hash
getHash (_, _, c, _, _, _) = c

getUtxoState :: Getter Spec.UTxOState
getUtxoState (_, _, _, d, _, _) = d

setUtxoState :: Setter Spec.UTxOState
setUtxoState d (a, b, c, _, e, f) = (a, b, c, d, e, f)

getDIState :: Getter Spec.DIState
getDIState (_, _, _, _, e, _) = e

setDIState :: Setter Spec.DIState
setDIState e (a, b, c, d, _, f) = (a, b, c, d, e, f)

-- There is a lens in Ledger.Delegation to do this but we are phasing out
-- @lens@ across all repos, so don't want to depend on it here
getDSState :: Getter Spec.DSState
getDSState st =
    Spec.DSState
      _dIStateScheduledDelegations
      _dIStateKeyEpochDelegations
  where
    Spec.DIState{..} = getDIState st

-- See comment about @lens@ for 'getDSState'
setDSState :: Setter Spec.DSState
setDSState Spec.DSState{..} st =
    setDIState (updateDIState (getDIState st)) st
  where
    updateDIState :: Spec.DIState -> Spec.DIState
    updateDIState diState = Spec.DIState
      (Spec._dIStateDelegationMap  diState)
      (Spec._dIStateLastDelegation diState)
      _dSStateScheduledDelegations
      _dSStateKeyEpochDelegations

getUPIState :: Getter Spec.UPIState
getUPIState (_, _, _, _, _, f) = f

setUPIState :: Setter Spec.UPIState
setUPIState f (a, b, c, d, e, _) = (a, b, c, d, e, f)

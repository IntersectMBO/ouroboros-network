{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

-- | Combine all Byron transaction types into single union type
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.ByronSpec.Ledger.GenTx (ByronSpecGenTx(..), ByronSpecGenTxErr(..))
-- > import qualified Ouroboros.Consensus.ByronSpec.Ledger.GenTx as GenTx
module Ouroboros.Consensus.ByronSpec.Ledger.GenTx (
    ByronSpecGenTx (..)
  , ByronSpecGenTxErr (..)
  , apply
  , partition
  ) where

import           Codec.Serialise
import           Control.Monad.Trans.Except
import           GHC.Generics (Generic)

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Ouroboros.Consensus.ByronSpec.Ledger.Genesis
                     (ByronSpecGenesis (..))
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Generalized transaction
--
-- The spec doesn't have a type for this, instead splitting the block body
-- into separate lists
data ByronSpecGenTx =
    ByronSpecGenTxDCert Spec.DCert
  | ByronSpecGenTxTx    Spec.Tx
  | ByronSpecGenTxUProp Spec.UProp
  | ByronSpecGenTxVote  Spec.Vote
  deriving (Show, Generic, Serialise)

-- | Transaction errors
--
-- We don't distinguish these from any other kind of CHAIN failure.
newtype ByronSpecGenTxErr = ByronSpecGenTxErr {
      unByronSpecGenTxErr :: [[Spec.PredicateFailure Spec.CHAIN]]
    }
  deriving (Show, Generic, Serialise)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

apply :: ByronSpecGenesis
      -> ByronSpecGenTx
      -> Spec.State Spec.CHAIN
      -> Except ByronSpecGenTxErr (Spec.State Spec.CHAIN)
apply cfg = \genTx -> withExcept ByronSpecGenTxErr . go genTx
  where
    go (ByronSpecGenTxDCert dcert) = Rules.liftSDELEG  cfg dcert
    go (ByronSpecGenTxTx    tx   ) = Rules.liftUTXOW   cfg tx
    go (ByronSpecGenTxUProp prop ) = Rules.liftUPIREG  cfg prop
    go (ByronSpecGenTxVote  vote ) = Rules.liftUPIVOTE cfg vote

partition :: [ByronSpecGenTx]
          -> ( [Spec.DCert]
             , [Spec.Tx]
             , [Spec.UProp]
             , [Spec.Vote]
             )
partition = go ([], [], [], [])
  where
    go (ds, ts, us, vs) []     = (reverse ds, reverse ts, reverse us, reverse vs)
    go (ds, ts, us, vs) (g:gs) =
        case g of
          ByronSpecGenTxDCert d -> go (d:ds,   ts,   us,   vs) gs
          ByronSpecGenTxTx    t -> go (  ds, t:ts,   us,   vs) gs
          ByronSpecGenTxUProp u -> go (  ds,   ts, u:us,   vs) gs
          ByronSpecGenTxVote  v -> go (  ds,   ts,   us, v:vs) gs

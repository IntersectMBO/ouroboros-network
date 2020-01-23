{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

-- | Combine all Byron transaction types into single union type
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.Ledger.ByronSpec.GenTx (ByronSpecGenTx(..), ByronSpecGenTxErr(..))
-- > import qualified Ouroboros.Consensus.Ledger.ByronSpec.GenTx as GenTx
module Ouroboros.Consensus.Ledger.ByronSpec.GenTx (
    ByronSpecGenTx(..)
  , ByronSpecGenTxErr(..)
  , apply
  , partition
  ) where

import           Codec.Serialise
import           Data.Bifunctor
import           GHC.Generics (Generic)

import qualified Cardano.Ledger.Spec.STS.UTXO as Spec
import qualified Cardano.Ledger.Spec.STS.UTXOW as Spec
import qualified Cardano.Spec.Chain.STS.Rule.Chain as Spec
import qualified Control.State.Transition as Spec
import qualified Ledger.Delegation as Spec
import qualified Ledger.Update as Spec
import qualified Ledger.UTxO as Spec

import           Ouroboros.Consensus.Ledger.ByronSpec.Genesis
                     (ByronSpecGenesis (..))
import           Ouroboros.Consensus.Ledger.ByronSpec.Orphans ()
import qualified Ouroboros.Consensus.Ledger.ByronSpec.Rules as Rules

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Generalized transaction
--
-- The spec doesn't have a type for this, instead splitting the block body
-- into separate lists
data ByronSpecGenTx =
    ByronSpecGenTxDCert Spec.DCert
  | ByronSpecGenTxTx    Spec.TxWits
  | ByronSpecGenTxUProp Spec.UProp
  | ByronSpecGenTxVote  Spec.Vote
  deriving (Show, Generic, Serialise)

data ByronSpecGenTxErr =
    ByronSpecGenTxErrDCert  [[Spec.PredicateFailure Spec.SDELEG]]
  | ByronSpecGenTxErrTx     [[Spec.PredicateFailure Spec.UTXOW]]
  | ByronSpecGenTxErrUProp  [[Spec.PredicateFailure Spec.UPIREG]]
  | ByronSpecGenTxErrVote   [[Spec.PredicateFailure Spec.UPIVOTE]]
  deriving (Show, Generic, Serialise)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

apply :: ByronSpecGenesis
      -> ByronSpecGenTx
      -> Spec.State Spec.CHAIN
      -> Either ByronSpecGenTxErr (Spec.State Spec.CHAIN)
apply cfg = \case
    ByronSpecGenTxDCert dcert -> first ByronSpecGenTxErrDCert . Rules.liftSDELEG  cfg dcert
    ByronSpecGenTxTx    tx    -> first ByronSpecGenTxErrTx    . Rules.liftUTXOW   cfg tx
    ByronSpecGenTxUProp prop  -> first ByronSpecGenTxErrUProp . Rules.liftUPIREG  cfg prop
    ByronSpecGenTxVote  vote  -> first ByronSpecGenTxErrVote  . Rules.liftUPIVOTE cfg vote

partition :: [ByronSpecGenTx]
          -> ( [Spec.DCert]
             , [Spec.TxWits]
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

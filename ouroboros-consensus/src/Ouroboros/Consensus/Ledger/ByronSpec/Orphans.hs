{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Missing instances for standard type classes in the Byron spec
module Ouroboros.Consensus.Ledger.ByronSpec.Orphans () where

import           Codec.Serialise
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           GHC.Generics (Generic)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import qualified Cardano.Ledger.Spec.STS.UTXO as Spec
import qualified Cardano.Ledger.Spec.STS.UTXOW as Spec
import qualified Cardano.Spec.Chain.STS.Block as Spec
import qualified Control.State.Transition as Spec
import qualified Ledger.Core as Spec
import qualified Ledger.Delegation as Spec
import qualified Ledger.Update as Spec
import qualified Ledger.UTxO as Spec

{-------------------------------------------------------------------------------
  Serialise

  We just use generic serializers, as this is for testing only anyway.
-------------------------------------------------------------------------------}

instance Serialise Spec.Addr
instance Serialise Spec.ApName
instance Serialise Spec.ApVer
instance Serialise Spec.BkSgnCntT
instance Serialise Spec.Block
instance Serialise Spec.BlockBody
instance Serialise Spec.BlockHeader
instance Serialise Spec.DCert
instance Serialise Spec.DIState
instance Serialise Spec.Epoch
instance Serialise Spec.EpochDiff
instance Serialise Spec.FactorA
instance Serialise Spec.FactorB
instance Serialise Spec.Hash
instance Serialise Spec.Lovelace
instance Serialise Spec.Metadata
instance Serialise Spec.Owner
instance Serialise Spec.PParams
instance Serialise Spec.ProtVer
instance Serialise Spec.Slot
instance Serialise Spec.SlotCount
instance Serialise Spec.SwVer
instance Serialise Spec.Tx
instance Serialise Spec.TxId
instance Serialise Spec.TxIn
instance Serialise Spec.TxOut
instance Serialise Spec.TxWits
instance Serialise Spec.UpAdptThd
instance Serialise Spec.UpdateConstraintViolation
instance Serialise Spec.UpId
instance Serialise Spec.UProp
instance Serialise Spec.UTxO
instance Serialise Spec.UTxOState
instance Serialise Spec.VKey
instance Serialise Spec.VKeyGenesis
instance Serialise Spec.Vote
instance Serialise Spec.Wit

instance Serialise (Spec.PredicateFailure Spec.ADDVOTE)
instance Serialise (Spec.PredicateFailure Spec.SDELEG)
instance Serialise (Spec.PredicateFailure Spec.UPIREG)
instance Serialise (Spec.PredicateFailure Spec.UPIVOTE)
instance Serialise (Spec.PredicateFailure Spec.UPPVV)
instance Serialise (Spec.PredicateFailure Spec.UPREG)
instance Serialise (Spec.PredicateFailure Spec.UPSVV)
instance Serialise (Spec.PredicateFailure Spec.UPV)
instance Serialise (Spec.PredicateFailure Spec.UPVOTE)
instance Serialise (Spec.PredicateFailure Spec.UTXO)
instance Serialise (Spec.PredicateFailure Spec.UTXOW)

instance Serialise a => Serialise (Spec.Sig       a)
instance Serialise a => Serialise (Spec.Threshold a)

{-------------------------------------------------------------------------------
  Missing Eq instances

  We require Eq on the block to check common prefix.
-------------------------------------------------------------------------------}

deriving instance Eq Spec.Block
deriving instance Eq Spec.BlockBody

{-------------------------------------------------------------------------------
  Not all types in cardano-ledger-specs have generic instances
-------------------------------------------------------------------------------}

deriving instance Generic Spec.DIState
deriving instance Generic Spec.EpochDiff
deriving instance Generic Spec.UpdateConstraintViolation
deriving instance Generic Spec.UTxO
deriving instance Generic Spec.UTxOState

deriving instance Generic (Spec.Threshold a)

deriving instance Generic (Spec.PredicateFailure Spec.ADDVOTE)
deriving instance Generic (Spec.PredicateFailure Spec.SDELEG)
deriving instance Generic (Spec.PredicateFailure Spec.UPIREG)
deriving instance Generic (Spec.PredicateFailure Spec.UPIVOTE)
deriving instance Generic (Spec.PredicateFailure Spec.UPPVV)
deriving instance Generic (Spec.PredicateFailure Spec.UPREG)
deriving instance Generic (Spec.PredicateFailure Spec.UPSVV)
deriving instance Generic (Spec.PredicateFailure Spec.UPV)
deriving instance Generic (Spec.PredicateFailure Spec.UPVOTE)
deriving instance Generic (Spec.PredicateFailure Spec.UTXO)
deriving instance Generic (Spec.PredicateFailure Spec.UTXOW)

{-------------------------------------------------------------------------------
  NoUnexpectedThunks
-------------------------------------------------------------------------------}

deriving via AllowThunk Spec.Hash instance NoUnexpectedThunks Spec.Hash

{-------------------------------------------------------------------------------
  Orphans for generic types

  TODO: Unlike the spec types above, this could actually lead to incoherence :/
  TODO: This should move someplace else.
-------------------------------------------------------------------------------}

instance ( Ord k, Ord v
         , Serialise k, Serialise v
         ) => Serialise (Bimap k v) where
  encode = encode . Bimap.toList
  decode = Bimap.fromList <$> decode

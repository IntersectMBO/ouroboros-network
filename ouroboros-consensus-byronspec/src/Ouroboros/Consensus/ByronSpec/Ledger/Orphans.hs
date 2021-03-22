{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Missing instances for standard type classes in the Byron spec
module Ouroboros.Consensus.ByronSpec.Ledger.Orphans () where

import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           GHC.Generics (Generic)

import           Cardano.Binary (enforceSize)
import qualified Cardano.Binary

import qualified Byron.Spec.Chain.STS.Block as Spec
import qualified Byron.Spec.Chain.STS.Rule.BBody as Spec
import qualified Byron.Spec.Chain.STS.Rule.Bupi as Spec
import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Chain.STS.Rule.Epoch as Spec
import qualified Byron.Spec.Chain.STS.Rule.Pbft as Spec
import qualified Byron.Spec.Chain.STS.Rule.SigCnt as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.STS.UTXO as Spec
import qualified Byron.Spec.Ledger.STS.UTXOW as Spec
import qualified Byron.Spec.Ledger.STS.UTXOWS as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Test.Cardano.Chain.Elaboration.Block as Spec.Test

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
instance Serialise Spec.TxBody
instance Serialise Spec.TxId
instance Serialise Spec.TxIn
instance Serialise Spec.TxOut
instance Serialise Spec.Tx
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

instance Serialise Spec.AddvotePredicateFailure
instance Serialise Spec.AdelegPredicateFailure
instance Serialise Spec.AdelegsPredicateFailure
instance Serialise Spec.ApplyVotesPredicateFailure
instance Serialise Spec.BbodyPredicateFailure
instance Serialise Spec.BupiPredicateFailure
instance Serialise Spec.ChainPredicateFailure
instance Serialise Spec.DelegPredicateFailure
instance Serialise Spec.EpochPredicateFailure
instance Serialise Spec.PbftPredicateFailure
instance Serialise Spec.PvbumpPredicateFailure
instance Serialise Spec.SdelegPredicateFailure
instance Serialise Spec.SdelegsPredicateFailure
instance Serialise Spec.SigcntPredicateFailure
instance Serialise Spec.UpendPredicateFailure
instance Serialise Spec.UpiecPredicateFailure
instance Serialise Spec.UpiendPredicateFailure
instance Serialise Spec.UpiregPredicateFailure
instance Serialise Spec.UpivotePredicateFailure
instance Serialise Spec.UpivotesPredicateFailure
instance Serialise Spec.UppvvPredicateFailure
instance Serialise Spec.UpregPredicateFailure
instance Serialise Spec.UpsvvPredicateFailure
instance Serialise Spec.UpvPredicateFailure
instance Serialise Spec.UpvotePredicateFailure
instance Serialise Spec.UtxoPredicateFailure
instance Serialise Spec.UtxowPredicateFailure
instance Serialise Spec.UtxowsPredicateFailure

instance Serialise a => Serialise (Spec.Sig       a)
instance Serialise a => Serialise (Spec.Threshold a)

{-------------------------------------------------------------------------------
  Test infrastructure
-------------------------------------------------------------------------------}

instance Serialise Spec.Test.AbstractToConcreteIdMaps where
  encode AbstractToConcreteIdMaps{..} = mconcat [
        encodeListLen 2
      , encode (ToFromCBOR <$> transactionIds)
      , encode (ToFromCBOR <$> proposalIds)
      ]

  decode = do
    enforceSize "AbstractToConcreteIdMaps" 2
    transactionIds <- fmap unToFromCBOR <$> decode
    proposalIds    <- fmap unToFromCBOR <$> decode
    return $ AbstractToConcreteIdMaps{..}

{-------------------------------------------------------------------------------
  Missing Eq instances

  We require Eq on the block to check common prefix.
-------------------------------------------------------------------------------}

deriving instance Eq Spec.Block
deriving instance Eq Spec.BlockBody

{-------------------------------------------------------------------------------
  Not all types in cardano-ledger-specs have generic instances
-------------------------------------------------------------------------------}

deriving instance Generic Spec.UTxO

deriving instance Generic (Spec.Threshold a)

deriving instance Generic Spec.BbodyPredicateFailure
deriving instance Generic Spec.BupiPredicateFailure
deriving instance Generic Spec.ChainPredicateFailure
deriving instance Generic Spec.EpochPredicateFailure
deriving instance Generic Spec.PbftPredicateFailure
deriving instance Generic Spec.SigcntPredicateFailure

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

{-------------------------------------------------------------------------------
  Auxiliary: Cardano.Binary.ToCBOR/FromCBOR to Serialise bridge
-------------------------------------------------------------------------------}

newtype ToFromCBOR a = ToFromCBOR { unToFromCBOR :: a }

instance ( Cardano.Binary.ToCBOR   a
         , Cardano.Binary.FromCBOR a
         ) => Serialise (ToFromCBOR a) where
  encode = Cardano.Binary.toCBOR . unToFromCBOR
  decode = ToFromCBOR <$> Cardano.Binary.fromCBOR

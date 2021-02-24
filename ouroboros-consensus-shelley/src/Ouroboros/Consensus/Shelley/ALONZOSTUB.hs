{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Shelley.ALONZOSTUB (
    module Ouroboros.Consensus.Shelley.ALONZOSTUB
  ) where

import           Data.Default.Class
import           Data.Group
import           Data.Sequence (Seq)
import           Data.Sequence.Strict (StrictSeq)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           GHC.Natural (Natural)
import           GHC.TypeLits (KnownSymbol, Symbol)
import           NoThunks.Class

import           Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Binary as CB
import qualified Cardano.Ledger.AuxiliaryData as CL
import qualified Cardano.Ledger.Compactible as Core
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Era as CL
import           Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.SafeHash as CL
import qualified Cardano.Ledger.Shelley.Constraints as SL
import qualified Cardano.Ledger.Val as CL
import qualified Control.State.Transition.Extended as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.CompactAddr as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Bbody as SL
import qualified Shelley.Spec.Ledger.STS.Delegs as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as SL
import qualified Shelley.Spec.Ledger.STS.Ledgers as SL
import qualified Shelley.Spec.Ledger.STS.Tick as SL
import qualified Shelley.Spec.Ledger.Tx as SL




data AlonzoEra c
data AlonzoPParams = AlonzoPParams { _a0 :: Rational , _d :: SL.UnitInterval , _extraEntropy :: SL.Nonce , _maxBBSize :: Natural , _maxBHSize :: Natural , _maxTxSize :: Natural , _nOpt :: Natural , _protocolVersion :: SL.ProtVer , _rho :: SL.UnitInterval , _tau :: SL.UnitInterval } deriving (Eq, Generic, NoThunks, Show)
data AlonzoTxBody c = AlonzoTxBody { outputs :: StrictSeq (SL.TxOut (AlonzoEra c)) } deriving (Eq, Generic, Show)
data AlonzoTxOut c = AlonzoTxOut { address :: SL.Addr c , compactAddress :: SL.CompactAddr c , value :: Core.Value (AlonzoEra c) } deriving (Eq, Generic)
data DummyAlonzoRule c (s :: Symbol)

instance {- ApplyBlock -} Crypto c => SL.ApplyBlock (AlonzoEra c)
instance {- ApplyTx -} Crypto c => SL.ApplyTx (AlonzoEra c)
instance {- CanStartFromGenesis -} SL.CanStartFromGenesis (AlonzoEra c) where { type AdditionalGenesisConfig (AlonzoEra c) = (); initialState = undefined; }
instance {- Default -} Default (SL.BbodyState (AlonzoEra c)) where { def = undefined; }
instance {- Default -} Default (SL.NewEpochState (AlonzoEra c)) where { def = undefined; }
instance {- Era -} Crypto c => CL.Era (AlonzoEra c) where { type Crypto (AlonzoEra c) = c; }
instance {- FromCBOR -} CB.FromCBOR AlonzoPParams where { fromCBOR = undefined; }
instance {- FromCBOR -} Typeable c => CB.FromCBOR (AlonzoTxOut c) where { fromCBOR = undefined; }
instance {- FromCBOR -} Typeable c => CB.FromCBOR (CB.Annotator (AlonzoTxBody c)) where { fromCBOR = undefined; }
instance {- GetLedgerView -} Crypto c => SL.GetLedgerView (AlonzoEra c)
instance {- HashAnnotated -} CL.HashAnnotated (AlonzoTxBody c) CL.EraIndependentTxBody c
instance {- NoThunks -} NoThunks (AlonzoTxBody c) where { wNoThunks = undefined; }
instance {- NoThunks -} NoThunks (AlonzoTxOut c) where { wNoThunks = undefined; }
instance {- STS -} ( Typeable c , Eq (DummyAlonzoPredicateFailure c s) , Show (DummyAlonzoPredicateFailure c s) , KnownSymbol s , Default (SL.State (DummyAlonzoRule c s)) ) => SL.STS (DummyAlonzoRule c s) where { type Environment (DummyAlonzoRule c s) = DummyAlonzoEnvironment c s; type PredicateFailure (DummyAlonzoRule c s) = DummyAlonzoPredicateFailure c s; type Signal (DummyAlonzoRule c s) = DummyAlonzoSignal c s; type State (DummyAlonzoRule c s) = DummyAlonzoState c s; type BaseM (DummyAlonzoRule c s) = SL.ShelleyBase; transitionRules = []; }
instance {- SafeToHash -} CL.SafeToHash (AlonzoTxBody c) where { originalBytes = undefined; }
instance {- ShelleyBasedEra -} SL.PraosCrypto c => SL.ShelleyBasedEra (AlonzoEra c)
instance {- Show -} Show (AlonzoTxOut c) where { show = undefined; }
instance {- ToCBOR -} CB.ToCBOR AlonzoPParams where { toCBOR = undefined; }
instance {- ToCBOR -} Typeable c => CB.ToCBOR (AlonzoTxBody c) where { toCBOR = undefined; }
instance {- ToCBOR -} Typeable c => CB.ToCBOR (AlonzoTxOut c) where { toCBOR = undefined; }
instance {- TranslateEra -} Crypto c => CL.TranslateEra (AlonzoEra c) SL.ShelleyGenesis where { type TranslationError (AlonzoEra c) SL.ShelleyGenesis = Void; translateEra = undefined; }
instance {- UsesPParams -} ( Crypto c , Eq (Core.PParams (AlonzoEra c)) , Show (Core.PParams (AlonzoEra c)) , CB.FromCBOR (Core.PParams (AlonzoEra c)) , CB.ToCBOR (Core.PParams (AlonzoEra c)) ) => SL.UsesPParams (AlonzoEra c) where { type PParamsDelta (AlonzoEra c) = SL.PParamsUpdate (AlonzoEra c); mergePPUpdates = undefined; }
instance {- UsesTxOut -} Crypto c => SL.UsesTxOut (AlonzoEra c) where { makeTxOut = undefined; }
instance {- UsesValue -} Crypto c => SL.UsesValue (AlonzoEra c)
instance {- ValidateAuxiliaryData -} CL.ValidateAuxiliaryData (AlonzoEra c) where { hashAuxiliaryData = undefined; validateAuxiliaryData = undefined; }
instance {- ValidateScript -} Crypto c => SL.ValidateScript (AlonzoEra c) where { hashScript = undefined; validateScript = undefined; }

type family DummyAlonzoEnvironment c (s :: Symbol)
type family DummyAlonzoPredicateFailure c (s :: Symbol)
type family DummyAlonzoSignal c (s :: Symbol)
type family DummyAlonzoState c (s :: Symbol)

type instance DummyAlonzoEnvironment c "BBODY" = SL.BbodyEnv (AlonzoEra c)
type instance DummyAlonzoEnvironment c "LEDGERS" = SL.MempoolEnv (AlonzoEra c)
type instance DummyAlonzoEnvironment c "TICK" = ()
type instance DummyAlonzoEnvironment c "TICKF" = ()
type instance DummyAlonzoPredicateFailure c "BBODY" = Stub c "BBODY PF"
type instance DummyAlonzoPredicateFailure c "DELEGS" = SL.DelegsPredicateFailure (AlonzoEra c)
type instance DummyAlonzoPredicateFailure c "DELPL" = Stub c "DELPL PF"
type instance DummyAlonzoPredicateFailure c "LEDGER" = SL.LedgerPredicateFailure (AlonzoEra c)
type instance DummyAlonzoPredicateFailure c "LEDGERS" = SL.LedgersPredicateFailure (AlonzoEra c)
type instance DummyAlonzoPredicateFailure c "NEWEPOCH" = Stub c "NEWEPOCH PF"
type instance DummyAlonzoPredicateFailure c "TICK" = Stub c "TICK PF"
type instance DummyAlonzoPredicateFailure c "TICKF" = SL.TickfPredicateFailure (AlonzoEra c)
type instance DummyAlonzoPredicateFailure c "TICKN" = Stub c "TICKN PF"
type instance DummyAlonzoPredicateFailure c "UTXO" = Stub c "UTXO PF"
type instance DummyAlonzoPredicateFailure c "UTXOW" = Stub c "UTXOW PF"
type instance DummyAlonzoSignal c "BBODY" = SL.Block (AlonzoEra c)
type instance DummyAlonzoSignal c "LEDGERS" = Seq (SL.Tx (AlonzoEra c))
type instance DummyAlonzoSignal c "TICK" = SlotNo
type instance DummyAlonzoSignal c "TICKF" = SlotNo
type instance DummyAlonzoState c "BBODY" = SL.BbodyState (AlonzoEra c)
type instance DummyAlonzoState c "LEDGERS" = SL.MempoolState (AlonzoEra c)
type instance DummyAlonzoState c "PPUP" = SL.PPUPState (AlonzoEra c)
type instance DummyAlonzoState c "TICK" = SL.NewEpochState (AlonzoEra c)
type instance DummyAlonzoState c "TICKF" = SL.NewEpochState (AlonzoEra c)
type instance {- AuxiliaryData -} Core.AuxiliaryData (AlonzoEra c) = Stub c "AuxiliaryData"
type instance {- EraRule -} Core.EraRule s (AlonzoEra c) = DummyAlonzoRule c s
type instance {- PParams -} Core.PParams (AlonzoEra c) = AlonzoPParams
type instance {- PreviousEra -} CL.PreviousEra (AlonzoEra c) = MaryEra c
type instance {- Script -} Core.Script (AlonzoEra c) = Stub c "Script"
type instance {- TranslationContext -} CL.TranslationContext (AlonzoEra c) = ()
type instance {- TxBody -} Core.TxBody (AlonzoEra c) = AlonzoTxBody c
type instance {- TxOut -} Core.TxOut (AlonzoEra c) = AlonzoTxOut c
type instance {- Value -} Core.Value (AlonzoEra c) = Stub c "Value"





data Stub c (s :: Symbol) = Stub deriving (Eq, Generic, NoThunks, Show)

instance Abelian (Stub c s)
instance Group (Stub c s) where
  invert _ = Stub
instance Monoid (Stub c s) where
  mempty = Stub
instance Semigroup (Stub c s) where
  (<>) _ _ = Stub

instance {- Compactible -} (Typeable c, KnownSymbol s) => Core.Compactible (Stub c s) where { data CompactForm (Stub c s) = CompactFormStub (Stub c s); toCompact = undefined; fromCompact = undefined; }
instance {- DecodeMint -} CL.DecodeMint (Stub c s) where { decodeMint = undefined; }
instance {- DecodeNonNegative -} CL.DecodeNonNegative (Stub c s) where { decodeNonNegative = undefined; }
instance {- EncodeMint -} CL.EncodeMint (Stub c s) where { encodeMint = undefined; }
instance {- Eq -} Eq (Core.CompactForm (Stub c s)) where { (==) = undefined }
instance {- FromCBOR -} (Typeable c, KnownSymbol s) => CB.FromCBOR (CB.Annotator (Stub c s)) where { fromCBOR = undefined; }
instance {- FromCBOR -} (Typeable c, KnownSymbol s) => CB.FromCBOR (Stub c s) where { fromCBOR = undefined; }
instance {- HashAnnotated -} CL.HashAnnotated (Stub c s) CL.EraIndependentTxBody c
instance {- SafeToHash -} CL.SafeToHash (Stub c s) where { originalBytes = undefined; }
instance {- Show -} Show (Core.CompactForm (Stub c s)) where { show = undefined; }
instance {- ToCBOR -} (Typeable c, KnownSymbol s) => CB.ToCBOR (CB.Annotator (Stub c s)) where { toCBOR = undefined; }
instance {- ToCBOR -} (Typeable c, KnownSymbol s) => CB.ToCBOR (Core.CompactForm (Stub c s)) where { toCBOR = undefined; }
instance {- ToCBOR -} (Typeable c, KnownSymbol s) => CB.ToCBOR (Stub c s) where { toCBOR = undefined; }
instance {- Val -} CL.Val (Stub c s) where { (<×>) = undefined; coin = undefined; inject = undefined; modifyCoin = undefined; size = undefined; pointwise = undefined;}

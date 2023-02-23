{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The implementation of the LedgerTables for Cardano
--
-- Note that normally the code that deals with tables is polymorphic on the @l@
-- used so this module is only needed to be imported for the instances (except
-- perhaps in tests).
--
-- > import Ouroboros.Consensus.Cardano.Tables ()
module Ouroboros.Consensus.Cardano.Tables (
    -- * Testing
    LedgerTables (CardanoLedgerTables, cardanoUTxOTable)
  , TranslateTxOutWrapper (..)
  , composeTxOutTranslationPairs
  ) where

import           Cardano.Binary (fromCBOR, toCBOR)
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import qualified Cardano.Ledger.Conway.Translation as Conway
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (DSignable, Hash)
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.Proxy
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

-- We reuse this for both HasLedgerTables and HasTickedLedgerTables instances,
-- so the @HasTickedLedgerTables x@ constraint here is excessive in the
-- HasLedgerTables case. However, since x is always a Cardano era, we do know we
-- have HasTickedLedgerTables for every x, so hardcoding the stronger constraint
-- is easier than parameterizing this helper over the constraint.
projectLedgerTablesHelper :: forall c mk fmk.
     (CardanoHardForkConstraints c, IsMapKind mk)
  => (forall blk.
         HasTickedLedgerTables (LedgerState blk)
      => fmk blk -> LedgerTables (LedgerState blk) mk
     )
  -> HardForkState fmk (CardanoEras c)
  -> LedgerTables (LedgerState (CardanoBlock c)) mk
projectLedgerTablesHelper prjLT (HardForkState st) =
      hcollapse
    $ Telescope.tip
    $ hcimap (Proxy @(Compose HasTickedLedgerTables LedgerState)) projectOne st
  where
    projectOne :: HasTickedLedgerTables (LedgerState x)
               => Index (CardanoEras c) x
               -> Current fmk           x
               -> K (LedgerTables
                      (LedgerState (HardForkBlock (CardanoEras c))) mk)
                                        x
    projectOne i Current{currentState = innerSt} =
        K
      $ applyInjectLedgerTables (projectNP i hardForkInjectLedgerTables)
      $ prjLT innerSt

-- Same note regarding the @HasTickedLedgerTables x@ constraint as
-- 'projectLedgerTablesHelper'
withLedgerTablesHelper ::
  forall c mk fany fmk.
     (CardanoHardForkConstraints c, IsMapKind mk)
  => (forall x.
         HasTickedLedgerTables (LedgerState x)
      => fany x -> LedgerTables (LedgerState x) mk -> fmk x
     )
  -> HardForkState fany (CardanoEras c)
  -> LedgerTables (LedgerState (CardanoBlock c)) mk
  -> HardForkState fmk (CardanoEras c)
withLedgerTablesHelper withLT (HardForkState st) tbs =
      HardForkState
    $ hcimap (Proxy @(Compose HasTickedLedgerTables LedgerState)) updateOne st
  where
    updateOne :: HasTickedLedgerTables (LedgerState x)
              => Index (CardanoEras c) x
              -> Current fany          x
              -> Current fmk           x
    updateOne i current@Current{currentState = innerSt} =
      current { currentState =
                  withLT innerSt
                    $ applyDistribLedgerTables
                        (projectNP i hardForkInjectLedgerTables)
                        tbs
              }

-- Note that this is a HardForkBlock instance, but it's not compositional. This
-- is because the LedgerTables relies on knowledge specific to Cardano and we
-- have so far not found a pleasant way to express that compositionally.
instance CardanoHardForkConstraints c => HasLedgerTables (LedgerState (CardanoBlock c)) where
  newtype LedgerTables (LedgerState (CardanoBlock c)) mk = CardanoLedgerTables {
        cardanoUTxOTable :: mk (SL.TxIn c) (ShelleyTxOut (ShelleyBasedEras c))
      }
    deriving (Generic)

  projectLedgerTables (HardForkLedgerState hfstate) =
      projectLedgerTablesHelper
        (projectLedgerTables . unFlip)
        hfstate

  withLedgerTables (HardForkLedgerState hfstate) tables =
        HardForkLedgerState
      $ withLedgerTablesHelper
          (\(Flip st) tables' -> Flip $ withLedgerTables st tables')
          hfstate
          tables

  pureLedgerTables     f                                                                         = CardanoLedgerTables f
  mapLedgerTables      f                                                 (CardanoLedgerTables x) = CardanoLedgerTables (f x)
  traverseLedgerTables f                                                 (CardanoLedgerTables x) = CardanoLedgerTables <$> f x
  zipLedgerTables      f                         (CardanoLedgerTables l) (CardanoLedgerTables r) = CardanoLedgerTables (f l r)
  zipLedgerTables2     f (CardanoLedgerTables l) (CardanoLedgerTables c) (CardanoLedgerTables r) = CardanoLedgerTables (f l c r)
  zipLedgerTablesA     f                         (CardanoLedgerTables l) (CardanoLedgerTables r) = CardanoLedgerTables <$> f l r
  zipLedgerTables2A    f (CardanoLedgerTables l) (CardanoLedgerTables c) (CardanoLedgerTables r) = CardanoLedgerTables <$> f l c r
  foldLedgerTables     f                                                 (CardanoLedgerTables x) = f x
  foldLedgerTables2    f                         (CardanoLedgerTables l) (CardanoLedgerTables r) = f l r

  namesLedgerTables = CardanoLedgerTables { cardanoUTxOTable = NameMK "cardanoUTxOTable" }

deriving newtype
         instance ( IsMapKind mk
                  , Praos.PraosCrypto c
                  , TPraos.PraosCrypto c
                  , DSignable c (Hash c EraIndependentTxBody)
                  )
                  => Eq (LedgerTables (LedgerState (CardanoBlock c)) mk)
deriving newtype
         instance ( IsMapKind mk
                  , Praos.PraosCrypto c
                  , TPraos.PraosCrypto c
                  , DSignable c (Hash c EraIndependentTxBody)
                  )
                  => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) mk)
deriving newtype
         instance ( IsMapKind mk
                  , Praos.PraosCrypto c
                  , TPraos.PraosCrypto c
                  , DSignable c (Hash c EraIndependentTxBody)
                  )
                  => Show (LedgerTables (LedgerState (CardanoBlock c)) mk)

instance CardanoHardForkConstraints c
      => HasTickedLedgerTables (LedgerState (CardanoBlock c)) where
  projectLedgerTablesTicked st = projectLedgerTablesHelper
        (\(FlipTickedLedgerState st') -> projectLedgerTablesTicked st')
        (tickedHardForkLedgerStatePerEra st)
  withLedgerTablesTicked st tables =
      st { tickedHardForkLedgerStatePerEra     =
             withLedgerTablesHelper
               (\(FlipTickedLedgerState st') tables' ->
                  FlipTickedLedgerState $ withLedgerTablesTicked st' tables')
               tickedHardForkLedgerStatePerEra
               tables
         }
    where
      TickedHardForkLedgerState{ tickedHardForkLedgerStatePerEra } = st

instance CardanoHardForkConstraints c
      => CanSerializeLedgerTables (LedgerState (CardanoBlock c)) where
    -- The Ledger and Consensus team discussed the fact that we need to be able
    -- to reach the TxIn key for an entry from any era, regardless of the era in
    -- which it was created, therefore we need to have a "canonical"
    -- serialization that doesn't change between eras. For now we are using
    -- @'toEraCBOR' \@('ShelleyEra' c)@ as a stop-gap, but Ledger will provide a
    -- serialization function into something more efficient.
    codecLedgerTables = CardanoLedgerTables (CodecMK
                                             (Core.toEraCBOR @(ShelleyEra c))
                                             toCBOR
                                             (Core.fromEraCBOR @(ShelleyEra c))
                                             fromCBOR)

{-------------------------------------------------------------------------------
  LedgerTablesCanHardFork
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c => LedgerTablesCanHardFork (CardanoEras c) where
  hardForkInjectLedgerTables =
         byron
      :* shelley IZ
      :* shelley (IS IZ)
      :* shelley (IS (IS IZ))
      :* shelley (IS (IS (IS IZ)))
      :* shelley (IS (IS (IS (IS IZ))))
      :* shelley (IS (IS (IS (IS (IS IZ)))))
      :* Nil
    where
      byron :: InjectLedgerTables (CardanoEras c) ByronBlock
      byron = InjectLedgerTables {
          applyInjectLedgerTables  = const emptyLedgerTables
        , applyDistribLedgerTables = const NoByronLedgerTables
        }

      shelley ::
           forall era proto. (EraCrypto era ~ c, Eq (Core.TxOut era))
        => Index (ShelleyBasedEras c) era
        -> InjectLedgerTables (CardanoEras c) (ShelleyBlock proto era)
      shelley idx = InjectLedgerTables {
          applyInjectLedgerTables  =
              CardanoLedgerTables . mapMK inj . shelleyUTxOTable
        , applyDistribLedgerTables =
              ShelleyLedgerTables . mapMK distrib . cardanoUTxOTable
        }
        where
          inj :: Core.TxOut era -> ShelleyTxOut (ShelleyBasedEras c)
          inj = ShelleyTxOut
              . injectNS idx
              . TxOutWrapper

          distrib :: ShelleyTxOut (ShelleyBasedEras c) -> Core.TxOut era
          distrib = unTxOutWrapper
                  . apFn (projectNP idx shelleyTxOutTranslations)
                  . K

-- | The composed translations for each possible era; see
-- 'composeTxOutTranslationPairs' to understand why this is partial but
-- is safe in the absence of Consensus bugs.
shelleyTxOutTranslations ::
     forall c. CardanoHardForkConstraints c
  => NP
        (K (ShelleyTxOut (ShelleyBasedEras c)) -.-> TxOutWrapper)
        (ShelleyBasedEras c)
shelleyTxOutTranslations =
  hmap
    (\f -> fn $ \(K (ShelleyTxOut x)) -> f `apFn` K x)
    (composeTxOutTranslationPairs translateTxOut)
 where
  translateTxOut :: InPairs
                      TranslateTxOutWrapper
                      (ShelleyBasedEras c)
  translateTxOut =
      PCons (TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (TranslateTxOutWrapper Alonzo.translateTxOut)
    $ PCons (TranslateTxOutWrapper Babbage.translateTxOut)
    $ PCons (TranslateTxOutWrapper Conway.translateTxOut)
    $ PNil

-- | Auxiliary for convenience
--
-- We can't reuse 'Translate' because we don't have the 'EpochNo' it requires.
newtype TranslateTxOutWrapper era1 era2 =
    TranslateTxOutWrapper (Core.TxOut era1 -> Core.TxOut era2)

-- | Auxiliary @SOP@ combinator
--
-- WARNING: The functions in the result fail if the 'NS' argument's tag
-- precedes the 'NP' index, i.e. if we try to translate into previous eras.
--
-- TODO use an accumulator instead of this quadratic traversal
composeTxOutTranslationPairs ::
     (SListI eras, HasCallStack)
  => InPairs
       TranslateTxOutWrapper
       eras
  -> NP
       (K (NS TxOutWrapper eras) -.-> TxOutWrapper)
       eras
composeTxOutTranslationPairs = \case
    PNil                                  ->
      (fn $ unZ . unK) :* Nil
    PCons (TranslateTxOutWrapper f) inner ->
         (fn $
            eitherNS
              id
              (error "composeTxOutTranslationPairs: anachrony")
          . unK
         )
      :* hmap
          (\innerf -> fn $
              apFn innerf
            . K
            . eitherNS
                (Z . TxOutWrapper . f . unTxOutWrapper)
                id
            . unK)
          (composeTxOutTranslationPairs inner)
  where
    eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
    eitherNS l r = \case
      Z x -> l x
      S x -> r x

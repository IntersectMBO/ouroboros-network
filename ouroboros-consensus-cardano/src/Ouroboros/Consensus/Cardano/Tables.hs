{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Ouroboros.Consensus.Cardano.Tables (
    InjectLedgerTables (..)
  , LedgerTablesCanHardFork (..)
  , ShelleyTxOut (..)
  , SndShelleyBasedEra
  , TxOutWrapper (..)
  , consolidateShelleyTele
  , unconsolidateShelleyTele
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Kind (Type)
import           Data.Monoid (First (..))
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Typeable
import           Data.Void
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()

import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as SL

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()

{-------------------------------------------------------------------------------
  Injecting Tables
-------------------------------------------------------------------------------}

-- | How to inject each era's ledger tables into their shared ledger tables
class LedgerTablesCanHardFork xs where
  hardForkInjectLedgerTablesKeysMK :: NP (InjectLedgerTables xs) xs

newtype InjectLedgerTables xs x = InjectLedgerTables {
      applyInjectLedgerTables :: forall mk. IsMapKind mk =>
           LedgerTables (LedgerState                  x) mk
        -> LedgerTables (LedgerState (HardForkBlock xs)) mk
    }

instance CardanoHardForkConstraints c => LedgerTablesCanHardFork (CardanoEras c) where
  hardForkInjectLedgerTablesKeysMK =
         byron
      :* shelley IZ
      :* shelley (IS IZ)
      :* shelley (IS (IS IZ))
      :* shelley (IS (IS (IS IZ)))
      :* shelley (IS (IS (IS (IS IZ))))
      :* Nil
    where
      byron :: InjectLedgerTables (CardanoEras c) ByronBlock
      byron = InjectLedgerTables $ \NoTables -> pureLedgerTables emptyMK

      shelley ::
           forall era proto. (SL.Crypto era ~ c, Eq (Core.TxOut era))
        => Index (ShelleyBasedEras c) era
        -> InjectLedgerTables (CardanoEras c) (ShelleyBlock proto era)
      shelley idx =
          InjectLedgerTables
        $ \(ShelleyLedgerTables lt) -> CardanoLedgerTables $ mapMK f lt
        where
          f :: Core.TxOut era -> CardanoTxOut c
          f = ShelleyTxOut . injectNS idx . TxOutWrapper

{-------------------------------------------------------------------------------
  Partially applying 'Core.TxOut'
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying the 'Core.TxOut' type family
--
-- For generality, Consensus uses that type family as eg the index of
-- 'Core.TranslateEra'. We thus need to partially apply it.
newtype TxOutWrapper era = TxOutWrapper {unTxOutWrapper :: Core.TxOut era}
  deriving (Generic)

deriving instance Eq       (Core.TxOut era) => Eq       (TxOutWrapper era)
deriving instance NoThunks (Core.TxOut era) => NoThunks (TxOutWrapper era)
deriving instance Show     (Core.TxOut era) => Show     (TxOutWrapper era)

instance ShelleyBasedEra (AllegraEra c) => Core.TranslateEra (AllegraEra c) TxOutWrapper where
  type TranslationError (AllegraEra c) TxOutWrapper = Void
  translateEra ctxt = fmap TxOutWrapper . Core.translateEra ctxt . unTxOutWrapper

instance ShelleyBasedEra (MaryEra c) => Core.TranslateEra (MaryEra c) TxOutWrapper where
  type TranslationError (MaryEra c) TxOutWrapper = Void
  translateEra ctxt = fmap TxOutWrapper . Core.translateEra ctxt . unTxOutWrapper

instance ShelleyBasedEra (AlonzoEra c) => Core.TranslateEra (AlonzoEra c) TxOutWrapper where
  type TranslationError (AlonzoEra c) TxOutWrapper = Void
  translateEra _ctxt =
        pure
      . TxOutWrapper
      . Alonzo.translateTxOut
      . unTxOutWrapper

instance ShelleyBasedEra (BabbageEra c) => Core.TranslateEra (BabbageEra c) TxOutWrapper where
  type TranslationError (BabbageEra c) TxOutWrapper = Void
  translateEra _ctxt =
        pure
      . TxOutWrapper
      . Babbage.translateTxOut
      . unTxOutWrapper

{-------------------------------------------------------------------------------
  The NS'ed tx out for Shelley eras
-------------------------------------------------------------------------------}

type ShelleyBasedEras c = MapSnd (ShelleyBasedProtosAndEras c)

-- | We use this type for clarity, and because we don't want to declare
-- 'FromCBOR' and 'ToCBOR' for 'NS'; serializations of sum involves design
-- decisions that cannot be captured by an all-purpose 'NS' instance
newtype ShelleyTxOut eras =
    ShelleyTxOut { unShelleyTxOut :: NS TxOutWrapper eras }
  deriving (Generic)

instance All ShelleyBasedEra eras => Eq       (ShelleyTxOut eras) where
  ShelleyTxOut (Z l) == ShelleyTxOut (Z r) = l == r
  ShelleyTxOut (S l) == ShelleyTxOut (S r) = ShelleyTxOut l == ShelleyTxOut r
  _                      == _              = False

instance All ShelleyBasedEra eras => NoThunks (ShelleyTxOut eras) where
  wNoThunks ctxt = (. unShelleyTxOut) $ \case
      Z l -> noThunks ("Z" : ctxt) l
      S r -> noThunks ("S" : ctxt) (ShelleyTxOut r)

instance All ShelleyBasedEra eras => Show (ShelleyTxOut eras) where
  showsPrec =
      \p (ShelleyTxOut ns) -> showParen (p > 10) $ showString "ShelleyTxOut " . go ns
    where
      go :: All ShelleyBasedEra eras' => NS TxOutWrapper eras' -> ShowS
      go = showParen True . \case
        Z l -> showString "Z " . shows l
        S r -> showString "S " . go r

-- | Unlike nsToIndex, this is not restricted to the interval [0, 24)
idxLength :: Index xs x -> Int
idxLength = \case
    IZ     -> 0
    IS idx -> 1 + idxLength idx

instance (All ShelleyBasedEra eras, Typeable eras) => ToCBOR (ShelleyTxOut eras) where
  toCBOR (ShelleyTxOut x) =
        hcollapse
      $ hcimap (Proxy @ShelleyBasedEra) each x
    where
      each ::
           ShelleyBasedEra era
        => Index eras era
        -> TxOutWrapper era
        -> K CBOR.Encoding era
      each idx (TxOutWrapper txout) = K $
           CBOR.encodeListLen 2
        <> CBOR.encodeWord (toEnum (idxLength idx))
        <> toCBOR txout

instance (All ShelleyBasedEra eras, Typeable eras) => FromCBOR (ShelleyTxOut eras) where
  fromCBOR = do
      CBOR.decodeListLenOf 2
      tag <- CBOR.decodeWord
      let aDecoder =
              mconcat
            $ hcollapse
            $ hcmap
                (Proxy @ShelleyBasedEra)
                each
                (indices @eras)
      case getFirst $ aDecoder tag of
        Nothing -> error $ "FromCBOR ShelleyTxOut, unknown tag: " <> show tag
        Just x  -> unADecoder x
    where
      each ::
           ShelleyBasedEra x
        => Index eras x
        -> K (Word -> First (ADecoder eras)) x
      each idx = K $ \w -> First $
        if w /= toEnum (idxLength idx) then Nothing else
        Just
          $ ADecoder
          $ (ShelleyTxOut . injectNS idx . TxOutWrapper) <$> fromCBOR

newtype ADecoder eras =
  ADecoder {unADecoder :: forall s. CBOR.Decoder s (ShelleyTxOut eras)}

{-------------------------------------------------------------------------------
  Mapping lists of tuples
-------------------------------------------------------------------------------}

type MapShelleyBlock :: [(Type, Type)] -> [Type]
type family MapShelleyBlock protosAndEras = blks | blks -> protosAndEras where
  MapShelleyBlock '[]                              = '[]
  MapShelleyBlock ('(proto, era) ': protosAndEras') = ShelleyBlock proto era ': MapShelleyBlock protosAndEras'

class IsShelleyTele protosAndEras where
  consolidateShelleyTele ::
       Telescope              g                            f               (MapShelleyBlock protosAndEras)
    -> Telescope (UncurryComp g ShelleyBlock) (UncurryComp f ShelleyBlock) protosAndEras
  unconsolidateShelleyTele ::
       Telescope (UncurryComp g ShelleyBlock) (UncurryComp f ShelleyBlock) protosAndEras
    -> Telescope              g                            f               (MapShelleyBlock protosAndEras)

instance IsShelleyTele '[] where
  consolidateShelleyTele   = \case {}
  unconsolidateShelleyTele = \case {}

instance IsShelleyTele xs => IsShelleyTele ('(x, y) ': xs) where
  consolidateShelleyTele = \case
    TZ x       -> TZ (UncurryComp x)
    TS p inner -> TS (UncurryComp p) (consolidateShelleyTele inner)
  unconsolidateShelleyTele   = \case
    TZ (UncurryComp x)       -> TZ x
    TS (UncurryComp p) inner -> TS p (unconsolidateShelleyTele inner)

class    (SL.Crypto (Snd a) ~ c, ShelleyBasedEra (Snd a)) => SndShelleyBasedEra c a
instance (SL.Crypto (Snd a) ~ c, ShelleyBasedEra (Snd a)) => SndShelleyBasedEra c a

{-------------------------------------------------------------------------------
  HasLedgerTables helpers
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
    case st of
      -- the first era is Byron
      TZ Current {
          currentState = prjLT -> NoTables
        } ->
        pureLedgerTables emptyMK

      -- all the remaining eras are Shelley
      TS _past tele ->
          hcollapse
        $ hcimap
            (Proxy @(SndShelleyBasedEra c))
            projectOne
            (Telescope.tip $ consolidateShelleyTele tele)
  where
    projectOne :: forall (a :: (Type, Type)).
         (SL.Crypto (Snd a) ~ c, ShelleyBasedEra (Snd a))
      => Index (ShelleyBasedProtosAndEras c)             a
         -- ^ the current era of the ledger state we're projecting from
      -> UncurryComp (Current fmk) ShelleyBlock a
         -- ^ the ledger state we're projecting from
      -> K (LedgerTables (LedgerState (CardanoBlock c)) mk) a
    projectOne idx (UncurryComp current) =
        K $ CardanoLedgerTables $ inj appliedMK
      where
        ShelleyLedgerTables appliedMK = prjLT $ currentState current

        inj ::
             mk (SL.TxIn c) (Core.TxOut (Snd a))
          -> mk (SL.TxIn c) (CardanoTxOut c)
        inj = mapMK (ShelleyTxOut . injectNS (castSndIdx idx) . TxOutWrapper)

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
withLedgerTablesHelper withLT (HardForkState st) (CardanoLedgerTables appliedMK) =
      HardForkState
    $ case st of
        -- the first era is Byron
        TZ Current {
            currentStart
          , currentState = byronSt
          } ->
          TZ Current {
                 currentStart
               , currentState = withLT byronSt NoTables
               }

        -- all the remaining eras are Shelley
        TS past tele ->
            TS past
          $ unconsolidateShelleyTele
          $ hap
              updateOne
              (consolidateShelleyTele tele)
  where
    -- how to update the ledger table of each possible individual era
    updateOne ::
      NP
        (     UncurryComp (Current fany) ShelleyBlock
         -.-> UncurryComp (Current fmk ) ShelleyBlock
        )
        (ShelleyBasedProtosAndEras c)
    updateOne =
      hcmap
        (Proxy @(SndShelleyBasedEra c))
        (\(ApOnlySnd translate) -> fn $ \(UncurryComp current) ->
            let Current{currentState = innerSt} = current
                newInnerSt =
                  withLT innerSt
                  $ ShelleyLedgerTables
                  $ mapMK
                      (unTxOutWrapper . apFn translate . K)
                      appliedMK
            in UncurryComp $ current{currentState = newInnerSt})
        translations

    -- the composed translations for each possible era; see
    -- 'composeTxOutTranslationPairs' to understand why this is partial but
    -- is safe in the absence of Consensus bugs
    translations ::
      NP
        (ApOnlySnd (K (CardanoTxOut c) -.-> TxOutWrapper))
        (ShelleyBasedProtosAndEras c)
    translations =
        hmap
          (\(ApOnlySnd f)
             -> ApOnlySnd $ fn $ \(K (ShelleyTxOut x)) -> f `apFn` K (nsMapSnd x))
          (composeTxOutTranslationPairs translateTxOut)

{-------------------------------------------------------------------------------
 Translating TxOuts through eras
-------------------------------------------------------------------------------}

-- | Auxiliary for convenience
--
-- We can't reuse 'Translate' because we don't have the 'EpochNo' it requires.
newtype TranslateTxOutWrapper era1 era2 =
    TranslateTxOutWrapper (Core.TxOut era1 -> Core.TxOut era2)

-- | The 'Core.TxOut' translations between the adjacent Shelley-based eras in
-- Cardano
translateTxOut ::
     CardanoHardForkConstraints c
  => InPairs
       (ApOnlySnd2 TranslateTxOutWrapper)
       (ShelleyBasedProtosAndEras c)
translateTxOut =
      PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ Alonzo.translateTxOut)
    $ PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ Babbage.translateTxOut)
    $ PNil

-- | Auxiliary @SOP@ combinator
--
-- WARNING: The functions in the result fail if the 'NS' argument's tag
-- precedes the 'NP' index.
--
-- TODO use an accumulator instead of this quadratic traversal
composeTxOutTranslationPairs ::
     (SListI protosAndEras, HasCallStack)
  => InPairs
       (ApOnlySnd2 TranslateTxOutWrapper)
       protosAndEras
  -> NP
       (ApOnlySnd (K (NS (ApOnlySnd TxOutWrapper) protosAndEras) -.-> TxOutWrapper))
       protosAndEras
composeTxOutTranslationPairs = \case
    PNil                                  ->
      (ApOnlySnd $ fn $ unApOnlySnd . unZ . unK) :* Nil
    PCons (ApOnlySnd2 (TranslateTxOutWrapper f)) inner ->
         (ApOnlySnd $ fn $
            unApOnlySnd
          . eitherNS
              id
              (error "composeTxOutTranslationPairs: anachrony")
          . unK
         )
      :* hmap
          (\(ApOnlySnd innerf) -> ApOnlySnd $ fn $
              apFn innerf
            . K
            . eitherNS
                (Z . ApOnlySnd . TxOutWrapper . f . unTxOutWrapper . unApOnlySnd)
                id
            . unK)
          (composeTxOutTranslationPairs inner)
  where
    eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
    eitherNS l r = \case
      Z x -> l x
      S x -> r x

{-------------------------------------------------------------------------------
 HasLedgerTables instances
-------------------------------------------------------------------------------}

type CardanoTxOut c = ShelleyTxOut (ShelleyBasedEras c)

-- Note that this is a HardForkBlock instance, but it's not compositional. This
-- is because the LedgerTables relies on knowledge specific to Cardano and we
-- have so far not found a pleasant way to express that compositionally.
instance CardanoHardForkConstraints c => HasLedgerTables (LedgerState (CardanoBlock c)) where
  newtype LedgerTables (LedgerState (CardanoBlock c)) mk = CardanoLedgerTables {
        cardanoUTxOTable :: mk (SL.TxIn c) (CardanoTxOut c)
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

deriving newtype instance ( IsMapKind mk
                          , Praos.PraosCrypto c
                          , TPraos.PraosCrypto c
                          , DSignable c (Hash c EraIndependentTxBody)
                          ) => Eq (LedgerTables (LedgerState (CardanoBlock c)) mk)
deriving newtype instance ( IsMapKind mk
                          , Praos.PraosCrypto c
                          , TPraos.PraosCrypto c
                          , DSignable c (Hash c EraIndependentTxBody)
                          ) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) mk)
deriving newtype instance ( IsMapKind mk
                          , Praos.PraosCrypto c
                          , TPraos.PraosCrypto c
                          , DSignable c (Hash c EraIndependentTxBody)
                          ) => Show (LedgerTables (LedgerState (CardanoBlock c)) mk)

instance CardanoHardForkConstraints c => HasTickedLedgerTables (LedgerState (CardanoBlock c)) where
  projectLedgerTablesTicked st = projectLedgerTablesHelper
        (\(FlipTickedLedgerState st') -> projectLedgerTablesTicked st')
        (tickedHardForkLedgerStatePerEra st)

  withLedgerTablesTicked TickedHardForkLedgerState{..} tables =
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition = tickedHardForkLedgerStateTransition
        , tickedHardForkLedgerStatePerEra     =
            withLedgerTablesHelper
              (\(FlipTickedLedgerState st) tables' ->
                 FlipTickedLedgerState $ withLedgerTablesTicked st tables')
              tickedHardForkLedgerStatePerEra
              tables
        }

instance CardanoHardForkConstraints c
      => CanSerializeLedgerTables (LedgerState (CardanoBlock c)) where
    codecLedgerTables = CardanoLedgerTables (CodecMK toCBOR toCBOR fromCBOR fromCBOR)

instance CardanoHardForkConstraints c
      => CanStowLedgerTables (LedgerState (CardanoBlock c)) where
  stowLedgerTables   hfstate =
    let
      innerState :: Telescope (K Past) (Current (Flip LedgerState ValuesMK)) (CardanoEras c)
      innerState = getHardForkState $ hardForkLedgerStatePerEra hfstate

      innerTable :: Telescope (K Past) (Current (Flip LedgerState EmptyMK)) (CardanoEras c)
      innerTable =
        hcmap
          (Proxy @(Compose CanStowLedgerTables LedgerState))
          (\(Current s (Flip st)) -> Current s $ Flip $ stowLedgerTables st)
          innerState
    in
    HardForkLedgerState
    $ HardForkState
    $ innerTable

  unstowLedgerTables hfstate =
    let
      innerState :: Telescope (K Past) (Current (Flip LedgerState EmptyMK)) (CardanoEras c)
      innerState = getHardForkState $ hardForkLedgerStatePerEra hfstate

      innerTable :: Telescope (K Past) (Current (Flip LedgerState ValuesMK)) (CardanoEras c)
      innerTable =
        hcmap
          (Proxy @(Compose CanStowLedgerTables LedgerState))
          (\(Current s (Flip st)) -> Current s $ Flip $ unstowLedgerTables st)
          innerState
    in
    HardForkLedgerState
    $ HardForkState
    $ innerTable

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Examples (
    -- * Setup
    codecConfig
    -- * Examples
  , exampleApplyTxErrWrongEraByron
  , exampleApplyTxErrWrongEraShelley
  , exampleEraMismatchByron
  , exampleEraMismatchShelley
  , exampleQueryAnytimeShelley
  , exampleQueryEraMismatchByron
  , exampleQueryEraMismatchShelley
  , exampleResultAnytimeShelley
  , exampleResultEraMismatchByron
  , exampleResultEraMismatchShelley
  , examples
  ) where

import           Data.Coerce (Coercible)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Data.Time
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

import           Cardano.Crypto.Hash
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Time

import           Cardano.Ledger.Alonzo.Genesis
import           Cardano.Ledger.Alonzo.Language
import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.BaseTypes (Network (Mainnet), Nonce (..),
                     unitIntervalFromRational)
import           Cardano.Ledger.Coin (Coin (Coin))
import qualified Cardano.Ledger.Era as Core
import           Cardano.Ledger.Keys
import           Shelley.Spec.Ledger.Genesis
import           Shelley.Spec.Ledger.PParams

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (Exactly (..))
import           Ouroboros.Consensus.Util.SOP (Index (..))

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerConfig (..), compactGenesis)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ByronPartialLedgerConfig (..),
                     ShelleyPartialLedgerConfig (..), TriggerHardFork (..))

import           Test.Util.Serialisation.Golden (Examples, Labelled, labelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import qualified Test.Consensus.Byron.Examples as Byron

import qualified Test.Consensus.Shelley.Examples as Shelley

type Crypto = StandardCrypto

eraExamples :: NP Examples (CardanoEras Crypto)
eraExamples =
       Byron.examples
    :* Shelley.examplesShelley
    :* Shelley.examplesAllegra
    :* Shelley.examplesMary
    :* Shelley.examplesAlonzo
    :* Nil

-- | By using this function, we can't forget to update this test when adding a
-- new era to 'CardanoEras'.
combineEras ::
     NP Examples (CardanoEras Crypto)
  -> Examples (CardanoBlock Crypto)
combineEras = mconcat . hcollapse . hap eraInjections
  where
    eraInjections :: NP (Examples -.-> K (Examples (CardanoBlock Crypto)))
                        (CardanoEras Crypto)
    eraInjections =
           fn (K . injExamples "Byron"   IZ)
        :* fn (K . injExamples "Shelley" (IS IZ))
        :* fn (K . injExamples "Allegra" (IS (IS IZ)))
        :* fn (K . injExamples "Mary"    (IS (IS (IS IZ))))
        :* fn (K . injExamples "Alonzo"  (IS (IS (IS (IS IZ)))))
        :* Nil

    injExamples ::
         String
      -> Index (CardanoEras Crypto) blk
      -> Examples blk
      -> Examples (CardanoBlock Crypto)
    injExamples eraName idx =
          Golden.prefixExamples eraName
        . inject exampleStartBounds idx

{-------------------------------------------------------------------------------
  Inject instances
-------------------------------------------------------------------------------}

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
instance Inject Serialised where
  inject _ _ (Serialised _) = Serialised "<CARDANO_BLOCK>"

instance Inject SomeResult where
  inject _ idx (SomeResult q r) =
      SomeResult (QueryIfCurrent (injectQuery idx q)) (Right r)

instance Inject Examples where
  inject startBounds (idx :: Index xs x) Golden.Examples {..} = Golden.Examples {
        exampleBlock            = inj (Proxy @I)                       exampleBlock
      , exampleSerialisedBlock  = inj (Proxy @Serialised)              exampleSerialisedBlock
      , exampleHeader           = inj (Proxy @Header)                  exampleHeader
      , exampleSerialisedHeader = inj (Proxy @SerialisedHeader)        exampleSerialisedHeader
      , exampleHeaderHash       = inj (Proxy @WrapHeaderHash)          exampleHeaderHash
      , exampleGenTx            = inj (Proxy @GenTx)                   exampleGenTx
      , exampleGenTxId          = inj (Proxy @WrapGenTxId)             exampleGenTxId
      , exampleApplyTxErr       = inj (Proxy @WrapApplyTxErr)          exampleApplyTxErr
      , exampleQuery            = inj (Proxy @(SomeSecond BlockQuery)) exampleQuery
      , exampleResult           = inj (Proxy @SomeResult)              exampleResult
      , exampleAnnTip           = inj (Proxy @AnnTip)                  exampleAnnTip
      , exampleLedgerConfig     = mempty -- We rely on `combineEras` to make use of the example ledger configs
      , exampleLedgerState      = inj (Proxy @LedgerState)             exampleLedgerState
      , exampleChainDepState    = inj (Proxy @WrapChainDepState)       exampleChainDepState
      , exampleExtLedgerState   = inj (Proxy @ExtLedgerState)          exampleExtLedgerState
      }
    where
      inj ::
           forall f a b.
           ( Inject f
           , Coercible a (f x)
           , Coercible b (f (HardForkBlock xs))
           )
        => Proxy f -> Labelled a -> Labelled b
      inj p = fmap (fmap (inject' p startBounds idx))

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

byronEraParams :: History.EraParams
byronEraParams = Byron.byronEraParams Byron.ledgerConfig

shelleyEraParams :: History.EraParams
shelleyEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

allegraEraParams :: History.EraParams
allegraEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

maryEraParams :: History.EraParams
maryEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

alonzoEraParams :: History.EraParams
alonzoEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

-- | We use 10, 20, 30, 40, ... as the transition epochs
shelleyTransitionEpoch :: EpochNo
shelleyTransitionEpoch = 10

byronStartBound :: History.Bound
byronStartBound = History.initBound

shelleyStartBound :: History.Bound
shelleyStartBound =
    History.mkUpperBound
      byronEraParams
      byronStartBound
      shelleyTransitionEpoch

allegraStartBound :: History.Bound
allegraStartBound =
    History.mkUpperBound
      shelleyEraParams
      shelleyStartBound
      20

maryStartBound :: History.Bound
maryStartBound =
    History.mkUpperBound
      allegraEraParams
      allegraStartBound
      30

alonzoStartBound :: History.Bound
alonzoStartBound =
    History.mkUpperBound
      maryEraParams
      maryStartBound
      40

exampleStartBounds :: Exactly (CardanoEras Crypto) History.Bound
exampleStartBounds = Exactly $
    (  K byronStartBound
    :* K shelleyStartBound
    :* K allegraStartBound
    :* K maryStartBound
    :* K alonzoStartBound
    :* Nil
    )

cardanoShape :: History.Shape (CardanoEras Crypto)
cardanoShape = History.Shape $ Exactly $
    (  K byronEraParams
    :* K shelleyEraParams
    :* K allegraEraParams
    :* K maryEraParams
    :* K alonzoEraParams
    :* Nil
    )

summary :: History.Summary (CardanoEras Crypto)
summary =
    State.reconstructSummary
      cardanoShape
      (State.TransitionKnown shelleyTransitionEpoch)
      (hardForkLedgerStatePerEra (ledgerStateByron byronLedger))
  where
    (_, byronLedger) = head $ Golden.exampleLedgerState Byron.examples

eraInfoByron :: SingleEraInfo ByronBlock
eraInfoByron = singleEraInfo (Proxy @ByronBlock)

eraInfoShelley :: SingleEraInfo (ShelleyBlock StandardShelley)
eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock StandardShelley))

codecConfig :: CardanoCodecConfig Crypto
codecConfig =
    CardanoCodecConfig
      Byron.codecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig

ledgerStateByron ::
     LedgerState ByronBlock
  -> LedgerState (CardanoBlock Crypto)
ledgerStateByron stByron =
    HardForkLedgerState $ HardForkState $ TZ cur
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stByron
        }

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Multi-era examples, e.g., applying a transaction to the wrong era.
multiEraExamples :: Examples (CardanoBlock Crypto)
multiEraExamples = mempty {
      Golden.exampleApplyTxErr = labelled [
          ("WrongEraByron",   exampleApplyTxErrWrongEraByron)
        , ("WrongEraShelley", exampleApplyTxErrWrongEraShelley)
        ]
    , Golden.exampleQuery = labelled [
          ("AnytimeByron",   exampleQueryAnytimeByron)
        , ("AnytimeShelley", exampleQueryAnytimeShelley)
        , ("HardFork",       exampleQueryHardFork)
        ]
    , Golden.exampleResult = labelled [
          ("EraMismatchByron",   exampleResultEraMismatchByron)
        , ("EraMismatchShelley", exampleResultEraMismatchShelley)
        , ("AnytimeByron",       exampleResultAnytimeByron)
        , ("AnytimeShelley",     exampleResultAnytimeShelley)
        , ("HardFork",           exampleResultHardFork)
        ]
    , Golden.exampleLedgerConfig = Golden.unlabelled exampleLedgerConfigCardano
    }

-- | The examples: the examples from each individual era lifted in to
-- 'CardanoBlock' /and/ the multi-era examples.
examples :: Examples (CardanoBlock Crypto)
examples = combineEras eraExamples <> multiEraExamples

-- | Applying a Shelley thing to a Byron ledger
exampleEraMismatchByron :: MismatchEraInfo (CardanoEras Crypto)
exampleEraMismatchByron =
    MismatchEraInfo $ MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)

-- | Applying a Byron thing to a Shelley ledger
exampleEraMismatchShelley :: MismatchEraInfo (CardanoEras Crypto)
exampleEraMismatchShelley =
    MismatchEraInfo $ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))

exampleApplyTxErrWrongEraByron :: ApplyTxErr (CardanoBlock Crypto)
exampleApplyTxErrWrongEraByron =
      HardForkApplyTxErrWrongEra exampleEraMismatchByron

exampleApplyTxErrWrongEraShelley :: ApplyTxErr (CardanoBlock Crypto)
exampleApplyTxErrWrongEraShelley =
      HardForkApplyTxErrWrongEra exampleEraMismatchShelley

exampleQueryEraMismatchByron :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryEraMismatchByron =
    SomeSecond (QueryIfCurrentShelley Shelley.GetLedgerTip)

exampleQueryEraMismatchShelley :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryEraMismatchShelley =
    SomeSecond (QueryIfCurrentByron Byron.GetUpdateInterfaceState)

exampleQueryAnytimeByron :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryAnytimeByron =
    SomeSecond (QueryAnytimeByron GetEraStart)

exampleQueryAnytimeShelley :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryAnytimeShelley =
    SomeSecond (QueryAnytimeShelley GetEraStart)

exampleQueryHardFork :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryHardFork =
    SomeSecond (QueryHardFork GetInterpreter)

exampleResultEraMismatchByron :: SomeResult (CardanoBlock Crypto)
exampleResultEraMismatchByron =
    SomeResult
      (QueryIfCurrentShelley Shelley.GetLedgerTip)
      (Left exampleEraMismatchByron)

exampleResultEraMismatchShelley :: SomeResult (CardanoBlock Crypto)
exampleResultEraMismatchShelley =
    SomeResult
      (QueryIfCurrentByron Byron.GetUpdateInterfaceState)
      (Left exampleEraMismatchShelley)

exampleResultAnytimeByron :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeByron =
    SomeResult (QueryAnytimeByron GetEraStart) (Just byronStartBound)

exampleResultAnytimeShelley :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeShelley =
    SomeResult (QueryAnytimeShelley GetEraStart) (Just shelleyStartBound)

exampleResultHardFork :: SomeResult (CardanoBlock Crypto)
exampleResultHardFork =
    SomeResult (QueryHardFork GetInterpreter) (History.mkInterpreter summary)

exampleLedgerConfigCardano :: HardForkLedgerConfig (CardanoEras Crypto)
exampleLedgerConfigCardano = HardForkLedgerConfig
  cardanoShape
  ( PerEraLedgerConfig
    -- We use 10, 20, 30, 40, ... as the transition epochs
    (  WrapPartialLedgerConfig (ByronPartialLedgerConfig   lcByron   (TriggerHardForkAtEpoch shelleyTransitionEpoch))
    :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcShelley (TriggerHardForkAtEpoch 20))
    :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcAllegra (TriggerHardForkAtEpoch 30))
    :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcMary    (TriggerHardForkAtEpoch 40))
    :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcAlonzo  TriggerHardForkNever)
    :* Nil
    )
  ) -- PerEraLedgerConfig
  where
    Golden.Examples { Golden.exampleLedgerConfig = lcByronEs } :* _ = eraExamples
    lcByron   = snd $ head lcByronEs
    lcShelley = arbitraryPartialShelleyLedgerConfig ()
    lcAllegra = arbitraryPartialShelleyLedgerConfig ()
    lcMary    = arbitraryPartialShelleyLedgerConfig ()
    lcAlonzo  = arbitraryPartialShelleyLedgerConfig @(AlonzoEra StandardCrypto)
      AlonzoGenesis {
          coinsPerUTxOWord     = Coin 1 -- :: Coin,
        , costmdls             = Map.fromList [(PlutusV1, CostModel (Map.fromList [("A", 79), ("V", 78)]))]
        , prices               = Prices (Coin 90) (Coin 91) -- :: Prices,
        , maxTxExUnits         = ExUnits 123 123
        , maxBlockExUnits      = ExUnits 223 223
        , maxValSize           = 1234
        , collateralPercentage = 20
        , maxCollateralInputs  = 30
        }

    arbitraryPartialShelleyLedgerConfig :: Core.TranslationContext era -> ShelleyLedgerConfig era
    arbitraryPartialShelleyLedgerConfig translationContext = ShelleyLedgerConfig {
      shelleyLedgerCompactGenesis = compactGenesis genesis
    , shelleyLedgerGlobals = mkShelleyGlobals
        genesis
        epochInfo
        26
    , shelleyLedgerTranslationContext = translationContext
    }
      where
        epochInfo = fixedEpochInfo epochSize slotLength

        epochSize = EpochSize 4

        slotLength' = secondsToNominalDiffTime 7
        slotLength = mkSlotLength slotLength'

        genesis = ShelleyGenesis {
            sgSystemStart       = UTCTime (fromOrdinalDate 2021 100) (secondsToDiffTime 1000)
          , sgNetworkMagic      = 1
          , sgNetworkId         = Mainnet
          , sgActiveSlotsCoeff  = 2
          , sgSecurityParam     = 3
          , sgEpochLength       = epochSize
          , sgSlotsPerKESPeriod = 5
          , sgMaxKESEvolutions  = 6
          , sgSlotLength        = slotLength'
          , sgUpdateQuorum      = 8
          , sgMaxLovelaceSupply = 9000
          , sgProtocolParams    = PParams {
                _minfeeA = 10
              , _minfeeB = 11
              , _maxBBSize = 1200
              , _maxTxSize = 1300
              , _maxBHSize = 1400
              , _keyDeposit = Coin 15
              , _poolDeposit = Coin 16
              , _eMax = 170
              , _nOpt = 18
              , _a0 = 0.19
              , _rho = unitIntervalFromRational 0.21
              , _tau = unitIntervalFromRational 0.22
              , _d = unitIntervalFromRational 0.23
              , _extraEntropy = Nonce (UnsafeHash "724")
              , _protocolVersion = ProtVer 25 26
              , _minUTxOValue = Coin 27
              , _minPoolCost = Coin 28
              }
          , sgGenDelegs         = Map.fromList [
              (KeyHash (UnsafeHash "829"), GenDelegPair (KeyHash (UnsafeHash "929")) (UnsafeHash "1029"))
            , (KeyHash (UnsafeHash "830"), GenDelegPair (KeyHash (UnsafeHash "930")) (UnsafeHash "1030"))
            ]
          , sgInitialFunds      = Map.empty -- This is cleared by `compactGenesis`
          , sgStaking           = emptyGenesisStaking -- This is cleared by `compactGenesis`
        }

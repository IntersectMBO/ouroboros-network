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
import           Data.SOP.Strict

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

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork ()

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

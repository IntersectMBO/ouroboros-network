{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Test.Consensus.Shelley.Examples (
    -- * Setup
    Block
  , testEpochInfo
  , testShelleyGenesis
  , codecConfig
  , mkDummyHash
    -- * Examples
  , examples
  , exampleBlock
  , exampleHeader
  , exampleHeaderHash
  , exampleGenTx
  , exampleGenTxId
  , exampleApplyTxErr
  , exampleChainDepState
  , exampleLedgerState
  , exampleHeaderState
  , exampleExtLedgerState
  ) where

import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Word (Word64)

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Prelude (Natural)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Util.Time

import qualified Shelley.Spec.Ledger.API as SL
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Scripts as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Ledgers as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS
import qualified Shelley.Spec.Ledger.STS.Tickn as STS
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto,
                     hashKeyVRF)
import qualified Test.Shelley.Spec.Ledger.Examples as Examples
import           Test.Shelley.Spec.Ledger.Orphans ()
import qualified Test.Shelley.Spec.Ledger.Utils as SL (testGlobals)

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Golden (labelled, unlabelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import           Test.Cardano.Crypto.VRF.Fake (VerKeyVRF (..))
import           Test.Consensus.Shelley.MockCrypto

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

testEpochInfo :: EpochInfo Identity
testEpochInfo = SL.epochInfo SL.testGlobals

testMaxMajorPV :: Natural
testMaxMajorPV = SL.maxMajorPV SL.testGlobals

-- | These are dummy values.
testShelleyGenesis :: SL.ShelleyGenesis c
testShelleyGenesis = SL.ShelleyGenesis {
      sgSystemStart       = UTCTime (fromGregorian 2020 5 14) 0
    , sgNetworkMagic      = 0
    , sgNetworkId         = SL.Testnet
      -- Chosen to match SL.activeSlotCoeff
    , sgActiveSlotsCoeff  = 0.9
    , sgSecurityParam     = SL.securityParameter SL.testGlobals
    , sgEpochLength       = runIdentity $ epochInfoSize testEpochInfo 0
    , sgSlotsPerKESPeriod = SL.slotsPerKESPeriod SL.testGlobals
    , sgMaxKESEvolutions  = SL.maxKESEvo SL.testGlobals
      -- Not important
    , sgSlotLength        = secondsToNominalDiffTime 2
    , sgUpdateQuorum      = SL.quorum  SL.testGlobals
    , sgMaxLovelaceSupply = SL.maxLovelaceSupply SL.testGlobals
    , sgProtocolParams    = SL.emptyPParams
    , sgGenDelegs         = Map.empty
    , sgInitialFunds      = Map.empty
    , sgStaking           = SL.emptyGenesisStaking
    }

codecConfig :: CodecConfig (Block ShortHash)
codecConfig = ShelleyCodecConfig

mkDummyHash :: forall c a. Crypto c => Proxy c -> Int -> SL.Hash c a
mkDummyHash _ = coerce . SL.hashWithSerialiser @(SL.HASH c) toCBOR

mkScriptHash :: Int -> SL.ScriptHash (TPraosMockCrypto ShortHash)
mkScriptHash = SL.ScriptHash . mkDummyHash (Proxy @(TPraosMockCrypto ShortHash))

mkKeyHash :: Int -> SL.KeyHash discriminator (TPraosMockCrypto ShortHash)
mkKeyHash = SL.KeyHash . mkDummyHash (Proxy @(TPraosMockCrypto ShortHash))

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Golden.Examples (Block ShortHash)
examples = Golden.Examples {
      exampleBlock            = unlabelled exampleBlock
    , exampleSerialisedBlock  = unlabelled exampleSerialisedBlock
    , exampleHeader           = unlabelled exampleHeader
    , exampleSerialisedHeader = unlabelled exampleSerialisedHeader
    , exampleHeaderHash       = unlabelled exampleHeaderHash
    , exampleGenTx            = unlabelled exampleGenTx
    , exampleGenTxId          = unlabelled exampleGenTxId
    , exampleApplyTxErr       = unlabelled exampleApplyTxErr
    , exampleQuery            = queries
    , exampleResult           = results
    , exampleAnnTip           = unlabelled exampleAnnTip
    , exampleLedgerState      = unlabelled exampleLedgerState
    , exampleChainDepState    = unlabelled exampleChainDepState
    , exampleExtLedgerState   = unlabelled exampleExtLedgerState
    }
  where
    queries = labelled [
          ("GetLedgerTip",              SomeBlock GetLedgerTip)
        , ("GetEpochNo",                SomeBlock GetEpochNo)
        , ("GetCurrentPParams",         SomeBlock GetCurrentPParams)
        , ("GetProposedPParamsUpdates", SomeBlock GetProposedPParamsUpdates)
        , ("GetStakeDistribution",      SomeBlock GetStakeDistribution)
        , ("GetNonMyopicMemberRewards",
            SomeBlock $ GetNonMyopicMemberRewards $ Set.fromList [
                Left  (SL.Coin 100)
              , Right (SL.ScriptHashObj (mkScriptHash 1))
              , Right (SL.KeyHashObj (mkKeyHash 2))
              ])

      ]

    results = labelled [
          ("LedgerTip",              SomeResult GetLedgerTip (blockPoint exampleBlock))
        , ("EpochNo",                SomeResult GetEpochNo 10)
        , ("EmptyPParams",           SomeResult GetCurrentPParams SL.emptyPParams)
        , ("ProposedPParamsUpdatse", SomeResult GetProposedPParamsUpdates proposedPParamsUpdates)
        , ("StakeDistribution",      SomeResult GetStakeDistribution stakeDistribution)
        , ("NonMyopicMemberRewards", SomeResult (GetNonMyopicMemberRewards Set.empty)
            (NonMyopicMemberRewards $ Map.fromList [
                (Left (SL.Coin 100), Map.singleton (mkKeyHash 2) (SL.Coin 3))
              , (Right (SL.ScriptHashObj (mkScriptHash 1)), Map.empty)
              , (Right (SL.KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (SL.Coin 9))
              ]))
        ]

    proposedPParamsUpdates :: SL.ProposedPPUpdates (TPraosMockCrypto ShortHash)
    proposedPParamsUpdates = SL.ProposedPPUpdates $ Map.singleton
        (SL.hashKey 0)
        (SL.emptyPParamsUpdate {SL._keyDeposit = SJust 100})

    stakeDistribution :: SL.PoolDistr (TPraosMockCrypto ShortHash)
    stakeDistribution = SL.PoolDistr $ Map.singleton
        (SL.KeyHash $ SL.hashWithSerialiser toCBOR 4)
        (1, hashKeyVRF $ VerKeyFakeVRF 0)

exampleBlock :: Block ShortHash
exampleBlock = mkShelleyBlock Examples.blockEx3B

exampleSerialisedBlock :: Serialised (Block ShortHash)
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleHeader :: Header (Block ShortHash)
exampleHeader = getHeader exampleBlock

exampleSerialisedHeader :: SerialisedHeader (Block ShortHash)
exampleSerialisedHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")

exampleHeaderHash :: HeaderHash (Block ShortHash)
exampleHeaderHash = blockHash exampleBlock

exampleGenTx :: GenTx (Block ShortHash)
exampleGenTx = mkShelleyTx Examples.txEx2A

exampleGenTxId :: GenTxId (Block ShortHash)
exampleGenTxId = txId exampleGenTx

-- TODO incomplete, this type has tons of constructors that can all change.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1896.
exampleApplyTxErr :: ApplyTxErr (Block ShortHash)
exampleApplyTxErr =
      ApplyTxError
    $ pure
    $ STS.LedgerFailure
    $ STS.UtxowFailure
    $ STS.InvalidWitnessesUTXOW [SL.VKey 1]

exampleAnnTip :: AnnTip (Block ShortHash)
exampleAnnTip = AnnTip {
      annTipSlotNo  = SlotNo 14
    , annTipBlockNo = BlockNo 6
    , annTipInfo    = exampleHeaderHash
    }

exampleChainDepState :: ChainDepState (BlockProtocol (Block ShortHash))
exampleChainDepState =
    TPraosState.append 2             (mkPrtclState 2) $
    TPraosState.empty  (NotOrigin 1) (mkPrtclState 1)
  where
    mkPrtclState :: Word64 -> SL.ChainDepState (TPraosMockCrypto ShortHash)
    mkPrtclState seed = SL.ChainDepState
      { SL.csProtocol = STS.PrtclState
          (Map.fromList
          [ (SL.KeyHash (mkDummyHash (Proxy @(TPraosMockCrypto ShortHash)) 1), 1)
          , (SL.KeyHash (mkDummyHash (Proxy @(TPraosMockCrypto ShortHash)) 2), 2)
          ])
          (SL.mkNonceFromNumber seed)
          (SL.mkNonceFromNumber seed)
      , SL.csTickn = STS.TicknState
          SL.NeutralNonce
          (SL.mkNonceFromNumber seed)
      , SL.csLabNonce =
          SL.mkNonceFromNumber seed
      }

exampleLedgerState :: LedgerState (Block ShortHash)
exampleLedgerState = reapplyLedgerBlock
    cfg
    (mkShelleyBlock newBlock :: Block ShortHash)
    (TickedShelleyLedgerState {
        untickedLedgerTip  = GenesisPoint
      , untickedHistory    = History.empty
      , tickedShelleyState = STS.chainNes startState
      })
  where
    Examples.CHAINExample { startState, newBlock } = Examples.ex2A (Proxy @ShortHash)
    cfg = FullBlockConfig {
          blockConfigLedger = mkShelleyLedgerConfig testShelleyGenesis testEpochInfo testMaxMajorPV
        , blockConfigBlock  = mkShelleyBlockConfig (SL.ProtVer 2 0) testShelleyGenesis NotABlockIssuer
        , blockConfigCodec  = codecConfig
        }

exampleHeaderState :: HeaderState (Block ShortHash)
exampleHeaderState = genesisHeaderState st
  where
    prtclState :: SL.ChainDepState (TPraosMockCrypto ShortHash)
    prtclState = SL.ChainDepState
      { SL.csProtocol = STS.PrtclState
          (Map.fromList
            [(SL.KeyHash (mkDummyHash (Proxy @(TPraosMockCrypto ShortHash)) 1), 1)])
          (SL.mkNonceFromNumber 1)
          SL.NeutralNonce
      , SL.csTickn = STS.TicknState
          SL.NeutralNonce
          (SL.mkNonceFromNumber 2)
      , SL.csLabNonce =
          SL.mkNonceFromNumber 2
      }

    st :: TPraosState (ConcreteCrypto ShortHash)
    st = TPraosState.empty (NotOrigin 1) prtclState

exampleExtLedgerState :: ExtLedgerState (Block ShortHash)
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

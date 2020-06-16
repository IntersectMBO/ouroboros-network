{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Test.Consensus.Shelley.Examples (
    -- * Setup
    Block
  , testEpochInfo
  , testShelleyGenesis
  , mkDummyHash
    -- * Examples
  , exampleBlock
  , exampleHeader
  , exampleHeaderHash
  , exampleGenTx
  , exampleGenTxId
  , exampleApplyTxErr
  , exampleConsensusState
  , exampleLedgerState
  , exampleHeaderState
  , exampleExtLedgerState
  ) where

import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Time (UTCTime (..), fromGregorian)

import           Cardano.Crypto (ProtocolMagicId (..))
import           Cardano.Prelude (Natural)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block (HeaderHash, blockHash, genesisPoint)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Time

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Ledgers as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import qualified Test.Shelley.Spec.Ledger.Examples as Examples
import qualified Test.Shelley.Spec.Ledger.Utils as SL (testGlobals)

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Test.Util.Orphans.Arbitrary ()

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
    , sgProtocolMagicId   = ProtocolMagicId 0
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

mkDummyHash :: forall c a. Crypto c => Proxy c -> Int -> SL.Hash c a
mkDummyHash _ = coerce . SL.hash @(SL.HASH c)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

exampleBlock :: Block
exampleBlock = mkShelleyBlock Examples.blockEx3B

exampleHeader :: Header Block
exampleHeader = getHeader exampleBlock

exampleHeaderHash :: HeaderHash Block
exampleHeaderHash = blockHash exampleBlock

exampleGenTx :: GenTx Block
exampleGenTx = mkShelleyTx Examples.txEx2A

exampleGenTxId :: GenTxId Block
exampleGenTxId = txId exampleGenTx

-- TODO incomplete, this type has tons of constructors that can all change.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1896.
exampleApplyTxErr :: ApplyTxErr Block
exampleApplyTxErr =
      ApplyTxError
    $ pure
    $ STS.LedgerFailure
    $ STS.UtxowFailure
    $ STS.InvalidWitnessesUTXOW ([SL.VKey 1], [])

exampleConsensusState :: ConsensusState (BlockProtocol Block)
exampleConsensusState =
    TPraosState.append 2      (mkPrtclState 2) $
    TPraosState.empty  (At 1) (mkPrtclState 1)
  where
    mkPrtclState :: Natural -> STS.PrtclState TPraosMockCrypto
    mkPrtclState seed = STS.PrtclState
      (Map.fromList
       [ (SL.KeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 1), 1)
       , (SL.KeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 2), 2)
       ])
      SL.NeutralNonce
      (SL.mkNonce seed)
      (SL.mkNonce seed)
      (SL.mkNonce seed)

exampleLedgerState :: LedgerState Block
exampleLedgerState = reapplyLedgerBlock
    ledgerCfg
    (mkShelleyBlock newBlock)
    (Ticked 0 ShelleyLedgerState {
        ledgerTip    = genesisPoint
      , history      = History.empty
      , shelleyState = STS.chainNes startState
      })
  where
    Examples.CHAINExample { startState, newBlock } = Examples.ex2A
    ledgerCfg = mkShelleyLedgerConfig testShelleyGenesis testEpochInfo testMaxMajorPV

exampleHeaderState :: HeaderState Block
exampleHeaderState = genesisHeaderState st
  where
    prtclState :: STS.PrtclState TPraosMockCrypto
    prtclState = STS.PrtclState
      (Map.fromList
        [(SL.KeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 1), 1)])
      SL.NeutralNonce
      (SL.mkNonce 1)
      SL.NeutralNonce
      (SL.mkNonce 2)

    st :: TPraosState ConcreteCrypto
    st = TPraosState.empty (At 1) prtclState

exampleExtLedgerState :: ExtLedgerState Block
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

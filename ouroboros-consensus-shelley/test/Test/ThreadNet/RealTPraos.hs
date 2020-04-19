{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.ThreadNet.RealTPraos (
    tests
  ) where

import           Control.Monad (replicateM)
import           Data.List ((!!))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time (Day (..), UTCTime (..))
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto (ProtocolMagicId (..))
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), SignKeyDSIGN,
                     signedDSIGN)
import           Cardano.Crypto.KES.Class (SignKeyKES, deriveVerKeyKES,
                     genKeyKES)
import           Cardano.Crypto.VRF.Class (SignKeyVRF, deriveVerKeyVRF,
                     genKeyVRF)
import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Mempool.API (extractTxs)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random

import           Test.ThreadNet.General
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.TxData as SL

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (DSIGN)

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import qualified Test.Shelley.Spec.Ledger.Generator.Constants as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Gen.Presets
import           Test.ThreadNet.TxGen.Shelley (ShelleyTxGenExtra (..))

tests :: TestTree
tests = testGroup "RealTPraos"
    [ testProperty "simple convergence" $
          forAll (SecurityParam <$> elements [5, 10])
            $ \k ->
          forAllShrink
              (genRealTPraosTestConfig k)
              shrinkRealTPraosTestConfig
            $ \testConfig ->
          prop_simple_real_tpraos_convergence k testConfig
    ]

tpraosSlotLength :: SlotLength
tpraosSlotLength = slotLengthFromSec 2

prop_simple_real_tpraos_convergence
  :: SecurityParam
  -> TestConfig
  -> Property
prop_simple_real_tpraos_convergence k
  testConfig@TestConfig{numCoreNodes = NumCoreNodes n, initSeed} =
    prop_general PropGeneralArgs
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = fromIntegral . length . extractTxs
      , pgaExpectedBlockRejection = const False
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     = Nothing
      , pgaFixedSchedule          = Nothing
      , pgaSecurityParam          = k
      , pgaTestConfig             = testConfig
      }
      testOutput
  where
    testOutput =
        runTestNetwork testConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \(CoreNodeId nid) ->
              plainTestNodeInitialization $
                mkProtocolRealTPraos
                  genesisConfig
                  (coreNodes !! fromIntegral nid)
            , rekeying    = Nothing
            , txGenExtra  = ShelleyTxGenExtra $ Gen.GenEnv keySpace constants
            }

    constants :: Gen.Constants
    constants = Gen.defaultConstants
      { Gen.frequencyMIRCert = 0
      }

    keySpace :: Gen.KeySpace
    keySpace = Gen.KeySpace
        (cnkiCoreNode <$> cn)
        (ksKeyPairs <> (cnkiKeyPair <$> cn))
        ksMSigScripts
        ksVRFKeyPairs
      where
        cn = coreNodeKeys <$> coreNodes
        Gen.KeySpace_ { ksKeyPairs, ksMSigScripts, ksVRFKeyPairs } =
          Gen.Presets.keySpace constants

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    maxKESEvolution :: Word64
    maxKESEvolution = 100 -- TODO

    coreNodes :: [CoreNode TPraosMockCrypto]
    coreNodes = withSeed initSeed $
      replicateM (fromIntegral n) $
      genCoreNode maxKESEvolution initialKESPeriod

    genesisConfig :: ShelleyGenesis TPraosMockCrypto
    genesisConfig = mkGenesisConfig k maxKESEvolution coreNodes

    epochSize :: EpochSize
    epochSize = sgEpochLength genesisConfig

data CoreNode c = CoreNode {
      cnGenesisKey  :: !(SignKeyDSIGN (SL.DSIGN c))
    , cnDelegateKey :: !(SignKeyDSIGN (SL.DSIGN c))
      -- ^ Cold delegate key. The hash of the corresponding verification
      -- (public) key will be used as the payment credential.
    , cnStakingKey  :: !(SignKeyDSIGN (SL.DSIGN c))
      -- ^ The hash of the corresponding verification (public) key will be
      -- used as the staking credential.
    , cnVRF         :: !(SignKeyVRF   (SL.VRF   c))
    , cnKES         :: !(SignKeyKES   (SL.KES   c))
    , cnOCert       :: !(SL.OCert               c)
    }

data CoreNodeKeyInfo = CoreNodeKeyInfo
  { cnkiKeyPair
      ::  ( SL.KeyPair 'SL.Regular TPraosMockCrypto
          , SL.KeyPair 'SL.Regular TPraosMockCrypto
          )
  , cnkiCoreNode :: (SL.KeyPair 'SL.Genesis TPraosMockCrypto, Gen.AllPoolKeys)
  }

coreNodeKeys
  :: CoreNode TPraosMockCrypto
  -> CoreNodeKeyInfo
coreNodeKeys CoreNode{cnGenesisKey, cnDelegateKey, cnStakingKey, cnVRF, cnKES}
  = CoreNodeKeyInfo
      { cnkiCoreNode =
          ( mkDSIGNKeyPair cnGenesisKey
          , Gen.AllPoolKeys
            { Gen.cold = mkDSIGNKeyPair cnDelegateKey
            , Gen.vrf  = mkVRFKeyPair cnVRF
            , Gen.hot  = [(SL.KESPeriod 100, mkKESKeyPair cnKES)]
            , Gen.hk   = SL.hashKey (SL.DiscVKey $ deriveVerKeyDSIGN cnDelegateKey)
            }
          )
      , cnkiKeyPair = (mkDSIGNKeyPair cnDelegateKey, mkDSIGNKeyPair cnStakingKey)
      }
  where
    mkDSIGNKeyPair k = SL.KeyPair (SL.DiscVKey $ deriveVerKeyDSIGN k)
                                  (SL.SKey k)
    mkVRFKeyPair k = (k, deriveVerKeyVRF k)
    mkKESKeyPair k = (SL.SKeyES k, SL.VKeyES $ deriveVerKeyKES k)

genCoreNode
  :: (MonadRandom m, TPraosCrypto c)
  => Word64  -- ^ Max KES evolutions
  -> SL.KESPeriod
  -> m (CoreNode c)
genCoreNode maxKESEvolutions startKESPeriod = do
    genKey <- genKeyDSIGN
    delKey <- genKeyDSIGN
    stkKey <- genKeyDSIGN
    vrfKey <- genKeyVRF
    kesKey <- genKeyKES (fromIntegral maxKESEvolutions)
    let kesPub = SL.VKeyES $ deriveVerKeyKES kesKey
    sigma <- signedDSIGN
      ()
      (kesPub, certificateIssueNumber, startKESPeriod)
      delKey
    let ocert = SL.OCert {
            ocertVkHot     = kesPub
          , ocertN         = certificateIssueNumber
          , ocertKESPeriod = startKESPeriod
          , ocertSigma     = SL.UnsafeSig sigma
          }
    return CoreNode {
        cnGenesisKey  = genKey
      , cnDelegateKey = delKey
      , cnStakingKey  = stkKey
      , cnVRF         = vrfKey
      , cnKES         = kesKey
      , cnOCert       = ocert
      }
  where
    certificateIssueNumber = 0

mkGenesisConfig
  :: forall c. TPraosCrypto c
  => SecurityParam
  -> Word64  -- ^ Max KES evolutions
  -> [CoreNode c]
  -> ShelleyGenesis c
mkGenesisConfig k maxKESEvolutions coreNodes = ShelleyGenesis {
      sgStartTime             = SystemStart $ UTCTime (ModifiedJulianDay 0) 0
    , sgNetworkMagic          = NetworkMagic 0
    , sgProtocolMagicId       = ProtocolMagicId 0
    , sgActiveSlotsCoeff      = 0.5 -- TODO 1 is not accepted by 'mkActiveSlotCoeff'
    , sgDecentralisationParam = 1
    , sgSecurityParam         = k
    , sgEpochLength           = EpochSize (10 * maxRollbacks k)
    , sgSlotsPerKESPeriod     = 10 -- TODO
    , sgMaxKESEvolutions      = maxKESEvolutions
    , sgSlotLength            = tpraosSlotLength
    , sgUpdateQuorum          = 1  -- TODO
    , sgMaxMajorPV            = 1000 -- TODO
    , sgMaxLovelaceSupply     = maxLovelaceSupply
    , sgMaxBodySize           = 10000 -- TODO
    , sgMaxHeaderSize         = 1000 -- TODO
    , sgGenDelegs             = coreNodesToGenesisMapping
    , sgInitialFunds          = initialFunds
    }
  where
    initialLovelacePerCoreNode :: Word64
    initialLovelacePerCoreNode = 1000

     -- TODO
    maxLovelaceSupply :: Word64
    maxLovelaceSupply =
      fromIntegral (length coreNodes) * initialLovelacePerCoreNode

    coreNodesToGenesisMapping :: Map (SL.GenKeyHash c) (SL.KeyHash c)
    coreNodesToGenesisMapping  = Map.fromList
      [ ( SL.GenKeyHash $ SL.hash $ deriveVerKeyDSIGN cnGenesisKey
        ,    SL.KeyHash $ SL.hash $ deriveVerKeyDSIGN cnDelegateKey
        )
      | CoreNode { cnGenesisKey, cnDelegateKey } <- coreNodes
      ]

    initialFunds :: Map (SL.Addr c) SL.Coin
    initialFunds = Map.fromList
      [ (addr, coin)
      | CoreNode { cnDelegateKey, cnStakingKey } <- coreNodes
      , let addr = SL.Addr (mkCredential cnDelegateKey)
                           (SL.StakeRefBase (mkCredential cnStakingKey))
            coin = SL.Coin $ fromIntegral initialLovelacePerCoreNode
      ]

    mkCredential :: SignKeyDSIGN (DSIGN c) -> SL.Credential c
    mkCredential = SL.KeyHashObj . SL.hashKey . SL.VKey . deriveVerKeyDSIGN

mkProtocolRealTPraos
  :: forall c. Crypto c
  => ShelleyGenesis c
  -> CoreNode c
  -> ProtocolInfo (ShelleyBlock c)
mkProtocolRealTPraos genesis CoreNode { cnDelegateKey, cnVRF, cnKES, cnOCert } =
    protocolInfoShelley genesis protVer (Just credentials)
  where
    protVer = SL.ProtVer 0 0

    credentials :: TPraosLeaderCredentials c
    credentials = TPraosLeaderCredentials {
        tpraosLeaderCredentialsSignKey    = cnKES
      , tpraosLeaderCredentialsIsCoreNode = TPraosIsCoreNode {
          tpraosIsCoreNodeOpCert     = cnOCert
        , tpraosIsCoreNodeColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , tpraosIsCoreNodeSignKeyVRF = cnVRF
        }
      }

genRealTPraosTestConfig :: SecurityParam -> Gen TestConfig
genRealTPraosTestConfig _k = do
    numCoreNodes <- arbitrary
    numSlots     <- arbitrary

    -- TODO generate more interesting scenarios
    let nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        nodeTopology = meshNodeTopology numCoreNodes

    initSeed <- arbitrary

    pure TestConfig
      { nodeJoinPlan
      , nodeRestarts = noRestarts
      , nodeTopology
      , numCoreNodes
      , numSlots
      , slotLength = tpraosSlotLength
      , initSeed
      }

shrinkRealTPraosTestConfig :: TestConfig -> [TestConfig]
shrinkRealTPraosTestConfig _ = [] -- TODO

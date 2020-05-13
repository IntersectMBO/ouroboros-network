{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.ThreadNet.RealTPraos (
    tests
  ) where

import           Control.Monad (replicateM)
import           Data.List ((!!))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
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
import           Cardano.Crypto.Seed (mkSeedFromBytes)
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

import qualified Shelley.Spec.Ledger.BaseTypes as SL
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
    [ testProperty "simple convergence" $ withMaxSuccess 20 $
          forAll (SecurityParam <$> elements [5, 10])
            $ \k ->
          -- TODO it would be good to test this with intermediate values, but at
          -- the moment the test sometimes fails if we do this.
          -- > choose (0,1)
          forAll (elements [0,1])
            $ \d ->
          forAllShrink
              (genRealTPraosTestConfig k)
              shrinkRealTPraosTestConfig
            $ \testConfig ->
          prop_simple_real_tpraos_convergence k d testConfig
    ]

tpraosSlotLength :: SlotLength
tpraosSlotLength = slotLengthFromSec 2

prop_simple_real_tpraos_convergence
  :: SecurityParam
  -> Double -- Decentralisation parameter
  -> TestConfig
  -> Property
prop_simple_real_tpraos_convergence k d
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
    coreNodes = withSeed initSeed $ do
      x <- genCoreNode (SL.KESPeriod 1)
      xs <- replicateM (fromIntegral $ n - 1) $
        genCoreNode initialKESPeriod
      return $ x:xs


    genesisConfig :: ShelleyGenesis TPraosMockCrypto
    genesisConfig = mkGenesisConfig k d maxKESEvolution coreNodes

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
      ::  ( SL.KeyPair 'SL.Payment TPraosMockCrypto
          , SL.KeyPair 'SL.Staking TPraosMockCrypto
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
            , Gen.hk   = SL.hashKey (SL.VKey $ deriveVerKeyDSIGN cnDelegateKey)
            }
          )
      , cnkiKeyPair = (mkDSIGNKeyPair cnDelegateKey, mkDSIGNKeyPair cnStakingKey)
      }
  where
    mkDSIGNKeyPair k = SL.KeyPair (SL.VKey $ deriveVerKeyDSIGN k)
                                  k
    mkVRFKeyPair k = (k, deriveVerKeyVRF k)
    mkKESKeyPair k = (k, deriveVerKeyKES k)

genCoreNode
  :: (MonadRandom m, TPraosCrypto c)
  => SL.KESPeriod
  -> m (CoreNode c)
genCoreNode startKESPeriod = do
    genKey <- withMRSeed 8 genKeyDSIGN
    delKey <- withMRSeed 8 genKeyDSIGN
    stkKey <- withMRSeed 8 genKeyDSIGN
    vrfKey <- withMRSeed 8 genKeyVRF
    kesKey <- withMRSeed 8 genKeyKES
    let kesPub = deriveVerKeyKES kesKey
        sigma = signedDSIGN
          ()
          (kesPub, certificateIssueNumber, startKESPeriod)
          delKey
    let ocert = SL.OCert {
            ocertVkHot     = kesPub
          , ocertN         = certificateIssueNumber
          , ocertKESPeriod = startKESPeriod
          , ocertSigma     = sigma
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
    withMRSeed sz go = do
      seed <- mkSeedFromBytes <$> getRandomBytes sz
      pure $ go seed

mkGenesisConfig
  :: forall c. TPraosCrypto c
  => SecurityParam
  -> Double -- ^ Decentralisation param
  -> Word64  -- ^ Max KES evolutions
  -> [CoreNode c]
  -> ShelleyGenesis c
mkGenesisConfig k d maxKESEvolutions coreNodes = ShelleyGenesis {
      sgStartTime             = SystemStart $ UTCTime (ModifiedJulianDay 0) 0
    , sgNetworkMagic          = NetworkMagic 0
    , sgProtocolMagicId       = ProtocolMagicId 0
    , sgActiveSlotsCoeff      = 0.5 -- TODO 1 is not accepted by 'mkActiveSlotCoeff'
    , sgSecurityParam         = k
    , sgEpochLength           = EpochSize (10 * maxRollbacks k)
    , sgSlotsPerKESPeriod     = 10 -- TODO
    , sgMaxKESEvolutions      = maxKESEvolutions
    , sgSlotLength            = tpraosSlotLength
    , sgUpdateQuorum          = 1  -- TODO
    , sgMaxMajorPV            = 1000 -- TODO
    , sgMaxLovelaceSupply     = maxLovelaceSupply
    , sgProtocolParams        = pparams
    , sgGenDelegs             = coreNodesToGenesisMapping
    , sgInitialFunds          = initialFunds
    , sgStaking               = initialStake
    }
  where
    initialLovelacePerCoreNode :: Word64
    initialLovelacePerCoreNode = 1000

     -- TODO
    maxLovelaceSupply :: Word64
    maxLovelaceSupply =
      fromIntegral (length coreNodes) * initialLovelacePerCoreNode

    pparams :: SL.PParams
    pparams = SL.emptyPParams
      { SL._d =
            SL.truncateUnitInterval
          . realToFrac
          $ d
      , SL._maxBBSize = 10000 -- TODO
      , SL._maxBHSize = 1000 -- TODO
      }

    coreNodesToGenesisMapping :: Map (SL.KeyHash 'SL.Genesis c) (SL.KeyHash 'SL.GenesisDelegate c)
    coreNodesToGenesisMapping  = Map.fromList
      [ ( SL.KeyHash $ SL.hash $ deriveVerKeyDSIGN cnGenesisKey
        , SL.KeyHash $ SL.hash $ deriveVerKeyDSIGN cnDelegateKey
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

    -- In this initial stake, each core node delegates its stake to itself.
    initialStake :: ShelleyGenesisStaking c
    initialStake = ShelleyGenesisStaking
      { sgsPools = Map.fromList
          [ (pk, pp)
          | pp@(SL.PoolParams { _poolPubKey = pk }) <- Map.elems coreNodeToPoolMapping
          ]
      , sgsStake = Map.mapKeysMonotonic SL.coerceKeyRole
                 . Map.map SL._poolPubKey
                 $ coreNodeToPoolMapping
      }
      where
        coreNodeToPoolMapping :: Map (SL.KeyHash 'SL.StakePool c) (SL.PoolParams c)
          = Map.fromList
            [ ( SL.KeyHash $ SL.hash $ deriveVerKeyDSIGN cnStakingKey
              , SL.PoolParams
                { SL._poolPubKey = poolHash
                , SL._poolVrf = vrfHash
                  -- Each core node pledges its full stake to the pool.
                , SL._poolPledge = SL.Coin $ fromIntegral initialLovelacePerCoreNode
                , SL._poolCost = SL.Coin 1
                , SL._poolMargin = SL.UnsafeUnitInterval 0
                  -- Reward accounts live in a separate "namespace" to other
                  -- accounts, so it should be fine to use the same address.
                , SL._poolRAcnt = SL.RewardAcnt $ mkCredential cnDelegateKey
                , SL._poolOwners = Set.singleton $ SL.coerceKeyRole poolHash
                , SL._poolRelays = Seq.empty
                , SL._poolMD = SL.SNothing
                }
              )
            | CoreNode { cnDelegateKey, cnStakingKey, cnVRF } <- coreNodes
            , let poolHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let vrfHash = SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF
            ]

    mkCredential :: SignKeyDSIGN (DSIGN c) -> SL.Credential r c
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

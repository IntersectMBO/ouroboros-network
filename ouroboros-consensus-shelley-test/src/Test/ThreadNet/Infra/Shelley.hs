{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.ThreadNet.Infra.Shelley (
    CoreNode(..)
  , CoreNodeKeyInfo(..)
  , ShelleyTestConfig(..)
  , TestTuning(..)
  , testTuning
  , genCoreNode
  , mkGenesisConfig
  , coreNodeKeys
  , mkProtocolRealTPraos
  , ocertNodeRestarts
  , ocertRekeying
  , tpraosSlotLength
  , genShelleyTestConfig
  , shrinkShelleyTestConfig
  ) where

import           Control.Monad (forM, join, replicateM)
import           Data.ByteString.Conversion (toByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Numeric.Natural (Natural)

import           Test.QuickCheck

import           Cardano.Crypto (ProtocolMagicId (..))
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), SignKeyDSIGN,
                     signedDSIGN)
import           Cardano.Crypto.Hash (MD5)
import           Cardano.Crypto.KES.Class (Period, SignKeyKES, deriveVerKeyKES,
                     genKeyKES)
import           Cardano.Crypto.Seed (expandSeed, mkSeedFromBytes)
import qualified Cardano.Crypto.Seed as Seed
import           Cardano.Crypto.VRF.Class (SignKeyVRF, deriveVerKeyVRF,
                     genKeyVRF)
import           Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block.Forge
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Util.Random

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Gen
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.Util.Orphans.Arbitrary ()
import qualified Test.Util.Stream as Stream
import           Test.Util.Time (dawnOfTime)

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (TestNodeInitialization (..))
import           Test.ThreadNet.Util.NodeTopology
import           Test.Util.WrappedClock (NumSlots (..))

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.TxData as SL

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Forge
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (DSIGN,
                     HotKey (..), KES)

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
tpraosSlotLength :: SlotLength
tpraosSlotLength = slotLengthFromSec 2

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
    deriving (Show)

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
    let ocert = genOCert startKESPeriod certificateIssueNumber delKey kesPub
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

genOCert
  :: TPraosCrypto c
  => SL.KESPeriod
  -> Natural -- ^ Certificate issue number
  -> SignKeyDSIGN (DSIGN c) -- ^ Signing cold key
  -> SL.VerKeyKES c -- ^ KES public key
  -> SL.OCert c
genOCert startKESPeriod certificateIssueNumber delKey kesPub =
  let sigma = signedDSIGN
        ()
        (kesPub, certificateIssueNumber, startKESPeriod)
        delKey
  in SL.OCert
    { ocertVkHot     = kesPub
    , ocertN         = certificateIssueNumber
    , ocertKESPeriod = startKESPeriod
    , ocertSigma     = sigma
    }

mkGenesisConfig
  :: forall c. TPraosCrypto c
  => SecurityParam
  -> Double -- ^ Decentralisation param
  -> [CoreNode c]
  -> ShelleyGenesis c
mkGenesisConfig k d coreNodes = ShelleyGenesis {
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = SystemStart dawnOfTime
    , sgNetworkMagic          = NetworkMagic 0
    , sgNetworkId             = networkId
    , sgProtocolMagicId       = ProtocolMagicId 0
    , sgActiveSlotsCoeff      = 0.5 -- TODO 1 is not accepted by 'mkActiveSlotCoeff'
    , sgSecurityParam         = k
    , sgEpochLength           = EpochSize (10 * maxRollbacks k)
    , sgSlotsPerKESPeriod     = 10 -- TODO
    , sgMaxKESEvolutions      = 100 -- TODO
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
    networkId :: SL.Network
    networkId = SL.Testnet

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
      [ ( SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnGenesisKey
        , SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        )
      | CoreNode { cnGenesisKey, cnDelegateKey } <- coreNodes
      ]

    initialFunds :: Map (SL.Addr c) SL.Coin
    initialFunds = Map.fromList
      [ (addr, coin)
      | CoreNode { cnDelegateKey, cnStakingKey } <- coreNodes
      , let addr = SL.Addr networkId
                           (mkCredential cnDelegateKey)
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
                , SL._poolRAcnt = SL.RewardAcnt networkId $ mkCredential cnDelegateKey
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
  :: forall m c. (MonadRandom m, TPraosCrypto c)
  => ShelleyGenesis c
  -> CoreNode c
  -> ProtocolInfo m (ShelleyBlock c)
mkProtocolRealTPraos genesis CoreNode { cnDelegateKey, cnVRF, cnKES, cnOCert } =
    protocolInfoShelley genesis protVer (Just credentials)
  where
    protVer = SL.ProtVer 0 0

    credentials :: TPraosLeaderCredentials c
    credentials = TPraosLeaderCredentials {
        tpraosLeaderCredentialsSignKey    = HotKey 0 cnKES
      , tpraosLeaderCredentialsIsCoreNode = TPraosIsCoreNode {
          tpraosIsCoreNodeOpCert     = cnOCert
        , tpraosIsCoreNodeColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , tpraosIsCoreNodeSignKeyVRF = cnVRF
        }
      }

{-------------------------------------------------------------------------------
  Shelley test configuration
-------------------------------------------------------------------------------}

data ShelleyTestConfig = ShelleyTestConfig
  { stcTestConfig :: TestConfig
  , stcGenesis    :: ShelleyGenesis TPraosMockCrypto
  , stcCoreNodes  :: [CoreNode TPraosMockCrypto]
  } deriving (Show)

data TestTuning = TestTuning
  { -- | Should KES keys be rotated on schedule?
    rotateKESKeys :: Bool
  }

testTuning :: TestTuning
testTuning = TestTuning
  { rotateKESKeys = False }

genShelleyTestConfig
  :: TestTuning
  -> Gen ShelleyTestConfig
genShelleyTestConfig TestTuning {rotateKESKeys} = do
    initSeed <- arbitrary
    k <- SecurityParam <$> elements [5,10]
    d <- elements [x / 10 | x <- [1..10]]
    numCoreNodes@(NumCoreNodes n) <- arbitrary

    -- TODO generate more interesting scenarios
    let
      nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      nodeTopology = meshNodeTopology numCoreNodes

      initialKESPeriod :: SL.KESPeriod
      initialKESPeriod = SL.KESPeriod 0

      genesisConfig :: ShelleyGenesis TPraosMockCrypto
      genesisConfig = (mkGenesisConfig k d coreNodes)
        { sgMaxKESEvolutions = if rotateKESKeys then 2 else 100
        , sgSlotsPerKESPeriod = if rotateKESKeys then 5 else 10
        }

      coreNodes :: [CoreNode TPraosMockCrypto]
      coreNodes = withSeed initSeed $
        replicateM (fromIntegral n) $
        genCoreNode initialKESPeriod

    numSlots     <-
      if rotateKESKeys
        then pure . NumSlots $ 30
        else arbitrary

    nodeRestarts <-
      if rotateKESKeys
        then ocertNodeRestarts nodeJoinPlan numSlots genesisConfig coreNodes (0,0)
        else pure noRestarts

    let
      tc = TestConfig
        { nodeJoinPlan
        , nodeRestarts = nodeRestarts
        , nodeTopology
        , numCoreNodes
        , numSlots
        , slotLength = tpraosSlotLength
        , initSeed
        }

    pure $ ShelleyTestConfig
      { stcTestConfig = tc
      , stcGenesis = genesisConfig
      , stcCoreNodes = coreNodes
      }

shrinkShelleyTestConfig :: ShelleyTestConfig -> [ShelleyTestConfig]
shrinkShelleyTestConfig _ = [] -- TODO

{-------------------------------------------------------------------------------
  Node restarts for operational certificates
-------------------------------------------------------------------------------}

-- | Generate a node restart schedule which cycles nodes in order to replace
-- their operational certificates after the maximum number of KES periods.
--
ocertNodeRestarts
  :: NodeJoinPlan
  -> NumSlots         -- ^ Duration of the test. No replacements will be scheduled
                      --   after this number of slots.
  -> ShelleyGenesis c -- ^ Shelley genesis configuration. Needed to extract the
                      --   slots per KES period and the maximum number of KES
                      --   evolutions.
  -> [CoreNode c]     -- ^ Core nodes, including all of their keys.
  -> (Word64, Word64) -- ^ Permitted (eagerness, laziness) in replacing operational
                      --   certificates - that is, nodes may replace them up to
                      --   `eagerness` KES periods early, and up to `laziness`
                      --   KES periods late.
  -> Gen NodeRestarts
ocertNodeRestarts
  (NodeJoinPlan _m)
  (NumSlots t)
  ShelleyGenesis {sgSlotsPerKESPeriod, sgMaxKESEvolutions}
  coreNodes
  (eagerness, laziness)
    | t < ocertRegenCadence = pure noRestarts
    | otherwise =
      fmap NodeRestarts $ do
        slotCNPairs <- fmap join $ forM (zip coreNodes [0..]) $ \(cn, coreNodeIdx) -> do
          offsets <- Gen.infiniteListOf $ Gen.choose (- eagerness, laziness)
          let SL.KESPeriod firstKESPeriod = SL.ocertKESPeriod $ cnOCert cn
              periods =
                unfoldr
                  ( \(startPeriod, ix) -> let
                      np = fromIntegral startPeriod + sgMaxKESEvolutions + offsets !! ix
                      in Just $
                    ( fromIntegral np
                    , (fromIntegral np, ix + 1)
                    )
                  )
                  (firstKESPeriod, 0)
          pure
            [ (SlotNo slot, CoreNodeId coreNodeIdx)
              | slot <- takeWhile (< t) ((kesPeriodStartSlots !!) <$> periods)
            ]
        pure
          . Map.map (Map.fromList . fmap (,NodeRekey))
          . Map.fromListWith (<>)
          $ fmap (: []) <$> slotCNPairs
    where
      -- KES period i begins in slot kesPeriodStartSlots !! i
      kesPeriodStartSlots = [i * sgSlotsPerKESPeriod | i <- [0 ..]]
      ocertRegenCadence = sgSlotsPerKESPeriod * sgMaxKESEvolutions

ocertRekeying
  :: forall c m. (TPraosCrypto c)
  => Seed
  -> ShelleyGenesis c
  -> [CoreNode c]
  -> Rekeying m (ShelleyBlock c)
ocertRekeying
  initSeed
  ShelleyGenesis {sgSlotsPerKESPeriod}
  coreNodes
  = Rekeying
  { rekeyOracle = \_ sn -> Just sn
  , rekeyUpd = \(CoreNodeId nodeIdx) oldProtocolInfo (SlotNo slotNo) kesKey ->
      let
        currentKESPeriod = fromIntegral $ slotNo `div` sgSlotsPerKESPeriod
        CoreNode { cnDelegateKey } = coreNodes !! fromIntegral nodeIdx
      in pure $ TestNodeInitialization
        { tniCrucialTxs = []
        , tniProtocolInfo = oldProtocolInfo
          { pInfoLeaderCreds =
              fmap
                ( rekeyNode
                  currentKESPeriod
                  (deriveVerKeyKES kesKey)
                  cnDelegateKey
                  kesKey
                )
                $ pInfoLeaderCreds oldProtocolInfo
          }
        }
  , rekeyFreshSKs = genKeyKES <$>
      Stream.unfoldr (expandSeed (Proxy :: Proxy MD5)) (mkSeedFromOtherSeed initSeed)
  }
  where
    rekeyNode
      :: Period
      -> SL.VerKeyKES c
      -> SignKeyDSIGN (DSIGN c)
      -> SignKeyKES (KES c)
      -> (TPraosIsCoreNode c, MaintainForgeState (ChaChaT m) (ShelleyBlock c))
      -> (TPraosIsCoreNode c, MaintainForgeState (ChaChaT m) (ShelleyBlock c))
    rekeyNode currentKESPeriod kk coldKey kesKey (icn, mfs) =
      ( icn
        { tpraosIsCoreNodeOpCert = newOpCert }
      , mfs
        { initForgeState = TPraosForgeState $ HotKey currentKESPeriod kesKey }
      )
      where
        newCertIssue = 1 + (SL.ocertN $ tpraosIsCoreNodeOpCert icn)
        newOpCert = genOCert (SL.KESPeriod currentKESPeriod) newCertIssue coldKey kk

    -- | Construct a seed from a bunch of Word64s
    --
    --   We multiply these words by some extra stuff to make sure they contain
    --   enough bits for our seed.
    mkSeedFromOtherSeed ::
      Seed ->
      Seed.Seed
    mkSeedFromOtherSeed (Seed (w1, w2, w3, w4, w5)) =
      mkSeedFromBytes . BSL.toStrict $
        toByteString (w1 * 15485867)
          <> toByteString (w2 * 32452867)
          <> toByteString (w3 * 49979693)
          <> toByteString (w4 * 67867979)
          <> toByteString (w5 * 86028157)
          <> toByteString (2147483647 :: Word64)

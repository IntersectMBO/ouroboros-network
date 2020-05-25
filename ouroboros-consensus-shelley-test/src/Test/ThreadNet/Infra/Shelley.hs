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
  , genCoreNode
  , mkGenesisConfig
  , coreNodeKeys
  , mkProtocolRealTPraos
  , ocertNodeRestarts
  , tpraosSlotLength
  ) where

import           Control.Monad (forM, join)
import           Data.List (unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Cardano.Crypto (ProtocolMagicId (..))
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), SignKeyDSIGN,
                     signedDSIGN)
import           Cardano.Crypto.KES.Class (SignKeyKES, deriveVerKeyKES,
                     genKeyKES)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import           Cardano.Crypto.VRF.Class (SignKeyVRF, deriveVerKeyVRF,
                     genKeyVRF)
import           Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))

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
import           Test.Util.Time (dawnOfTime)

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
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (DSIGN,
                     HotKey (..))

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
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = SystemStart dawnOfTime
    , sgNetworkMagic          = NetworkMagic 0
    , sgNetworkId             = networkId
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

    coreNodesToGenesisMapping :: Map (SL.KeyHash 'SL.Genesis c)  (SL.KeyHash 'SL.GenesisDelegate c, SL.Hash c (SL.VerKeyVRF c))
    coreNodesToGenesisMapping  = Map.fromList
      [ ( SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnGenesisKey
        , ( SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
          , SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF
          )
        )
      | CoreNode { cnGenesisKey, cnDelegateKey, cnVRF } <- coreNodes
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

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.ThreadNet.Infra.Shelley (
    CoreNode(..)
  , CoreNodeKeyInfo(..)
  , genCoreNode
  , mkLeaderCredentials
  , mkGenesisConfig
  , coreNodeKeys
  , mkProtocolRealTPraos
  , tpraosSlotLength
  ) where

import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Test.QuickCheck

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), SignKeyDSIGN,
                     signedDSIGN)
import           Cardano.Crypto.KES.Class (SignKeyKES, deriveVerKeyKES,
                     genKeyKES)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.Seed as Cardano.Crypto
import           Cardano.Crypto.VRF.Class (SignKeyVRF, deriveVerKeyVRF,
                     genKeyVRF)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Time (dawnOfTime)

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
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (DSIGN)

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

data CoreNodeKeyInfo h = CoreNodeKeyInfo
  { cnkiKeyPair
      ::  ( SL.KeyPair 'SL.Payment (TPraosMockCrypto h)
          , SL.KeyPair 'SL.Staking (TPraosMockCrypto h)
          )
  , cnkiCoreNode ::
      ( SL.KeyPair 'SL.Genesis (TPraosMockCrypto h)
      , Gen.AllIssuerKeys h 'SL.GenesisDelegate
      )
  }

coreNodeKeys :: CoreNode (TPraosMockCrypto h) -> CoreNodeKeyInfo h
coreNodeKeys CoreNode{cnGenesisKey, cnDelegateKey, cnStakingKey} =
    CoreNodeKeyInfo {
        cnkiCoreNode =
          ( mkDSIGNKeyPair cnGenesisKey
          , Gen.AllIssuerKeys
            { Gen.cold = mkDSIGNKeyPair cnDelegateKey
              -- 'CoreNodeKeyInfo' is used for all sorts of generators, not
              -- only transaction generators. To generate transactions we
              -- don't need all these keys, hence the 'error's.
            , Gen.vrf  = error "vrf used while generating transactions"
            , Gen.hot  = error "hot used while generating transactions"
            , Gen.hk   = error "hk used while generating transactions"
            }
          )
      , cnkiKeyPair = (mkDSIGNKeyPair cnDelegateKey, mkDSIGNKeyPair cnStakingKey)
      }
  where
    mkDSIGNKeyPair k = SL.KeyPair (SL.VKey $ deriveVerKeyDSIGN k) k

genCoreNode
  :: TPraosCrypto c
  => SL.KESPeriod
  -> Gen (CoreNode c)
genCoreNode startKESPeriod = do
    genKey <- genKeyDSIGN <$> genSeed 8
    delKey <- genKeyDSIGN <$> genSeed 8
    stkKey <- genKeyDSIGN <$> genSeed 8
    vrfKey <- genKeyVRF   <$> genSeed 8
    kesKey <- genKeyKES   <$> genSeed 8
    let kesPub = deriveVerKeyKES kesKey
        sigma = signedDSIGN
          ()
          (SL.OCertSignable kesPub certificateIssueNumber startKESPeriod)
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

    genSeed :: Int -> Gen Cardano.Crypto.Seed
    genSeed nbBytes =
      mkSeedFromBytes . BS.pack <$> vectorOf nbBytes arbitrary

mkLeaderCredentials :: TPraosCrypto c => CoreNode c -> TPraosLeaderCredentials c
mkLeaderCredentials CoreNode { cnDelegateKey, cnVRF, cnKES, cnOCert } =
    TPraosLeaderCredentials {
        tpraosLeaderCredentialsSignKey    = cnKES
      , tpraosLeaderCredentialsIsCoreNode = TPraosIsCoreNode {
          tpraosIsCoreNodeOpCert     = cnOCert
        , tpraosIsCoreNodeColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , tpraosIsCoreNodeSignKeyVRF = cnVRF
        }
      }

-- | Note: a KES algorithm supports a particular max number of KES evolutions,
-- but we can configure a potentially lower maximum for the ledger, that's why
-- we take it as an argument.
mkGenesisConfig
  :: forall c. TPraosCrypto c
  => ProtVer  -- ^ Initial protocol version
  -> SecurityParam
  -> Double  -- ^ Decentralisation param
  -> SlotLength
  -> Word64  -- ^ Max KES evolutions
  -> [CoreNode c]
  -> ShelleyGenesis c
mkGenesisConfig pVer k d slotLength maxKESEvolutions coreNodes =
    ShelleyGenesis {
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = dawnOfTime
    , sgNetworkMagic          = 0
    , sgNetworkId             = networkId
    , sgActiveSlotsCoeff      = recip recipF   -- ie f
    , sgSecurityParam         = maxRollbacks k
    , sgEpochLength           = EpochSize (10 * maxRollbacks k * recipF)
      -- TODO maxKESEvolutions * sgSlotsPerKESPeriod = max number of slots the
      -- test can run without needing new ocerts. The maximum number of slots
      -- the tests run now is 200 and the mock KES supports 10 evolutions, so
      -- 10 * 20 == 200 is enough.
      -- We can relax this in:
      -- <https://github.com/input-output-hk/ouroboros-network/issues/2107>
    , sgSlotsPerKESPeriod     = 20
    , sgMaxKESEvolutions      = maxKESEvolutions
    , sgSlotLength            = getSlotLength slotLength
    , sgUpdateQuorum          = 1  -- TODO
    , sgMaxLovelaceSupply     = maxLovelaceSupply
    , sgProtocolParams        = pparams
    , sgGenDelegs             = coreNodesToGenesisMapping
    , sgInitialFunds          = initialFunds
    , sgStaking               = initialStake
    }
  where
    -- the reciprocal of the active slot coefficient
    recipF :: Num a => a
    recipF = 2   -- so f = 0.5

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
      { SL._d               =
            SL.truncateUnitInterval
          . realToFrac
          $ d
      , SL._maxBBSize       = 10000 -- TODO
      , SL._maxBHSize       = 1000 -- TODO
      , SL._protocolVersion = pVer
      }

    coreNodesToGenesisMapping :: Map (SL.KeyHash 'SL.Genesis c) (SL.GenDelegPair c)
    coreNodesToGenesisMapping  = Map.fromList
      [ let
          gkh :: SL.KeyHash 'SL.Genesis c
          gkh = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnGenesisKey

          gdpair :: SL.GenDelegPair c
          gdpair = SL.GenDelegPair
              (SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey)
              (SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF)

        in (gkh, gdpair)
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
        -- The staking key maps to the key hash of the pool, which is set to the
        -- "delegate key" in order that nodes may issue blocks both as delegates
        -- and as stake pools.
      , sgsStake = Map.fromList
          [ ( SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnStakingKey
            , SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            )
          | CoreNode {cnDelegateKey, cnStakingKey} <- coreNodes
          ]
      }
      where
        coreNodeToPoolMapping :: Map (SL.KeyHash 'SL.StakePool c) (SL.PoolParams c)
          = Map.fromList
            [ ( SL.KeyHash $ SL.hashWithSerialiser toCBOR
                  $ deriveVerKeyDSIGN cnStakingKey
              , SL.PoolParams
                { SL._poolPubKey = poolHash
                , SL._poolVrf = vrfHash
                  -- Each core node pledges its full stake to the pool.
                , SL._poolPledge = SL.Coin $ fromIntegral initialLovelacePerCoreNode
                , SL._poolCost = SL.Coin 1
                , SL._poolMargin = SL.truncateUnitInterval 0
                  -- Reward accounts live in a separate "namespace" to other
                  -- accounts, so it should be fine to use the same address.
                , SL._poolRAcnt = SL.RewardAcnt networkId $ mkCredential cnDelegateKey
                , SL._poolOwners = Set.singleton poolOwnerHash
                , SL._poolRelays = Seq.empty
                , SL._poolMD = SL.SNothing
                }
              )
            | CoreNode { cnDelegateKey, cnStakingKey, cnVRF } <- coreNodes
              -- The pool and owner hashes are derived from the same key, but
              -- use different hashing schemes
            , let poolHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let poolOwnerHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let vrfHash = SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF
            ]

    mkCredential :: SignKeyDSIGN (DSIGN c) -> SL.Credential r c
    mkCredential = SL.KeyHashObj . SL.hashKey . SL.VKey . deriveVerKeyDSIGN

mkProtocolRealTPraos
  :: forall m c. (IOLike m, TPraosCrypto c)
  => ShelleyGenesis c
  -> SL.Nonce
  -> CoreNode c
  -> ProtocolInfo m (ShelleyBlock c)
mkProtocolRealTPraos genesis initialNonce coreNode =
    protocolInfoShelley
      genesis
      initialNonce
      maxMajorPV
      protVer
      (Just (mkLeaderCredentials coreNode))
  where
    protVer = SL.ProtVer 0 0
    maxMajorPV = 1000 -- TODO

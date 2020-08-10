{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
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
  , DecentralizationParam(..)
  , KesConfig(..)
  , coreNodeKeys
  , genCoreNode
  , incrementMinorProtVer
  , mkEpochSize
  , mkGenesisConfig
  , mkKesConfig
  , mkLeaderCredentials
  , mkProtocolRealTPraos
  , mkSetDecentralizationParamTxs
  , tpraosSlotLength
  ) where

import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Test.QuickCheck

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), SignKeyDSIGN,
                     signedDSIGN)
import           Cardano.Crypto.Hash.Class (hashWithSerialiser)
import           Cardano.Crypto.KES.Class (KESAlgorithm, SignKeyKES,
                     deriveVerKeyKES, genKeyKES, seedSizeKES, totalPeriodsKES)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.Seed as Cardano.Crypto
import           Cardano.Crypto.VRF.Class (SignKeyVRF, deriveVerKeyVRF,
                     genKeyVRF, seedSizeVRF)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Time (dawnOfTime)

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL

import           Ouroboros.Consensus.Shelley.Ledger (GenTx (..), ShelleyBlock,
                     mkShelleyTx)
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (DSIGN)

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen

{-------------------------------------------------------------------------------
  The decentralization parameter
-------------------------------------------------------------------------------}

-- | A suitable value for the @d@ protocol parameter
--
-- In the range @0@ to @1@, inclusive. Beware the misnomer: @0@ means fully
-- decentralized, and @1@ means fully centralized.
newtype DecentralizationParam =
    DecentralizationParam {decentralizationParamToRational :: Rational }
  deriving (Eq, Generic, Ord)
  deriving (Show) via (Quiet DecentralizationParam)

-- | A fraction with denominator @10@ and numerator @0@ to @10@ inclusive
instance Arbitrary DecentralizationParam where
  arbitrary = do
      let d = 10
      n <- choose (0, d)
      pure $ DecentralizationParam $ fromInteger n / fromInteger d

{-------------------------------------------------------------------------------
  Important constants
-------------------------------------------------------------------------------}

tpraosSlotLength :: SlotLength
tpraosSlotLength = slotLengthFromSec 2

-- | The reciprocal of the active slots coefficient
--
-- To simplify related calculations throughout the test suite, we require this
-- to be a natural number.
recipActiveSlotsCoeff :: Num a => a
recipActiveSlotsCoeff = 2   -- so f = 0.5

{-------------------------------------------------------------------------------
  CoreNode secrets/etc
-------------------------------------------------------------------------------}

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
      , Gen.AllIssuerKeys (TPraosMockCrypto h) 'SL.GenesisDelegate
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
  :: forall c. TPraosCrypto c
  => SL.KESPeriod
  -> Gen (CoreNode c)
genCoreNode startKESPeriod = do
    genKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    delKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    stkKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    vrfKey <- genKeyVRF   <$> genSeed (seedSizeVRF (Proxy @(SL.VRF c)))
    kesKey <- genKeyKES   <$> genSeed (seedSizeKES (Proxy @(SL.KES c)))
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

    genSeed :: Integral a => a -> Gen Cardano.Crypto.Seed
    genSeed nbBytes =
      mkSeedFromBytes . BS.pack <$> vectorOf (fromIntegral nbBytes) arbitrary

mkLeaderCredentials :: TPraosCrypto c => CoreNode c -> TPraosLeaderCredentials c
mkLeaderCredentials CoreNode { cnDelegateKey, cnVRF, cnKES, cnOCert } =
    TPraosLeaderCredentials {
        tpraosLeaderCredentialsInitSignKey = cnKES
      , tpraosLeaderCredentialsCanBeLeader = TPraosCanBeLeader {
          tpraosCanBeLeaderOpCert     = cnOCert
        , tpraosCanBeLeaderColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , tpraosCanBeLeaderSignKeyVRF = cnVRF
        }
      }

{-------------------------------------------------------------------------------
  KES configuration
-------------------------------------------------------------------------------}

-- | Currently @'maxEvolutions' * 'slotsPerEvolution'@ is the max number of
-- slots the test can run without needing new ocerts.
--
-- TODO This limitation may be lifted by PR #2107, see
-- <https://github.com/input-output-hk/ouroboros-network/issues/2107>.
data KesConfig = KesConfig
  { maxEvolutions     :: Word64
  , slotsPerEvolution :: Word64
  }

-- | A 'KesConfig' that will not require more evolutions than this test's
-- crypto @c@ allows
mkKesConfig
  :: forall proxy c. KESAlgorithm c
  => proxy c -> NumSlots -> KesConfig
mkKesConfig prx (NumSlots t) = KesConfig
    { maxEvolutions
    , slotsPerEvolution = divCeiling t maxEvolutions
    }
  where
    maxEvolutions = fromIntegral $ totalPeriodsKES prx

    -- | Like 'div', but rounds-up.
    divCeiling :: Integral a => a -> a -> a
    divCeiling n d = q + min 1 r
      where
        (q, r) = quotRem n d

{-------------------------------------------------------------------------------
  TPraos node configuration
-------------------------------------------------------------------------------}

mkEpochSize :: SecurityParam -> EpochSize
mkEpochSize (SecurityParam k) = EpochSize $ 10 * k * recipActiveSlotsCoeff

-- | Note: a KES algorithm supports a particular max number of KES evolutions,
-- but we can configure a potentially lower maximum for the ledger, that's why
-- we take it as an argument.
mkGenesisConfig
  :: forall c. TPraosCrypto c
  => ProtVer  -- ^ Initial protocol version
  -> SecurityParam
  -> DecentralizationParam
  -> SlotLength
  -> KesConfig
  -> [CoreNode c]
  -> ShelleyGenesis c
mkGenesisConfig pVer k d slotLength kesCfg coreNodes =
    ShelleyGenesis {
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = dawnOfTime
    , sgNetworkMagic          = 0
    , sgNetworkId             = networkId
    , sgActiveSlotsCoeff      = recip recipActiveSlotsCoeff
    , sgSecurityParam         = maxRollbacks k
    , sgEpochLength           = mkEpochSize k
    , sgSlotsPerKESPeriod     = slotsPerEvolution kesCfg
    , sgMaxKESEvolutions      = maxEvolutions     kesCfg
    , sgSlotLength            = getSlotLength slotLength
    , sgUpdateQuorum          = quorum
    , sgMaxLovelaceSupply     = maxLovelaceSupply
    , sgProtocolParams        = pparams
    , sgGenDelegs             = coreNodesToGenesisMapping
    , sgInitialFunds          = initialFunds
    , sgStaking               = initialStake
    }
  where
     -- TODO
    maxLovelaceSupply :: Word64
    maxLovelaceSupply =
      fromIntegral (length coreNodes) * initialLovelacePerCoreNode

    quorum :: Word64
    quorum = nbCoreNodes `min` ((nbCoreNodes `div` 2) + 1)
      where
        nbCoreNodes = fromIntegral (length coreNodes)

    pparams :: SL.PParams
    pparams = SL.emptyPParams
      { SL._d               =
          SL.unitIntervalFromRational $ decentralizationParamToRational d
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

mkProtocolRealTPraos
  :: forall m c. (IOLike m, TPraosCrypto c)
  => ShelleyGenesis c
  -> SL.Nonce
  -> ProtVer
  -> CoreNode c
  -> ProtocolInfo m (ShelleyBlock c)
mkProtocolRealTPraos genesis initialNonce protVer coreNode =
    protocolInfoShelley
      genesis
      initialNonce
      maxMajorPV
      protVer
      (Just (mkLeaderCredentials coreNode))
  where
    maxMajorPV = 1000 -- TODO

{-------------------------------------------------------------------------------
  Necessary transactions for updating the 'DecentralizationParam'
-------------------------------------------------------------------------------}

incrementMinorProtVer :: SL.ProtVer -> SL.ProtVer
incrementMinorProtVer (SL.ProtVer major minor) = SL.ProtVer major (succ minor)

mkSetDecentralizationParamTxs
  :: forall c. (TPraosCrypto c)
  => [CoreNode c]
  -> ProtVer   -- ^ The proposed protocol version
  -> SlotNo   -- ^ The TTL
  -> DecentralizationParam   -- ^ The new value
  -> [GenTx (ShelleyBlock c)]
mkSetDecentralizationParamTxs coreNodes pVer ttl dNew =
    (:[]) $
    mkShelleyTx $
    SL.Tx
      { _body       = body
      , _witnessSet = witnessSet
      , _metadata   = SL.SNothing
      }
  where
    -- The funds touched by this transaction assume it's the first transaction
    -- executed.
    scheduledEpoch :: EpochNo
    scheduledEpoch = EpochNo 0

    witnessSet :: SL.WitnessSet c
    witnessSet = SL.WitnessSet
      { addrWits = Set.fromList signatures
      , bootWits = Set.empty
      , msigWits = Map.empty
      }

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    signatures :: [SL.WitVKey c 'SL.Witness]
    signatures =
        [ SL.WitVKey (SL.VKey vk) $
          signedDSIGN () (hashWithSerialiser toCBOR body) sk
        | cn <- coreNodes
        , let sk = cnDelegateKey cn
        , let vk = deriveVerKeyDSIGN sk
        ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: SL.TxBody c
    body = SL.TxBody
      { _certs    = Seq.empty
      , _inputs   = Set.singleton (fst touchCoins)
      , _mdHash   = SL.SNothing
      , _outputs  = Seq.singleton (snd touchCoins)
      , _ttl      = ttl
      , _txfee    = SL.Coin 0
      , _txUpdate = SL.SJust update
      , _wdrls    = SL.Wdrl Map.empty
      }

    -- Every Shelley transaction requires one input.
    --
    -- We use the input of the first node, but we just put it all right back.
    --
    -- ASSUMPTION: This transaction runs in the first slot.
    touchCoins :: (SL.TxIn c, SL.TxOut c)
    touchCoins = case coreNodes of
        []   -> error "no nodes!"
        cn:_ ->
            ( SL.initialFundsPseudoTxIn addr
            , SL.TxOut addr coin
            )
          where
            addr = SL.Addr networkId
                (mkCredential (cnDelegateKey cn))
                (SL.StakeRefBase (mkCredential (cnStakingKey cn)))
            coin = SL.Coin $ fromIntegral initialLovelacePerCoreNode

    -- One replicant of the parameter update per each node.
    update :: SL.Update c
    update =
        flip SL.Update scheduledEpoch $ SL.ProposedPPUpdates $
        Map.fromList $
        [ ( SL.hashKey $ SL.VKey $ deriveVerKeyDSIGN $ cnGenesisKey cn
          , SL.emptyPParamsUpdate
              { SL._d =
                  SL.SJust $
                  SL.unitIntervalFromRational $
                  decentralizationParamToRational dNew
              , SL._protocolVersion =
                  SL.SJust pVer
              }
          )
        | cn <- coreNodes
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

initialLovelacePerCoreNode :: Word64
initialLovelacePerCoreNode = 1000

mkCredential :: TPraosCrypto c => SignKeyDSIGN (DSIGN c) -> SL.Credential r c
mkCredential = SL.KeyHashObj . SL.hashKey . SL.VKey . deriveVerKeyDSIGN

networkId :: SL.Network
networkId = SL.Testnet

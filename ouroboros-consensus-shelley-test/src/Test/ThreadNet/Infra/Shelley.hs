{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
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
  , mkCredential
  , mkEpochSize
  , mkGenesisConfig
  , mkKesConfig
  , mkKeyHash
  , mkKeyHashVrf
  , mkLeaderCredentials
  , mkProtocolRealTPraos
  , mkSetDecentralizationParamTxs
  , mkVerKey
  , networkId
  , tpraosSlotLength
  ) where

import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ratio (denominator, numerator)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Test.QuickCheck

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashWithSerialiser)
import           Cardano.Crypto.KES (KESAlgorithm (..))
import           Cardano.Crypto.Libsodium.MLockedBytes (mlsbFromByteString)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.Seed as Cardano.Crypto
import           Cardano.Crypto.VRF (SignKeyVRF, VRFAlgorithm, VerKeyVRF,
                     deriveVerKeyVRF, genKeyVRF, seedSizeVRF)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Time (dawnOfTime)

import           Cardano.Ledger.Crypto (DSIGN, KES, VRF)
import           Cardano.Ledger.Era (Era (Crypto))
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL (truncateUnitInterval,
                     unitIntervalFromRational)
import qualified Shelley.Spec.Ledger.Keys as SL (signedDSIGN)
import qualified Shelley.Spec.Ledger.OCert as SL (OCertSignable (..))
import qualified Shelley.Spec.Ledger.PParams as SL (emptyPParams,
                     emptyPParamsUpdate)
import qualified Shelley.Spec.Ledger.Tx as SL (WitnessSetHKD (..))

import           Ouroboros.Consensus.Shelley.Eras (ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger (GenTx (..), ShelleyBlock,
                     mkShelleyTx)
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol

import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as CSL
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

{-------------------------------------------------------------------------------
  CoreNode secrets/etc
-------------------------------------------------------------------------------}

data CoreNode era = CoreNode {
      cnGenesisKey  :: !(SL.SignKeyDSIGN era)
    , cnDelegateKey :: !(SL.SignKeyDSIGN era)
      -- ^ Cold delegate key. The hash of the corresponding verification
      -- (public) key will be used as the payment credential.
    , cnStakingKey  :: !(SL.SignKeyDSIGN era)
      -- ^ The hash of the corresponding verification (public) key will be
      -- used as the staking credential.
    , cnVRF         :: !(SL.SignKeyVRF   era)
    , cnKES         :: !(SL.SignKeyKES   era)
    , cnOCert       :: !(SL.OCert        era)
    }

data CoreNodeKeyInfo era = CoreNodeKeyInfo
  { cnkiKeyPair
      ::  ( SL.KeyPair 'SL.Payment era
          , SL.KeyPair 'SL.Staking era
          )
  , cnkiCoreNode ::
      ( SL.KeyPair 'SL.Genesis era
      , Gen.AllIssuerKeys era 'SL.GenesisDelegate
      )
  }

coreNodeKeys :: CSL.Mock (Crypto era) => CoreNode era -> CoreNodeKeyInfo era
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

genCoreNode ::
     forall era. TPraosCrypto era
  => SL.KESPeriod
  -> Gen (CoreNode era)
genCoreNode startKESPeriod = do
    genKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN (Crypto era))))
    delKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN (Crypto era))))
    stkKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN (Crypto era))))
    vrfKey <- genKeyVRF   <$> genSeed (seedSizeVRF   (Proxy @(VRF   (Crypto era))))
    kesKey <- genKeyKES . mlsbFromByteString
                          <$> genBytes (seedSizeKES  (Proxy @(KES   (Crypto era))))
    let kesPub = deriveVerKeyKES kesKey
        sigma = SL.signedDSIGN
          @era
          delKey
          (SL.OCertSignable kesPub certificateIssueNumber startKESPeriod)
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

    genBytes :: Integral a => a -> Gen BS.ByteString
    genBytes nbBytes = BS.pack <$> vectorOf (fromIntegral nbBytes) arbitrary

    genSeed :: Integral a => a -> Gen Cardano.Crypto.Seed
    genSeed = fmap mkSeedFromBytes . genBytes

mkLeaderCredentials :: TPraosCrypto era => CoreNode era -> TPraosLeaderCredentials era
mkLeaderCredentials CoreNode { cnDelegateKey, cnVRF, cnKES, cnOCert } =
    TPraosLeaderCredentials {
        tpraosLeaderCredentialsInitSignKey = cnKES
      , tpraosLeaderCredentialsCanBeLeader = TPraosCanBeLeader {
          tpraosCanBeLeaderOpCert     = cnOCert
        , tpraosCanBeLeaderColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , tpraosCanBeLeaderSignKeyVRF = cnVRF
        }
      , tpraosLeaderCredentialsLabel       = "ThreadNet"
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

-- | A 'KesConfig' that will not require more evolutions than this test's crypto
-- allows.
mkKesConfig
  :: forall proxy era. Era era
  => proxy era -> NumSlots -> KesConfig
mkKesConfig _ (NumSlots t) = KesConfig
    { maxEvolutions
    , slotsPerEvolution = divCeiling t maxEvolutions
    }
  where
    maxEvolutions = fromIntegral $ totalPeriodsKES (Proxy @(KES (Crypto era)))

    -- | Like 'div', but rounds-up.
    divCeiling :: Integral a => a -> a -> a
    divCeiling n d = q + min 1 r
      where
        (q, r) = quotRem n d

{-------------------------------------------------------------------------------
  TPraos node configuration
-------------------------------------------------------------------------------}

-- | The epoch size, given @k@ and @f@.
--
-- INVARIANT: @10 * k / f@ must be a whole number.
mkEpochSize :: SecurityParam -> Rational -> EpochSize
mkEpochSize (SecurityParam k) f =
    if r /= 0 then error "10 * k / f must be a whole number" else
    EpochSize q
  where
    n = numerator   f
    d = denominator f

    (q, r) = quotRem (10 * k * fromInteger d) (fromInteger n)

-- | Note: a KES algorithm supports a particular max number of KES evolutions,
-- but we can configure a potentially lower maximum for the ledger, that's why
-- we take it as an argument.
mkGenesisConfig
  :: forall era. TPraosCrypto era
  => ProtVer  -- ^ Initial protocol version
  -> SecurityParam
  -> Rational   -- ^ Initial active slot coefficient
  -> DecentralizationParam
  -> SlotLength
  -> KesConfig
  -> [CoreNode era]
  -> ShelleyGenesis era
mkGenesisConfig pVer k f d slotLength kesCfg coreNodes =
    ShelleyGenesis {
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = dawnOfTime
    , sgNetworkMagic          = 0
    , sgNetworkId             = networkId
    , sgActiveSlotsCoeff      = f
    , sgSecurityParam         = maxRollbacks k
    , sgEpochLength           = mkEpochSize k f
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

    pparams :: SL.PParams era
    pparams = SL.emptyPParams
      { SL._d               =
          SL.unitIntervalFromRational $ decentralizationParamToRational d
      , SL._maxBBSize       = 10000 -- TODO
      , SL._maxBHSize       = 1000 -- TODO
      , SL._protocolVersion = pVer
      }

    coreNodesToGenesisMapping :: Map (SL.KeyHash 'SL.Genesis era) (SL.GenDelegPair era)
    coreNodesToGenesisMapping  = Map.fromList
      [ let
          gkh :: SL.KeyHash 'SL.Genesis era
          gkh = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnGenesisKey

          gdpair :: SL.GenDelegPair era
          gdpair = SL.GenDelegPair
              (SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey)
              (SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF)

        in (gkh, gdpair)
      | CoreNode { cnGenesisKey, cnDelegateKey, cnVRF } <- coreNodes
      ]

    initialFunds :: Map (SL.Addr era) SL.Coin
    initialFunds = Map.fromList
      [ (addr, coin)
      | CoreNode { cnDelegateKey, cnStakingKey } <- coreNodes
      , let addr = SL.Addr networkId
                           (mkCredential cnDelegateKey)
                           (SL.StakeRefBase (mkCredential cnStakingKey))
            coin = SL.Coin $ fromIntegral initialLovelacePerCoreNode
      ]

    -- In this initial stake, each core node delegates its stake to itself.
    initialStake :: ShelleyGenesisStaking era
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
        coreNodeToPoolMapping :: Map (SL.KeyHash 'SL.StakePool era) (SL.PoolParams era)
          = Map.fromList
            [ ( SL.hashKey . SL.VKey . deriveVerKeyDSIGN $ cnStakingKey
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
  :: forall m c. (IOLike m, TPraosCrypto (ShelleyEra c))
  => ShelleyGenesis (ShelleyEra c)
  -> SL.Nonce
  -> ProtVer
  -> CoreNode (ShelleyEra c)
  -> ProtocolInfo m (ShelleyBlock (ShelleyEra c))
mkProtocolRealTPraos genesis initialNonce protVer coreNode =
    protocolInfoShelley $ ProtocolParamsShelley {
        shelleyGenesis           = genesis
      , shelleyInitialNonce      = initialNonce
      , shelleyProtVer           = protVer
      , shelleyLeaderCredentials = Just $ mkLeaderCredentials coreNode
      }
{-------------------------------------------------------------------------------
  Necessary transactions for updating the 'DecentralizationParam'
-------------------------------------------------------------------------------}

incrementMinorProtVer :: SL.ProtVer -> SL.ProtVer
incrementMinorProtVer (SL.ProtVer major minor) = SL.ProtVer major (succ minor)

mkSetDecentralizationParamTxs
  :: forall era. (TPraosCrypto era)
  => [CoreNode era]
  -> ProtVer   -- ^ The proposed protocol version
  -> SlotNo   -- ^ The TTL
  -> DecentralizationParam   -- ^ The new value
  -> [GenTx (ShelleyBlock era)]
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

    witnessSet :: SL.WitnessSet era
    witnessSet = SL.WitnessSet
      { addrWits = Set.fromList signatures
      , bootWits = Set.empty
      , msigWits = Map.empty
      }

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    signatures :: [SL.WitVKey era 'SL.Witness]
    signatures =
        [ SL.WitVKey (SL.VKey vk) $
          SL.signedDSIGN @era sk (hashWithSerialiser toCBOR body)
        | cn <- coreNodes
        , let sk = cnDelegateKey cn
        , let vk = deriveVerKeyDSIGN sk
        ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: SL.TxBody era
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
    touchCoins :: (SL.TxIn era, SL.TxOut era)
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
    update :: SL.Update era
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

mkCredential :: TPraosCrypto era => SL.SignKeyDSIGN era -> SL.Credential r era
mkCredential = SL.KeyHashObj . mkKeyHash

mkKeyHash :: TPraosCrypto era => SL.SignKeyDSIGN era -> SL.KeyHash r era
mkKeyHash = SL.hashKey . mkVerKey

mkVerKey :: TPraosCrypto era => SL.SignKeyDSIGN era -> SL.VKey r era
mkVerKey = SL.VKey . deriveVerKeyDSIGN

mkKeyHashVrf :: (HashAlgorithm h, VRFAlgorithm vrf)
             => SignKeyVRF vrf
             -> Hash h (VerKeyVRF vrf)
mkKeyHashVrf = SL.hashVerKeyVRF . deriveVerKeyVRF

networkId :: SL.Network
networkId = SL.Testnet

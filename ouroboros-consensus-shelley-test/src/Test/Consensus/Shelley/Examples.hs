{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.Consensus.Shelley.Examples (
    -- * Setup
    codecConfig
  , keyToCredential
  , mkDummyHash
  , testEpochInfo
  , testShelleyGenesis
    -- * Examples
  , examplesAllegra
  , examplesMary
  , examplesShelley
    -- * Era-specific examples
  , exampleCoin
    -- * Individual examples
  , exampleApplyTxErr
  , exampleBlock
  , exampleChainDepState
  , exampleExtLedgerState
  , exampleHeaderHash
  , exampleHeaderState
  , exampleLedgerState
  , exampleTx
    -- * Keys
  , exampleKeys
  , examplePayKey
  , examplePoolParams
  , exampleStakeKey
  ) where

import qualified Data.ByteString as Strict
import           Data.Coerce (coerce)
import           Data.Default.Class (def)
import           Data.Functor.Identity (Identity (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Word (Word64, Word8)

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm)
import qualified Cardano.Crypto.DSIGN as DSIGN
import           Cardano.Crypto.Hash (Blake2b_256, HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Seed as Seed
import           Cardano.Crypto.VRF (VRFAlgorithm)
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Time

import qualified Cardano.Ledger.AuxiliaryData as SL (AuxiliaryDataHash (..))
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH, VRF)
import qualified Cardano.Ledger.SafeHash as SL
import qualified Control.State.Transition.Extended as SL (PredicateFailure)
import           Shelley.Spec.Ledger.API (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL (Seed (..),
                     mkNonceFromNumber, textToUrl)
import qualified Shelley.Spec.Ledger.BlockChain as SL (TxSeq (..))
import qualified Shelley.Spec.Ledger.Coin as SL (DeltaCoin (..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
                     (IndividualPoolStake (..))
import qualified Shelley.Spec.Ledger.EpochBoundary as SL (BlocksMade (..),
                     emptySnapShots)
import qualified Shelley.Spec.Ledger.Keys as SL (asWitness, hashWithSerialiser,
                     signedKES)
import qualified Shelley.Spec.Ledger.LedgerState as SL (PulsingRewUpdate,
                     startStep)
import qualified Shelley.Spec.Ledger.PParams as SL (emptyPParams,
                     emptyPParamsUpdate)
import qualified Shelley.Spec.Ledger.STS.Delegs as SL
                     (DelegsPredicateFailure (..))
import qualified Shelley.Spec.Ledger.STS.Ledger as SL
                     (LedgerPredicateFailure (..))
import qualified Shelley.Spec.Ledger.STS.Ledgers as SL
                     (LedgersPredicateFailure (..))
import qualified Shelley.Spec.Ledger.Tx as SL (addrWits)
import qualified Shelley.Spec.Ledger.UTxO as SL (makeWitnessesVKey)
import qualified Test.Shelley.Spec.Ledger.Generator.Core as SL
                     (AllIssuerKeys (..), mkOCert)
import           Test.Shelley.Spec.Ledger.Orphans ()
import qualified Test.Shelley.Spec.Ledger.Utils as SL hiding (mkKeyPair,
                     mkKeyPair', mkVRFKeyPair)

import qualified Cardano.Ledger.Mary.Value as MA
import qualified Cardano.Ledger.Shelley.Constraints as SL (PParamsDelta,
                     makeTxOut)
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (PraosCrypto, TPraos,
                     TPraosState (..))

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Golden (labelled, unlabelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

testEpochInfo :: EpochInfo Identity
testEpochInfo = SL.epochInfo SL.testGlobals

-- | These are dummy values.
testShelleyGenesis :: SL.ShelleyGenesis era
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

codecConfig :: CodecConfig (ShelleyBlock StandardShelley)
codecConfig = ShelleyCodecConfig

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . SL.hashWithSerialiser @h toCBOR

mkDummySafeHash :: forall c a. Crypto c => Proxy c -> Int -> SL.SafeHash c a
mkDummySafeHash _ =
      SL.unsafeMakeSafeHash
    . mkDummyHash (Proxy @(HASH c))

mkKeyHash :: forall c discriminator. Crypto c => Int -> SL.KeyHash discriminator c
mkKeyHash = SL.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

mkScriptHash :: forall c. Crypto c => Int -> SL.ScriptHash c
mkScriptHash = SL.ScriptHash . mkDummyHash (Proxy @(ADDRHASH c))

-- | @mkKeyPair'@ from @Test.Shelley.Spec.Ledger.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/input-output-hk/cardano-ledger-specs/issues/1770>
mkDSIGNKeyPair ::
     forall c kd. DSIGNAlgorithm (DSIGN c)
  => Word8
  -> SL.KeyPair kd c
mkDSIGNKeyPair byte = SL.KeyPair (SL.VKey $ DSIGN.deriveVerKeyDSIGN sk) sk
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @(DSIGN c))))
          byte

    sk = DSIGN.genKeyDSIGN seed

mkVRFKeyPair ::
     forall c. VRFAlgorithm (VRF c)
  => Proxy c
  -> Word8
  -> (SL.SignKeyVRF c, SL.VerKeyVRF c)
mkVRFKeyPair _ byte = (sk, VRF.deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (VRF.seedSizeVRF (Proxy @(VRF c))))
          byte

    sk = VRF.genKeyVRF seed

keyToCredential :: Crypto c => SL.KeyPair r c -> SL.Credential r c
keyToCredential = SL.KeyHashObj . SL.hashKey . SL.vKey

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples ::
     forall era.
     ( ShelleyBasedEra era
     ,   SL.PredicateFailure (Core.EraRule "LEDGER" era)
       ~ SL.LedgerPredicateFailure era
     ,   SL.PredicateFailure (Core.EraRule "DELEGS" era)
       ~ SL.DelegsPredicateFailure era
     , Core.PParams era ~ SL.PParams era
     , SL.PParamsDelta era ~ SL.PParams' StrictMaybe era
     )
  => Core.Value era
  -> Core.TxBody era
  -> Core.AuxiliaryData era
  -> Golden.Examples (ShelleyBlock era)
examples value txBody auxiliaryData = Golden.Examples {
      exampleBlock            = unlabelled blk
    , exampleSerialisedBlock  = unlabelled exampleSerialisedBlock
    , exampleHeader           = unlabelled (getHeader blk)
    , exampleSerialisedHeader = unlabelled exampleSerialisedHeader
    , exampleHeaderHash       = unlabelled (exampleHeaderHash (Proxy @era))
    , exampleGenTx            = unlabelled exampleGenTx
    , exampleGenTxId          = unlabelled (txId exampleGenTx)
    , exampleApplyTxErr       = unlabelled exampleApplyTxErr
    , exampleQuery            = queries
    , exampleResult           = results
    , exampleAnnTip           = unlabelled exampleAnnTip
    , exampleLedgerState      = unlabelled (exampleLedgerState value)
    , exampleChainDepState    = unlabelled exampleChainDepState
    , exampleExtLedgerState   = unlabelled (exampleExtLedgerState value)
    }
  where
    tx = exampleTx txBody auxiliaryData
    exampleGenTx = mkShelleyTx tx
    blk = exampleBlock tx

    queries = labelled [
          ("GetLedgerTip",              SomeSecond GetLedgerTip)
        , ("GetEpochNo",                SomeSecond GetEpochNo)
        , ("GetCurrentPParams",         SomeSecond GetCurrentPParams)
        , ("GetProposedPParamsUpdates", SomeSecond GetProposedPParamsUpdates)
        , ("GetStakeDistribution",      SomeSecond GetStakeDistribution)
        , ("GetNonMyopicMemberRewards",
            SomeSecond $ GetNonMyopicMemberRewards $ Set.fromList [
                Left  (SL.Coin 100)
              , Right (SL.ScriptHashObj (mkScriptHash 1))
              , Right (SL.KeyHashObj (mkKeyHash 2))
              ])
        , ("GetGenesisConfig",          SomeSecond GetGenesisConfig)
      ]

    results = labelled [
          ("LedgerTip",              SomeResult GetLedgerTip (blockPoint blk))
        , ("EpochNo",                SomeResult GetEpochNo 10)
        , ("EmptyPParams",           SomeResult GetCurrentPParams def)
        , ("ProposedPParamsUpdates", SomeResult GetProposedPParamsUpdates proposedPParamsUpdates)
        , ("StakeDistribution",      SomeResult GetStakeDistribution examplePoolDistr)
        , ("NonMyopicMemberRewards", SomeResult (GetNonMyopicMemberRewards Set.empty)
            (NonMyopicMemberRewards $ Map.fromList [
                (Left (SL.Coin 100), Map.singleton (mkKeyHash 2) (SL.Coin 3))
              , (Right (SL.ScriptHashObj (mkScriptHash 1)), Map.empty)
              , (Right (SL.KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (SL.Coin 9))
              ]))
        , ("GenesisConfig",          SomeResult GetGenesisConfig (compactGenesis testShelleyGenesis))
        ]

    proposedPParamsUpdates :: SL.ProposedPPUpdates era
    proposedPParamsUpdates = SL.ProposedPPUpdates $ Map.singleton
        (mkKeyHash 0)
        (SL.emptyPParamsUpdate {SL._keyDeposit = SJust (SL.Coin 100)})

examplesShelley :: Golden.Examples (ShelleyBlock StandardShelley)
examplesShelley =
    examples
      exampleCoin
      exampleTxBodyShelley
      exampleAuxiliaryDataShelley

examplesAllegra :: Golden.Examples (ShelleyBlock StandardAllegra)
examplesAllegra =
    examples
      exampleCoin
      exampleTxBodyAllegra
      exampleAuxiliaryDataMA

examplesMary :: Golden.Examples (ShelleyBlock StandardMary)
examplesMary =
    examples
      exampleMultiAssetValue
      exampleTxBodyMary
      exampleAuxiliaryDataMA

{-------------------------------------------------------------------------------
  Era-specific individual examples
-------------------------------------------------------------------------------}

exampleCoin :: SL.Coin
exampleCoin = SL.Coin 10

exampleMultiAssetValue :: forall c. Crypto c => MA.Value c
exampleMultiAssetValue =
    MA.Value 100 $ Map.singleton policyId $ Map.singleton couttsCoin 1000
  where
    policyId :: MA.PolicyID c
    policyId = MA.PolicyID $ mkScriptHash 1

    couttsCoin :: MA.AssetName
    couttsCoin = MA.AssetName "couttsCoin"

exampleTxBodyShelley :: SL.TxBody StandardShelley
exampleTxBodyShelley = SL.TxBody
    exampleTxIns
    (StrictSeq.fromList [
        SL.TxOut (SL.mkAddr (examplePayKey, exampleStakeKey)) (SL.Coin 100000)
      ])
    exampleCerts
    exampleWithdrawals
    (SL.Coin 3)
    (SlotNo 10)
    (SJust (SL.Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
  where
    -- Dummy hash to decouple from the auxiliaryData in 'exampleTx'.
    auxiliaryDataHash :: SL.AuxiliaryDataHash StandardCrypto
    auxiliaryDataHash =
        SL.AuxiliaryDataHash $ mkDummySafeHash (Proxy @StandardCrypto) 30

exampleTxBodyMA ::
     forall era.
     ( ShelleyBasedEra era
     , SL.PParamsDelta era ~ SL.PParams' StrictMaybe era
     )
  => Core.Value era -> MA.TxBody era
exampleTxBodyMA value = MA.TxBody
    exampleTxIns
    (StrictSeq.fromList [
        SL.TxOut (SL.mkAddr (examplePayKey, exampleStakeKey)) value
      ])
    exampleCerts
    exampleWithdrawals
    (SL.Coin 3)
    (MA.ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4)))
    (SJust (SL.Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
    value
  where
    -- Dummy hash to decouple from the auxiliary data in 'exampleTx'.
    auxiliaryDataHash :: SL.AuxiliaryDataHash (EraCrypto era)
    auxiliaryDataHash =
        SL.AuxiliaryDataHash $ mkDummySafeHash (Proxy @(EraCrypto era)) 30

exampleTxBodyAllegra :: Core.TxBody StandardAllegra
exampleTxBodyAllegra = exampleTxBodyMA exampleCoin

exampleTxBodyMary :: Core.TxBody StandardMary
exampleTxBodyMary = exampleTxBodyMA exampleMultiAssetValue

exampleScriptMA :: Crypto c => Core.Script (MA.ShelleyMAEra ma c)
exampleScriptMA =
    MA.RequireMOf 2 $ StrictSeq.fromList [
        MA.RequireAllOf $ StrictSeq.fromList [
            MA.RequireTimeStart  (SlotNo 0)
          , MA.RequireTimeExpire (SlotNo 9)
          ]
      , MA.RequireAnyOf $ StrictSeq.fromList [
            MA.RequireSignature (mkKeyHash 0)
          , MA.RequireSignature (mkKeyHash 1)
          ]
      , MA.RequireSignature (mkKeyHash 100)
      ]

exampleMetadataMap :: Map Word64 SL.Metadatum
exampleMetadataMap = Map.fromList [
      (1, SL.S "string")
    , (2, SL.B "bytes")
    , (3, SL.List [SL.I 1, SL.I 2])
    , (4, SL.Map [(SL.I 3, SL.B "b")])
    ]

exampleAuxiliaryDataShelley :: Core.AuxiliaryData StandardShelley
exampleAuxiliaryDataShelley = SL.Metadata exampleMetadataMap

exampleAuxiliaryDataMA :: Crypto c => Core.AuxiliaryData (MA.ShelleyMAEra ma c)
exampleAuxiliaryDataMA =
    MA.AuxiliaryData
      exampleMetadataMap
      (StrictSeq.fromList [exampleScriptMA])

{-------------------------------------------------------------------------------
  Individual examples
-------------------------------------------------------------------------------}

exampleTxIns :: Crypto c => Set (SL.TxIn c)
exampleTxIns = Set.fromList [
      SL.TxIn (SL.TxId (mkDummySafeHash Proxy 1)) 0
    ]

exampleCerts :: Crypto c => StrictSeq (SL.DCert c)
exampleCerts = StrictSeq.fromList [
      SL.DCertDeleg (SL.RegKey (keyToCredential exampleStakeKey))
    , SL.DCertPool (SL.RegPool examplePoolParams)
    , SL.DCertMir $ SL.MIRCert SL.ReservesMIR $ SL.StakeAddressesMIR $ Map.fromList [
        (keyToCredential (mkDSIGNKeyPair 2), SL.DeltaCoin 110)
      ]
    ]

-- | Shortening @Withdrawals@ to @Wdrl@, seriously?
exampleWithdrawals :: Crypto c => SL.Wdrl c
exampleWithdrawals = SL.Wdrl $ Map.fromList [
      (SL._poolRAcnt examplePoolParams, SL.Coin 100)
    ]

examplePoolDistr :: forall c. PraosCrypto c => SL.PoolDistr c
examplePoolDistr = SL.PoolDistr $ Map.fromList [
      (mkKeyHash 1, SL.IndividualPoolStake
                      1
                      (SL.hashVerKeyVRF (snd (SL.vrf (exampleKeys @c)))))
    ]

exampleProposedPPUpdates ::
  ( SL.PParamsDelta era ~ SL.PParams' StrictMaybe era
  , ShelleyBasedEra era
  ) => SL.ProposedPPUpdates era
exampleProposedPPUpdates = SL.ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (SL.emptyPParamsUpdate { SL._maxBHSize = SJust 4000 })

-- | This is not a valid block. We don't care, we are only interested in
-- serialisation, not validation.
exampleBlock ::
     forall era. ShelleyBasedEra era
  => SL.Tx era
  -> ShelleyBlock era
exampleBlock tx = mkShelleyBlock $ SL.Block blockHeader blockBody
  where
    keys :: SL.AllIssuerKeys (EraCrypto era) 'SL.StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ SL.hot keys
    SL.KeyPair vKeyCold _ = SL.cold keys

    blockHeader :: SL.BHeader (EraCrypto era)
    blockHeader = SL.BHeader blockHeaderBody (SL.signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: SL.BHBody (EraCrypto era)
    blockHeaderBody = SL.BHBody {
          bheaderBlockNo = BlockNo 3
        , bheaderSlotNo  = SlotNo 9
        , bheaderPrev    = SL.BlockHash (SL.HashHeader (mkDummyHash Proxy 2))
        , bheaderVk      = SL.coerceKeyRole vKeyCold
        , bheaderVrfVk   = snd $ SL.vrf keys
        , bheaderEta     = SL.mkCertifiedVRF (mkBytes 0) (fst $ SL.vrf keys)
        , bheaderL       = SL.mkCertifiedVRF (mkBytes 1) (fst $ SL.vrf keys)
        , bsize          = 2345
        , bhash          = SL.bbHash blockBody
        , bheaderOCert   = SL.mkOCert keys 0 (SL.KESPeriod 0)
        , bprotver       = SL.ProtVer 2 0
        }

    blockBody :: SL.TxSeq era
    blockBody = SL.TxSeq (StrictSeq.fromList [tx])

    mkBytes :: Int -> SL.Seed
    mkBytes = SL.Seed . mkDummyHash (Proxy @Blake2b_256)

exampleSerialisedBlock :: Serialised (ShelleyBlock era)
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleSerialisedHeader :: SerialisedHeader (ShelleyBlock era)
exampleSerialisedHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")

exampleHeaderHash ::
     forall era. ShelleyBasedEra era
  => Proxy era
  -> HeaderHash (ShelleyBlock era)
exampleHeaderHash _ = coerce $ mkDummyHash (Proxy @(HASH (EraCrypto era))) 0

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
     forall era. ShelleyBasedEra era
  => Core.TxBody era
  -> Core.AuxiliaryData era
  -> SL.Tx era
exampleTx txBody auxiliaryData = SL.Tx txBody witnessSet (SJust auxiliaryData)
  where
    witnessSet :: SL.WitnessSet era
    witnessSet = mempty {
          SL.addrWits =
            SL.makeWitnessesVKey (coerce (SL.hashAnnotated txBody)) witnesses
        }
      where
        witnesses :: [SL.KeyPair 'SL.Witness (EraCrypto era)]
        witnesses = [
              SL.asWitness examplePayKey
            , SL.asWitness exampleStakeKey
            , SL.asWitness $ SL.cold (exampleKeys @(EraCrypto era) @'SL.StakePool)
            ]

-- TODO incomplete, this type has tons of constructors that can all change.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1896.
exampleApplyTxErr ::
     forall era.
     ( ShelleyBasedEra era
     ,   SL.PredicateFailure (Core.EraRule "LEDGER" era)
       ~ SL.LedgerPredicateFailure era
     ,   SL.PredicateFailure (Core.EraRule "DELEGS" era)
       ~ SL.DelegsPredicateFailure era
     )
  => ApplyTxErr (ShelleyBlock era)
exampleApplyTxErr =
      ApplyTxError
    $ pure
    $ SL.LedgerFailure
    $ SL.DelegsFailure
    $ SL.DelegateeNotRegisteredDELEG @era (mkKeyHash 1)

exampleAnnTip :: forall era. ShelleyBasedEra era => AnnTip (ShelleyBlock era)
exampleAnnTip = AnnTip {
      annTipSlotNo  = SlotNo 14
    , annTipBlockNo = BlockNo 6
    , annTipInfo    = exampleHeaderHash (Proxy @era)
    }

exampleChainDepState ::
     forall c. PraosCrypto c => ChainDepState (TPraos c)
exampleChainDepState = TPraosState (NotOrigin 1) (mkPrtclState 1)
  where
    mkPrtclState :: Word64 -> SL.ChainDepState c
    mkPrtclState seed = SL.ChainDepState
      { SL.csProtocol = SL.PrtclState
          (Map.fromList [
              (mkKeyHash 1, 1)
            , (mkKeyHash 2, 2)
            ])
          (SL.mkNonceFromNumber seed)
          (SL.mkNonceFromNumber seed)
      , SL.csTickn = SL.TicknState
          SL.NeutralNonce
          (SL.mkNonceFromNumber seed)
      , SL.csLabNonce =
          SL.mkNonceFromNumber seed
      }

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
     forall era.
     ( ShelleyBasedEra era
     , Core.PParams era ~ SL.PParams era
     )
  => Core.Value era
  -> SL.NewEpochState era
exampleNewEpochState value = SL.NewEpochState {
      nesEL     = EpochNo 0
    , nesBprev  = SL.BlocksMade (Map.singleton (mkKeyHash 1) 10)
    , nesBcur   = SL.BlocksMade (Map.singleton (mkKeyHash 2) 3)
    , nesEs     = epochState
    , nesRu     = SJust rewardUpdate
    , nesPd     = examplePoolDistr
    }
  where
    epochState :: SL.EpochState era
    epochState = SL.EpochState {
          esAccountState = SL.AccountState {
              _treasury = SL.Coin 10000
            , _reserves = SL.Coin 1000
            }
        , esSnapshots    = SL.emptySnapShots
        , esLState       = SL.LedgerState {
              _utxoState = SL.UTxOState {
                  _utxo      = SL.UTxO $ Map.fromList [
                      (SL.TxIn (SL.TxId (mkDummySafeHash Proxy 1)) 0,
                       SL.makeTxOut (Proxy @era) addr value)
                    ]
                , _deposited = SL.Coin 1000
                , _fees      = SL.Coin 1
                , _ppups     = def

                }
            , _delegationState = def
            }
        , esPrevPp       = SL.emptyPParams
        , esPp           = SL.emptyPParams { SL._minUTxOValue = SL.Coin 1 }
        , esNonMyopic    = nonMyopic
        }
      where
        addr :: SL.Addr (EraCrypto era)
        addr = SL.Addr
                 SL.Testnet
                 (keyToCredential examplePayKey)
                 (SL.StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: SL.PulsingRewUpdate (EraCrypto era)
    rewardUpdate = SL.startStep @era
                     (EpochSize 432000)
                     (SL.BlocksMade (Map.singleton (mkKeyHash 1) 10))
                     epochState
                     (SL.Coin 45)
                     (SL.activeSlotCoeff SL.testGlobals)
                     10

    nonMyopic :: SL.NonMyopic (EraCrypto era)
    nonMyopic = def

exampleLedgerState ::
     forall era.
     ( ShelleyBasedEra era
     , Core.PParams era ~ SL.PParams era
     )
  => Core.Value era
  -> LedgerState (ShelleyBlock era)
exampleLedgerState value = ShelleyLedgerState {
      shelleyLedgerTip        = NotOrigin ShelleyTip {
                                    shelleyTipSlotNo  = SlotNo 9
                                  , shelleyTipBlockNo = BlockNo 3
                                  , shelleyTipHash    = exampleHeaderHash (Proxy @era)
                                  }
    , shelleyLedgerState      = exampleNewEpochState value
    , shelleyLedgerTransition = ShelleyTransitionInfo {shelleyAfterVoting = 0}
    }

exampleHeaderState :: ShelleyBasedEra era => HeaderState (ShelleyBlock era)
exampleHeaderState = genesisHeaderState exampleChainDepState

exampleExtLedgerState ::
     ( ShelleyBasedEra era
     , Core.PParams era ~ SL.PParams era
     )
  => Core.Value era
  -> ExtLedgerState (ShelleyBlock era)
exampleExtLedgerState value = ExtLedgerState {
      ledgerState = exampleLedgerState value
    , headerState = exampleHeaderState
    }

{-------------------------------------------------------------------------------
  Keys
-------------------------------------------------------------------------------}

examplePayKey :: Crypto c => SL.KeyPair 'SL.Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: Crypto c => SL.KeyPair 'SL.Staking c
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: forall c r. Crypto c => SL.AllIssuerKeys c r
exampleKeys =
    SL.AllIssuerKeys
      coldKey
      (mkVRFKeyPair (Proxy @c) 1)
      [(SL.KESPeriod 0, SL.mkKESKeyPair (1, 0, 0, 0, 3))]
      (SL.hashKey (SL.vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

examplePoolParams :: forall c. Crypto c => SL.PoolParams c
examplePoolParams = SL.PoolParams {
      _poolId     = SL.hashKey $ SL.vKey $ SL.cold poolKeys
    , _poolVrf    = SL.hashVerKeyVRF $ snd $ SL.vrf poolKeys
    , _poolPledge = SL.Coin 1
    , _poolCost   = SL.Coin 5
    , _poolMargin = SL.unsafeMkUnitInterval 0.1
    , _poolRAcnt  = SL.RewardAcnt SL.Testnet (keyToCredential exampleStakeKey)
    , _poolOwners = Set.singleton $ SL.hashKey $ SL.vKey exampleStakeKey
    , _poolRelays = StrictSeq.empty
    , _poolMD     = SJust $ SL.PoolMetadata {
          _poolMDUrl  = fromJust $ SL.textToUrl "consensus.pool"
        , _poolMDHash = "{}"
        }
    }
  where
    poolKeys = exampleKeys @c @'SL.StakePool

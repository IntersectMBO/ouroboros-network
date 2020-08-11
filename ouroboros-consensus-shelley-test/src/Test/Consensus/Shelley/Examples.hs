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
    testEpochInfo
  , testShelleyGenesis
  , codecConfig
  , mkDummyHash
    -- * Examples
  , examples
  , exampleBlock
  , exampleHeader
  , exampleHeaderHash
  , exampleTx
  , exampleGenTx
  , exampleGenTxId
  , exampleApplyTxErr
  , exampleChainDepState
  , exampleLedgerState
  , exampleHeaderState
  , exampleExtLedgerState
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Word (Word64, Word8)

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import           Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Seed as Seed
import           Cardano.Crypto.VRF (VRFAlgorithm (..))
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Util.Time

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Hashing as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.MetaData as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Rewards as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Ledgers as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS
import qualified Shelley.Spec.Ledger.STS.Tickn as STS
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL
import qualified Test.Shelley.Spec.Ledger.Generator.Core as SL
                     (AllIssuerKeys (..), genesisId, mkOCert)
import           Test.Shelley.Spec.Ledger.Orphans ()
import qualified Test.Shelley.Spec.Ledger.Utils as SL hiding (mkKeyPair,
                     mkKeyPair', mkVRFKeyPair)

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCrypto,
                     TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

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

codecConfig :: CodecConfig (ShelleyBlock TPraosStandardCrypto)
codecConfig = ShelleyCodecConfig

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . SL.hashWithSerialiser @h toCBOR

mkKeyHash :: forall c discriminator. Crypto c => Int -> SL.KeyHash discriminator c
mkKeyHash = SL.KeyHash . mkDummyHash (Proxy @(SL.ADDRHASH c))

mkScriptHash :: forall c. Crypto c => Int -> SL.ScriptHash c
mkScriptHash = SL.ScriptHash . mkDummyHash (Proxy @(SL.ADDRHASH c))

-- | @mkKeyPair'@ from @Test.Shelley.Spec.Ledger.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/input-output-hk/cardano-ledger-specs/issues/1770>
mkDSIGNKeyPair ::
     forall c kd. DSIGNAlgorithm (SL.DSIGN c)
  => Word8
  -> SL.KeyPair kd c
mkDSIGNKeyPair byte = SL.KeyPair (SL.VKey $ deriveVerKeyDSIGN sk) sk
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (seedSizeDSIGN (Proxy @(SL.DSIGN c))))
          byte

    sk = genKeyDSIGN seed

mkVRFKeyPair ::
     forall v. VRFAlgorithm v
  => Word8
  -> (SignKeyVRF v, VerKeyVRF v)
mkVRFKeyPair byte = (sk, deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (seedSizeVRF (Proxy @v)))
          byte

    sk = genKeyVRF seed

keyToCredential :: Crypto c => SL.KeyPair r c -> SL.Credential r c
keyToCredential = SL.KeyHashObj . SL.hashKey . SL.vKey

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Golden.Examples (ShelleyBlock TPraosStandardCrypto)
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
        , ("StakeDistribution",      SomeResult GetStakeDistribution examplePoolDistr)
        , ("NonMyopicMemberRewards", SomeResult (GetNonMyopicMemberRewards Set.empty)
            (NonMyopicMemberRewards $ Map.fromList [
                (Left (SL.Coin 100), Map.singleton (mkKeyHash 2) (SL.Coin 3))
              , (Right (SL.ScriptHashObj (mkScriptHash 1)), Map.empty)
              , (Right (SL.KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (SL.Coin 9))
              ]))
        ]

    proposedPParamsUpdates :: SL.ProposedPPUpdates TPraosStandardCrypto
    proposedPParamsUpdates = SL.ProposedPPUpdates $ Map.singleton
        (mkKeyHash 0)
        (SL.emptyPParamsUpdate {SL._keyDeposit = SJust 100})

examplePoolDistr :: forall c. TPraosCrypto c => SL.PoolDistr c
examplePoolDistr = SL.PoolDistr $ Map.fromList [
      (mkKeyHash 1, SL.IndividualPoolStake
                      1
                      (SL.hashVerKeyVRF (snd (SL.vrf (exampleKeys @c)))))
    ]

-- | This is not a valid block. We don't care, we are only interested in
-- serialisation, not validation.
exampleBlock :: ShelleyBlock TPraosStandardCrypto
exampleBlock = mkShelleyBlock $ SL.Block blockHeader blockBody
  where
    keys :: SL.AllIssuerKeys TPraosStandardCrypto 'SL.StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ SL.hot keys
    SL.KeyPair vKeyCold _ = SL.cold keys

    blockHeader :: SL.BHeader TPraosStandardCrypto
    blockHeader = SL.BHeader blockHeaderBody (SL.signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: SL.BHBody TPraosStandardCrypto
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

    blockBody :: SL.TxSeq TPraosStandardCrypto
    blockBody = SL.TxSeq (StrictSeq.fromList [exampleTx])

    mkBytes :: Word8 -> ByteString
    mkBytes =
      Strict.replicate
        (fromIntegral (sizeOutputVRF (Proxy @(SL.VRF TPraosStandardCrypto))))

exampleSerialisedBlock :: Serialised (ShelleyBlock TPraosStandardCrypto)
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleHeader :: Header (ShelleyBlock TPraosStandardCrypto)
exampleHeader = getHeader exampleBlock

exampleSerialisedHeader :: SerialisedHeader (ShelleyBlock TPraosStandardCrypto)
exampleSerialisedHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")

exampleHeaderHash :: HeaderHash (ShelleyBlock TPraosStandardCrypto)
exampleHeaderHash = blockHash exampleBlock

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx :: forall c. c ~ TPraosStandardCrypto => SL.Tx c
exampleTx = SL.Tx txBody witnessSet (SJust metadata)
  where
    txBody :: SL.TxBody c
    txBody = SL.TxBody
        (Set.fromList [
            SL.TxIn SL.genesisId 0
          ])
        (StrictSeq.fromList [
            SL.TxOut (SL.mkAddr (examplePayKey, exampleStakeKey)) (SL.Coin 100000)
          ])
        (StrictSeq.fromList [
            SL.DCertDeleg (SL.RegKey (keyToCredential exampleStakeKey))
          , SL.DCertPool (SL.RegPool examplePoolParams)
          , SL.DCertMir $ SL.MIRCert SL.ReservesMIR $ Map.fromList [
              (keyToCredential (mkDSIGNKeyPair 2), 110)
            ]
          ])
        (SL.Wdrl $ Map.fromList [
            (SL._poolRAcnt examplePoolParams, (SL.Coin 100))
          ])
        (SL.Coin 3)
        (SlotNo 10)
        (SJust (SL.Update ppup (EpochNo 0)))
        (SJust (SL.hashMetaData metadata))
      where
        ppup :: SL.ProposedPPUpdates c
        ppup = SL.ProposedPPUpdates $
            Map.singleton
              (mkKeyHash 1)
              (SL.emptyPParamsUpdate { SL._maxBHSize = SJust 4000 })

    witnessSet :: SL.WitnessSet c
    witnessSet = mempty {
          SL.addrWits =
            SL.makeWitnessesVKey (SL.hashAnnotated txBody) witnesses
        }
      where
        witnesses :: [SL.KeyPair 'SL.Witness c]
        witnesses = [
              SL.asWitness examplePayKey
            , SL.asWitness exampleStakeKey
            , SL.asWitness $ SL.cold (exampleKeys @c @'SL.StakePool)
            ]

    metadata :: SL.MetaData
    metadata = SL.MetaData $ Map.fromList [
          (1, SL.S "string")
        , (2, SL.B "bytes")
        , (3, SL.List [SL.I 1, SL.I 2])
        , (4, SL.Map [(SL.I 3, SL.B "b")])
        ]

exampleGenTx :: GenTx (ShelleyBlock TPraosStandardCrypto)
exampleGenTx = mkShelleyTx exampleTx

exampleGenTxId :: GenTxId (ShelleyBlock TPraosStandardCrypto)
exampleGenTxId = txId exampleGenTx

-- TODO incomplete, this type has tons of constructors that can all change.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1896.
exampleApplyTxErr :: ApplyTxErr (ShelleyBlock TPraosStandardCrypto)
exampleApplyTxErr =
      ApplyTxError
    $ pure
    $ STS.LedgerFailure
    $ STS.UtxowFailure
    $ STS.InvalidWitnessesUTXOW [SL.asWitness (SL.vKey exampleStakeKey)]

exampleAnnTip :: AnnTip (ShelleyBlock TPraosStandardCrypto)
exampleAnnTip = AnnTip {
      annTipSlotNo  = SlotNo 14
    , annTipBlockNo = BlockNo 6
    , annTipInfo    = exampleHeaderHash
    }

exampleChainDepState :: ChainDepState (BlockProtocol (ShelleyBlock TPraosStandardCrypto))
exampleChainDepState =
    TPraosState.append 2             (mkPrtclState 2) $
    TPraosState.empty  (NotOrigin 1) (mkPrtclState 1)
  where
    mkPrtclState :: Word64 -> SL.ChainDepState TPraosStandardCrypto
    mkPrtclState seed = SL.ChainDepState
      { SL.csProtocol = STS.PrtclState
          (Map.fromList [
              (mkKeyHash 1, 1)
            , (mkKeyHash 2, 2)
            ])
          (SL.mkNonceFromNumber seed)
          (SL.mkNonceFromNumber seed)
      , SL.csTickn = STS.TicknState
          SL.NeutralNonce
          (SL.mkNonceFromNumber seed)
      , SL.csLabNonce =
          SL.mkNonceFromNumber seed
      }

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
     forall c. c ~ TPraosStandardCrypto
  => SL.NewEpochState c
exampleNewEpochState = SL.NewEpochState {
      nesEL     = EpochNo 0
    , nesBprev  = SL.BlocksMade (Map.singleton (mkKeyHash 1) 10)
    , nesBcur   = SL.BlocksMade (Map.singleton (mkKeyHash 2) 3)
    , nesEs     = epochState
    , nesRu     = SJust rewardUpdate
    , nesPd     = examplePoolDistr
    , nesOsched = Map.fromList [
          (SlotNo 0, SL.NonActiveSlot)
        , (SlotNo 1, SL.ActiveSlot (mkKeyHash 1))
        ]
    }
  where
    epochState :: SL.EpochState c
    epochState = SL.EpochState {
          esAccountState = SL.AccountState {
              _treasury = SL.Coin 10000
            , _reserves = SL.Coin 1000
            }
        , esSnapshots    = SL.emptySnapShots
        , esLState       = SL.LedgerState {
              _utxoState = SL.UTxOState {
                  _utxo      = SL.UTxO $ Map.fromList [
                      (SL.TxIn (SL.TxId (mkDummyHash Proxy 1)) 0,
                       SL.TxOut addr (SL.Coin 10))
                    ]
                , _deposited = SL.Coin 1000
                , _fees      = SL.Coin 1
                , _ppups     = SL.emptyPPUPState

                }
            , _delegationState = SL.emptyDPState
            }
        , esPrevPp       = SL.emptyPParams
        , esPp           = SL.emptyPParams { SL._minUTxOValue = SL.Coin 1 }
        , esNonMyopic    = nonMyopic
        }
      where
        addr :: SL.Addr c
        addr = SL.Addr
                 SL.Testnet
                 (keyToCredential examplePayKey)
                 (SL.StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: SL.RewardUpdate c
    rewardUpdate = SL.RewardUpdate {
          deltaT    = SL.Coin 10
        , deltaR    = SL.Coin 100
        , rs        = Map.singleton (keyToCredential exampleStakeKey) (SL.Coin 10)
        , deltaF    = SL.Coin 3
        , nonMyopic = nonMyopic
        }

    nonMyopic :: SL.NonMyopic c
    nonMyopic = SL.emptyNonMyopic

exampleLedgerState :: LedgerState (ShelleyBlock TPraosStandardCrypto)
exampleLedgerState = ShelleyLedgerState {
      ledgerTip    = blockPoint exampleBlock
    , history      = history
    , shelleyState = exampleNewEpochState
    }
  where
    history :: History.LedgerViewHistory TPraosStandardCrypto
    history = History.snapOld
        (SL.securityParameter SL.testGlobals)
        (SlotNo 10)
        (SL.currentLedgerView exampleNewEpochState)
        History.empty

exampleHeaderState :: HeaderState (ShelleyBlock TPraosStandardCrypto)
exampleHeaderState = genesisHeaderState st
  where
    prtclState :: SL.ChainDepState TPraosStandardCrypto
    prtclState = SL.ChainDepState
      { SL.csProtocol = STS.PrtclState
          (Map.fromList [
              (mkKeyHash 1, 1)
            ])
          (SL.mkNonceFromNumber 1)
          SL.NeutralNonce
      , SL.csTickn    = STS.TicknState {
            STS.ticknStateEpochNonce    = SL.NeutralNonce
          , STS.ticknStatePrevHashNonce = SL.mkNonceFromNumber 2
          }
      , SL.csLabNonce = SL.mkNonceFromNumber 2
      }

    st :: TPraosState TPraosStandardCrypto
    st = TPraosState.empty (NotOrigin 1) prtclState

exampleExtLedgerState :: ExtLedgerState (ShelleyBlock TPraosStandardCrypto)
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

{-------------------------------------------------------------------------------
  Keys
-------------------------------------------------------------------------------}

examplePayKey :: Crypto c => SL.KeyPair 'SL.Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: Crypto c => SL.KeyPair 'SL.Staking c
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: Crypto c => SL.AllIssuerKeys c r
exampleKeys =
    SL.AllIssuerKeys
      coldKey
      (mkVRFKeyPair 1)
      [(SL.KESPeriod 0, SL.mkKESKeyPair (1, 0, 0, 0, 3))]
      (SL.hashKey (SL.vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

examplePoolParams :: forall c. Crypto c => SL.PoolParams c
examplePoolParams = SL.PoolParams {
      _poolPubKey = SL.hashKey $ SL.vKey $ SL.cold poolKeys
    , _poolVrf    = SL.hashVerKeyVRF $ snd $ SL.vrf poolKeys
    , _poolPledge = SL.Coin 1
    , _poolCost   = SL.Coin 5
    , _poolMargin = SL.unsafeMkUnitInterval 0.1
    , _poolRAcnt  = SL.RewardAcnt SL.Testnet (keyToCredential exampleStakeKey)
    , _poolOwners = Set.singleton $ SL.hashKey $ SL.vKey exampleStakeKey
    , _poolRelays = StrictSeq.empty
    , _poolMD     = SJust $ SL.PoolMetaData {
          _poolMDUrl  = fromJust $ SL.textToUrl "consensus.pool"
        , _poolMDHash = "{}"
        }
    }
  where
    poolKeys = exampleKeys @c @'SL.StakePool

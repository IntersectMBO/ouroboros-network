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
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Word (Word64, Word8)

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm)
import qualified Cardano.Crypto.DSIGN as DSIGN
import           Cardano.Crypto.Hash (HashAlgorithm)
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

import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, VRF)
import           Cardano.Ledger.Era (Era (Crypto))
import qualified Shelley.Spec.Ledger.API as SL
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
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
import qualified Shelley.Spec.Ledger.STS.Utxow as STS
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.UTxO as SL
import qualified Test.Shelley.Spec.Ledger.Generator.Core as SL
                     (AllIssuerKeys (..), genesisId, mkOCert)
import           Test.Shelley.Spec.Ledger.Orphans ()
import qualified Test.Shelley.Spec.Ledger.Utils as SL hiding (mkKeyPair,
                     mkKeyPair', mkVRFKeyPair)

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley,
                     TPraosCrypto, TPraosState (..))

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

mkKeyHash :: forall era discriminator. Era era => Int -> SL.KeyHash discriminator era
mkKeyHash = SL.KeyHash . mkDummyHash (Proxy @(ADDRHASH (Crypto era)))

mkScriptHash :: forall era. Era era => Int -> SL.ScriptHash era
mkScriptHash = SL.ScriptHash . mkDummyHash (Proxy @(ADDRHASH (Crypto era)))

-- | @mkKeyPair'@ from @Test.Shelley.Spec.Ledger.Utils@ doesn't work for real
-- crypto:
-- <https://github.com/input-output-hk/cardano-ledger-specs/issues/1770>
mkDSIGNKeyPair ::
     forall era kd. DSIGNAlgorithm (DSIGN (Crypto era))
  => Word8
  -> SL.KeyPair kd era
mkDSIGNKeyPair byte = SL.KeyPair (SL.VKey $ DSIGN.deriveVerKeyDSIGN sk) sk
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @(DSIGN (Crypto era)))))
          byte

    sk = DSIGN.genKeyDSIGN seed

mkVRFKeyPair ::
     forall era. VRFAlgorithm (VRF (Crypto era))
  => Proxy era
  -> Word8
  -> (SL.SignKeyVRF era, SL.VerKeyVRF era)
mkVRFKeyPair _ byte = (sk, VRF.deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (VRF.seedSizeVRF (Proxy @(VRF (Crypto era)))))
          byte

    sk = VRF.genKeyVRF seed

keyToCredential :: Era era => SL.KeyPair r era -> SL.Credential r era
keyToCredential = SL.KeyHashObj . SL.hashKey . SL.vKey

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Golden.Examples (ShelleyBlock StandardShelley)
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

    proposedPParamsUpdates :: SL.ProposedPPUpdates StandardShelley
    proposedPParamsUpdates = SL.ProposedPPUpdates $ Map.singleton
        (mkKeyHash 0)
        (SL.emptyPParamsUpdate {SL._keyDeposit = SJust (SL.Coin 100)})

examplePoolDistr :: forall era. TPraosCrypto era => SL.PoolDistr era
examplePoolDistr = SL.PoolDistr $ Map.fromList [
      (mkKeyHash 1, SL.IndividualPoolStake
                      1
                      (SL.hashVerKeyVRF (snd (SL.vrf (exampleKeys @era)))))
    ]

-- | This is not a valid block. We don't care, we are only interested in
-- serialisation, not validation.
exampleBlock :: ShelleyBlock StandardShelley
exampleBlock = mkShelleyBlock $ SL.Block blockHeader blockBody
  where
    keys :: SL.AllIssuerKeys StandardShelley 'SL.StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ SL.hot keys
    SL.KeyPair vKeyCold _ = SL.cold keys

    blockHeader :: SL.BHeader StandardShelley
    blockHeader = SL.BHeader blockHeaderBody (SL.signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: SL.BHBody StandardShelley
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

    blockBody :: SL.TxSeq StandardShelley
    blockBody = SL.TxSeq (StrictSeq.fromList [exampleTx])

    mkBytes :: Word8 -> ByteString
    mkBytes =
      Strict.replicate
        (fromIntegral (VRF.sizeOutputVRF (Proxy @(VRF (Crypto StandardShelley)))))

exampleSerialisedBlock :: Serialised (ShelleyBlock StandardShelley)
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleHeader :: Header (ShelleyBlock StandardShelley)
exampleHeader = getHeader exampleBlock

exampleSerialisedHeader :: SerialisedHeader (ShelleyBlock StandardShelley)
exampleSerialisedHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")

exampleHeaderHash :: HeaderHash (ShelleyBlock StandardShelley)
exampleHeaderHash = blockHash exampleBlock

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx :: forall era. era ~ StandardShelley => SL.Tx era
exampleTx = SL.Tx txBody witnessSet (SJust metadata)
  where
    txBody :: SL.TxBody era
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
              (keyToCredential (mkDSIGNKeyPair 2), SL.Coin 110)
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
        ppup :: SL.ProposedPPUpdates era
        ppup = SL.ProposedPPUpdates $
            Map.singleton
              (mkKeyHash 1)
              (SL.emptyPParamsUpdate { SL._maxBHSize = SJust 4000 })

    witnessSet :: SL.WitnessSet era
    witnessSet = mempty {
          SL.addrWits =
            SL.makeWitnessesVKey (SL.hashAnnotated txBody) witnesses
        }
      where
        witnesses :: [SL.KeyPair 'SL.Witness era]
        witnesses = [
              SL.asWitness examplePayKey
            , SL.asWitness exampleStakeKey
            , SL.asWitness $ SL.cold (exampleKeys @era @'SL.StakePool)
            ]

    metadata :: SL.MetaData
    metadata = SL.MetaData $ Map.fromList [
          (1, SL.S "string")
        , (2, SL.B "bytes")
        , (3, SL.List [SL.I 1, SL.I 2])
        , (4, SL.Map [(SL.I 3, SL.B "b")])
        ]

exampleGenTx :: GenTx (ShelleyBlock StandardShelley)
exampleGenTx = mkShelleyTx exampleTx

exampleGenTxId :: GenTxId (ShelleyBlock StandardShelley)
exampleGenTxId = txId exampleGenTx

-- TODO incomplete, this type has tons of constructors that can all change.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1896.
exampleApplyTxErr :: ApplyTxErr (ShelleyBlock StandardShelley)
exampleApplyTxErr =
      ApplyTxError
    $ pure
    $ STS.LedgerFailure
    $ STS.UtxowFailure
    $ STS.InvalidWitnessesUTXOW [SL.asWitness (SL.vKey exampleStakeKey)]

exampleAnnTip :: AnnTip (ShelleyBlock StandardShelley)
exampleAnnTip = AnnTip {
      annTipSlotNo  = SlotNo 14
    , annTipBlockNo = BlockNo 6
    , annTipInfo    = exampleHeaderHash
    }

exampleChainDepState :: ChainDepState (BlockProtocol (ShelleyBlock StandardShelley))
exampleChainDepState = TPraosState (NotOrigin 1) (mkPrtclState 1)
  where
    mkPrtclState :: Word64 -> SL.ChainDepState StandardShelley
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
     forall era. era ~ StandardShelley
  => SL.NewEpochState era
exampleNewEpochState = SL.NewEpochState {
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
        addr :: SL.Addr era
        addr = SL.Addr
                 SL.Testnet
                 (keyToCredential examplePayKey)
                 (SL.StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: SL.RewardUpdate era
    rewardUpdate = SL.RewardUpdate {
          deltaT    = SL.Coin 10
        , deltaR    = SL.Coin 100
        , rs        = Map.singleton (keyToCredential exampleStakeKey) (SL.Coin 10)
        , deltaF    = SL.Coin 3
        , nonMyopic = nonMyopic
        }

    nonMyopic :: SL.NonMyopic era
    nonMyopic = SL.emptyNonMyopic

exampleLedgerState :: LedgerState (ShelleyBlock StandardShelley)
exampleLedgerState = ShelleyLedgerState {
      shelleyLedgerTipPoint = blockPoint exampleBlock
    , shelleyLedgerState    = exampleNewEpochState
    }

exampleHeaderState :: HeaderState (ShelleyBlock StandardShelley)
exampleHeaderState = genesisHeaderState exampleChainDepState

exampleExtLedgerState :: ExtLedgerState (ShelleyBlock StandardShelley)
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

{-------------------------------------------------------------------------------
  Keys
-------------------------------------------------------------------------------}

examplePayKey :: Era era => SL.KeyPair 'SL.Payment era
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: Era era => SL.KeyPair 'SL.Staking era
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: forall era r. Era era => SL.AllIssuerKeys era r
exampleKeys =
    SL.AllIssuerKeys
      coldKey
      (mkVRFKeyPair (Proxy @era) 1)
      [(SL.KESPeriod 0, SL.mkKESKeyPair (1, 0, 0, 0, 3))]
      (SL.hashKey (SL.vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

examplePoolParams :: forall era. Era era => SL.PoolParams era
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
    poolKeys = exampleKeys @era @'SL.StakePool

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeApplications         #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Ledger (tests) where

import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy (..))
import           Data.Ratio ((%))
import           Numeric.Natural (Natural)

import           Cardano.Binary (fromCBOR, serialize, toCBOR)

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     HeaderHash, Point, SlotNo (..), blockHash, genesisPoint)
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     HashInfo (..))

import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Crypto as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Rewards as SL
import qualified Shelley.Spec.Ledger.Scripts as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Deleg as STS
import qualified Shelley.Spec.Ledger.STS.Delegs as STS
import qualified Shelley.Spec.Ledger.STS.Delpl as STS
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Ledgers as STS
import qualified Shelley.Spec.Ledger.STS.Pool as STS
import qualified Shelley.Spec.Ledger.STS.Ppup as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS
import qualified Shelley.Spec.Ledger.STS.Utxo as STS
import qualified Shelley.Spec.Ledger.STS.Utxow as STS
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Test.Shelley.Spec.Ledger.Examples.Examples as Examples
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Gen
import qualified Test.Shelley.Spec.Ledger.PreSTSGenerator as SL
import qualified Test.Shelley.Spec.Ledger.Utils as SL (testGlobals)

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.History (LedgerViewHistory)
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck hiding (Result)
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.QuickCheck

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Roundtrip

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)


tests :: TestTree
tests = testGroup "Shelley"
  [ testGroup "Serialisation roundtrips"
      [ testProperty "roundtrip Block"          prop_roundtrip_Block
      , testProperty "roundtrip Header"         prop_roundtrip_Header
      , testProperty "roundtrip HeaderHash"     prop_roundtrip_HeaderHash
      , testProperty "roundtrip GenTx"          prop_roundtrip_GenTx
      , testProperty "roundtrip GenTxId"        prop_roundtrip_GenTxId
      , testProperty "roundtrip ApplyTxErr"     prop_roundtrip_ApplyTxErr
      , testProperty "roundtrip Query"          prop_roundtrip_Query
      , testProperty "roundtrip Result"         prop_roundtrip_Result
      , testProperty "roundtrip ConsensusState" prop_roundtrip_ConsensusState
      , testProperty "roundtrip LedgerState"    prop_roundtrip_LedgerState
      ]

  , testProperty "BinaryInfo sanity check" prop_encodeShelleyBlockWithInfo


  , testHashInfo (Proxy @TPraosStandardCrypto) "Real crypto"
  , testHashInfo (Proxy @TPraosMockCrypto)     "Mock crypto"

  , testGroup "Golden tests"
      [ test_golden_Block
      , test_golden_Header
      , test_golden_HeaderHash
      , test_golden_GenTx
      , test_golden_GenTxId
      , test_golden_ApplyTxErr
      , test_golden_ConsensusState
      , test_golden_LedgerState
        -- TODO Query and result
      ]

  , testGroup "Integrity"
      $ const [] $ -- TODO these tests are disabled awaiting #1821
      [ testProperty "detect corruption in blocks"  prop_detectCorruption_Block
      , testProperty "detect corruption in headers" prop_detectCorruption_Header
      ]
  ]
  where
    testHashInfo :: forall c. Crypto c => Proxy c -> String -> TestTree
    testHashInfo _ name = testGroup ("HashInfo sanity check: " <> name)
      [ testProperty "putHash/getHash roundtrip" (prop_shelleyHashInfo_roundtrip @c)
      , testProperty "hashSize"                  (prop_shelleyHashInfo_hashSize  @c)
      ]

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | We run the tests with mock crypto, as it is easier to generate and debug
-- things. The code is parametric in the crypto, so it shouldn't make much of
-- a difference. This also has the important advantage that we can reuse the
-- generators from cardano-ledger-specs.
type Block = ShelleyBlock TPraosMockCrypto

{-------------------------------------------------------------------------------
  SomeResult
-------------------------------------------------------------------------------}

-- | To easily generate all the possible @result@s of the 'Query' GADT, we
-- introduce an existential that also bundles the corresponding 'Query' as
-- evidence. We also capture an 'Eq' and a 'Show' constraint, as we need them
-- in the tests.
data SomeResult where
  SomeResult :: (Eq result, Show result)
             => Query Block result -> result -> SomeResult

deriving instance Show SomeResult

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: Block -> Property
prop_roundtrip_Block = roundtrip toCBOR fromCBOR

prop_roundtrip_Header :: Header Block -> Property
prop_roundtrip_Header = roundtrip toCBOR fromCBOR

prop_roundtrip_HeaderHash :: HeaderHash Block -> Property
prop_roundtrip_HeaderHash = roundtrip toCBOR fromCBOR

prop_roundtrip_GenTx :: GenTx Block -> Property
prop_roundtrip_GenTx = roundtrip toCBOR fromCBOR

prop_roundtrip_GenTxId :: GenTxId Block -> Property
prop_roundtrip_GenTxId = roundtrip toCBOR fromCBOR

prop_roundtrip_ApplyTxErr :: ApplyTxErr Block -> Property
prop_roundtrip_ApplyTxErr = roundtrip toCBOR fromCBOR

prop_roundtrip_Query :: Some (Query Block) -> Property
prop_roundtrip_Query =
    roundtrip
      (\case { Some query -> encodeShelleyQuery query })
      decodeShelleyQuery

prop_roundtrip_Result :: SomeResult -> Property
prop_roundtrip_Result (SomeResult query result) =
    roundtrip
      (encodeShelleyResult query)
      (decodeShelleyResult query)
      result

prop_roundtrip_ConsensusState
  :: TPraosState TPraosMockCrypto
  -> Property
prop_roundtrip_ConsensusState = roundtrip toCBOR fromCBOR

prop_roundtrip_LedgerState :: LedgerState Block -> Property
prop_roundtrip_LedgerState =
    roundtrip encodeShelleyLedgerState decodeShelleyLedgerState

{-------------------------------------------------------------------------------
  BinaryInfo
-------------------------------------------------------------------------------}

prop_encodeShelleyBlockWithInfo :: Block -> Property
prop_encodeShelleyBlockWithInfo blk =
    encodedHeader === shelleyAddHeaderEnvelope extractedHeader
  where
    BinaryInfo { binaryBlob, headerOffset, headerSize } =
      encodeShelleyBlockWithInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString binaryBlob

    encodedHeader :: Lazy.ByteString
    encodedHeader = serialize (getHeader blk)

{-------------------------------------------------------------------------------
  HashInfo
-------------------------------------------------------------------------------}

prop_shelleyHashInfo_roundtrip :: Crypto c => ShelleyHash c -> Property
prop_shelleyHashInfo_roundtrip h =
    case Get.runGetOrFail getHash serialisedHash of
      Left (_, _, e)
        -> counterexample e $ property False
      Right (unconsumed, _, h')
        | Lazy.null unconsumed
        -> h === h'
        | otherwise
        -> counterexample ("unconsumed bytes: " <> show unconsumed) $
           property False
  where
    HashInfo { getHash, putHash } = shelleyHashInfo

    serialisedHash = Put.runPut (putHash h)

prop_shelleyHashInfo_hashSize :: Crypto c => ShelleyHash c -> Property
prop_shelleyHashInfo_hashSize h =
    hashSize === fromIntegral (Lazy.length serialisedHash)
  where
    HashInfo { hashSize, putHash } = shelleyHashInfo

    serialisedHash = Put.runPut (putHash h)

{-------------------------------------------------------------------------------
  Golden tests
-------------------------------------------------------------------------------}

exampleBlock :: Block
exampleBlock = mkShelleyBlock Examples.blockEx3B

exampleGenTx :: GenTx Block
exampleGenTx = mkShelleyTx Examples.txEx2A

test_golden_Block :: TestTree
test_golden_Block = goldenTestCBOR
    "Block"
    toCBOR
    exampleBlock
    "test/golden/cbor/shelley/Block"

test_golden_Header :: TestTree
test_golden_Header = goldenTestCBOR
    "Header"
    toCBOR
    (getHeader exampleBlock)
    "test/golden/cbor/shelley/Header"

test_golden_HeaderHash :: TestTree
test_golden_HeaderHash = goldenTestCBOR
    "HeaderHash"
    toCBOR
    (blockHash exampleBlock)
    "test/golden/cbor/shelley/HeaderHash"

test_golden_GenTx :: TestTree
test_golden_GenTx = goldenTestCBOR
    "GenTx"
    toCBOR
    exampleGenTx
    "test/golden/cbor/shelley/GenTx"

test_golden_GenTxId :: TestTree
test_golden_GenTxId = goldenTestCBOR
    "GenTxId"
    toCBOR
    (txId exampleGenTx)
    "test/golden/cbor/shelley/GenTxId"

test_golden_ApplyTxErr :: TestTree
test_golden_ApplyTxErr = goldenTestCBOR
    "ApplyTxErr"
    toCBOR
    exampleApplyTxErr
    "test/golden/cbor/shelley/ApplyTxErr"
  where
    -- TODO incomplete, this type has tons of constructors that can all
    -- change.
    exampleApplyTxErr :: ApplyTxErr Block
    exampleApplyTxErr =
        ApplyTxError
      $ pure
      $ STS.LedgerFailure
      $ STS.UtxowFailure
      $ STS.InvalidWitnessesUTXOW

test_golden_ConsensusState :: TestTree
test_golden_ConsensusState = goldenTestCBOR
    "ConsensusState"
    toCBOR
    exampleConsensusState
    "test/golden/cbor/shelley/ConsensusState"
  where
    exampleConsensusState :: ConsensusState (BlockProtocol Block)
    exampleConsensusState =
      TPraosState.append (mkPrtclState 2) $
      TPraosState.empty  (mkPrtclState 1)

    mkPrtclState :: SlotNo -> STS.PrtclState TPraosMockCrypto
    mkPrtclState slot = STS.PrtclState
      (Map.fromList
       [ (SL.DiscKeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 1), 1)
       , (SL.DiscKeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 2), 2)
       ])
      (At SL.LastAppliedBlock {
          labBlockNo = 0
        , labSlotNo  = slot
        , labHash    = SL.HashHeader (mkDummyHash (Proxy @TPraosMockCrypto) 1)
        })
      SL.NeutralNonce
      (SL.mkNonce 1)
      (SL.mkNonce 2)
      (SL.mkNonce 3)

test_golden_LedgerState :: TestTree
test_golden_LedgerState = goldenTestCBOR
    "LedgerState"
    encodeShelleyLedgerState
    exampleLedgerState
    "test/golden/cbor/shelley/LedgerState"
  where
    Examples.CHAINExample { startState, newBlock } = Examples.ex2A

    exampleLedgerState :: LedgerState Block
    exampleLedgerState = reapplyLedgerBlock
      (ShelleyLedgerConfig SL.testGlobals)
      (mkShelleyBlock newBlock)
      (ShelleyLedgerState {
          ledgerTip    = genesisPoint
        , history      = History.empty
        , shelleyState = STS.chainNes startState
        })

goldenTestCBOR :: String -> (a -> Encoding) -> a -> FilePath -> TestTree
goldenTestCBOR name enc a path =
    goldenVsString name path (return bs)
  where
    bs = CBOR.toLazyByteString (enc a)

{-------------------------------------------------------------------------------
  Integrity
-------------------------------------------------------------------------------}

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Block :: Block -> Corruption -> Property
prop_detectCorruption_Block =
    detectCorruption toCBOR (const <$> fromCBOR) verifyBlockIntegrity

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Header :: Header Block -> Corruption -> Property
prop_detectCorruption_Header =
    detectCorruption toCBOR (const <$> fromCBOR) verifyHeaderIntegrity

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

mkDummyHash :: forall c a. Crypto c => Proxy c -> Int -> SL.Hash (SL.HASH c) a
mkDummyHash _ = coerce . SL.hash @(SL.HASH c)

genHash :: forall a c. Crypto c => Proxy c -> Gen (SL.Hash (SL.HASH c) a)
genHash proxy = mkDummyHash proxy <$> arbitrary

instance Arbitrary Block where
  arbitrary = do
    prevHash         <- unShelleyHash <$> arbitrary
    allPoolKeys      <- elements (map snd Gen.coreNodeKeys)
    txs              <- return [] -- arbitrary
    curSlotNo        <- arbitrary
    curBlockNo       <- BlockNo <$> choose (0, 100)
    epochNonce       <- arbitrary
    blockNonce       <- Gen.NatNonce . fromIntegral <$> choose (1, 100 :: Int)
    praosLeaderValue <- arbitrary
    let kesPeriod       = 10
        keyRegKesPeriod = 10
        ocert           = Gen.mkOCert allPoolKeys 10 (SL.KESPeriod kesPeriod)
    return $ mkShelleyBlock $ Gen.mkBlock
      prevHash
      allPoolKeys
      txs
      curSlotNo
      curBlockNo
      epochNonce
      blockNonce
      praosLeaderValue
      kesPeriod
      keyRegKesPeriod
      ocert

instance Arbitrary (Header Block) where
  arbitrary = getHeader <$> arbitrary

instance Crypto c => Arbitrary (ShelleyHash c) where
  arbitrary = ShelleyHash . SL.HashHeader <$> genHash (Proxy @c)

instance Arbitrary (GenTx Block) where
  arbitrary = mkShelleyTx <$> arbitrary

instance Arbitrary (GenTxId Block) where
  arbitrary = ShelleyTxId . SL.TxId <$> genHash (Proxy @TPraosMockCrypto)

instance Arbitrary (ApplyTxError TPraosMockCrypto) where
  arbitrary = ApplyTxError <$> arbitrary
  shrink (ApplyTxError xs) = [ApplyTxError xs' | xs' <- shrink xs]

instance Arbitrary (Some (Query Block)) where
  arbitrary = oneof
    [ pure $ Some GetLedgerTip
    , pure $ Some GetEpochNo
    , Some . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ Some GetCurrentPParams
    , pure $ Some GetProposedPParamsUpdates
    , pure $ Some GetStakeDistribution
    ]

instance Arbitrary SomeResult where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> arbitrary
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    ]

instance Arbitrary (NonMyopicMemberRewards TPraosMockCrypto) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance Arbitrary (Point Block) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance Arbitrary (TPraosState TPraosMockCrypto) where
  arbitrary = do
      steps     <- choose (0, 5)
      startSlot <- SlotNo <$> choose (0, 100)
      initState <-
        TPraosState.empty . setSlot startSlot <$> arbitrary
      go steps startSlot initState
    where
      go :: Int
         -> SlotNo
         -> TPraosState TPraosMockCrypto
         -> Gen (TPraosState TPraosMockCrypto)
      go steps prevSlot st
        | 0 <- steps = return st
        | otherwise  = do
          let slot = prevSlot + 1
          newPrtclState <- setSlot slot <$> arbitrary
          go (steps - 1) slot (TPraosState.append newPrtclState st)

      setSlot
        :: SlotNo
        -> STS.PrtclState TPraosMockCrypto
        -> STS.PrtclState TPraosMockCrypto
      setSlot slot (STS.PrtclState a b c d e f) =
          STS.PrtclState a b' c d e f
        where
          b' = case b of
            At lab -> At lab { SL.labSlotNo = slot }
            Origin -> At SL.LastAppliedBlock {
              labBlockNo = BlockNo (unSlotNo slot)
            , labSlotNo  = slot
            , labHash    = SL.HashHeader $
                mkDummyHash
                  (Proxy @TPraosMockCrypto)
                  (fromIntegral (unSlotNo slot))
            }

instance Arbitrary (History.LedgerViewHistory TPraosMockCrypto) where
  arbitrary = do
      snapshots   <- choose (0, 5)
      startSlot   <- SlotNo <$> choose (0, 100)
      go snapshots startSlot History.empty
    where
      maxRollback = 5

      go :: Int
         -> SlotNo
         -> LedgerViewHistory TPraosMockCrypto
         -> Gen (LedgerViewHistory TPraosMockCrypto)
      go snapshots slot hist
        | 0 <- snapshots = return hist
        | otherwise      = do
          ledgerView <- arbitrary
          let hist' = History.snapOld maxRollback slot ledgerView hist
          go (snapshots - 1) (succ slot) hist'

instance Arbitrary (LedgerState Block) where
  arbitrary = ShelleyLedgerState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

-- Convenient to have
instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (0, 1000)

{-------------------------------------------------------------------------------
  Generators for cardano-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary SL.Nonce where
  arbitrary = oneof
    [ return SL.NeutralNonce
    , SL.mkNonce . fromIntegral <$> choose (1, 123 :: Int)
    ]

instance Arbitrary SL.UnitInterval where
  arbitrary = fromJust . SL.mkUnitInterval . (% 100) <$> choose (1, 99)

instance Arbitrary SL.Coin where
   -- Cannot be negative even though it is an 'Integer'
  arbitrary = SL.Coin <$> choose (0, 1000)

-- Most generators below don't care about correctness, they just generate
-- random values to exercise the roundtrip generators

instance Crypto c => Arbitrary (SL.DiscKeyHash a c) where
  arbitrary = SL.DiscKeyHash <$> genHash (Proxy @c)

instance Crypto c => Arbitrary (SL.LastAppliedBlock c) where
  arbitrary = SL.LastAppliedBlock
    <$> (BlockNo <$> arbitrary)
    <*> arbitrary
    <*> (SL.HashHeader <$> genHash (Proxy @c))

instance Crypto c => Arbitrary (STS.PrtclState c) where
  arbitrary =
    STS.PrtclState
      <$> arbitrary
      <*> frequency
          [ (1, return Origin)
          , (9, At <$> arbitrary)
          ]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (SL.BlocksMade c) where
  arbitrary = SL.BlocksMade <$> arbitrary

instance Crypto c => Arbitrary (SL.Credential c) where
  arbitrary = oneof
    [ SL.ScriptHashObj . SL.ScriptHash <$> genHash (Proxy @c)
    , SL.KeyHashObj <$> arbitrary
    ]

instance Crypto c => Arbitrary (SL.RewardAcnt c) where
  arbitrary = SL.RewardAcnt <$> arbitrary

instance Crypto c => Arbitrary (SL.RewardUpdate c) where
  arbitrary = return SL.emptyRewardUpdate

instance Crypto c => Arbitrary (SL.PoolDistr c) where
  arbitrary = SL.PoolDistr . Map.fromList <$>
      listOf ((,) <$> arbitrary <*> genVal)
    where
      genVal = (,) <$> arbitrary <*> genHash (Proxy @c)

instance Arbitrary (SL.NewEpochState TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.AccountState where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.LedgerState TPraosMockCrypto) where
  arbitrary = do
    (_ledgerState, _steps, _txfee, _tx, ledgerState) <- hedgehog SL.genValidStateTx
    return ledgerState

instance Crypto c => Arbitrary (SL.Stake c) where
  arbitrary = SL.Stake <$> arbitrary

instance Arbitrary SL.Url where
  arbitrary = return $ SL.Url $ SL.text64 "text"

instance Arbitrary SL.PoolMetaData where
  arbitrary = (`SL.PoolMetaData` "bytestring") <$> arbitrary

instance Crypto c => Arbitrary (SL.PoolParams c) where
  arbitrary = SL.PoolParams
    <$> arbitrary
    <*> genHash (Proxy @c)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary (SL.SnapShot TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.SnapShots TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.ApparentPerformance where
  arbitrary = SL.ApparentPerformance <$> arbitrary

instance Arbitrary (SL.NonMyopic TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.EpochState TPraosMockCrypto) where
  arbitrary = SL.EpochState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary SL.PParams where
  arbitrary = Gen.genPParams

instance Crypto c => Arbitrary (SL.GenDelegs c) where
  arbitrary = SL.GenDelegs <$> arbitrary

instance Crypto c => Arbitrary (SL.LedgerView c) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.Tx TPraosMockCrypto) where
  arbitrary = do
    (_ledgerState, _steps, _txfee, tx, _lv) <- hedgehog SL.genStateTx
    return tx

instance Arbitrary SL.ProtVer where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.ActiveSlotCoeff where
  arbitrary = SL.mkActiveSlotCoeff <$> arbitrary

instance Arbitrary (SL.PParams' Maybe) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.ProposedPPUpdates TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.PPUP TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.UTXO TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.UTXOW TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.POOL TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.DELPL TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.DELEG TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.DELEGS TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.LEDGER TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (STS.PredicateFailure (STS.LEDGERS TPraosMockCrypto)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Eq (Some (Query Block)) where
  Some GetLedgerTip == Some GetLedgerTip = True
  Some GetLedgerTip == _ = False
  Some GetEpochNo == Some GetEpochNo = True
  Some GetEpochNo == _ = False
  Some (GetNonMyopicMemberRewards creds) == Some (GetNonMyopicMemberRewards creds') =
    creds == creds'
  Some (GetNonMyopicMemberRewards _) == _ = False
  Some GetCurrentPParams == Some GetCurrentPParams = True
  Some GetCurrentPParams == _ = False
  Some GetProposedPParamsUpdates == Some GetProposedPParamsUpdates = True
  Some GetProposedPParamsUpdates == _ = False
  Some GetStakeDistribution == Some GetStakeDistribution = True
  Some GetStakeDistribution == _ = False

deriving instance Show (Some (Query Block))

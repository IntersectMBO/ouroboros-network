{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Generators (
    SomeResult (..)
  , genHash
  ) where

import           Data.Coerce (coerce)
import           Data.IP (IPv4, IPv6, toIPv4, toIPv6)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy (..))
import           Data.Ratio ((%))
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           Data.Word (Word8)
import           Numeric.Natural (Natural)

import           Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     Point, SlotNo (..), mkSerialised)
import           Ouroboros.Network.Point (WithOrigin (..), withOrigin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.MetaData as SL
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
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Gen.Preset
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Gen
import qualified Test.Shelley.Spec.Ledger.NonTraceProperties.Generator as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.History (LedgerViewHistory)
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck hiding (Result)
import           Test.QuickCheck.Hedgehog (hedgehog)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation (SomeResult (..), WithVersion (..))

import           Test.Consensus.Shelley.Examples (mkDummyHash)
import           Test.Consensus.Shelley.MockCrypto

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valied
-------------------------------------------------------------------------------}

genHash :: forall a c. Crypto c => Proxy c -> Gen (SL.Hash c a)
genHash proxy = mkDummyHash proxy <$> arbitrary

instance Arbitrary Block where
  arbitrary = do
    let Gen.KeySpace_ { Gen.ksCoreNodes } = Gen.geKeySpace Gen.Preset.genEnv
    prevHash         <- unShelleyHash <$> arbitrary
    allPoolKeys      <- elements (map snd ksCoreNodes)
    txs              <- arbitrary
    curSlotNo        <- SlotNo  <$> choose (0, 10)
    curBlockNo       <- BlockNo <$> choose (0, 100)
    epochNonce       <- arbitrary
    blockNonce       <- Gen.NatNonce . fromIntegral <$> choose (1, 100 :: Int)
    praosLeaderValue <- arbitrary
    let kesPeriod       = 1
        keyRegKesPeriod = 1
        ocert           = Gen.mkOCert allPoolKeys 1 (SL.KESPeriod kesPeriod)
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

instance Arbitrary (SomeBlock Query Block) where
  arbitrary = oneof
    [ pure $ SomeBlock GetLedgerTip
    , pure $ SomeBlock GetEpochNo
    , SomeBlock . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ SomeBlock GetCurrentPParams
    , pure $ SomeBlock GetProposedPParamsUpdates
    , pure $ SomeBlock GetStakeDistribution
    , pure $ SomeBlock GetCurrentLedgerState
    , (\(SomeBlock q) -> SomeBlock (GetCBOR q)) <$> arbitrary
    , SomeBlock . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
    ]

instance Arbitrary (SomeResult Block) where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> arbitrary
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    , SomeResult GetCurrentLedgerState <$> arbitrary
    , (\(SomeResult q r) ->
        SomeResult (GetCBOR q) (mkSerialised (encodeShelleyResult q) r)) <$>
      arbitrary
    , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
    ]

instance Arbitrary (NonMyopicMemberRewards TPraosMockCrypto) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance Arbitrary (Point Block) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance Arbitrary (TPraosState TPraosMockCrypto) where
  arbitrary = do
      steps     <- choose (0, 5)
      startSlot <- frequency
        [ (1, return Origin)
        , (5, At . SlotNo <$> choose (0, 100))
        ]
      initState <- TPraosState.empty startSlot <$> arbitrary
      go steps startSlot initState
    where
      go :: Int
         -> WithOrigin SlotNo
         -> TPraosState TPraosMockCrypto
         -> Gen (TPraosState TPraosMockCrypto)
      go steps prevSlot st
        | 0 <- steps = return st
        | otherwise  = do
          let slot = withOrigin (SlotNo 0) succ prevSlot
          newPrtclState <- arbitrary
          go (steps - 1) (At slot) (TPraosState.append slot newPrtclState st)

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

instance Arbitrary (AnnTip Block) where
  arbitrary = AnnTip
    <$> arbitrary
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary ShelleyNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ShelleyNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

-- Convenient to have
instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (0, 1000)

instance Arbitrary (SomeBlock (NestedCtxt f) Block) where
  arbitrary = return (SomeBlock indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for cardano-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary SL.MIRPot where
  arbitrary = oneof
    [ pure SL.ReservesMIR
    , pure SL.TreasuryMIR
    ]

instance Crypto c => Arbitrary (SL.StakeCreds c) where
  arbitrary = SL.StakeCreds <$> arbitrary

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

instance (HashAlgorithm h)
  => Arbitrary (Hash h a) where
  arbitrary = coerce $ SL.hash @h <$> arbitrary @Int

instance Crypto c => Arbitrary (SL.KeyHash a c) where
  arbitrary = SL.KeyHash <$> genHash (Proxy @c)

instance Crypto c => Arbitrary (STS.PrtclState c) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Crypto c => Arbitrary (SL.BlocksMade c) where
  arbitrary = SL.BlocksMade <$> arbitrary

instance Crypto c => Arbitrary (SL.Credential r c) where
  arbitrary = oneof
    [ SL.ScriptHashObj . SL.ScriptHash <$> genHash (Proxy @c)
    , SL.KeyHashObj <$> arbitrary
    ]

instance Crypto c => Arbitrary (SL.RewardAcnt c) where
  arbitrary = SL.RewardAcnt <$> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (SL.RewardUpdate c) where
  arbitrary = return SL.emptyRewardUpdate

instance Crypto c => Arbitrary (SL.PoolDistr c) where
  arbitrary = SL.PoolDistr . Map.fromList <$>
      listOf ((,) <$> arbitrary <*> genVal)
    where
      genVal = (,) <$> arbitrary <*> genHash (Proxy @c)

instance Arbitrary a => Arbitrary (SL.StrictMaybe a) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Crypto c => Arbitrary (SL.OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

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
  arbitrary = return . fromJust $ SL.textToUrl "text"

instance Arbitrary a => Arbitrary (StrictSeq a) where
  arbitrary = StrictSeq.toStrict <$> arbitrary
  shrink = map StrictSeq.toStrict . shrink . StrictSeq.getSeq

instance Arbitrary SL.PoolMetaData where
  arbitrary = (`SL.PoolMetaData` "bytestring") <$> arbitrary

instance Crypto c => Arbitrary (SL.MetaDataHash c) where
  arbitrary = SL.MetaDataHash <$> genHash (Proxy @c)

instance Crypto c => Arbitrary (SL.ScriptHash c) where
  arbitrary = SL.ScriptHash <$> genHash (Proxy @c)

instance Arbitrary SL.Port where
  arbitrary = fromIntegral @Word8 @SL.Port <$> arbitrary

instance Arbitrary IPv4 where
  arbitrary = pure $ toIPv4 [192, 0, 2, 1]

instance Arbitrary IPv6 where
  arbitrary = pure $ toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]

instance Arbitrary SL.DnsName where
  arbitrary = pure . fromJust $ SL.textToDns "foo.example.com"

instance Arbitrary SL.StakePoolRelay where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

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
  arbitrary = Gen.genPParams (Gen.geConstants Gen.Preset.genEnv)

instance Crypto c => Arbitrary (SL.GenDelegs c) where
  arbitrary = SL.GenDelegs <$> arbitrary

instance Crypto c => Arbitrary (SL.LedgerView c) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.Ptr where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Crypto c => Arbitrary (SL.StakeReference c) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.Network where
  arbitrary = elements [SL.Mainnet, SL.Testnet]

instance Crypto c => Arbitrary (SL.Addr c) where
  arbitrary = oneof
    [ SL.Addr <$> arbitrary <*> arbitrary <*> arbitrary
    -- TODO generate Byron addresses too
    -- SL.AddrBootstrap
    ]

instance Arbitrary (SL.Tx TPraosMockCrypto) where
  arbitrary = do
    (_ledgerState, _steps, _txfee, tx, _lv) <- hedgehog SL.genStateTx
    return tx

instance Arbitrary (SL.TxIn TPraosMockCrypto) where
  arbitrary = SL.TxIn
    <$> (SL.TxId <$> genHash (Proxy @TPraosMockCrypto))
    <*> arbitrary

instance Arbitrary (SL.TxOut TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.ProtVer where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary SL.ActiveSlotCoeff where
  arbitrary = SL.mkActiveSlotCoeff <$> arbitrary

instance Arbitrary (SL.PParams' SL.StrictMaybe) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.VKey kd TPraosMockCrypto) where
  arbitrary = SL.VKey . VerKeyMockDSIGN <$> arbitrary

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
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | We only have single version, so no special casing required.
--
-- This blanket orphan instance will have to be replaced with more specific
-- ones, once we introduce a different Shelley version.
instance Arbitrary a => Arbitrary (WithVersion ShelleyNodeToNodeVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | We only have single version, so no special casing required.
--
-- This blanket orphan instance will have to be replaced with more specific
-- ones, once we introduce a different Shelley version.
instance Arbitrary a => Arbitrary (WithVersion ShelleyNodeToClientVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Generators (
    epochSlots
  , protocolMagicId
  , RegularBlock (..)
  ) where

import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Chain.Block (ABlockOrBoundary (..),
                     ABlockOrBoundaryHdr (..))
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Byron.API as API
import           Cardano.Chain.Common (KeyHash)
import           Cardano.Chain.Slotting (EpochNumber, EpochSlots (..),
                     SlotNumber)
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Byron.Ledger

import           Test.QuickCheck hiding (Result)
import           Test.QuickCheck.Hedgehog (hedgehog)

import qualified Test.Cardano.Chain.Block.Gen as CC
import qualified Test.Cardano.Chain.Common.Gen as CC
import qualified Test.Cardano.Chain.Delegation.Gen as CC
import qualified Test.Cardano.Chain.MempoolPayload.Gen as CC
import qualified Test.Cardano.Chain.Slotting.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as UG
import qualified Test.Cardano.Chain.UTxO.Gen as CC
import qualified Test.Cardano.Crypto.Gen as CC

import           Test.Util.Orphans.Arbitrary ()

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Eq (Some (Query ByronBlock)) where
  Some GetUpdateInterfaceState == Some GetUpdateInterfaceState = True

deriving instance Show (Some (Query ByronBlock))

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

epochSlots :: EpochSlots
epochSlots = EpochSlots 21600

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

-- | A 'ByronBlock' that is never an EBB.
newtype RegularBlock = RegularBlock { unRegularBlock :: ByronBlock }
  deriving (Eq, Show)

instance Arbitrary RegularBlock where
  arbitrary =
    RegularBlock .annotateByronBlock epochSlots <$>
    hedgehog (CC.genBlock protocolMagicId epochSlots)

instance Arbitrary ByronBlock where
  arbitrary = frequency
      [ (3, genBlock)
      , (1, genBoundaryBlock)
      ]
    where
      genBlock :: Gen ByronBlock
      genBlock = unRegularBlock <$> arbitrary
      genBoundaryBlock :: Gen ByronBlock
      genBoundaryBlock =
        mkByronBlock epochSlots . ABOBBoundary . API.reAnnotateBoundary protocolMagicId <$>
        hedgehog (CC.genBoundaryBlock)

instance Arbitrary (Header ByronBlock) where
  arbitrary = frequency
      [ (3, genHeader)
      , (1, genBoundaryHeader)
      ]
    where
      genHeader :: Gen (Header ByronBlock)
      genHeader = do
        blockSize <- arbitrary
        flip (mkByronHeader epochSlots) blockSize . ABOBBlockHdr .
          API.reAnnotateUsing
            (CC.Block.toCBORHeader epochSlots)
            (CC.Block.fromCBORAHeader epochSlots) <$>
          hedgehog (CC.genHeader protocolMagicId epochSlots)

      genBoundaryHeader :: Gen (Header ByronBlock)
      genBoundaryHeader = do
        blockSize <- arbitrary
        flip (mkByronHeader epochSlots) blockSize . ABOBBoundaryHdr .
          API.reAnnotateUsing
            (CC.Block.toCBORABoundaryHeader protocolMagicId)
            CC.Block.fromCBORABoundaryHeader <$>
          hedgehog CC.genBoundaryHeader

instance Arbitrary ByronHash where
  arbitrary = ByronHash <$> hedgehog CC.genHeaderHash

instance Arbitrary KeyHash where
  arbitrary = hedgehog CC.genKeyHash

instance Arbitrary (GenTx ByronBlock) where
  arbitrary =
    fromMempoolPayload . API.reAnnotateUsing toCBOR fromCBOR <$>
    hedgehog (CC.genMempoolPayload protocolMagicId)

instance Arbitrary (GenTxId ByronBlock) where
  arbitrary = oneof
      [ ByronTxId             <$> hedgehog CC.genTxId
      , ByronDlgId            <$> hedgehog genCertificateId
      , ByronUpdateProposalId <$> hedgehog (UG.genUpId protocolMagicId)
      , ByronUpdateVoteId     <$> hedgehog genUpdateVoteId
      ]
    where
      genCertificateId = CC.genAbstractHash (CC.genCertificate protocolMagicId)
      genUpdateVoteId  = CC.genAbstractHash (UG.genVote protocolMagicId)

instance Arbitrary API.ApplyMempoolPayloadErr where
  arbitrary = oneof
    [ API.MempoolTxErr  <$> hedgehog CC.genUTxOValidationError
    , API.MempoolDlgErr <$> hedgehog CC.genError
    -- TODO there is no generator for
    -- Cardano.Chain.Update.Validation.Interface.Error and we can't write one
    -- either because the different Error types it wraps are not exported.
    -- , MempoolUpdateProposalErr <$> arbitrary
    -- , MempoolUpdateVoteErr     <$> arbitrary
    ]

instance Arbitrary (Some (Query ByronBlock)) where
  arbitrary = pure $ Some GetUpdateInterfaceState

instance Arbitrary EpochNumber where
  arbitrary = hedgehog CC.genEpochNumber

instance Arbitrary SlotNumber where
  arbitrary = hedgehog CC.genSlotNumber

instance Arbitrary CC.Update.UpId where
  arbitrary = hedgehog (UG.genUpId protocolMagicId)

instance Arbitrary CC.Update.ApplicationName where
  arbitrary = hedgehog UG.genApplicationName

instance Arbitrary CC.Update.SystemTag where
  arbitrary = hedgehog UG.genSystemTag

instance Arbitrary CC.Update.InstallerHash where
  arbitrary = hedgehog UG.genInstallerHash

instance Arbitrary CC.Update.ProtocolVersion where
  arbitrary = hedgehog UG.genProtocolVersion

instance Arbitrary CC.Update.ProtocolParameters where
  arbitrary = hedgehog UG.genProtocolParameters

instance Arbitrary CC.Update.SoftwareVersion where
  arbitrary = hedgehog UG.genSoftwareVersion

instance Arbitrary CC.UPI.State where
  arbitrary = CC.UPI.State
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure mempty -- TODO CandidateProtocolUpdate's constructor is not exported
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure mempty -- TODO Endorsement is not exported
    <*> arbitrary

instance Arbitrary (SerialisationVersion ByronBlock) where
  arbitrary =
      elements $ concat [
          [SerialisedToDisk]
        , map (SerialisedAcrossNetwork . SerialisedNodeToNode)   nodeToNode
        , map (SerialisedAcrossNetwork . SerialisedNodeToClient) nodeToClient
        ]
    where
      -- We enumerate /all/ nodeToNode here, not just the ones returned by
      -- 'supportedNetworkProtocolVersions', which may be fewer.
      -- We might want to reconsider that later.

      nodeToNode :: [ByronNodeToNodeVersion]
      nodeToNode = [minBound .. maxBound]

      nodeToClient :: [ByronNodeToClientVersion]
      nodeToClient = [minBound .. maxBound]

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Ledger.Byron (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Reflection (give)

import           Cardano.Binary (ByteSpan, fromCBOR, slice, toCBOR)
import           Cardano.Chain.Block (ABlockOrBoundary (..))
import qualified Cardano.Chain.Block as CC.Block
import           Cardano.Chain.Common (KeyHash)
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (BlockProtocol, Header)
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.Protocol.Abstract (ChainState)
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.PBFT.ChainState
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Block.Gen as CC
import qualified Test.Cardano.Chain.Common.Gen as CC
import qualified Test.Cardano.Chain.Delegation.Gen as CC
import qualified Test.Cardano.Chain.MempoolPayload.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as CC
import qualified Test.Cardano.Chain.UTxO.Gen as CC
import qualified Test.Cardano.Crypto.Gen as CC

import           Test.Util.Orphans.Arbitrary ()


tests :: TestTree
tests = give protocolMagicId $ testGroup "Byron"
  [ testGroup "Serialisation roundtrips"
      [ testProperty "roundtrip Block"       prop_roundtrip_Block
      , testProperty "roundtrip Header"      prop_roundtrip_Header
      , testProperty "roundtrip HeaderHash"  prop_roundtrip_HeaderHash
      , testProperty "roundtrip ChainState"  prop_roundtrip_ChainState
      , testProperty "roundtrip GenTx"       prop_roundtrip_GenTx
      , testProperty "roundtrip GenTxId"     prop_roundtrip_GenTxId
      , testProperty "roundtrip ApplyTxErr"  prop_roundtrip_ApplyTxErr
      ]
      -- TODO LedgerState
  ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

roundtrip :: (Eq a, Show a)
          => (a -> Encoding)
          -> (forall s. Decoder s a)
          -> a
          -> Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

-- | Roundtrip property for values annotated with their serialized form
--
-- NOTE: Suppose @a@ consists of a pair of the unannotated value @a'@ and some
-- 'Lazy.ByteString'. The roundtrip property will fail if that
-- 'Lazy.ByteString' encoding is not equal to @enc a'@. One way in which this
-- might happen is if the annotation is not canonical CBOR, but @enc@ does
-- produce canonical CBOR.
roundtrip' :: (Eq a, Show a)
           => (a -> Encoding)  -- ^ @enc@
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtrip' enc dec a = case deserialiseFromBytes dec bs of
    Right (bs', a')
      | Lazy.null bs'
      -> a === a' bs
      | otherwise
      -> counterexample ("left-over bytes: " <> show bs') False
    Left e
      -> counterexample (show e) False
  where
    bs = toLazyByteString (enc a)

annotate :: forall f. Functor f
         => (f () -> Encoding)
         -> (forall s. Decoder s (f ByteSpan))
         -> f ()
         -> f Strict.ByteString
annotate encode decoder =
      (\bs -> splice bs (deserialiseFromBytes decoder bs))
    . toLazyByteString
    . encode
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, f ByteSpan)
           -> f Strict.ByteString
    splice _ (Left _err) =
        error "annotate: serialization roundtrip failure"
    splice bs (Right (bs', x))
      | Lazy.null bs'
      = Lazy.toStrict . slice bs <$> x
      | otherwise
      = error ("left-over bytes: " <> show bs')

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: Block -> Property
prop_roundtrip_Block b =
    roundtrip' encodeByronBlock (decodeByronBlock epochSlots) b

prop_roundtrip_Header :: Header Block -> Property
prop_roundtrip_Header h =
    roundtrip' encodeByronHeader (decodeByronHeader epochSlots) h

prop_roundtrip_HeaderHash :: HeaderHash Block -> Property
prop_roundtrip_HeaderHash =
    roundtrip encodeByronHeaderHash decodeByronHeaderHash

prop_roundtrip_ChainState :: ChainState (BlockProtocol Block) -> Property
prop_roundtrip_ChainState = give protocolMagicId $
    roundtrip encodeByronChainState decodeByronChainState

prop_roundtrip_GenTx :: GenTx Block -> Property
prop_roundtrip_GenTx =
    roundtrip encodeByronGenTx decodeByronGenTx

prop_roundtrip_GenTxId :: GenTxId Block -> Property
prop_roundtrip_GenTxId =
    roundtrip encodeByronGenTxId decodeByronGenTxId

prop_roundtrip_ApplyTxErr :: ApplyTxErr Block -> Property
prop_roundtrip_ApplyTxErr =
    roundtrip encodeByronApplyTxError decodeByronApplyTxError

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

type Block = ByronBlockOrEBB

epochSlots :: EpochSlots
epochSlots = EpochSlots 2160

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

instance Arbitrary Block where
  arbitrary = frequency
      [ (3, genBlock)
      , (1, genBoundaryBlock)
      ]
    where
      genBlock :: Gen Block
      genBlock =
        annotateByronBlock epochSlots <$>
        hedgehog (CC.genBlock protocolMagicId epochSlots)
      genBoundaryBlock :: Gen Block
      genBoundaryBlock =
        mkByronBlockOrEBB epochSlots . ABOBBoundary . annotateBoundary protocolMagicId <$>
        hedgehog (CC.genBoundaryBlock)


instance Arbitrary (Header Block) where
  arbitrary = frequency
      [ (3, genHeader)
      , (1, genBoundaryHeader)
      ]
    where
      genHeader :: Gen (Header Block)
      genHeader =
        mkByronHeaderOrEBB epochSlots . Right .
        annotate
          (CC.Block.toCBORHeader epochSlots)
          (CC.Block.fromCBORAHeader epochSlots) <$>
        hedgehog (CC.genHeader protocolMagicId epochSlots)
      genBoundaryHeader :: Gen (Header Block)
      genBoundaryHeader =
        mkByronHeaderOrEBB epochSlots . Left .
        annotate
          (CC.Block.toCBORABoundaryHeader protocolMagicId)
          CC.Block.fromCBORABoundaryHeader <$>
        hedgehog CC.genBoundaryHeader

instance Arbitrary ByronHash where
  arbitrary = ByronHash <$> hedgehog CC.genHeaderHash

instance Arbitrary KeyHash where
  arbitrary = hedgehog CC.genKeyHash

instance Arbitrary (PBftChainState PBftCardanoCrypto) where
  arbitrary = give protocolMagicId $
    fromMap <$> oneof [return Origin, At <$> arbitrary] <*> arbitrary

instance Arbitrary (ChainStateWithEBBs (PBft ByronConfig PBftCardanoCrypto)) where
  arbitrary = mkChainStateWithEBBs <$> arbitrary <*> arbitrary

instance Arbitrary (GenTx Block) where
  arbitrary =
    mkByronGenTx . annotate toCBOR fromCBOR <$>
    hedgehog (CC.genMempoolPayload protocolMagicId)

instance Arbitrary (GenTxId Block) where
  arbitrary = oneof
      [ ByronTxId             <$> hedgehog CC.genTxId
      , ByronDlgId            <$> hedgehog genCertificateId
      , ByronUpdateProposalId <$> hedgehog (CC.genUpId protocolMagicId)
      , ByronUpdateVoteId     <$> hedgehog genUpdateVoteId
      ]
    where
      genCertificateId = CC.genAbstractHash (CC.genCertificate protocolMagicId)
      genUpdateVoteId  = CC.genAbstractHash (CC.genVote protocolMagicId)

instance Arbitrary ByronApplyTxError where
  arbitrary = oneof
    [ ByronApplyTxError             <$> hedgehog CC.genUTxOValidationError
    , ByronApplyDlgError            <$> hedgehog CC.genError
    -- TODO there is no generator for
    -- Cardano.Chain.Update.Validation.Interface.Error and we can't write one
    -- either because the different Error types it wraps are not exported.
    -- , ByronApplyUpdateProposalError <$> arbitrary
    -- , ByronApplyUpdateVoteError     <$> arbitrary
    ]

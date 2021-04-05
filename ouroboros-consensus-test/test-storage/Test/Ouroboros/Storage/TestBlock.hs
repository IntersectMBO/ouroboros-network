{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.TestBlock (
    -- * Test block
    BlockConfig (..)
  , ChainLength (..)
  , CodecConfig (..)
  , EBB (..)
  , Header (..)
  , StorageConfig (..)
  , TestBlock (..)
  , TestBody (..)
  , TestBodyHash (..)
  , TestHeader (..)
  , TestHeaderHash (..)
    -- ** Construction
  , firstBlock
  , firstEBB
  , mkBlock
  , mkNextBlock
  , mkNextBlock'
  , mkNextEBB
  , mkNextEBB'
    -- ** Query
  , testBlockChainLength
  , testBlockIsEBB
  , testBlockIsValid
    -- ** Serialisation
  , testBlockFromLazyByteString
  , testBlockToBuilder
  , testBlockToLazyByteString
    -- * Ledger
  , TestBlockError (..)
  , TestBlockOtherHeaderEnvelopeError (..)
  , mkTestConfig
  , testInitExtLedger
    -- * Corruptions
  , Corruptions
  , FileCorruption (..)
  , corruptFile
  , corruptionFiles
  , generateCorruptions
  , shrinkCorruptions
  ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise (decode, encode), serialise)
import           Control.Monad (forM, when)
import           Control.Monad.Except (throwError)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor (($>))
import           Data.Hashable
import           Data.Int (Int64)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Typeable (Typeable)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)
import           Test.QuickCheck

import           Control.Monad.Class.MonadThrow

import           Cardano.Crypto.DSIGN

import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ModChainSel
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Storage.FS.API (HasFS (..), hGetExactly,
                     hPutAll, hSeek, withFile)
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.Serialisation

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.SignableRepresentation ()

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

data TestBlock = TestBlock {
      testHeader :: !TestHeader
    , testBody   :: !TestBody
    }
  deriving stock    (Show, Eq, Generic, Typeable)
  deriving anyclass (NoThunks, Serialise)

-- | Hash of a 'TestHeader'
newtype TestHeaderHash = TestHeaderHash Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (Condense, NoThunks, Hashable, Serialise, Binary)

-- | Hash of a 'TestBody'
newtype TestBodyHash = TestBodyHash Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (Condense, NoThunks, Hashable, Serialise)

data TestHeader = TestHeader {
      thHash        :: HeaderHash TestHeader
      -- ^ Not included in the calculation of the hash of the 'TestHeader',
      -- i.e., in its own value, which would be pretty hard to do.
      --
      -- Note the absence of a bang: this field caches the 'TestHeader's hash.
      -- To calculate it, the 'TestHeader' is passed to the hashing function,
      -- even though the field is not read, making the field strict would
      -- create an infinite loop.
    , thPrevHash    :: !(ChainHash TestHeader)
    , thBodyHash    :: !TestBodyHash
    , thSlotNo      :: !SlotNo
    , thBlockNo     :: !BlockNo
    , thChainLength :: !ChainLength
    , thIsEBB       :: !EBB
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

-- | Strict variant of @Maybe EpochNo@
data EBB =
    EBB !EpochNo
  | RegularBlock
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

instance Hashable EBB where
  hashWithSalt s (EBB epoch)  = hashWithSalt s (unEpochNo epoch)
  hashWithSalt s RegularBlock = hashWithSalt s (-1 :: Int)

data TestBody = TestBody {
      tbForkNo  :: !Word
      -- ^ If we don't have something that can vary per block, we're not
      -- generating forks, except when skipping slots. For example, when we
      -- want to have multiple different valid successor blocks created in the
      -- same slot, all fields in the header and body will be the same.
      -- Consequently, the hashes will also be the same, so we don't have
      -- different blocks after all. By using a different 'tbForkNo' for each
      -- block, we have different bodies, and thus different hashes.
      --
      -- Note that this is a /local/ number, it is specific to this block,
      -- other blocks need not be aware of it.
    , tbIsValid :: !Bool
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise, Hashable)

newtype instance Header TestBlock = TestHeader' { unTestHeader :: TestHeader }
  deriving newtype (Eq, Show, NoThunks, Serialise)

instance GetHeader TestBlock where
  getHeader = TestHeader' . testHeader

  blockMatchesHeader (TestHeader' hdr) blk =
      thBodyHash hdr == hashBody (testBody blk)

  headerIsEBB (TestHeader' hdr) = case thIsEBB hdr of
    EBB epochNo  -> Just epochNo
    RegularBlock -> Nothing

instance StandardHash TestBlock
instance StandardHash TestHeader

type instance HeaderHash TestBlock = TestHeaderHash
type instance HeaderHash TestHeader = TestHeaderHash

instance ConvertRawHash TestBlock where
  toRawHash   _ = Lazy.toStrict . Binary.encode
  fromRawHash _ = Binary.decode . Lazy.fromStrict
  hashSize    _ = 8

instance HasHeader TestBlock where
  getHeaderFields = getBlockHeaderFields

instance HasHeader (Header TestBlock) where
  getHeaderFields (TestHeader' TestHeader{..}) = HeaderFields {
        headerFieldHash    = thHash
      , headerFieldSlot    = thSlotNo
      , headerFieldBlockNo = thBlockNo
      }

instance GetPrevHash TestBlock where
  headerPrevHash = castHash . thPrevHash . unTestHeader

data instance BlockConfig TestBlock = TestBlockConfig {
      -- | Whether the test block can be EBBs or not. This can vary per test
      -- case. It will be used by 'validateEnvelope' to forbid EBBs 'False'.
      testBlockEBBsAllowed  :: !Bool

      -- | Number of core nodes
      --
      -- We need this in order to compute the 'ValidateView', which must
      -- conjure up a validation key out of thin air
    , testBlockNumCoreNodes :: !NumCoreNodes
    }
  deriving (Generic, NoThunks)

data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Generic, NoThunks, Show)

data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Generic, NoThunks, Show)

instance Condense TestBlock where
  condense = show -- TODO

instance Condense TestHeader where
  condense = show -- TODO

hashBody :: TestBody -> TestBodyHash
hashBody = TestBodyHash . hash

hashHeader :: TestHeader -> TestHeaderHash
hashHeader (TestHeader _ a b c d e f) = TestHeaderHash (hash (a, b, c, d, e, f))

testBlockIsEBB :: TestBlock -> IsEBB
testBlockIsEBB = headerToIsEBB . getHeader

testBlockChainLength :: TestBlock -> ChainLength
testBlockChainLength = thChainLength . unTestHeader . getHeader

-- | Check whether the header matches its hash and whether the body matches
-- its hash.
testBlockIsValid :: TestBlock -> Bool
testBlockIsValid (TestBlock hdr body) =
  thHash     hdr == hashHeader hdr &&
  thBodyHash hdr == hashBody   body

testBlockToBuilder :: TestBlock -> Builder
testBlockToBuilder = CBOR.toBuilder . encode

testBlockHeaderOffset :: Word16
testBlockHeaderOffset = 2 -- For the 'encodeListLen'

testBlockHeaderSize :: TestBlock -> Word16
testBlockHeaderSize = fromIntegral . Lazy.length . serialise . testHeader

testBlockToLazyByteString :: TestBlock -> Lazy.ByteString
testBlockToLazyByteString = CBOR.toLazyByteString . encode

testBlockFromLazyByteString :: HasCallStack => Lazy.ByteString -> TestBlock
testBlockFromLazyByteString bs = case CBOR.deserialiseFromBytes decode bs of
    Left e -> error $ show e
    Right (bs', a)
      | Lazy.null bs'
      -> a
      | otherwise
      -> error $ "left-over bytes: " <> show bs'

{-------------------------------------------------------------------------------
  Real chain length
-------------------------------------------------------------------------------}

-- | In chain selection, we use 'BlockNo' as a proxy for the block length.
-- This is entirely correct, except for those dreadful EBBs, which share their
-- block number with their predecessor. So it is possible that two chains with
-- the same 'BlockNo' at the tip have a different length because the longer
-- chain contains more EBBs than the shorter.
--
-- For example:
--
-- > .. :> EBB (100, slotNo 10, blockNo 1) :> (400, slotNo 10, blockNo 2)
-- > .. :> (999, slotNo 10, blockNo 2)
--
-- The chain selection for this 'TestBlock' looks at the hashes in case of a
-- 'BlockNo' tie (after prefering the chain ending with an EBB) and will pick
-- the block with the highest hash. This is to have a more deterministic chain
-- selection (less implementation specific) which will keep the model better
-- in sync with the implementation.
--
-- In the example above, that would mean picking the second chain, /even
-- though it is shorter/! The implementation does not support switching to a
-- shorter chain.
--
-- Note that this is not a problem for Byron, because we don't look at the
-- hashes or anything else in case of a tie (we just prefer a chain ending
-- with an EBB, which /must/ be longer).
--
-- Note that is not a problem for Shelley either, where we do look at the
-- certificate number and VRF hash in case of a tie, because there are no EBBs.
--
-- This is only an issue when:
-- * There can be EBBs in the chain
-- * In case of equal 'blockNo's, we still prefer one over the other because
--   of some additional condition.
--
-- Which is the case for this TestBlock.
--
-- To solve this, we store the /real/ chain length inside the block. The only
-- difference with the 'BlockNo' is that 'ChainLength' takes EBBs into account.
--
-- When there is 'BlockNo' tie as in the example above and we would look at
-- the hashes, we will first look at the 'ChainLength' (and prefer the longest
-- one). Only if that is equal do we actually look at the hashes. This
-- guarantees that we never prefer a chain that is shorter.
--
-- NOTE: we start counting from 1 (unlike 'BlockNo', which starts from 0),
-- because it corresponds to the /length/.
newtype ChainLength = ChainLength Int
  deriving stock   (Show, Generic)
  deriving newtype (Eq, Ord, Enum, NoThunks, Serialise, Hashable)

{-------------------------------------------------------------------------------
  Creating blocks
-------------------------------------------------------------------------------}

mkBlock
  :: HasCallStack
  => (SlotNo -> Bool)
  -- ^ Is this slot allowed contain an EBB?
  --
  -- This argument is used primarily to detect the generation of invalid blocks
  -- with different kind of 'ChunkInfo'.
  -> TestBody
  -> ChainHash TestHeader
  -- ^ Hash of previous header
  -> SlotNo
  -> BlockNo
  -> ChainLength
  -> Maybe EpochNo
  -> TestBlock
mkBlock canContainEBB testBody thPrevHash thSlotNo thBlockNo thChainLength ebb =
    case (canContainEBB thSlotNo, ebb) of
      (False, Just _) ->
        error "mkBlock: EBB in invalid slot"
      _otherwise ->
        TestBlock { testHeader, testBody }
  where
    testHeader = TestHeader {
        thHash     = hashHeader testHeader
      , thPrevHash
      , thBodyHash = hashBody testBody
      , thSlotNo
      , thBlockNo
      , thChainLength
      , thIsEBB = case ebb of
          Just epoch -> EBB epoch
          Nothing    -> RegularBlock
      }

-- | Note the first block need not be an EBB, see 'firstEBB'.
firstBlock :: SlotNo -> TestBody -> TestBlock
firstBlock slotNo testBody =
    mkBlock
      (const False)
      testBody
      GenesisHash
      slotNo
      0
      (ChainLength 1)
      Nothing

mkNextBlock' ::
     (HeaderFields TestBlock, ChainLength)
     -- ^ Information about the previous block
  -> SlotNo
  -> TestBody
  -> TestBlock
mkNextBlock' (prevHeaderFields, prevChainLength) slotNo testBody =
    mkBlock
      (const False)
      testBody
      (BlockHash (headerFieldHash prevHeaderFields))
      slotNo
      (succ (headerFieldBlockNo prevHeaderFields))
      (succ prevChainLength)
      Nothing

firstEBB :: (SlotNo -> Bool)
         -> TestBody
         -> TestBlock
firstEBB canContainEBB testBody =
    mkBlock canContainEBB testBody GenesisHash 0 0 (ChainLength 1) (Just 0)

-- | Note that in various places, e.g., the ImmutableDB, we rely on the fact
-- that the @slotNo@ should correspond to the first slot number of the epoch,
-- as is the case for real EBBs.
mkNextEBB' ::
     (SlotNo -> Bool)
  -> (HeaderFields TestBlock, ChainLength)
     -- ^ Information about the previous block
  -> SlotNo
  -> EpochNo
  -> TestBody
  -> TestBlock
mkNextEBB' canContainEBB (prevHeaderFields, prevChainLength) slotNo epochNo testBody =
    mkBlock
      canContainEBB
      testBody
      (BlockHash (headerFieldHash prevHeaderFields))
      slotNo
      (headerFieldBlockNo prevHeaderFields)
      (succ prevChainLength)
      (Just epochNo)

-- | Variant of 'mkNextBlock' that takes the entire previous block.
mkNextBlock ::
     TestBlock
     -- ^ Previous block
  -> SlotNo
  -> TestBody
  -> TestBlock
mkNextBlock tb =
    mkNextBlock' (getBlockHeaderFields tb, testBlockChainLength tb)

-- | Variant of 'mkNextEBB' that takes the entire previous block.
mkNextEBB ::
     (SlotNo -> Bool)
  -> TestBlock
     -- ^ Previous block
  -> SlotNo
  -> EpochNo
  -> TestBody
  -> TestBlock
mkNextEBB canContainEBB tb =
    mkNextEBB' canContainEBB (getBlockHeaderFields tb, testBlockChainLength tb)

{-------------------------------------------------------------------------------
  Test infrastructure: protocol
-------------------------------------------------------------------------------}

data BftWithEBBsSelectView = BftWithEBBsSelectView {
      bebbBlockNo     :: BlockNo
    , bebbIsEBB       :: IsEBB
    , bebbChainLength :: ChainLength
    , bebbHash        :: TestHeaderHash
    }
  deriving (Show, Eq)

instance Ord BftWithEBBsSelectView where
  compare (BftWithEBBsSelectView lBlockNo lIsEBB lChainLength lHash)
          (BftWithEBBsSelectView rBlockNo rIsEBB rChainLength rHash) =
      mconcat [
          -- Prefer the highest block number, as it is a proxy for chain length
          lBlockNo `compare` rBlockNo

          -- If the block numbers are the same, check if one of them is an EBB.
          -- An EBB has the same block number as the block before it, so the
          -- chain ending with an EBB is actually longer than the one ending
          -- with a regular block.
        , score lIsEBB `compare` score rIsEBB

          -- In case of a tie, look at the real chain length, so that we never
          -- prefer a shorter chain over a longer one, see 'ChainLength'.
        , lChainLength `compare` rChainLength

        -- In case of another tie, pick the largest hash, so that the model and
        -- the implementation will make the same choice, regardless
        -- implementation details (e.g., sort order).
        , lHash        `compare` rHash
        ]
   where
     score :: IsEBB -> Int
     score IsEBB    = 1
     score IsNotEBB = 0

type instance BlockProtocol TestBlock =
  ModChainSel (Bft BftMockCrypto) BftWithEBBsSelectView

{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance Signed (Header TestBlock) = ()
instance SignedHeader (Header TestBlock) where
  headerSigned _ = ()

instance BlockSupportsProtocol TestBlock where
  validateView TestBlockConfig{..} =
      bftValidateView bftFields
    where
      NumCoreNodes numCore = testBlockNumCoreNodes

      bftFields :: Header TestBlock -> BftFields BftMockCrypto ()
      bftFields hdr = BftFields {
            bftSignature = SignedDSIGN $ mockSign () (signKey (blockSlot hdr))
          }

      -- We don't want /our/ signing key, but rather the signing key of the
      -- node that produced the block
      signKey :: SlotNo -> SignKeyDSIGN MockDSIGN
      signKey (SlotNo n) = SignKeyMockDSIGN $ n `mod` numCore

  selectView _ hdr = BftWithEBBsSelectView {
        bebbBlockNo     = blockNo hdr
      , bebbIsEBB       = headerToIsEBB hdr
      , bebbChainLength = thChainLength (unTestHeader hdr)
      , bebbHash        = blockHash hdr
      }

data TestBlockError =
    -- | The hashes don't line up
    InvalidHash
      (ChainHash TestBlock)  -- ^ Expected hash
      (ChainHash TestBlock)  -- ^ Invalid hash

    -- | The block itself is invalid
  | InvalidBlock
  deriving (Eq, Show, Generic, NoThunks)

type instance LedgerCfg (LedgerState TestBlock) = HardFork.EraParams

instance GetTip (LedgerState TestBlock) where
  getTip = castPoint . lastAppliedPoint

instance GetTip (Ticked (LedgerState TestBlock)) where
  getTip = castPoint . getTip . getTickedTestLedger

instance IsLedger (LedgerState TestBlock) where
  type LedgerErr (LedgerState TestBlock) = TestBlockError

  applyChainTick _ _ = TickedTestLedger

instance ApplyBlock (LedgerState TestBlock) TestBlock where
  applyLedgerBlock _ tb@TestBlock{..} (TickedTestLedger TestLedger{..})
    | blockPrevHash tb /= lastAppliedHash
    = throwError $ InvalidHash lastAppliedHash (blockPrevHash tb)
    | not $ tbIsValid testBody
    = throwError $ InvalidBlock
    | otherwise
    = return     $ TestLedger (Chain.blockPoint tb) (BlockHash (blockHash tb))

  reapplyLedgerBlock _ tb _ =
    TestLedger (Chain.blockPoint tb) (BlockHash (blockHash tb))

data instance LedgerState TestBlock =
    TestLedger {
        -- The ledger state simply consists of the last applied block
        lastAppliedPoint :: !(Point TestBlock)
      , lastAppliedHash  :: !(ChainHash TestBlock)
      }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise, NoThunks)

-- Ticking has no effect on the test ledger state
newtype instance Ticked (LedgerState TestBlock) = TickedTestLedger {
      getTickedTestLedger :: LedgerState TestBlock
    }

instance UpdateLedger TestBlock

instance HasAnnTip TestBlock where
  type TipInfo TestBlock = TipInfoIsEBB TestBlock
  tipInfoHash _ (TipInfoIsEBB h _) = h
  getTipInfo b = TipInfoIsEBB (blockHash b) (headerToIsEBB b)

data TestBlockOtherHeaderEnvelopeError =
    UnexpectedEBBInSlot !SlotNo
  deriving (Eq, Show, Generic, NoThunks)

instance BasicEnvelopeValidation TestBlock where
  minimumPossibleSlotNo _ = SlotNo 0

  -- EBB shares its slot number with its successor
  minimumNextSlotNo _ (TipInfoIsEBB _ prevIsEBB) (TipInfoIsEBB _ curIsEBB) s =
      case (prevIsEBB, curIsEBB) of
        (IsEBB, IsNotEBB) -> s
        _otherwise        -> succ s

  -- The chain always starts with block number 0.
  expectedFirstBlockNo _ = BlockNo 0

  -- EBB shares its block number with its predecessor.
  expectedNextBlockNo _ (TipInfoIsEBB _ prevIsEBB) (TipInfoIsEBB _ curIsEBB) b =
      case (prevIsEBB, curIsEBB) of
        (IsNotEBB, IsEBB) -> b
        _otherwise        -> succ b

instance ValidateEnvelope TestBlock where
  type OtherHeaderEnvelopeError TestBlock = TestBlockOtherHeaderEnvelopeError

  additionalEnvelopeChecks cfg _ledgerView hdr =
      when (fromIsEBB newIsEBB && not (canBeEBB actualSlotNo)) $
        throwError $ UnexpectedEBBInSlot actualSlotNo
    where
      actualSlotNo :: SlotNo
      actualSlotNo = blockSlot hdr

      newIsEBB :: IsEBB
      newIsEBB = headerToIsEBB hdr

      canBeEBB :: SlotNo -> Bool
      canBeEBB (SlotNo s) = testBlockEBBsAllowed (configBlock cfg)
                         && s `mod` epochSlots == 0

      epochSlots :: Word64
      epochSlots =
          unEpochSize
        . HardFork.eraEpochSize
        . configLedger
        $ cfg

instance LedgerSupportsProtocol TestBlock where
  protocolLedgerView   _ _  = TickedTrivial
  ledgerViewForecastAt _    = trivialForecast

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[TestBlock]
  hardForkSummary = neverForksHardForkSummary id

instance InspectLedger TestBlock where
  -- Use defaults

testInitLedger :: LedgerState TestBlock
testInitLedger = TestLedger GenesisPoint GenesisHash

testInitExtLedger :: ExtLedgerState TestBlock
testInitExtLedger = ExtLedgerState {
      ledgerState = testInitLedger
    , headerState = genesisHeaderState ()
    }

-- Only for a single node
mkTestConfig :: SecurityParam -> ChunkSize -> TopLevelConfig TestBlock
mkTestConfig k ChunkSize { chunkCanContainEBB, numRegularBlocks } =
    TopLevelConfig {
        topLevelConfigProtocol = McsConsensusConfig $ BftConfig {
            bftParams  = BftParams {
                             bftSecurityParam = k
                           , bftNumNodes      = numCoreNodes
                           }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
          }
      , topLevelConfigLedger  = eraParams
      , topLevelConfigBlock   = TestBlockConfig {
            testBlockEBBsAllowed  = chunkCanContainEBB
          , testBlockNumCoreNodes = numCoreNodes
          }
      , topLevelConfigCodec   = TestBlockCodecConfig
      , topLevelConfigStorage = TestBlockStorageConfig
      }
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 1

    eraParams :: HardFork.EraParams
    eraParams = HardFork.EraParams {
        eraEpochSize  = EpochSize numRegularBlocks
      , eraSlotLength = slotLength
      , eraSafeZone   = HardFork.StandardSafeZone (maxRollbacks k * 2)
      }

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ TestBlock f a where
  CtxtTestBlock :: NestedCtxt_ TestBlock f (f TestBlock)

deriving instance Show (NestedCtxt_ TestBlock f a)

instance TrivialDependency (NestedCtxt_ TestBlock f) where
  type TrivialIndex (NestedCtxt_ TestBlock f) = f TestBlock
  hasSingleIndex CtxtTestBlock CtxtTestBlock = Refl
  indexIsTrivial = CtxtTestBlock

instance SameDepIndex (NestedCtxt_ TestBlock f)
instance HasNestedContent f TestBlock

{-------------------------------------------------------------------------------
  Test infrastructure: serialisation
-------------------------------------------------------------------------------}

instance HasBinaryBlockInfo TestBlock where
  getBinaryBlockInfo tb = BinaryBlockInfo
      { headerOffset = testBlockHeaderOffset
      , headerSize   = testBlockHeaderSize tb
      }

instance SerialiseDiskConstraints TestBlock

instance EncodeDisk TestBlock TestBlock
instance DecodeDisk TestBlock (Lazy.ByteString -> TestBlock) where
  decodeDisk _ = const <$> decode

instance EncodeDisk TestBlock (Header TestBlock)
instance DecodeDisk TestBlock (Lazy.ByteString -> Header TestBlock) where
  decodeDisk _ = const <$> decode

instance EncodeDisk TestBlock (LedgerState TestBlock)
instance DecodeDisk TestBlock (LedgerState TestBlock)

instance EncodeDisk TestBlock (AnnTip TestBlock) where
  encodeDisk _ = encodeAnnTipIsEBB encode

instance DecodeDisk TestBlock (AnnTip TestBlock) where
  decodeDisk _ = decodeAnnTipIsEBB decode

instance ReconstructNestedCtxt       Header  TestBlock
instance EncodeDiskDepIx (NestedCtxt Header) TestBlock
instance EncodeDiskDep   (NestedCtxt Header) TestBlock
instance DecodeDiskDepIx (NestedCtxt Header) TestBlock
instance DecodeDiskDep   (NestedCtxt Header) TestBlock

-- ChainDepState
instance EncodeDisk TestBlock ()
instance DecodeDisk TestBlock ()

{-------------------------------------------------------------------------------
  Corruption
-------------------------------------------------------------------------------}

data FileCorruption
  = DeleteFile
  | DropLastBytes Word64
    -- ^ Drop the last @n@ bytes of a file.
  | Corrupt Word64
    -- ^ Corrupt the file by adding 1 to the byte at the given location
    -- (modulo the file size).
  deriving (Show, Eq)

-- | Returns 'True' when something was actually corrupted. For example, when
-- drop the last bytes of an empty file, we don't actually corrupt it.
corruptFile :: MonadThrow m => HasFS m h -> FileCorruption -> FsPath -> m Bool
corruptFile hasFS@HasFS{..} fc file = case fc of
    DeleteFile              -> removeFile file $> True
    DropLastBytes n         -> withFile hasFS file (AppendMode AllowExisting) $ \hnd -> do
      fileSize <- hGetSize hnd
      let newFileSize = if n >= fileSize then 0 else fileSize - n
      hTruncate hnd newFileSize
      return $ fileSize /= newFileSize
    Corrupt n               -> withFile hasFS file (ReadWriteMode AllowExisting) $ \hnd -> do
      fileSize <- hGetSize hnd
      if fileSize == 0 then
        return False
      else do
        let offset :: Int64
            offset = fromIntegral $ n `mod` fileSize
        hSeek hnd AbsoluteSeek offset
        bs <- hGetExactly hasFS hnd 1
        hSeek hnd RelativeSeek (-1)
        _ <- hPutAll hasFS hnd (Lazy.map (+ 1) bs)
        return True


instance Arbitrary FileCorruption where
  arbitrary = frequency
    [ (1, return DeleteFile)
    , (1, DropLastBytes . getSmall . getPositive <$> arbitrary)
    , (1, Corrupt . getSmall . getPositive <$> arbitrary)
    ]
  shrink DeleteFile         = []
  shrink (DropLastBytes n)  =
    DropLastBytes . getSmall . getPositive <$> shrink (Positive (Small n))
  shrink (Corrupt n) =
    Corrupt . getSmall . getPositive <$> shrink (Positive (Small n))

-- | Multiple corruptions
type Corruptions = NonEmpty (FileCorruption, FsPath)

-- | The same file will not occur twice.
generateCorruptions :: NonEmpty FsPath -> Gen Corruptions
generateCorruptions allFiles = sized $ \n -> do
    subl  <- sublistOf (NE.toList allFiles) `suchThat` (not . null)
    k     <- choose (1, 1 `max` n)
    let files = NE.fromList $ take k subl
    forM files $ \file -> (, file) <$> arbitrary

shrinkCorruptions :: Corruptions -> [Corruptions]
shrinkCorruptions cs =
    [ cs''
    | cs'  <- shrinkList shrinkCor (NE.toList cs)
    , cs'' <- maybeToList $ NE.nonEmpty cs'
    ]
  where
    shrinkCor :: (FileCorruption, FsPath) -> [(FileCorruption, FsPath)]
    shrinkCor (c, f) = [(c', f) | c' <- shrink c]

-- | Return a list of all files that will be corrupted
corruptionFiles :: Corruptions -> [FsPath]
corruptionFiles = map snd . NE.toList

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving newtype instance Hashable SlotNo
deriving newtype instance Hashable BlockNo
instance Hashable IsEBB
  -- use generic instance

instance Hashable (HeaderHash b) => Hashable (ChainHash b)
  -- use generic instance

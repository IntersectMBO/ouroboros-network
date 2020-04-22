{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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
    TestBlock(..)
  , TestHeader(..)
  , TestHeaderHash(..)
  , TestBody(..)
  , TestBodyHash(..)
  , Header(..)
  , BlockConfig(..)
    -- ** Construction
  , mkBlock
  , firstBlock
  , firstEBB
  , mkNextBlock
  , mkNextEBB
    -- ** Query
  , testBlockToBlockInfo
  , testBlockIsValid
  , testBlockIsEBB
  , testHeaderEpochNoIfEBB
    -- ** Serialisation
  , testHashInfo
  , testBlockToBuilder
  , testBlockToBinaryInfo
  , testBlockFromBinaryInfo
  , testBlockFromLazyByteString
  , testBlockToLazyByteString
    -- * Ledger
  , TestBlockError(..)
  , testInitExtLedger
  , TestBlockOtherHeaderEnvelopeError(..)
  , mkTestConfig
    -- * Corruptions
  , Corruptions
  , FileCorruption(..)
  , corruptionFiles
  , generateCorruptions
  , shrinkCorruptions
  , corruptFile
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise (decode, encode), serialise)
import           Control.Arrow ((&&&))
import           Control.Monad (forM, when)
import           Control.Monad.Except (throwError)
import           Data.Binary (Binary (get, put))
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Functor (($>))
import           Data.Hashable
import           Data.Int (Int64)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Control.Monad.Class.MonadThrow

import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain (Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ModChainSel
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Storage.FS.API (HasFS (..), hGetExactly,
                     hPutAll, hSeek, withFile)
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Types (BinaryInfo (..),
                     HashInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB.Types (BlockInfo (..))

import           Test.Util.Orphans.Arbitrary ()

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

data TestBlock = TestBlock {
      testHeader :: !TestHeader
    , testBody   :: !TestBody
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks, Serialise)

-- | Hash of a 'TestHeader'
newtype TestHeaderHash = TestHeaderHash Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (Condense, NoUnexpectedThunks, Hashable, Serialise, Binary)

-- | Hash of a 'TestBody'
newtype TestBodyHash = TestBodyHash Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (Condense, NoUnexpectedThunks, Hashable, Serialise)

data TestHeader = TestHeader {
      thHash     :: HeaderHash TestHeader
      -- ^ Not included in the calculation of the hash of the 'TestHeader',
      -- i.e., in its own value, which would be pretty hard to do.
      --
      -- Note the absence of a bang: this field caches the 'TestHeader's hash.
      -- To calculate it, the 'TestHeader' is passed to the hashing function,
      -- even though the field is not read, making the field strict would
      -- create an infinite loop.
    , thPrevHash :: !(ChainHash TestHeader)
    , thBodyHash :: !TestBodyHash
    , thSlotNo   :: !SlotNo
    , thBlockNo  :: !BlockNo
    , thIsEBB    :: !IsEBB
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks, Serialise)

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
  deriving anyclass (NoUnexpectedThunks, Serialise, Hashable)

instance GetHeader TestBlock where
  newtype Header TestBlock = TestHeader' { unTestHeader :: TestHeader }
    deriving newtype (Eq, Show, NoUnexpectedThunks, Serialise)
  getHeader = TestHeader' . testHeader

instance StandardHash TestBlock
instance StandardHash TestHeader

type instance HeaderHash TestBlock = TestHeaderHash
type instance HeaderHash TestHeader = TestHeaderHash

instance Measured BlockMeasure TestBlock where
  measure = blockMeasure

instance HasHeader TestBlock where
  blockHash      =            blockHash      . getHeader
  blockPrevHash  = castHash . blockPrevHash  . getHeader
  blockSlot      =            blockSlot      . getHeader
  blockNo        =            blockNo        . getHeader
  blockInvariant =            blockInvariant . getHeader

instance HasHeader (Header TestBlock) where
  blockHash      =            thHash     . unTestHeader
  blockPrevHash  = castHash . thPrevHash . unTestHeader
  blockSlot      =            thSlotNo   . unTestHeader
  blockNo        =            thBlockNo  . unTestHeader
  blockInvariant = const True

data instance BlockConfig TestBlock = TestBlockConfig {
      testBlockSlotLength  :: !SlotLength

      -- | Era parameters
      --
      -- TODO: This should obsolete 'testBlockSlotLengths' (#1637)
    , testBlockEraParams    :: !HardFork.EraParams

      -- | Whether the test block can be EBBs or not. This can vary per test
      -- case. It will be used by 'validateEnvelope' to forbid EBBs 'False'.
    , testBlockEBBsAllowed  :: !Bool

      -- | Number of core nodes
      --
      -- We need this in order to compute the 'ValidateView', which must
      -- conjure up a validation key out of thin air
    , testBlockNumCoreNodes :: !NumCoreNodes
    }
  deriving (Generic, NoUnexpectedThunks)

instance Condense TestBlock where
  condense = show -- TODO

instance Condense TestHeader where
  condense = show -- TODO

hashBody :: TestBody -> TestBodyHash
hashBody = TestBodyHash . hash

hashHeader :: TestHeader -> TestHeaderHash
hashHeader (TestHeader _ a b c d e) = TestHeaderHash (hash (a, b, c, d, e))

testHashInfo :: HashInfo TestHeaderHash
testHashInfo = HashInfo
    { hashSize = 8
    , getHash  = get
    , putHash  = put
    }

testBlockIsEBB :: TestBlock -> IsEBB
testBlockIsEBB = thIsEBB . testHeader

testHeaderEpochNoIfEBB :: HasCallStack
                       => ChunkInfo -> Header TestBlock -> Maybe EpochNo
testHeaderEpochNoIfEBB chunkInfo (TestHeader' hdr) = case thIsEBB hdr of
    IsNotEBB -> Nothing
    IsEBB    -> Just $
      case slotMightBeEBB chunkInfo (thSlotNo hdr) of
        Just epochNo -> epochNo
        Nothing      -> error "testHeaderEpochNoIfEBB: EBB in incorrect slot"

-- | Check whether the header matches its hash and whether the body matches
-- its hash.
testBlockIsValid :: TestBlock -> Bool
testBlockIsValid (TestBlock hdr body) =
  thHash     hdr == hashHeader hdr &&
  thBodyHash hdr == hashBody   body

testBlockToBuilder :: TestBlock -> Builder
testBlockToBuilder = CBOR.toBuilder . encode

testBlockToBinaryInfo :: TestBlock -> BinaryInfo CBOR.Encoding
testBlockToBinaryInfo tb = BinaryInfo
    { binaryBlob   = encode tb
    , headerOffset = testBlockHeaderOffset
    , headerSize   = testBlockHeaderSize tb
    }

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

testBlockFromBinaryInfo :: HasCallStack => BinaryInfo Lazy.ByteString -> TestBlock
testBlockFromBinaryInfo = testBlockFromLazyByteString . binaryBlob

testBlockToBlockInfo :: TestBlock -> BlockInfo TestHeaderHash
testBlockToBlockInfo tb = BlockInfo {
      bbid          = thHash
    , bslot         = thSlotNo
    , bpreBid       = case thPrevHash of
        GenesisHash -> Origin
        BlockHash h -> At h
    , bisEBB        = thIsEBB
    , bheaderOffset = testBlockHeaderOffset
    , bheaderSize   = testBlockHeaderSize tb
    }
  where
    TestHeader{..} = testHeader tb

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
  -> IsEBB
  -> TestBlock
mkBlock canContainEBB testBody thPrevHash thSlotNo thBlockNo thIsEBB =
    case (canContainEBB thSlotNo, thIsEBB) of
      (False, IsEBB) ->
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
      , thIsEBB
      }

-- | Note the first block need not be an EBB, see 'firstEBB'.
firstBlock :: HasCallStack
           => (SlotNo -> Bool)
           -> SlotNo
           -> TestBody
           -> TestBlock
firstBlock canContainEBB slotNo testBody = mkBlock canContainEBB
    testBody
    GenesisHash
    slotNo
    0
    IsNotEBB

mkNextBlock :: (SlotNo -> Bool)
            -> TestBlock  -- ^ Previous block
            -> SlotNo
            -> TestBody
            -> TestBlock
mkNextBlock canContainEBB prev slotNo testBody = mkBlock canContainEBB
    testBody
    (BlockHash (blockHash prev))
    slotNo
    (succ (blockNo prev))
    IsNotEBB

firstEBB :: (SlotNo -> Bool)
         -> TestBody
         -> TestBlock
firstEBB canContainEBB testBody = mkBlock canContainEBB testBody GenesisHash 0 0 IsEBB

-- | Note that in various places, e.g., the ImmutableDB, we rely on the fact
-- that the @slotNo@ should correspond to the first slot number of the epoch,
-- as is the case for real EBBs.
mkNextEBB :: (SlotNo -> Bool)
          -> TestBlock  -- ^ Previous block
          -> SlotNo     -- ^ @slotNo@
          -> TestBody
          -> TestBlock
mkNextEBB canContainEBB prev slotNo testBody = mkBlock canContainEBB
    testBody
    (BlockHash (blockHash prev))
    slotNo
    (blockNo prev)
    IsEBB


{-------------------------------------------------------------------------------
  Test infrastructure: protocol
-------------------------------------------------------------------------------}

data BftWithEBBs

ebbAwareCompareCandidates :: (BlockNo, IsEBB) -> (BlockNo, IsEBB) -> Ordering
ebbAwareCompareCandidates (lBlockNo, lIsEBB) (rBlockNo, rIsEBB) =
    -- Prefer the highest block number, as it is a proxy for chain length
    case lBlockNo `compare` rBlockNo of
      LT -> LT
      GT -> GT
      -- If the block numbers are the same, check if one of them is an EBB.
      -- An EBB has the same block number as the block before it, so the
      -- chain ending with an EBB is actually longer than the one ending
      -- with a regular block.
      EQ -> score lIsEBB `compare` score rIsEBB
   where
     score :: IsEBB -> Int
     score IsEBB    = 1
     score IsNotEBB = 0

instance ChainSelection (Bft BftMockCrypto) BftWithEBBs where
  type SelectView' (Bft BftMockCrypto) = (BlockNo, IsEBB)

  compareCandidates' _ _ = ebbAwareCompareCandidates

  preferCandidate' _ _ ours cand =
    ebbAwareCompareCandidates ours cand == LT

type instance BlockProtocol TestBlock =
  ModChainSel (Bft BftMockCrypto) BftWithEBBs

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
      signKey (SlotNo n) = SignKeyMockDSIGN $ fromIntegral (n `mod` numCore)

  selectView _ hdr = (blockNo hdr, thIsEBB (unTestHeader hdr))

data TestBlockError =
    -- | The hashes don't line up
    InvalidHash
      (ChainHash TestBlock)  -- ^ Expected hash
      (ChainHash TestBlock)  -- ^ Invalid hash

    -- | The block itself is invalid
  | InvalidBlock
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance IsLedger (LedgerState TestBlock) where
  type LedgerCfg (LedgerState TestBlock) = ()
  type LedgerErr (LedgerState TestBlock) = TestBlockError

  applyChainTick _ = TickedLedgerState

instance ApplyBlock (LedgerState TestBlock) TestBlock where
  applyLedgerBlock _ tb@TestBlock{..} (TickedLedgerState _ TestLedger{..})
    | blockPrevHash tb /= lastAppliedHash
    = throwError $ InvalidHash lastAppliedHash (blockPrevHash tb)
    | not $ tbIsValid testBody
    = throwError $ InvalidBlock
    | otherwise
    = return     $ TestLedger (Chain.blockPoint tb) (BlockHash (blockHash tb))

  reapplyLedgerBlock _ tb _ =
    TestLedger (Chain.blockPoint tb) (BlockHash (blockHash tb))

  ledgerTipPoint = lastAppliedPoint

instance UpdateLedger TestBlock where
  data LedgerState TestBlock =
      TestLedger {
          -- The ledger state simply consists of the last applied block
          lastAppliedPoint :: !(Point TestBlock)
        , lastAppliedHash  :: !(ChainHash TestBlock)
        }
    deriving stock    (Show, Eq, Generic)
    deriving anyclass (Serialise, NoUnexpectedThunks)

instance HasAnnTip TestBlock where
  type TipInfo TestBlock = IsEBB
  getTipInfo = thIsEBB . unTestHeader

data TestBlockOtherHeaderEnvelopeError =
    UnexpectedEBBInSlot !SlotNo
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance ValidateEnvelope TestBlock where
  type OtherHeaderEnvelopeError TestBlock = TestBlockOtherHeaderEnvelopeError

  -- Not used in the storage tests, only the default implementation of
  -- 'validateEnvelope' and block production use it.
  firstBlockNo _ = error "unused"

  validateEnvelope cfg _ledgerView oldTip hdr = do
      when (actualBlockNo /= expectedBlockNo) $
        throwError $ UnexpectedBlockNo expectedBlockNo actualBlockNo
      when (actualSlotNo < expectedSlotNo) $
        throwError $ UnexpectedSlotNo expectedSlotNo actualSlotNo
      when (actualPrevHash /= expectedPrevHash) $
        throwError $ UnexpectedPrevHash expectedPrevHash actualPrevHash
      when (fromIsEBB newIsEBB && not (canBeEBB actualSlotNo)) $
        throwError $ OtherHeaderEnvelopeError $ UnexpectedEBBInSlot actualSlotNo
    where
      newIsEBB :: IsEBB
      newIsEBB = thIsEBB (unTestHeader hdr)

      actualSlotNo   :: SlotNo
      actualBlockNo  :: BlockNo
      actualPrevHash :: ChainHash TestBlock

      actualSlotNo   =            blockSlot     hdr
      actualBlockNo  =            blockNo       hdr
      actualPrevHash = castHash $ blockPrevHash hdr

      expectedSlotNo   :: SlotNo           -- Lower bound only
      expectedBlockNo  :: BlockNo
      expectedPrevHash :: ChainHash TestBlock

      (expectedSlotNo, expectedBlockNo, expectedPrevHash) = (
            nextSlotNo  ((annTipInfo &&& annTipSlotNo)  <$> oldTip) newIsEBB
          , nextBlockNo ((annTipInfo &&& annTipBlockNo) <$> oldTip) newIsEBB
          , withOrigin GenesisHash (BlockHash . annTipHash) oldTip
          )

      -- EBB shares its slot number with its successor
      nextSlotNo :: WithOrigin (IsEBB, SlotNo) -> IsEBB -> SlotNo
      nextSlotNo Origin          _        = SlotNo 0
      nextSlotNo (At (IsEBB, s)) IsNotEBB = s
      nextSlotNo (At (_    , s)) _        = succ s

      -- EBB shares its block number with its predecessor. The chain always
      -- starts with block number 0.
      nextBlockNo :: WithOrigin (IsEBB, BlockNo) -> IsEBB -> BlockNo
      nextBlockNo Origin             _     = BlockNo 0
      nextBlockNo (At (IsNotEBB, b)) IsEBB = b
      nextBlockNo (At (_       , b)) _     = succ b

      canBeEBB :: SlotNo -> Bool
      canBeEBB (SlotNo s) = testBlockEBBsAllowed (configBlock cfg)
                         && s `mod` epochSlots == 0

      epochSlots :: Word64
      epochSlots =
          unEpochSize
        . HardFork.eraEpochSize
        . testBlockEraParams
        . configBlock
        $ cfg

instance LedgerSupportsProtocol TestBlock where
  protocolLedgerView _ _ = ()
  ledgerViewForecastAt_ _ _ = Just . trivialForecast

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[()]
  hardForkShape           = HardFork.singletonShape . testBlockEraParams
  hardForkTransitions _ _ = HardFork.transitionsUnknown

instance LedgerDerivedInfo TestBlock where
  knownSlotLengths = singletonSlotLengths . testBlockSlotLength

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
        configConsensus = McsConsensusConfig BftConfig {
            bftParams   = BftParams {
                bftSecurityParam = k
              , bftNumNodes      = numCoreNodes
              }
          , bftNodeId   = CoreId (CoreNodeId 0)
          , bftSignKey  = SignKeyMockDSIGN 0
          , bftVerKeys  = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
          }
      , configLedger = ()
      , configBlock  = TestBlockConfig {
            testBlockSlotLength   = slotLength
          , testBlockEraParams    = eraParams
          , testBlockEBBsAllowed  = chunkCanContainEBB
          , testBlockNumCoreNodes = numCoreNodes
          }
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
      , eraSafeZone   =
          HardFork.SafeZone (maxRollbacks k * 2) HardFork.NoLowerBound
      }

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

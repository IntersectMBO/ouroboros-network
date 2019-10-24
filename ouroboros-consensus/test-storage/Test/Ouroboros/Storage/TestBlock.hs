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
module Test.Ouroboros.Storage.TestBlock where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise (decode, encode))
import           Control.Monad (forM)
import           Control.Monad.Except (throwError)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Functor (($>))
import           Data.Hashable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Control.Monad.Class.MonadThrow

import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain (Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

import           Ouroboros.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Storage.FS.API.Types

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
  deriving newtype  (Condense, NoUnexpectedThunks, Hashable, Serialise)

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
    deriving newtype (Eq, Show, NoUnexpectedThunks)
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


instance Condense TestBlock where
  condense = show -- TODO

instance Condense TestHeader where
  condense = show -- TODO

hashBody :: TestBody -> TestBodyHash
hashBody = TestBodyHash . hash

hashHeader :: TestHeader -> TestHeaderHash
hashHeader (TestHeader _ a b c d e) = TestHeaderHash (hash (a, b, c, d, e))

testBlockIsEBB :: TestBlock -> IsEBB
testBlockIsEBB = thIsEBB . testHeader

testBlockToBuilder :: TestBlock -> Builder
testBlockToBuilder = CBOR.toBuilder . encode

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
  Creating blocks
-------------------------------------------------------------------------------}

mkBlock
  :: TestBody
  -> ChainHash TestHeader  -- ^ Hash of previous header
  -> SlotNo
  -> BlockNo
  -> IsEBB
  -> TestBlock
mkBlock testBody thPrevHash thSlotNo thBlockNo thIsEBB =
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
firstBlock :: SlotNo
           -> TestBody
           -> TestBlock
firstBlock slotNo testBody = mkBlock
    testBody
    GenesisHash
    slotNo
    1
    IsNotEBB

mkNextBlock :: TestBlock  -- ^ Previous block
            -> SlotNo
            -> TestBody
            -> TestBlock
mkNextBlock prev slotNo testBody = mkBlock
    testBody
    (BlockHash (blockHash prev))
    slotNo
    (succ (blockNo prev))
    IsNotEBB

firstEBB :: TestBody
         -> TestBlock
firstEBB testBody = mkBlock testBody GenesisHash 0 0 IsEBB

-- | Note that in various places, e.g., the ImmutableDB, we rely on the fact
-- that the @slotNo@ should correspond to the first slot number of the epoch,
-- as is the case for real EBBs.
mkNextEBB :: TestBlock  -- ^ Previous block
          -> SlotNo     -- ^ @slotNo@
          -> TestBody
          -> TestBlock
mkNextEBB prev slotNo testBody = mkBlock
    testBody
    (BlockHash (blockHash prev))
    slotNo
    (succ (blockNo prev))
    IsEBB


{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol TestBlock = Bft BftMockCrypto

instance SignedHeader (Header TestBlock) where
  type Signed (Header TestBlock) = ()
  headerSigned _ = ()

instance HeaderSupportsBft BftMockCrypto (Header TestBlock) where
  headerBftFields cfg hdr = BftFields {
        bftSignature = SignedDSIGN $
                         mockSign
                           ()
                           (signKey cfg (blockSlot hdr))
      }
    where
      -- We don't want /our/ signing key, but rather the signing key of the
      -- node that produced the block
      signKey :: NodeConfig (Bft BftMockCrypto)
              -> SlotNo
              -> SignKeyDSIGN MockDSIGN
      signKey BftNodeConfig{ bftParams = BftParams{..} } (SlotNo n) =
          SignKeyMockDSIGN $ fromIntegral (n `mod` bftNumNodes)

data TestBlockError
  = InvalidHash
    { _expectedHash :: ChainHash TestBlock
    , _invalidHash  :: ChainHash TestBlock
    }
    -- ^ The hashes don't line up
  | InvalidBlock
    -- ^ The block itself is invalid
  deriving (Show)

instance SupportedBlock TestBlock

instance UpdateLedger TestBlock where
  data LedgerState TestBlock =
      TestLedger {
          -- The ledger state simply consists of the last applied block
          lastAppliedPoint :: !(Point TestBlock)
        , lastAppliedHash  :: !(ChainHash TestBlock)
        }
    deriving stock    (Show, Eq, Generic)
    deriving anyclass (Serialise, NoUnexpectedThunks)

  data LedgerConfig TestBlock = LedgerConfig
  type LedgerError  TestBlock = TestBlockError

  ledgerConfigView _ = LedgerConfig

  applyChainTick _ _ = return

  applyLedgerBlock _ tb@TestBlock{..} TestLedger{..}
    | blockPrevHash tb /= lastAppliedHash
    = throwError $ InvalidHash lastAppliedHash (blockPrevHash tb)
    | not $ tbIsValid testBody
    = throwError $ InvalidBlock
    | otherwise
    = return     $ TestLedger (Chain.blockPoint tb) (BlockHash (blockHash tb))

  reapplyLedgerBlock _ tb _ =
    TestLedger (Chain.blockPoint tb) (BlockHash (blockHash tb))

  ledgerTipPoint = lastAppliedPoint

instance ProtocolLedgerView TestBlock where
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Right $ SB.unbounded ()

testInitLedger :: LedgerState TestBlock
testInitLedger = TestLedger GenesisPoint GenesisHash

testInitExtLedger :: ExtLedgerState TestBlock
testInitExtLedger = ExtLedgerState {
      ledgerState         = testInitLedger
    , ouroborosChainState = ()
    }

-- | Trivial test configuration with a single core node
singleNodeTestConfig :: NodeConfig (Bft BftMockCrypto)
singleNodeTestConfig = BftNodeConfig {
      bftParams   = BftParams { bftSecurityParam = k
                              , bftNumNodes      = 1
                              }
    , bftNodeId   = CoreId 0
    , bftSignKey  = SignKeyMockDSIGN 0
    , bftVerKeys  = Map.singleton (CoreId 0) (VerKeyMockDSIGN 0)
    }
  where
    -- We fix k at 4 for now
    k = SecurityParam 4

{-------------------------------------------------------------------------------
  Corruption
-------------------------------------------------------------------------------}

data FileCorruption
  = DeleteFile
  | DropLastBytes Word64
    -- ^ Drop the last @n@ bytes of a file.
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

instance Arbitrary FileCorruption where
  arbitrary = frequency
    [ (1, return DeleteFile)
    , (1, DropLastBytes . getSmall . getPositive <$> arbitrary)
    ]
  shrink DeleteFile         = []
  shrink (DropLastBytes n)  =
    DropLastBytes . getSmall . getPositive <$> shrink (Positive (Small n))

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

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving newtype instance Hashable SlotNo
deriving newtype instance Hashable BlockNo
instance Hashable IsEBB
  -- use generic instance

instance Hashable (HeaderHash b) => Hashable (ChainHash b)
  -- use generic instance

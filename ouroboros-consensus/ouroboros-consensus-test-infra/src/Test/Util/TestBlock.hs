{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | Minimal instantiation of the consensus layer to be able to run the ChainDB
module Test.Util.TestBlock (
    -- * Blocks
    TestHash(TestHash)
  , unTestHash
  , testHashFromList
  , testHashInfo
  , TestBlock(..)
  , TestBlockError(..)
  , Header(..)
  , BlockConfig(..)
  , Query(..)
  , firstBlock
  , successorBlock
  , modifyFork
  , forkBlock
    -- * Chain
  , BlockChain(..)
  , blockChain
  , chainToBlocks
    -- * Tree
  , BlockTree(..)
  , blockTree
  , treeToBlocks
  , treeToChains
  , treePreferredChain
    -- * Ledger infrastructure
  , lastAppliedBlock
  , testInitLedger
  , testInitExtLedger
  , singleNodeTestConfig
    -- * Support for tests
  , Permutation(..)
  , permute
  ) where

import           Codec.Serialise (Serialise)
import           Control.DeepSeq (force)
import           Control.Monad.Except (throwError)
import           Data.Binary (get, put)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Function (on)
import           Data.Int
import           Data.List (maximumBy, transpose)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import           Data.TreeDiff (ToExpr)
import           Data.Type.Equality ((:~:) (Refl))
import           Data.Word
import           GHC.Generics (Generic)
import qualified System.Random as R
import           Test.QuickCheck hiding (Result)

import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (ChainHash (..), HeaderHash,
                     SlotNo (..))
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.MockChain.Chain (Chain (..), Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain

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
import           Ouroboros.Consensus.Protocol.MockChainSel
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Storage.ImmutableDB (HashInfo (..))

import           Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  Test infrastructure: test block
-------------------------------------------------------------------------------}

-- The hash represents a path through a tree (of forks) as a list of
-- 'Word64's: @[0, 1, 0]@ means we forked off at the first block.
--
-- The following tree is just a small subset of the actual tree, which allows
-- multiple forks at /each/ node:
--
-- > G--A--B--C
-- > \  \--B"-C"
-- >  \-A'-B'-C'
--
-- Some examples:
--
-- > []      = G = Genesis
-- > [0]     = A
-- > [1]     = A'
-- > [0,0]   = B
-- > [0,0,0] = C
-- > [0,1]   = B"
-- > [0,1,0] = C"
-- > [1,0]   = B'
-- > [1,0,0] = C'
-- > ...
--
-- Since the empty list represents Genesis, which does not correspond to an
-- actual block, we use a NonEmpty list.
--
-- As it is easier to prepend to and access the front of a (NonEmpty) list, we
-- store the list in reverse order, e.g., @C'@ is stored as @[0,0,1]@, but we
-- print ('Show' and 'Condense') it as @[1,0,0]@.
--
-- The predecessor (parent in the tree) can be obtained by dropping the last
-- (in the printed representation) element in the list (or the head in the
-- in-memory representation).
--
-- The 'BlockNo' of the corresponding block is just the length of the list.
newtype TestHash = UnsafeTestHash {
      unTestHash :: NonEmpty Word64
    }
  deriving stock   (Generic)
  deriving newtype (Eq, Ord, Serialise, ToExpr)
  deriving anyclass NoUnexpectedThunks

pattern TestHash :: NonEmpty Word64 -> TestHash
pattern TestHash path <- UnsafeTestHash path where
  TestHash path = UnsafeTestHash (force path)

{-# COMPLETE TestHash #-}

testHashFromList :: [Word64] -> TestHash
testHashFromList = TestHash . NE.fromList . reverse

-- | 'TestHash' doesn't have a fixed size binary serialisation, which is
-- required for 'HashInfo'.
--
-- Fortunately, in most cases we know all the hashes upfront, so we can
-- compute the max size we'll need and use that as the size. Shorter
-- serialisations will be padded to the right size using 'maxBound'.
testHashInfo :: [TestHash] -> HashInfo TestHash
testHashInfo hashes = HashInfo
    { hashSize
    , getHash = TestHash . NE.fromList . takeWhile (/= maxBound) <$> get
    , putHash = \(TestHash l) ->
        let len = NE.length l
        in put (NE.toList l <> replicate (longestHashLen - len) maxBound)
    }
  where
    TestHash longestHash = maximumBy (compare `on` length . unTestHash) hashes
    longestHashLen = length longestHash
    hashSize       = fromIntegral $ Lazy.length $ Put.runPut $ put longestHash

instance Show TestHash where
  show (TestHash h) = "(testHashFromList " <> show (reverse (NE.toList h)) <> ")"

instance Condense TestHash where
  condense = condense . reverse . NE.toList . unTestHash

data TestBlock = TestBlock {
      tbHash  :: TestHash
    , tbSlot  :: Block.SlotNo
      -- ^ We store a separate 'Block.SlotNo', as slots can have gaps between
      -- them, unlike block numbers.
      --
      -- Note that when generating a 'TestBlock', you must make sure that
      -- blocks with the same 'TestHash' have the same slot number.
    , tbValid :: Bool
      -- ^ Note that when generating a 'TestBlock', you must make sure that
      -- blocks with the same 'TestHash' have the same value for 'tbValid'.
    }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoUnexpectedThunks, ToExpr)

instance GetHeader TestBlock where
  newtype Header TestBlock = TestHeader { testHeader :: TestBlock }
    deriving stock   (Eq, Show)
    deriving newtype (NoUnexpectedThunks, Serialise)
  getHeader = TestHeader

type instance HeaderHash TestBlock = TestHash

instance Block.HasHeader TestBlock where
  blockHash      = tbHash
  blockPrevHash b = case NE.nonEmpty . NE.tail . unTestHash . tbHash $ b of
    Nothing       -> GenesisHash
    Just prevHash -> BlockHash (TestHash prevHash)
  blockSlot      = tbSlot
  blockNo        = fromIntegral . NE.length . unTestHash . tbHash
  blockInvariant = const True

instance Block.HasHeader (Header TestBlock) where
  blockHash      =                  Block.blockHash     . testHeader
  blockPrevHash  = Block.castHash . Block.blockPrevHash . testHeader
  blockSlot      =                  Block.blockSlot     . testHeader
  blockNo        =                  Block.blockNo       . testHeader
  blockInvariant = const True

instance Block.StandardHash TestBlock

instance Measured Block.BlockMeasure TestBlock where
  measure = Block.blockMeasure

instance Condense TestBlock where
  condense b = mconcat [
        "(H:"
      , condense (Block.blockHash b)
      , ",S:"
      , condense (Block.blockSlot b)
      , ",B:"
      , condense (Block.unBlockNo (Block.blockNo b))
      , ")"
      ]

instance Condense (Header TestBlock) where
  condense = condense . testHeader

instance Condense (ChainHash TestBlock) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = show h

data instance BlockConfig TestBlock = TestBlockConfig {
      testBlockSlotLength :: !SlotLength

      -- | Era parameters
      --
      -- TODO: This should obsolete 'testBlockSlotLengths' (#1637)
    , testBlockEraParams :: !HardFork.EraParams

      -- | Number of core nodes
      --
      -- We need this in order to compute the 'ValidateView', which must
      -- conjure up a validation key out of thin air
    , testBlockNumCoreNodes :: !NumCoreNodes
    }
  deriving (Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol TestBlock = Bft BftMockCrypto

type instance Signed (Header TestBlock) = ()
instance SignedHeader (Header TestBlock) where
  headerSigned _ = ()

data TestBlockError =
    -- | The hashes don't line up
    InvalidHash
      (ChainHash TestBlock)  -- ^ Expected hash
      (ChainHash TestBlock)  -- ^ Invalid hash

    -- | The block itself is invalid
  | InvalidBlock
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance BlockSupportsProtocol TestBlock where
  validateView TestBlockConfig{..} =
      bftValidateView bftFields
    where
      NumCoreNodes numCore = testBlockNumCoreNodes

      bftFields :: Header TestBlock -> BftFields BftMockCrypto ()
      bftFields (TestHeader tb) = BftFields {
            bftSignature = SignedDSIGN $ mockSign () (signKey (tbSlot tb))
          }

      -- We don't want /our/ signing key, but rather the signing key of the
      -- node that produced the block
      signKey :: Block.SlotNo -> SignKeyDSIGN MockDSIGN
      signKey (SlotNo n) = SignKeyMockDSIGN $ fromIntegral (n `mod` numCore)

instance IsLedger (LedgerState TestBlock) where
  type LedgerCfg (LedgerState TestBlock) = ()
  type LedgerErr (LedgerState TestBlock) = TestBlockError

  applyChainTick _ = TickedLedgerState

instance ApplyBlock (LedgerState TestBlock) TestBlock where
  applyLedgerBlock _ tb@TestBlock{..} (TickedLedgerState _ TestLedger{..})
    | Block.blockPrevHash tb /= Block.pointHash lastAppliedPoint
    = throwError $ InvalidHash (Block.pointHash lastAppliedPoint) (Block.blockPrevHash tb)
    | not tbValid
    = throwError $ InvalidBlock
    | otherwise
    = return     $ TestLedger (Chain.blockPoint tb)

  reapplyLedgerBlock _ tb _ = TestLedger (Chain.blockPoint tb)

  ledgerTipPoint = lastAppliedPoint

instance UpdateLedger TestBlock where
  newtype LedgerState TestBlock =
      TestLedger {
          -- The ledger state simply consists of the last applied block
          lastAppliedPoint :: Point TestBlock
        }
    deriving stock   (Show, Eq, Generic)
    deriving newtype (Serialise, NoUnexpectedThunks, ToExpr)

-- | Last applied block
--
-- Returns 'Nothing' if the ledger is empty.
lastAppliedBlock :: LedgerState TestBlock -> Maybe TestBlock
lastAppliedBlock (TestLedger p) = go p
  where
    -- We can only have applied valid blocks
    go :: Point TestBlock -> Maybe TestBlock
    go Block.GenesisPoint           = Nothing
    go (Block.BlockPoint slot hash) = Just $ TestBlock hash slot True

instance HasAnnTip TestBlock where
  -- Use defaults

instance ValidateEnvelope TestBlock where
  -- The block number of a test block is derived from the length of the hash
  firstBlockNo _ = Block.BlockNo 1

instance LedgerSupportsProtocol TestBlock where
  protocolLedgerView _ _ = ()
  ledgerViewForecastAt_ _ _ = Just . trivialForecast

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[()]
  hardForkShape           = HardFork.singletonShape . testBlockEraParams
  hardForkTransitions _ _ = HardFork.transitionsUnknown

instance LedgerDerivedInfo TestBlock where
  knownSlotLengths = singletonSlotLengths . testBlockSlotLength

instance QueryLedger TestBlock where
  data Query TestBlock result where
    QueryLedgerTip :: Query TestBlock (Point TestBlock)

  answerQuery _cfg QueryLedgerTip (TestLedger { lastAppliedPoint }) =
    lastAppliedPoint
  eqQuery QueryLedgerTip QueryLedgerTip = Just Refl

deriving instance Eq (Query TestBlock result)
deriving instance Show (Query TestBlock result)

instance ShowQuery (Query TestBlock) where
  showResult QueryLedgerTip = show

testInitLedger :: LedgerState TestBlock
testInitLedger = TestLedger Block.genesisPoint

testInitExtLedger :: ExtLedgerState TestBlock
testInitExtLedger = ExtLedgerState {
      ledgerState = testInitLedger
    , headerState = genesisHeaderState ()
    }

-- | Trivial test configuration with a single core node
singleNodeTestConfig :: TopLevelConfig TestBlock
singleNodeTestConfig = TopLevelConfig {
      configConsensus = BftConfig {
          bftParams   = BftParams { bftSecurityParam = k
                                  , bftNumNodes      = numCoreNodes
                                  }
        , bftNodeId   = CoreId (CoreNodeId 0)
        , bftSignKey  = SignKeyMockDSIGN 0
        , bftVerKeys  = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , configLedger = ()
    , configBlock  = TestBlockConfig slotLength eraParams numCoreNodes
    }
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 1

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams k slotLength

    -- We fix k at 4 for now
    k = SecurityParam 4

{-------------------------------------------------------------------------------
  Chain of blocks
-------------------------------------------------------------------------------}

newtype BlockChain = BlockChain Word64
  deriving (Show)

blockChain :: BlockChain -> Chain TestBlock
blockChain = Chain.fromOldestFirst . chainToBlocks

chainToBlocks :: BlockChain -> [TestBlock]
chainToBlocks (BlockChain c) =
    take (fromIntegral c) $ iterate successorBlock (firstBlock 0)

instance Arbitrary BlockChain where
  arbitrary = BlockChain <$> choose (0, 30)
  shrink (BlockChain c) = BlockChain <$> shrink c

-- Create the first block in the given fork: @[fork]@
-- The 'SlotNo' will be 1.
firstBlock :: Word64 -> TestBlock
firstBlock forkNo = TestBlock
    { tbHash  = TestHash (forkNo NE.:| [])
    , tbSlot  = 1
    , tbValid = True
    }

-- Create the successor of the given block without forking:
-- @b -> b ++ [0]@ (in the printed representation)
-- The 'SlotNo' is increased by 1.
--
-- In Zipper parlance, this corresponds to going down in a tree.
successorBlock :: TestBlock -> TestBlock
successorBlock TestBlock{..} = TestBlock
    { tbHash  = TestHash (NE.cons 0 (unTestHash tbHash))
    , tbSlot  = succ tbSlot
    , tbValid = True
    }

-- Modify the (last) fork number of the given block:
-- @g@ -> @[.., f]@ -> @[.., g f]@
-- The 'SlotNo' is left unchanged.
modifyFork :: (Word64 -> Word64) -> TestBlock -> TestBlock
modifyFork g tb@TestBlock{ tbHash = UnsafeTestHash (f NE.:| h) } = tb
    { tbHash = let !gf = g f in UnsafeTestHash (gf NE.:| h)
    }

-- Increase the fork number of the given block:
-- @[.., f]@ -> @[.., f+1]@
-- The 'SlotNo' is left unchanged.
--
-- In Zipper parlance, this corresponds to going right in a tree.
forkBlock :: TestBlock -> TestBlock
forkBlock = modifyFork succ

{-------------------------------------------------------------------------------
  Tree of blocks
-------------------------------------------------------------------------------}

newtype BlockTree = BlockTree (Tree ())

blockTree :: BlockTree -> Tree TestBlock
blockTree (BlockTree t) = go (firstBlock 0) t
  where
    go :: TestBlock -> Tree () -> Tree TestBlock
    go b (Node () ts) = Node b (zipWith go bs ts)
      where
        -- The first child of a node is the sucessor of b ("go down"), each
        -- subsequent child is a "fork" ("go right")
        bs = iterate forkBlock (successorBlock b)

treeToBlocks :: BlockTree -> [TestBlock]
treeToBlocks = Tree.flatten . blockTree

treeToChains :: BlockTree -> [Chain TestBlock]
treeToChains = map Chain.fromOldestFirst . allPaths . blockTree

treePreferredChain :: TopLevelConfig TestBlock
                   -> BlockTree -> Chain TestBlock
treePreferredChain cfg =
      fromMaybe Genesis
    . selectUnvalidatedChain Block.blockNo (configConsensus cfg) Genesis
    . treeToChains

instance Show BlockTree where
  show (BlockTree t) = Tree.drawTree (fmap show t)

instance Arbitrary BlockTree where
  arbitrary = sized $ \n ->
      BlockTree <$> mkTree 0.2 (replicate (max 1 n) ())
  shrink (BlockTree t) =
      BlockTree <$> shrinkTree t

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

-- | Construct random binary tree from given set of elements
mkTree :: forall a.
          Double -- ^ Likelyhood of branching at any point
       -> [a] -> Gen (Tree a)
mkTree threshold = go
  where
    go :: [a] -> Gen (Tree a)
    go []     = error "go: no elements"
    go [a]    = return $ Node a []
    go (a:as) = do n <- choose (0, 1)
                   if n >= threshold || null right
                     then (\t   -> Node a [t])    <$> go as
                     else (\l r -> Node a [l, r]) <$> go left <*> go right
      where
        (left, right) = split as

-- | Shrink tree (without shrinking any elements)
shrinkTree :: Tree a -> [Tree a]
shrinkTree (Node a ts) = map (Node a) (shrinkList shrinkTree ts)
                         -- Also try shrinking all subtrees at once
                      ++ map (Node a) (transpose (map shrinkTree ts))


-- | Split list into two
--
-- > split [1..6] == ([1,3,5],[2,4,6])
-- > take 5 (fst (split [1..])) == [1,3,5,7,9]
-- > take 5 (snd (split [1..])) == [2,4,6,8,10]
split :: [a] -> ([a], [a])
split []     = ([], [])
split (a:as) = let (xs, ys) = split as in (a:ys, xs)

-- | All paths through a tree
allPaths :: Tree a -> [[a]]
allPaths t = [] : nonEmptyPaths t

nonEmptyPaths :: Tree a -> [[a]]
nonEmptyPaths (Node a ts) = [a] : map (a:) (concatMap nonEmptyPaths ts)

{-------------------------------------------------------------------------------
  Test auxiliary
-------------------------------------------------------------------------------}

newtype Permutation = Permutation Int
  deriving (Show)

instance Arbitrary Permutation where
  arbitrary = Permutation . cast <$> arbitrary
    where
      -- Use the generator for 'Int64' (rather than 'Int') as it is not biased
      -- towards small values
      cast :: Int64 -> Int
      cast = fromIntegral

  -- Doesn't make sense to shrink PRNG seed
  shrink _ = []

permute :: Permutation -> [a] -> [a]
permute (Permutation n) = go (R.mkStdGen n)
  where
    go :: R.StdGen -> [a] -> [a]
    go _ [] = []
    go g as = let (i, g')           = R.randomR (0, length as - 1) g
                  (before, a:after) = splitAt i as
              in a : go g' (before ++ after)

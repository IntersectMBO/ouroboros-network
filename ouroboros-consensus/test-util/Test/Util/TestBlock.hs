{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Minimal instantiation of the consensus layer to be able to run the ChainDB
module Test.Util.TestBlock (
    -- * Blocks
    TestHash(..)
  , mkTestHash
  , TestBlock(..)
  , Header(..)
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
  , testInitExtLedger
  , singleNodeTestConfig
    -- * Mempool integration
  , GenTx (..)
  , GenTxId (..)
  , ApplyTxErr
    -- * Support for tests
  , Permutation(..)
  , permute
  ) where

import           Codec.Serialise (Serialise (encode))
import           Control.Monad.Except (throwError)
import           Data.FingerTree (Measured (..))
import           Data.Int
import           Data.List (transpose)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import           Data.Word
import           GHC.Generics (Generic)
import qualified System.Random as R
import           Test.QuickCheck

import           Cardano.Crypto.DSIGN

import           Ouroboros.Network.Block (ChainHash (..), HeaderHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Chain (..), Point)
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool (ApplyTx (..))
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

import           Test.Util.TestTx (TestTx (..), TestTxId)

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
newtype TestHash = TestHash {
      unTestHash :: NonEmpty Word64
    }
  deriving stock   (Generic)
  deriving newtype (Eq, Ord, Serialise)

mkTestHash :: [Word64] -> TestHash
mkTestHash = TestHash . NE.fromList . reverse

instance Show TestHash where
  show (TestHash h) = "(mkTestHash " <> show (reverse (NE.toList h)) <> ")"

instance Condense TestHash where
  condense = condense . reverse . NE.toList . unTestHash

data TestBlock = TestBlock {
      tbHash :: TestHash
    , tbSlot :: Block.SlotNo
      -- ^ We store a separate 'Block.SlotNo', as slots can have gaps between
      -- them, unlike block numbers.
      --
      -- Note that when generating a 'TestBlock', you must make sure that
      -- blocks with the same 'TestHash' have the same slot number.
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)

instance GetHeader TestBlock where
  newtype Header TestBlock = TestHeader { testHeader :: TestBlock }
    deriving (Eq, Show)
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


{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol TestBlock = Bft BftMockCrypto

instance SignedHeader (Header TestBlock) where
  type Signed (Header TestBlock) = ()
  headerSigned _ = ()
  encodeSigned _ = encode

instance HeaderSupportsBft BftMockCrypto (Header TestBlock) where
  headerBftFields cfg (TestHeader tb) = BftFields {
        bftSignature = SignedDSIGN $
                         mockSign
                           encode
                           ()
                           (signKey cfg (tbSlot tb))
      }
    where
      -- We don't want /our/ signing key, but rather the signing key of the
      -- node that produced the block
      signKey :: NodeConfig (Bft BftMockCrypto)
              -> Block.SlotNo
              -> SignKeyDSIGN MockDSIGN
      signKey BftNodeConfig{bftParams = BftParams{..}} (Block.SlotNo n) =
          SignKeyMockDSIGN $ fromIntegral (n `mod` bftNumNodes)

-- | The only error possible is that hashes don't line up
data InvalidHash = InvalidHash {
      expectedHash :: ChainHash TestBlock
    , invalidHash  :: ChainHash TestBlock
    }
  deriving (Show)

instance SupportedBlock TestBlock

instance UpdateLedger TestBlock where
  data LedgerState TestBlock =
      TestLedger {
          -- The ledger state simply consists of the last applied block
          lastApplied :: (Point TestBlock, ChainHash TestBlock)
        }
    deriving (Show, Eq, Generic, Serialise)

  data LedgerConfig TestBlock = LedgerConfig
  type LedgerError  TestBlock = InvalidHash

  ledgerConfigView _ = LedgerConfig

  applyLedgerBlock _ tb@TestBlock{..} TestLedger{..} =
      if Block.blockPrevHash tb == snd lastApplied
        then return     $ TestLedger (Chain.blockPoint tb, BlockHash (Block.blockHash tb))
        else throwError $ InvalidHash (snd lastApplied) (Block.blockPrevHash tb)

  applyLedgerHeader _ _ = return

  ledgerTipPoint = fst . lastApplied

instance ProtocolLedgerView TestBlock where
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Just $ SB.unbounded ()

testInitLedger :: LedgerState TestBlock
testInitLedger = TestLedger (Chain.genesisPoint, GenesisHash)

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
  Test infrastructure: mempool support
-------------------------------------------------------------------------------}

newtype TestTxError = TestTxErrorInvalid TestTx
  deriving (Show)

instance ApplyTx TestBlock where
  newtype GenTx TestBlock = TestGenTx
    { testGenTx :: TestTx
    } deriving (Show, Eq, Ord)

  newtype GenTxId TestBlock = TestGenTxId
    { testGenTxId :: TestTxId
    } deriving (Show, Eq, Ord)

  txId (TestGenTx (ValidTestTx   txid)) = TestGenTxId txid
  txId (TestGenTx (InvalidTestTx txid)) = TestGenTxId txid

  txSize _ = 2000 -- TODO #745

  type ApplyTxErr TestBlock = TestTxError

  applyTx = \_ (TestGenTx tx) st -> case tx of
    ValidTestTx _   -> pure st
    InvalidTestTx _ -> throwError $ TestTxErrorInvalid tx

  reapplyTx = applyTx

  reapplyTxSameState = \_ _ st -> st

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
    { tbHash = TestHash (forkNo NE.:| [])
    , tbSlot = 1
    }

-- Create the successor of the given block without forking:
-- @b -> b ++ [0]@ (in the printed representation)
-- The 'SlotNo' is increased by 1.
--
-- In Zipper parlance, this corresponds to going down in a tree.
successorBlock :: TestBlock -> TestBlock
successorBlock TestBlock{..} = TestBlock
    { tbHash = TestHash (NE.cons 0 (unTestHash tbHash))
    , tbSlot = succ tbSlot
    }

-- Modify the (last) fork number of the given block:
-- @g@ -> @[.., f]@ -> @[.., g f]@
-- The 'SlotNo' is left unchanged.
modifyFork :: (Word64 -> Word64) -> TestBlock -> TestBlock
modifyFork g tb@TestBlock{ tbHash = TestHash (f NE.:| h) } = tb
    { tbHash = TestHash (g f NE.:| h)
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

treePreferredChain :: NodeConfig (Bft BftMockCrypto)
                   -> BlockTree -> Chain TestBlock
treePreferredChain cfg = fromMaybe Genesis
                       . selectUnvalidatedChain cfg Genesis
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

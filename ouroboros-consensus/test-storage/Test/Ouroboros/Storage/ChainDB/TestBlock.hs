{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Minimal instantiation of the consensus layer to be able to run the ChainDB
module Test.Ouroboros.Storage.ChainDB.TestBlock (
    -- * Blocks
    TestHash(..)
  , TestBlock(..)
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
  , testConfig
    -- * Support for tests
  , Permutation(..)
  , permute
  ) where

import           Codec.Serialise (Serialise(encode))
import           Control.Monad.Except (throwError)
import           Data.FingerTree (Measured (..))
import           Data.Int
import           Data.List (transpose)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import           Data.Word
import qualified System.Random as R
import           Test.QuickCheck

import           Ouroboros.Network.Block (ChainHash (..))
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

{-------------------------------------------------------------------------------
  Test infrastructure: test block
-------------------------------------------------------------------------------}

newtype TestHash = TestHash Word64
  deriving newtype (Show, Eq, Ord, Serialise, Num)


data TestBlock = TestBlock {
      tbHash     :: TestHash
    , tbPrevHash :: ChainHash TestBlock
    , tbNo       :: Block.BlockNo
    , tbSlot     :: Block.SlotNo
    }
  deriving (Show, Eq)

instance Block.HasHeader TestBlock where
  type HeaderHash TestBlock = TestHash

  blockHash      = tbHash
  blockPrevHash  = tbPrevHash
  blockSlot      = tbSlot
  blockNo        = tbNo
  blockInvariant = const True

instance Block.StandardHash TestBlock

instance Measured Block.BlockMeasure TestBlock where
  measure = Block.blockMeasure

{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol TestBlock = Bft BftMockCrypto

instance HasPreHeader TestBlock where
  type PreHeader TestBlock = ()
  blockPreHeader _ = ()

instance HasPayload (Bft BftMockCrypto) TestBlock where
  blockPayload _ _ = BftPayload {
        bftSignature = SignedDSIGN (mockSign encode () (SignKeyMockDSIGN 0))
      }

instance UpdateLedger TestBlock where
  data LedgerState TestBlock =
      TestLedger {
          -- The ledger state simply consists of the last applied block
          lastApplied :: ChainHash TestBlock
        }
    deriving (Show)

  data LedgerError TestBlock =
      -- | The only error possible is that hashes don't line up
      InvalidHash {
          expectedHash :: ChainHash TestBlock
        , invalidHash  :: ChainHash TestBlock
        }
    deriving (Show)

  applyLedgerState TestBlock{..} TestLedger{..} =
      if tbPrevHash == lastApplied
        then return     $ TestLedger (BlockHash tbHash)
        else throwError $ InvalidHash lastApplied tbPrevHash

instance ProtocolLedgerView TestBlock where
  protocolLedgerView _ _ = ()

testInitLedger :: LedgerState TestBlock
testInitLedger = TestLedger GenesisHash

testInitExtLedger :: ExtLedgerState TestBlock
testInitExtLedger = ExtLedgerState {
      ledgerState         = testInitLedger
    , ouroborosChainState = ()
    }

-- | Trivial test configuration with a single core node
testConfig :: NodeConfig (Bft BftMockCrypto)
testConfig = BftNodeConfig {
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
  Chain of blocks
-------------------------------------------------------------------------------}

newtype BlockChain = BlockChain [TestHash]
  deriving (Show)

blockChain :: BlockChain -> Chain TestBlock
blockChain = Chain.fromOldestFirst . chainToBlocks

chainToBlocks :: BlockChain -> [TestBlock]
chainToBlocks (BlockChain c) = go 1 GenesisHash c
  where
    go :: Word64 -> ChainHash TestBlock -> [TestHash] -> [TestBlock]
    go _ _    []          = []
    go n prev (this:rest) = b : go (n + 1) (BlockHash this) rest
      where
        b :: TestBlock
        b = TestBlock { tbHash     = this
                      , tbPrevHash = prev
                      , tbNo       = Block.BlockNo n
                      , tbSlot     = Block.SlotNo  n
                      }

instance Arbitrary BlockChain where
  arbitrary = sized $ \n ->
    BlockChain <$> return (map TestHash [1 .. fromIntegral n])
  shrink (BlockChain c) =
    BlockChain <$> shrinkList (const []) c

{-------------------------------------------------------------------------------
  Tree of blocks
-------------------------------------------------------------------------------}

newtype BlockTree = BlockTree (Tree TestHash)

blockTree :: BlockTree -> Tree TestBlock
blockTree (BlockTree t) = go 1 GenesisHash t
  where
    go :: Word64 -> ChainHash TestBlock -> Tree TestHash -> Tree TestBlock
    go n prev (Node this ts) =
        Node b $ map (go (n + 1) (BlockHash this)) ts
      where
        b :: TestBlock
        b = TestBlock { tbHash     = this
                      , tbPrevHash = prev
                      , tbNo       = Block.BlockNo n
                      , tbSlot     = Block.SlotNo  n
                      }

treeToBlocks :: BlockTree -> [TestBlock]
treeToBlocks = Tree.flatten . blockTree

treeToChains :: BlockTree -> [Chain TestBlock]
treeToChains = map Chain.fromOldestFirst . allPaths . blockTree

treePreferredChain :: BlockTree -> Chain TestBlock
treePreferredChain = fromMaybe Genesis
                   . selectUnvalidatedChain
                       testConfig
                       Genesis
                   . treeToChains

instance Show BlockTree where
  show (BlockTree t) = Tree.drawTree (fmap show t)

instance Arbitrary BlockTree where
  arbitrary = sized $ \n ->
      BlockTree <$> mkTree 0.2 (map TestHash [1 .. max 1 (fromIntegral n)])
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

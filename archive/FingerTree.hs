{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Chain
    ( ChainFragment (..)
    , Chain
    , emptyChain
    , singleton
    , fromList
    , ChainMeasure (..)
    , lookupBySlot
    , lookupByIndexFromEnd
    , splitBeforeSlot
    , findIntersection
    , intersectChains
    , successorBlock

    , addBlock
    , rollback
    , drop
    , take
    , append
    , length
    , pointOnChain

    , selectChain

    , applyChainUpdate
    , applyChainUpdates

    , chainHead
    , chainGenesis
    , chainHeadBlockId
    , chainHeadSlot
    , chainHeadPoint
    , chainBackwardsFrom

    -- testing
    , reifyChainFragment
    , absChainFragment
    , genChain
    , validChain
    , validChainFragment
    , chain

    , AddBlockTest (..)
    , TestChain (..)
    , ChainFork (..)
    , prop_ChainFork
    , prop_addBlock
    , prop_drop
    , prop_take
    , prop_append
    , prop_reifyChainFragment
    , prop_absChainFragment
    , invChain
    , prop_TestChain
    , runTests

    )
    where

import           Prelude hiding (drop, length, take)

import           Data.FingerTree (FingerTree, Measured (..), SearchResult (..), ViewL (..),
                                  ViewR (..), (<|), (|>))
import qualified Data.FingerTree as FT
import qualified Data.List as L

import           Test.QuickCheck hiding ((><))

import           Block (Block (..), BlockId, Point, Slot, blockPoint, genBlock,
                        genNBlocks)
import qualified Chain.Abstract as Chain.Abs
import           Chain.Update (ChainUpdate (..))

{-# ANN module "HLint: ignore Reduce duplication" #-}

--
-- Blockchain fragment data type.
--

-- |
-- The chain grows to the right, it should never contain a block with slot `0`
-- (it will not be possible to find it with `lookupBySlot`, since `minBound
-- @Word == 0`.
newtype ChainFragment = ChainFragment (FingerTree ChainMeasure Block)
  deriving (Show, Eq)

type Chain = ChainFragment

data ChainMeasure = ChainMeasure {
       minSlot :: !Slot,
       maxSlot :: !Slot,
       size    :: !Int
     }
  deriving Show

instance Monoid ChainMeasure where
  mempty = ChainMeasure maxBound minBound 0
  mappend vl vr =
    ChainMeasure (min (minSlot vl) (minSlot vr))
                 (max (maxSlot vl) (maxSlot vr))
                 (size vl + size vr)

instance Measured ChainMeasure Block where
  measure Block{blockSlot} = ChainMeasure blockSlot blockSlot 1

emptyChain :: Chain
emptyChain = ChainFragment FT.empty

singleton :: Block -> ChainFragment
singleton = ChainFragment . FT.singleton

fromList :: [Block] -> ChainFragment
fromList = ChainFragment . FT.fromList

-- |
-- It assumes the chain is growing to the right.
lookupBySlot :: ChainFragment -> Slot -> FT.SearchResult ChainMeasure Block
lookupBySlot (ChainFragment t) s =
    FT.search (\vl vr -> maxSlot vl >= s && minSlot vr >= s) t

chain :: ChainFragment
chain = ChainFragment $ FT.fromList [Block 1 0 1 "", Block 2 1 2 "", Block 3 2 3 "", Block 4 3 10 "", Block 5 4 20 ""]

lookupByIndexFromEnd :: ChainFragment -> Int -> FT.SearchResult ChainMeasure Block
lookupByIndexFromEnd (ChainFragment t) n =
    FT.search (\vl vr -> size vl >= len - n && size vr <= n) t
  where
    len = size (measure t)

-- |
-- Find next block after the given point
successorBlock :: Point -> ChainFragment -> Maybe Block
successorBlock p cf = case lookupBySlot cf (fst p) of
    Position _ b ft'
        | blockPoint b == p
        -> case FT.viewl ft' of
            n :< _ -> Just n
            EmptyL -> Nothing
    _ -> Nothing

selectChain
    :: Chain
    -> Chain
    -> Chain
selectChain c1 c2 =
    if length c1 >= length c2
        then c1
        else c2

splitBeforeSlot :: ChainFragment -> Slot -> (ChainFragment, ChainFragment)
splitBeforeSlot (ChainFragment t) s =
    (\(l, r) -> (ChainFragment l, ChainFragment r))
  $ FT.split (\v -> maxSlot v >= s) t

findIntersection :: Chain -> Point -> [Point] -> Maybe Point
findIntersection c hpoint points =
    go (hpoint : points)
  where
    go [] = Nothing
    go (p:ps)
        | pointOnChain c p = Just p
        | otherwise        = go ps

intersectChains :: Chain -> Chain -> Maybe Point
intersectChains c (ChainFragment t) =
  case FT.viewr t of
    EmptyR -> Nothing
    t' :> b  ->
      let p = blockPoint b
      in if pointOnChain c p
        then Just p
        else intersectChains c (ChainFragment t')

data AddBlockTest = AddBlockTest Chain Block
  deriving Show

instance Arbitrary AddBlockTest where
  arbitrary = do
    Positive n <- arbitrary
    chain <- genChain n
    let Just h = chainHead chain
    block <- genBlock (blockId h) (blockSlot h)
    return $ AddBlockTest chain block

addBlock :: Block -> Chain -> Chain
addBlock b (ChainFragment ft) = ChainFragment (ft |> b)

prop_addBlock :: AddBlockTest -> Bool
prop_addBlock (AddBlockTest c b) =
    b : absChainFragment c == absChainFragment (addBlock b c)

rollback :: Point -> Chain -> Chain
rollback p (ChainFragment c) =
    ChainFragment (go c)
  where
    go v = case FT.viewr v of
        EmptyR  -> v
        v' :> b | blockPoint b == p -> v
                | otherwise         -> go v'

drop :: Int -> Chain -> Chain
drop n (ChainFragment t)
  | n <= 0    = ChainFragment t
  | otherwise = case FT.viewr t of
      EmptyR  -> ChainFragment t
      t' :> _ -> drop (n - 1) (ChainFragment t')

prop_drop :: Int -> Chain.Abs.TestChain -> Bool
prop_drop n (Chain.Abs.TestChain c) =
    L.drop n c == absChainFragment (drop n $ reifyChainFragment c)

-- Take from the right side (oldest)
take :: Int -> Chain -> Chain
take n (ChainFragment ft) = ChainFragment $ FT.takeUntil (\v -> size v > n) ft

prop_take :: Int -> TestChain -> Bool
prop_take n (TestChain c) =
    (L.reverse $ L.take n $ L.reverse $ (absChainFragment c)) == absChainFragment (take n $ c)

append :: Chain -> [Block] -> Chain
append (ChainFragment r) bs = ChainFragment (L.foldl' (|>) r bs)

length :: ChainFragment -> Int
length (ChainFragment ft) = size (measure ft)

pointOnChain :: Chain -> Point -> Bool
pointOnChain (ChainFragment ft) p = go ft
    where
    -- recursivelly search the fingertree from the right
    go t = case FT.viewr t of
        EmptyR                      -> False
        t' :> b | blockPoint b == p -> True
                | otherwise         -> go t'

data AppendTest = AppendTest Chain [Block]
  deriving Show

instance Arbitrary AppendTest where
  arbitrary = do
    Positive n <- arbitrary
    chain <- genChain n

    let Just h = chainHead chain
    NonNegative k <- arbitrary
    blocks <- genNBlocks k (blockId h) (blockSlot h + 1)

    return $ AppendTest chain blocks

  -- TODO: shrink is to large
  shrink (AppendTest chain blocks) =
    let len = L.length blocks
    in AppendTest chain `map` (L.take (len - 1) $ L.inits blocks)

prop_append :: AppendTest -> Bool
prop_append (AppendTest chain bs) =
    bs ++ absChainFragment chain == absChainFragment (chain `append` reverse bs)

chainGenesis :: Chain -> Maybe Block
chainGenesis (ChainFragment ft) = case FT.viewl ft of
    EmptyL -> Nothing
    b :< _ -> Just b

chainHead :: Chain -> Maybe Block
chainHead (ChainFragment ft) = case FT.viewr ft of
    EmptyR -> Nothing
    _ :> b -> Just b

chainHeadBlockId :: Chain -> BlockId
chainHeadBlockId = maybe 0 blockId . chainHead

chainHeadSlot :: Chain -> Slot
chainHeadSlot = maybe 0 blockSlot . chainHead

chainHeadPoint :: Chain -> Point
chainHeadPoint c = (chainHeadSlot c, chainHeadBlockId c)

-- This is the key operation on chains in this model
applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock b) c = addBlock b c
applyChainUpdate (RollBack p) c = rollback p c

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

chainBackwardsFrom :: Chain -> BlockId -> Chain
chainBackwardsFrom c bid = go c
    where
    go :: Chain -> Chain
    go c@(ChainFragment ft) = case FT.viewr ft of
        EmptyR   -> c
        ft' :> b | blockId b == bid -> ChainFragment (ft' |> b)
                 | otherwise        -> go (ChainFragment ft')

reifyChainFragment :: Chain.Abs.ChainFragment -> ChainFragment
reifyChainFragment = fromList . reverse

prop_reifyChainFragment :: Chain.Abs.TestChain -> Bool
prop_reifyChainFragment (Chain.Abs.TestChain bs) =
    absChainFragment (reifyChainFragment bs) == bs

-- |
-- Note the `foldl'`, this is that it's easy to append new blocks to the
-- abstract representation.
absChainFragment :: ChainFragment -> Chain.Abs.ChainFragment
absChainFragment (ChainFragment ft) = L.foldl' (flip (:)) [] ft

prop_absChainFragment :: TestChain -> Bool
prop_absChainFragment (TestChain cf) =
    reifyChainFragment (absChainFragment cf) == cf

validChain :: Chain -> Bool
validChain = Chain.Abs.validChain . absChainFragment

validChainFragment :: ChainFragment -> Bool
validChainFragment = Chain.Abs.validChainFragment . absChainFragment

genChain :: Int -> Gen Chain
genChain n = reifyChainFragment <$> Chain.Abs.genChain n

newtype TestChain = TestChain Chain
    deriving Show

instance Arbitrary TestChain where
    arbitrary = do
      Positive n <- arbitrary
      TestChain <$> genChain n
    shrink (TestChain cf) =
      [ TestChain (reifyChainFragment cf')
      | cf' <- L.take (length cf) $ L.inits $ absChainFragment cf
      ]

-- |
-- Not null chains, which have a common prefix.
data ChainFork = ChainFork Chain Chain
    deriving Show

instance Arbitrary ChainFork where
    arbitrary = do
        Positive n <- arbitrary
        chain <- genChain n
        let Just h = chainHead chain

        -- at least 5% of forks should be equal
        equalChains <- frequency [(1, return True), (19, return False)]
        if equalChains
          then return $ ChainFork chain chain
          else do
            Positive k <- arbitrary
            bs1 <- genNBlocks k (blockId h) (blockSlot h + 1)
            let chain1 = foldr addBlock chain bs1

            Positive l <- arbitrary
            bs2 <- genNBlocks l (blockId h) (blockSlot h + 1)
            let chain2 = foldr addBlock chain bs2

            return $ ChainFork chain1 chain2

    shrink (ChainFork c d) =
      [ ChainFork (fromList c') d
      | c' <- L.take (length c - 1) $ L.inits $ L.reverse $ absChainFragment c
      , not (null c')
      ] ++
      [ ChainFork c (fromList d')
      | d' <- L.take (length d - 1) $ L.inits $ L.reverse $ absChainFragment d
      , not (null d')
      ] ++
      [ChainFork (drop 1 c) (drop 1 d) | length c > 1 && length d > 1]


-- Test Chain fork distribution
-- 5% forks equal
-- 5% forks of equal length
prop_ChainFork :: ChainFork -> Property
prop_ChainFork (ChainFork pchain cchain) =
  let plen = Chain.length pchain
      clen = Chain.length cchain
  in withMaxSuccess 1000
    $ cover ( 3/100) (pchain == cchain) "chains are equal"
    $ cover (38/100) (plen >  clen) "producer chain is longer"
    $ cover ( 3/100) (plen == clen) "chains of equal length"
    $ cover (38/100) (clen <  plen) "consumer chain is longer"
    $    counterexample (show pchain) (validChain pchain)
    .&&. counterexample (show cchain) (validChain cchain)

prop_TestChain :: TestChain -> Bool
prop_TestChain (TestChain chain) = validChain chain

-- |
-- TODO: like 'Chain.Volatile.invChainState'
invChain :: Chain -> Bool
invChain = undefined

return []
runTests = $quickCheckAll

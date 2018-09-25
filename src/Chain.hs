{-# LANGUAGE DeriveFunctor #-}
-- | Reference implementation of a representation of a block chain
--
module Chain where

import Prelude hiding (head, drop)

import Block ( Block(..), BlockHeader(..), HasHeader(..)
             , Slot(..), BlockNo (..), HeaderHash(..)
             , genBlock, genNBlocks
             {-, BlockId, invBlock , Point, Slot, blockPoint,
             - pointSlot, pointHash-} )
import qualified Chain.Abstract as Chain.Abs

import Control.Exception (assert)
import qualified Data.List as L

import Test.QuickCheck

--
-- Blockchain type
--

data Chain block = Genesis | Chain block :> block
  deriving (Eq, Show, Functor)

infixl 5 :>

toList :: Chain block -> [block]
toList Genesis  = []
toList (c :> b) = b : toList c

fromList :: [block] -> Chain block
fromList = L.foldl' (:>) Genesis

--
-- Points on blockchains
--

-- | A point on the chain is identified by its 'Slot' and 'HeaderHash'.
--
-- The 'Slot' tells us where to look and the 'HeaderHash' either simply servesk
-- as a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
data Point = Point {
       pointSlot :: Slot,
       pointHash :: HeaderHash
     }
  deriving (Eq, Show)

blockPoint :: HasHeader block => block -> Point
blockPoint b =
    Point {
      pointSlot = blockSlot b,
      pointHash = blockHash b
    }


genesis :: Chain b
genesis = Genesis

genesisSlot :: Slot
genesisSlot = Slot 0

genesisHash :: HeaderHash
genesisHash = HeaderHash 0

genesisBlockNo :: BlockNo
genesisBlockNo = BlockNo 0

genesisPoint :: Point
genesisPoint = Point genesisSlot genesisHash


valid :: HasHeader block => Chain block -> Bool
valid Genesis  = True
valid (c :> b) = valid c && validChain c

validExtension ::  HasHeader block => Chain block -> block -> Bool
validExtension c b = blockInvariant b
                  && headHash c == blockPrevHash b
                  && headSlot c <  blockSlot b

validChain :: HasHeader block => Chain block -> Bool
validChain Genesis   = True
validChain (bs :> b) = validExtension bs b && validChain bs

invChain :: HasHeader block => Chain block -> Bool
invChain _ = undefined

head :: Chain b -> Maybe b
head Genesis  = Nothing
head (c :> b) = Just b

drop :: Int -> Chain b -> Chain b
drop 0 c = c
drop n Genesis  = Genesis
drop n (c :> _) = drop (n - 1) c

headPoint :: HasHeader block => Chain block -> Point
headPoint Genesis  = genesisPoint
headPoint (_ :> b) = blockPoint b

headSlot :: HasHeader block => Chain block -> Slot
headSlot = pointSlot . headPoint

headHash :: HasHeader block => Chain block -> HeaderHash
headHash = pointHash . headPoint

headBlockNo :: HasHeader block => Chain block -> BlockNo
headBlockNo Genesis  = genesisBlockNo
headBlockNo (_ :> b) = blockNo b

addBlock :: HasHeader block => block -> Chain block -> Chain block
addBlock b c = assert (validExtension c b) $
               c :> b

pointOnChain :: HasHeader block => Chain block -> Point -> Bool
pointOnChain Genesis  p = p == genesisPoint
pointOnChain (c :> b) p = p == blockPoint b || pointOnChain c p

rollback :: HasHeader block => Point -> Chain block -> Chain block
rollback p c@(c' :> b) | blockPoint b == p = c
                       | otherwise         = rollback p c'
rollback p Genesis  | p == genesisPoint = Genesis
                    | otherwise         = error "rollback: point not on chain"

successorBlock :: HasHeader block => Point -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 = go c0
  where
    go (c :> b' :> b) | blockPoint b' == p = Just b
                      | otherwise          = go (c :> b')
    go _ = error "successorBlock: point not on chain"

selectChain
  :: HasHeader block
  => Chain block
  -> Chain block
  -> Chain block
selectChain c1 c2 = 
  if headBlockNo c1 >= headBlockNo c2
    then c1
    else c2

lookupBySlot
  :: HasHeader block
  => Chain block
  -> Slot
  -> Maybe block
lookupBySlot Genesis  slot = Nothing
lookupBySlot (c :> b) slot | blockSlot b == slot = Just b
                           | blockSlot b < slot  = Nothing
                           | otherwise           = lookupBySlot c slot

data ChainUpdate block = AddBlock block
                       | RollBack Point
  deriving Show

applyChainUpdate :: HasHeader block
                 => ChainUpdate block
                 -> Chain block
                 -> Chain block
applyChainUpdate (AddBlock b) c = addBlock b c
applyChainUpdate (RollBack p) c = rollback p c

applyChainUpdates :: HasHeader block
                  => [ChainUpdate block]
                  -> Chain block
                  -> Chain block
applyChainUpdates = flip (foldl (flip applyChainUpdate))

findIntersection
  :: HasHeader block
  => Chain block
  -> Point
  -> [Point]
  -> Maybe Point
findIntersection c hpoint points =
    go (hpoint : points)
  where
    go [] = Nothing
    go (p:ps)
        | pointOnChain c p = Just p
        | otherwise        = go ps

intersectChains
  :: HasHeader block
  => Chain block
  -> Chain block
  -> Maybe Point
intersectChains _ Genesis   = Nothing
intersectChains c (bs :> b) =
  let p = blockPoint b
  in if pointOnChain c (blockPoint b)
       then Just p
       else intersectChains c bs

absChainFragment :: Chain Block -> Chain.Abs.Chain
absChainFragment = toList

reifyChainFragment :: Chain.Abs.Chain -> Chain Block
reifyChainFragment = L.foldl' (:>) Genesis

absApplyChainUpdate :: ChainUpdate Block -> Chain.Abs.Chain -> Chain.Abs.Chain
absApplyChainUpdate (AddBlock     b)  c = b:c
absApplyChainUpdate (RollBack p)      c = go c
    where
    go [] = []
    go (b : bs) | blockPoint b == p = b : bs
                | otherwise         = go bs

absApplyChainUpdates :: [ChainUpdate Block] -> Chain.Abs.Chain -> Chain.Abs.Chain
absApplyChainUpdates = flip (foldl (flip absApplyChainUpdate))

--
-- Generators
--

newtype TestBlockChain = TestBlockChain (Chain Block)
    deriving (Show, Eq)

instance Arbitrary TestBlockChain where
    arbitrary = do
        Positive n <- arbitrary
        TestBlockChain <$> genChain n
    shrink (TestBlockChain c) = TestBlockChain <$> (L.drop 1 $ inits c)
      where
      inits :: Chain block -> [Chain block]
      inits Genesis     = [Genesis]
      inits c@(bs :> _) = c : inits bs

genChain :: Int -> Gen (Chain Block)
genChain n = L.foldr (flip (:>)) Genesis <$> genNBlocks n genesisHash (succ genesisSlot) (succ genesisBlockNo)

-- make chain by making bodies and deriving the hashes
-- shrinking by remaking the hashes

--prop_addBlock:

data AddBlockTest = AddBlockTest (Chain Block) Block
  deriving Show

instance Arbitrary AddBlockTest where
  arbitrary = do
    Positive n <- arbitrary
    chain <- genChain n
    let Just h = Chain.head chain
    block <- genBlock (blockHash h) (succ $ blockSlot h) (succ $ blockNo h)
    return $ AddBlockTest chain block

  shrink (AddBlockTest Genesis  _)  = []
  shrink (AddBlockTest (c :> b) _) = [AddBlockTest c b]

prop_addBlock :: AddBlockTest -> Property
prop_addBlock t@(AddBlockTest c b) =
  let c' = addBlock b c
  in
    -- after adding a block, that block is at the head
       headPoint c' === blockPoint b
    -- chain is still valid
    .&&. valid c'
    -- removing the block gives the original
    .&&. rollback (headPoint c) c' === c

data RollbackTest = RollbackTest (Chain Block) Point
  deriving Show

instance Arbitrary RollbackTest where
  arbitrary = do
    Positive n <- arbitrary
    chain <- genChain n
    -- choose point from the chain
    idx <- choose (0, fromIntegral n)
    let p = headPoint $ drop idx chain
    return $ RollbackTest chain p

  shrink (RollbackTest c p) | headPoint c == p = []
                            | otherwise = [RollbackTest (drop 1 c) p]

prop_rollback :: RollbackTest -> Property
prop_rollback (RollbackTest c p) = 
  let c' = rollback p c
  in
    -- chain is a prefix of original
       isPrefix c' c
    -- chain head point is the rollback point
    .&&. headPoint c' === p
  where
  isPrefix (_ :> _) Genesis = False
  isPrefix c c' | c == c'   = True
                | otherwise = isPrefix c (drop 1 c')

data ChainFork = ChainFork (Chain Block) (Chain Block)
  deriving Show

instance Arbitrary ChainFork where
  arbitrary = do
    Positive n <- arbitrary
    chain <- genChain n
    let Just h = head chain
    -- at least 5% of forks should be equal
    equalChains <- frequency [(1, return True), (19, return False)]
    if equalChains
      then return $ ChainFork chain chain
      else do
        Positive k <- arbitrary
        bs1 <- genNBlocks k (blockHash h) (succ $ blockSlot h) (blockNo h)
        let chain1 = foldr addBlock chain bs1

        Positive l <- arbitrary
        bs2 <- genNBlocks l (blockHash h) (succ $ blockSlot h) (blockNo h)
        let chain2 = foldr addBlock chain bs2

        return $ ChainFork chain1 chain2

  shrink (ChainFork c d) =
    let c_ = toList c
        d_ = toList d
    in 
    [ ChainFork (fromList c') d
    | c' <- L.take (length c_ - 1) $ L.inits $ L.reverse $ c_
    , not (null c')
    ] ++
    [ ChainFork c (fromList d')
    | d' <- L.take (length d_ - 1) $ L.inits $ L.reverse $ d_
    , not (null d')
    ] ++
    case (c, d) of
      (c' :> _, d' :> _) -> [ChainFork c' d']
      _                  -> []

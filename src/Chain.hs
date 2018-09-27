{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}

-- | Reference implementation of a representation of a block chain
--
module Chain where

import Prelude hiding (head, drop)

import Block ( Block(..), BlockHeader(..), HasHeader(..)
             , Slot(..), BlockNo (..), HeaderHash(..)
             , genBlock, genNBlocks
             , BlockBody(..), BodyHash(..)
             , Slot(..), BlockNo(..), BlockSigner(..)
             , HeaderHash(..), hashHeader, hashBody )
import qualified Chain.Abstract as Chain.Abs

import Control.Exception (assert)
import qualified Data.List as L
import Data.Maybe (fromMaybe)

import Test.QuickCheck

--
-- Blockchain type
--

data Chain block = Genesis | Chain block :> block
  deriving (Eq, Show, Functor)

infixl 5 :>

foldChain :: (a -> b -> a) -> a -> Chain b -> a
foldChain _blk gen Genesis  = gen
foldChain  blk gen (c :> b) = blk (foldChain blk gen c) b

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

genPoint :: Gen Point
genPoint = (\s h -> Point (Slot s) (HeaderHash h)) <$> arbitrary <*> arbitrary

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
valid (c :> b) = valid c && validExtension c b

validExtension ::  HasHeader block => Chain block -> block -> Bool
validExtension c b = blockInvariant b
                  && headHash c == blockPrevHash b
                  && headSlot c <  blockSlot b
                  && headBlockNo c == pred (blockNo b)

head :: Chain b -> Maybe b
head Genesis  = Nothing
head (_ :> b) = Just b

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

-- | Produce the list of blocks, from most recent back to genesis
--
toList :: Chain block -> [block]
toList = foldChain (flip (:)) []

-- | Make a chain from a list of blocks. The head of the list is the head
-- of the chain.
--
fromList :: HasHeader block => [block] -> Chain block
fromList bs = assert (valid c) c
  where
    c = foldr (flip (:>)) Genesis bs

drop :: Int -> Chain block -> Chain block
drop 0 c        = c
drop _ Genesis  = Genesis
drop n (c :> _) = drop (n - 1) c

length :: Chain block -> Int
length = foldChain (\n _ -> n+1) 0

addBlock :: HasHeader block => block -> Chain block -> Chain block
addBlock b c = assert (validExtension c b) $
               c :> b

pointOnChain :: HasHeader block => Point -> Chain block -> Bool
pointOnChain p Genesis  = p == genesisPoint
pointOnChain p (c :> b) = p == blockPoint b || pointOnChain p c

rollback :: HasHeader block => Point -> Chain block -> Maybe (Chain block)
rollback p (c :> b) | blockPoint b == p = Just (c :> b)
                    | otherwise         = rollback p c
rollback p Genesis  | p == genesisPoint = Just Genesis
                    | otherwise         = Nothing

successorBlock :: HasHeader block => Point -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 = go c0
  where
    go (c :> b' :> b) | blockPoint b' == p = Just b
                      | otherwise          = go (c :> b')
    go (Genesis :> b) | p == genesisPoint  = Just b
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
applyChainUpdate (RollBack p) c = fromMaybe c $ rollback p c

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
        | pointOnChain p c = Just p
        | otherwise        = go ps

intersectChains
  :: HasHeader block
  => Chain block
  -> Chain block
  -> Maybe Point
intersectChains _ Genesis   = Nothing
intersectChains c (bs :> b) =
  let p = blockPoint b
  in if pointOnChain (blockPoint b) c
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
-- Generators for chains
--

newtype TestBlockChain = TestBlockChain (Chain Block)
    deriving (Eq, Show)

newtype TestHeaderChain = TestHeaderChain (Chain BlockHeader)
    deriving (Eq, Show)

instance Arbitrary TestBlockChain where
    arbitrary = do
        NonNegative n <- arbitrary
        TestBlockChain <$> genBlockChain n

    shrink (TestBlockChain c) =
        [ TestBlockChain (fromListFixupBlocks c')
        | c' <- shrinkList (const []) (toList c) ]

instance Arbitrary TestHeaderChain where
    arbitrary = do
        NonNegative n <- arbitrary
        TestHeaderChain <$> genHeaderChain n

    shrink (TestHeaderChain c) =
        [ TestHeaderChain (fromListFixupHeaders c')
        | c' <- shrinkList (const []) (toList c) ]

prop_arbitrary_TestBlockChain :: TestBlockChain -> Bool
prop_arbitrary_TestBlockChain (TestBlockChain c) = valid c

prop_arbitrary_TestHeaderChain :: TestHeaderChain -> Bool
prop_arbitrary_TestHeaderChain (TestHeaderChain c) = valid c

prop_shrink_TestBlockChain :: TestBlockChain -> Bool
prop_shrink_TestBlockChain c =
    and [ valid c' | TestBlockChain c' <- shrink c ]

prop_shrink_TestHeaderChain :: TestHeaderChain -> Bool
prop_shrink_TestHeaderChain c =
    and [ valid c' | TestHeaderChain c' <- shrink c ]

genBlockChain :: Int -> Gen (Chain Block)
genBlockChain n = do
    bodies <- vector n
    slots  <- mkSlots <$> vectorOf n genSlotGap
    return (mkChain slots bodies)
  where
    mkSlots :: [Int] -> [Slot]
    mkSlots = map toEnum . tail . scanl (+) 0

    mkChain :: [Slot] -> [BlockBody] -> Chain Block
    mkChain slots bodies =
        fromListFixupBlocks
      . reverse
      $ zipWith mkPartialBlock slots bodies

genSlotGap :: Gen Int
genSlotGap = frequency [(25, pure 1), (5, pure 2), (1, pure 3)]

addSlotGap :: Int -> Slot -> Slot
addSlotGap g (Slot n) = Slot (n + fromIntegral g)

genHeaderChain :: Int -> Gen (Chain BlockHeader)
genHeaderChain = fmap (fmap blockHeader) . genBlockChain

mkPartialBlock :: Slot -> BlockBody -> Block
mkPartialBlock sl body =
    Block {
      blockHeader = BlockHeader {
        headerSlot     = sl,
        headerSigner   = expectedBFTSigner sl,
        headerHash     = partialField "headerHash",
        headerPrevHash = partialField "headerPrevHash",
        headerBlockNo  = partialField "headerBlockNo",
        headerBodyHash = hashBody body
      }
    , blockBody = body
    }
  where
    partialField n = error ("mkPartialBlock: you didn't fill in field " ++ n)

expectedBFTSigner :: Slot -> BlockSigner
expectedBFTSigner (Slot n) = BlockSigner (n `mod` 7)


-- | To help with chain construction and shrinking it's handy to recalculate
-- all the hashes.
--
fromListFixupBlocks :: [Block] -> Chain Block
fromListFixupBlocks []      = Genesis
fromListFixupBlocks (b : c) = c' :> b'
  where
    c' = fromListFixupBlocks c
    b' = fixupBlock (headPoint c') (headBlockNo c') b

fromListFixupHeaders :: [BlockHeader] -> Chain BlockHeader
fromListFixupHeaders []      = Genesis
fromListFixupHeaders (b : c) = c' :> b'
  where
    c' = fromListFixupHeaders c
    b' = fixupBlockHeader (headPoint c') (headBlockNo c')
                          (headerBodyHash b) b

fixupBlock :: Point -> BlockNo -> Block -> Block
fixupBlock p bn b@Block{blockBody, blockHeader} =
    b { blockHeader = fixupBlockHeader p bn (hashBody blockBody) blockHeader }

fixupBlockHeader :: Point -> BlockNo -> BodyHash -> BlockHeader -> BlockHeader
fixupBlockHeader p n h b = b'
  where
    b' = BlockHeader {
      headerHash     = hashHeader b',
      headerPrevHash = pointHash p,
      headerSlot     = headerSlot b,   -- keep the existing slot number
      headerSigner   = headerSigner b, -- and signer
      headerBlockNo  = succ n,
      headerBodyHash = h
    }

k :: Int
k = 5

--
-- Generator for chain updates
--

data TestBlockChainAndUpdates =
       TestBlockChainAndUpdates (Chain Block) [ChainUpdate Block]
  deriving Show

instance Arbitrary TestBlockChainAndUpdates where
  arbitrary = do
    (NonNegative n, NonNegative m) <- arbitrary
    chain   <- genBlockChain n
    updates <- genChainUpdates chain m
    return (TestBlockChainAndUpdates chain updates)


genChainUpdate :: Chain Block -> Gen (ChainUpdate Block)
genChainUpdate chain = do
    let maxRollback = Chain.length chain `min` k
    n <- choose (-10, maxRollback)
    if n <= 0
      then AddBlock <$> genAddBlock chain
      else pure (RollBack (mkRollbackPoint chain n))

genAddBlock :: HasHeader block => Chain block -> Gen Block
genAddBlock chain = do
    slotGap <- genSlotGap
    body    <- arbitrary
    let pb = mkPartialBlock (addSlotGap slotGap (headSlot chain)) body
        b  = fixupBlock (headPoint chain) (headBlockNo chain) pb
    return b

mkRollbackPoint :: HasHeader block => Chain block -> Int -> Point
mkRollbackPoint chain n = headPoint $ drop n chain

genChainUpdates :: Chain Block -> Int -> Gen [ChainUpdate Block]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let chain' = applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)


--
-- Properties
--

data AddBlockTest = AddBlockTest (Chain Block) Block
  deriving Show

instance Arbitrary AddBlockTest where
  arbitrary = do
    NonNegative n <- arbitrary
    chain <- genBlockChain n
    block <- case Chain.head chain of
      Nothing -> genBlock genesisHash (succ genesisSlot) (succ genesisBlockNo)
      Just h  -> genBlock (blockHash h) (succ $ blockSlot h) (succ $ blockNo h)
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
    .&&. rollback (headPoint c) c' === Just c

data ChainWithPointTest = ChainWithPointTest (Chain Block) Point
  deriving Show

instance Arbitrary ChainWithPointTest where
  arbitrary = do
    NonNegative n <- arbitrary
    chain <- genBlockChain n
    point <- frequency
      [ (2, return (headPoint chain))
      , (2, return (mkRollbackPoint chain n))
      , (8, mkRollbackPoint chain <$> choose (1, fromIntegral n - 1))
      , (1, genPoint)
      ]
    return $ ChainWithPointTest chain point

  shrink (ChainWithPointTest c p)
    | pointOnChain p c = [ChainWithPointTest c' (fixupPoint c' p) | TestBlockChain c' <- shrink (TestBlockChain c)]
    | otherwise = [ChainWithPointTest c' p | TestBlockChain c' <- shrink (TestBlockChain c) ]

fixupPoint :: HasHeader block => Chain block -> Point -> Point
fixupPoint c p =
  case lookupBySlot c (pointSlot p) of
    Just b  -> blockPoint b
    Nothing -> headPoint c

prop_arbitrary_ChainWithPointTest :: ChainWithPointTest -> Bool
prop_arbitrary_ChainWithPointTest (ChainWithPointTest c p) =
  valid c

prop_shrink_ChainWithPointTest :: ChainWithPointTest -> Bool
prop_shrink_ChainWithPointTest cp@(ChainWithPointTest c _) =
  and [ valid c' && (not (pointOnChain p c) || pointOnChain p c') | ChainWithPointTest c' p <- shrink cp]

prop_rollback :: ChainWithPointTest -> Property
prop_rollback (ChainWithPointTest c p) =
  case rollback p c of
    Nothing -> property True
    Just c' ->
      -- chain is a prefix of original
           isPrefix c' c
      -- chain head point is the rollback point
      .&&. headPoint c' === p
  where
  isPrefix (_ :> _) Genesis = False
  isPrefix c c' | c == c'   = True
                | otherwise = isPrefix c (drop 1 c')

prop_successorBlock :: ChainWithPointTest -> Property
prop_successorBlock (ChainWithPointTest c p) =
  pointOnChain p c ==>
  case successorBlock p c of
    Nothing -> headPoint c === p
    Just b  -> property $ pointOnChain (blockPoint b) c

prop_lookupBySlot :: ChainWithPointTest -> Bool
prop_lookupBySlot (ChainWithPointTest c p) =
  case lookupBySlot c (pointSlot p) of
    Just b  -> pointOnChain (blockPoint b) c
    Nothing | p == genesisPoint -> True
            | otherwise         -> not (pointOnChain p c)

data ChainFork = ChainFork (Chain Block) (Chain Block)
  deriving Show

instance Arbitrary ChainFork where
  arbitrary = do
    NonNegative n <- arbitrary
    chain <- genBlockChain n
    let h = head chain
    -- at least 5% of forks should be equal
    equalChains <- frequency [(1, return True), (19, return False)]
    if equalChains
      then return $ ChainFork chain chain
      else do
        NonNegative k <- arbitrary
        bs1 <- genNBlocks k
          (maybe genesisHash blockHash h)
          (succ $ maybe genesisSlot blockSlot h)
          (succ $ maybe genesisBlockNo blockNo h)
        let chain1 = foldr addBlock chain bs1

        NonNegative l <- arbitrary
        bs2 <- genNBlocks l
          (maybe genesisHash blockHash h)
          (succ $ maybe genesisSlot blockSlot h)
          (succ $ maybe genesisBlockNo blockNo h)
        let chain2 = foldr addBlock chain bs2

        return $ ChainFork chain1 chain2

  shrink (ChainFork c d) =
    [ ChainFork c' d | TestBlockChain c' <- shrink (TestBlockChain c) ] ++
    [ ChainFork c d' | TestBlockChain d' <- shrink (TestBlockChain d) ]

prop_shrink_ChainFork :: ChainFork -> Property
prop_shrink_ChainFork f =
  withMaxSuccess 50 $
  all (\(ChainFork c1 c2) -> valid c1 && valid c2) (shrink f)

prop_intersectChains :: ChainFork -> Property
prop_intersectChains (ChainFork c1 c2) =
  case intersectChains c1 c2 of
    Nothing -> L.intersect (toList c1) (toList c2) === []
    Just p  -> counterexample (show (c1, c2, p)) $
           pointOnChain p c1
      .&&. pointOnChain p c2

return []
runTests = $quickCheckAll

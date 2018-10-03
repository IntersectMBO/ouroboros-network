{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ChainExperiment2 where

import           Data.List (find)

import           Block (Block (..), HasHeader (..), genBlock)
import           Chain (Chain, ChainUpdate (..), Point, absChainFragment, applyChainUpdate, absApplyChainUpdate, blockPoint, pointOnChain)
import qualified Chain
import qualified Chain.Abstract as Chain.Abs

-- import Control.Applicative
import           Control.Exception (assert)

import           Test.QuickCheck

validChainUpdate :: HasHeader block => ChainUpdate block -> Chain block -> Bool
validChainUpdate cu c = Chain.valid (applyChainUpdate cu c)

k :: Int
k = 5 -- maximum fork length in these tests

--
-- Generating valid chain updates
--

genChainUpdate :: Chain.Abs.Chain -> Gen (ChainUpdate Block)
genChainUpdate chain = do
    let maxRollback = length (take k chain)
    n <- choose (-10, maxRollback)
    if n <= 0
      then AddBlock <$> genBlock (Chain.Abs.chainHeaderHash chain)
                                 (succ $ Chain.Abs.chainHeadSlot chain)
                                 (Chain.Abs.chainHeadBlockNo chain)
      else return $ RollBack (blockPoint (head (drop (n - 1) chain)))

genChainUpdates :: Chain.Abs.Chain -> Int -> Gen [ChainUpdate Block]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let chain' = absApplyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

data TestChainAndUpdates = TestChainAndUpdates Chain.Abs.Chain [ChainUpdate Block]
  deriving Show

instance Arbitrary TestChainAndUpdates where
  arbitrary = do
    (Positive n, Positive m) <- arbitrary
    chain   <- Chain.Abs.genChain n
    updates <- genChainUpdates chain m
    return (TestChainAndUpdates chain updates)

prop_TestChainAndUpdates :: TestChainAndUpdates -> Bool
prop_TestChainAndUpdates (TestChainAndUpdates chain updates) =
    all Chain.valid chains
 && all (uncurry validChainUpdate) (zip updates chains)
  where
    chains = scanl (flip applyChainUpdate) (Chain.reifyChainFragment chain) updates

-- | This is now the simulation property covering both the add block and
-- switch fork operations.
--
prop_switchFork :: TestChainAndUpdates -> Bool
prop_switchFork (TestChainAndUpdates chain updates) =
    all Chain.valid chains'
    && all (\(c, c') -> c == absChainFragment c') (zip chains chains')
  where
    chains' = scanl
        (flip Chain.applyChainUpdate)
        (Chain.reifyChainFragment chain)
        updates
    chains  = scanl
        (flip absApplyChainUpdate)
        chain
        updates

--
-- Step 4: switching forks again! Roll back and roll forward.
--

-- For the chain following protocol we will need to relax the constraint that
-- we always switch fork all in one go (and to a longer chain), and have to
-- break it up into a rollback followed by adding more blocks.
--
-- Furthermore, it turns out for the chain following protocol that it works
-- better to specify the rollback in terms of where to roll back to, rather
-- than on how many blocks to roll back.

--rollBackToVolatile :: Point -> Volatile -> Volatile
--rollBackToVolatile _ (Volatile _ Nothing) =
--    error "rollBackToVolatile: precondition violation"

--rollBackToVolatile (slot, bid) (Volatile blocks (Just tip)) =


--
-- Read pointer operations
--

-- A 'ChainState' plus an associated set of readers/consumers of the chain.

data ChainProducerState rep = ChainProducerState {
       chainState   :: rep,
       chainReaders :: ReaderStates
     }
  deriving (Eq, Show)

-- | Readers are represented here as a relation.
--
type ReaderStates = [ReaderState]

type ReaderId     = Int
data ReaderState  = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the same as the 'readerHead',
       -- but if the consumer 'readerHead' is off the chain then this is the
       -- point the consumer will need to rollback to.
       readerIntersection :: Point,

       -- | Where the chain consumer was last reading from (typically the
       -- head of the consumer's chain). If this is on the producer chain
       -- then it is equal to the 'readerIntersection'.
       readerHead         :: Point,

       -- | A unique tag per reader, to distinguish different readers.
       readerId           :: ReaderId
     }
  deriving (Eq, Show)

-- | Readers are represented here as a relation.
invChainProducerState :: HasHeader block => ChainProducerState (Chain block) -> Bool
invChainProducerState (ChainProducerState cs rs) =
    Chain.valid cs
 && invReaderStates cs rs

-- like 'Chain.Volatile.invReaderState'
invReaderStates :: HasHeader block => Chain block -> readerState -> Bool
invReaderStates = undefined


{-
Hmm, perhaps this version does too much, lets simplify

initialiseReadPointer :: [Point]
                      -> ChainState
                      -> Maybe (ChainState, ReadPointer)
initialiseReadPointer checkpoints (ChainState v rs) = do
    (c, c') <- findIntersectionRange checkpoints
    let rs' = (c, readPtr, ) : rs
    return (ChainState v rs')
  where
    readPtr = freshReaderId rs

    findIntersectionRange cs =
      find (checkpointOnChain . fst)
           (zip cs (head cs ++ cs))

-}

-- Given a list of points, find the most recent pair such that older is on the
-- chain and the newer is not.
--
-- > [x, x'] `subseq` xs, not (onChain x), onChain x'
--


initialiseReader :: HasHeader block
                 => Point
                 -> Point
                 -> ChainProducerState (Chain block)
                 -> (ChainProducerState (Chain block), ReaderId)
initialiseReader hpoint ipoint (ChainProducerState cs rs) =
    assert (pointOnChain ipoint cs) $
    (ChainProducerState cs (r:rs), readerId r)
  where
    r = ReaderState {
          readerIntersection = ipoint,
          readerHead         = hpoint,
          readerId           = freshReaderId rs
        }

freshReaderId :: ReaderStates -> ReaderId
freshReaderId [] = 0
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]

updateReader :: HasHeader block
             => ReaderId
             -> Point       -- ^ new reader head pointer
             -> Maybe Point -- ^ new reader intersection pointer
             -> ChainProducerState (Chain block)
             -> ChainProducerState (Chain block)
updateReader rid hpoint mipoint (ChainProducerState cs rs) =
    ChainProducerState cs [ if readerId r == rid then update r else r
                          | r <- rs ]
  where
    update r = case mipoint of
      Nothing     -> r { readerHead = hpoint }
      Just ipoint -> assert (pointOnChain ipoint cs) $
                     r {
                       readerHead         = hpoint,
                       readerIntersection = ipoint
                     }

lookupReader :: ChainProducerState rep -> ReaderId -> ReaderState
lookupReader (ChainProducerState _ rs) rid = r
  where
    Just r = find (\r -> readerId r == rid) rs

-- |
-- Compute @'ConsumeChain'@ for the reader and optimistically update
-- @'ReaderState'@ inside @'ChainProducerState'@.
readerInstruction :: forall block . HasHeader block
                  => ChainProducerState (Chain block)
                  -> ReaderId
                  -> Maybe (ChainProducerState (Chain block), ConsumeChain block)
readerInstruction cps@(ChainProducerState cf _) rid =
  if readerHead == readerIntersection
  then
    maybe Nothing
      (Just . uncurry fn . (\b -> (blockPoint b, RollForward b)))
      (Chain.successorBlock readerIntersection cf)
  else
    Just $ fn readerIntersection (RollBackward readerIntersection)
  where
    ReaderState {readerHead, readerIntersection} = lookupReader cps rid

    fn :: Point
       -> ConsumeChain block
       -> (ChainProducerState (Chain block), ConsumeChain block)
    fn p cc =
        ( updateReader rid p (Just p) cps
        , cc
        )

{--
  - applyChainProducerUpdate :: ChainUpdate -> ChainProducerState -> ChainProducerState
  - applyChainProducerUpdate cu (cps@ChainProducerState {chainState})
  -     = (normalizeChainProducerState cu cps) { chainState = applyChainStateUpdate cu chainState }
  -
  - invApplyChainProducerUpdate :: ChainUpdate -> ChainProducerState ->  Bool
  - invApplyChainProducerUpdate cu cps = case applyChainProducerUpdate cu cps of
  -     ChainProducerState
  -         { chainState    = ChainState (Volatile blocks _)
  -         , chainReaders
  -         } -> and
  -             [
  -               -- all pointers should be still in volatile chain
  -               and [ Map.member intersectionBlockId blocks && Map.member readerBlockId blocks
  -                   | ReaderState
  -                       { readerIntersection = (_, intersectionBlockId)
  -                       , readerHead         = (_, readerBlockId)
  -                       } <- chainReaders
  -                   ]
  -             ]
  --}

data ConsumeChain block = RollForward  block
                        | RollBackward Point
  deriving Show


--
-- Final simulation property
--

--TODO !
--
-- The general simulation propert for a suitablely constrained sequence of
-- the concrete operations.
--
-- The flush and prune operations allow quite a bit of flexibility about when
-- we do them, but there is a constraint that we flush before we prune so
-- that we do not break the chain overlap.
--
-- Could pick a specific flush policy but would like to check that an arbitrary
-- valid policy is still ok.

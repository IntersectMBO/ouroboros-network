{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Model implementation of the chain DB
--
-- Intended for qualified import
module Ouroboros.Storage.ChainDB.Model (
    Model -- opaque
    -- * Construction
  , empty
  , addBlock
  , addBlocks
    -- * Queries
  , currentChain
  , currentLedger
  , lastK
  , tipBlock
  , tipPoint
  , getBlock
  , getBlockByPoint
  , hasBlock
  , hasBlockByPoint
  , pointOnChain
    -- * Iterators
  , streamBlocks
  , iteratorNext
    -- * Readers
  , newReader
  , readerInstruction
  , readerForward
  ) where

import           Control.Monad.Except (runExcept)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (ChainHash (..), ChainUpdate (..),
                     HasHeader, HeaderHash, Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Chain)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as Fragment
import qualified Ouroboros.Network.ChainProducerState as CPS

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedly)

import           Ouroboros.Storage.ChainDB.API (IteratorId (..),
                     IteratorResult (..), StreamFrom (..), StreamTo (..))

-- | Model of the chain DB
data Model blk = Model {
      blocks        :: Map (HeaderHash blk) blk
    , cps           :: CPS.ChainProducerState blk
    , currentLedger :: ExtLedgerState blk
    , initLedger    :: ExtLedgerState blk
    , iterators     :: Map IteratorId [blk]
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

currentChain :: Model blk -> Chain blk
currentChain = CPS.producerChain . cps

getBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Maybe blk
getBlock hash Model{..} = Map.lookup hash blocks

hasBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Bool
hasBlock hash = isJust . getBlock hash

getBlockByPoint :: (HasHeader blk, HasCallStack)
                => Point blk -> Model blk -> Maybe blk
getBlockByPoint = getBlock . notGenesis

hasBlockByPoint :: (HasHeader blk, HasCallStack)
                => Point blk -> Model blk -> Bool
hasBlockByPoint = hasBlock . notGenesis

tipBlock :: Model blk -> Maybe blk
tipBlock = Chain.head . currentChain

tipPoint :: HasHeader blk => Model blk -> Point blk
tipPoint = maybe Chain.genesisPoint Block.blockPoint . tipBlock

lastK :: HasHeader a
      => SecurityParam
      -> (blk -> a)  -- ^ Provided since `ChainFragment` is not a functor
      -> Model blk
      -> ChainFragment a
lastK (SecurityParam k) f =
      Fragment.takeNewest (fromIntegral k)
    . Fragment.fromChain
    . fmap f
    . currentChain

pointOnChain :: HasHeader blk => Model blk -> Point blk -> Bool
pointOnChain m p = Chain.pointOnChain p (currentChain m)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: ExtLedgerState blk -> Model blk
empty initLedger = Model {
      blocks        = Map.empty :: Map (HeaderHash blk) blk
    , cps           = CPS.initChainProducerState Chain.Genesis
    , currentLedger = initLedger
    , initLedger    = initLedger
    , iterators     = Map.empty
    }

addBlock :: forall blk. ProtocolLedgerView blk
         => NodeConfig (BlockProtocol blk)
         -> blk -> Model blk -> Model blk
addBlock cfg blk m = Model {
      blocks        = blocks'
    , cps           = CPS.switchFork newChain (cps m)
    , currentLedger = newLedger
    , initLedger    = initLedger m
    , iterators     = iterators  m
    }
  where
    blocks' :: Map (HeaderHash blk) blk
    blocks' = Map.insert (Block.blockHash blk) blk (blocks m)

    candidates :: [(Chain blk, ExtLedgerState blk)]
    candidates = mapMaybe (validate cfg (initLedger m)) $ chains blocks'

    newChain  :: Chain blk
    newLedger :: ExtLedgerState blk
    (newChain, newLedger) = fromMaybe (currentChain m, currentLedger m) $
                               selectChain cfg (currentChain m) candidates

addBlocks :: forall blk. ProtocolLedgerView blk
          => NodeConfig (BlockProtocol blk)
          -> [blk] -> Model blk -> Model blk
addBlocks cfg = repeatedly (addBlock cfg)

{-------------------------------------------------------------------------------
  Iterators
-------------------------------------------------------------------------------}

streamBlocks :: HasHeader blk
             => StreamFrom blk -> StreamTo blk
             -> Model blk -> (IteratorId, Model blk)
streamBlocks from to m = (itrId, m {
      iterators = Map.insert
                    itrId
                    (between from to (currentChain m))
                    (iterators m)
    })
  where
    itrId :: IteratorId
    itrId = IteratorId (Map.size (iterators m)) -- we never delete iterators

iteratorNext :: IteratorId -> Model blk -> (IteratorResult blk, Model blk)
iteratorNext itrId m =
    case Map.lookup itrId (iterators m) of
      Just []     -> ( IteratorExhausted
                     , m
                     )
      Just (b:bs) -> ( IteratorResult b
                     , m { iterators = Map.insert itrId bs (iterators m) }
                     )
      Nothing      -> error "iteratorNext: unknown iterator ID"

{-------------------------------------------------------------------------------
  Readers
-------------------------------------------------------------------------------}

newReader :: HasHeader blk => Model blk -> (CPS.ReaderId, Model blk)
newReader m = (rdrId, m { cps = cps' })
  where
    (cps', rdrId) = CPS.initReader Chain.genesisPoint (cps m)

readerInstruction :: forall blk. HasHeader blk
                  => CPS.ReaderId
                  -> Model blk
                  -> (Maybe (ChainUpdate blk), Model blk)
readerInstruction rdrId m =
    rewrap $ CPS.readerInstruction rdrId (cps m)
  where
    rewrap :: Maybe (ChainUpdate blk, CPS.ChainProducerState blk)
           -> (Maybe (ChainUpdate blk), Model blk)
    rewrap Nothing            = (Nothing, m)
    rewrap (Just (upd, cps')) = (Just upd, m { cps = cps' })

readerForward :: HasHeader blk
              => CPS.ReaderId
              -> [Point blk]
              -> Model blk
              -> (Maybe (Point blk), Model blk)
readerForward rdrId points m =
    case CPS.findFirstPoint points (cps m) of
      Nothing     -> (Nothing, m)
      Just ipoint -> (Just ipoint, m { cps = cps' })
        where
          cps' = CPS.updateReader rdrId ipoint (cps m)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

notGenesis :: HasCallStack => Point blk -> HeaderHash blk
notGenesis p =
    case Block.pointHash p of
      GenesisHash -> error "Ouroboros.Storage.ChainDB.Model: notGenesis"
      BlockHash h -> h

validate :: ProtocolLedgerView blk
         => NodeConfig (BlockProtocol blk)
         -> ExtLedgerState blk
         -> Chain blk
         -> Maybe (Chain blk, ExtLedgerState blk)
validate cfg initLedger chain =
      either (const Nothing) (\ledger -> Just (chain, ledger))
    . runExcept
    $ chainExtLedgerState cfg chain initLedger

chains :: forall blk. (HasHeader blk)
       => Map (HeaderHash blk) blk -> [Chain blk]
chains bs = (Chain.fromNewestFirst . reverse) <$> go GenesisHash
  where
    -- Construct chains,
    go :: ChainHash blk -> [[blk]]
    go prev =
          [] -- the empty chain is always a possibility
        : concatMap withSucc succs
      where
        succs :: [HeaderHash blk]
        succs = Set.toList $ Map.findWithDefault Set.empty prev fwd

        withSucc :: HeaderHash blk -> [[blk]]
        withSucc next = map (this :) (go (BlockHash next))
          where
            this :: blk
            this = Map.findWithDefault blockMustExist next bs

    fwd :: Map (ChainHash blk) (Set (HeaderHash blk))
    fwd = successors (Map.elems bs)

    blockMustExist :: a
    blockMustExist = error "chains: impossible"

successors :: forall blk. HasHeader blk
           => [blk] -> Map (ChainHash blk) (Set (HeaderHash blk))
successors = Map.unionsWith Set.union . map single
  where
    single :: blk -> Map (ChainHash blk) (Set (HeaderHash blk))
    single b = Map.singleton (Block.blockPrevHash b)
                             (Set.singleton (Block.blockHash b))

between :: forall blk. HasHeader blk
        => StreamFrom blk -> StreamTo blk -> Chain blk -> [blk]
between from to =
      (reverse . dropAfterTo to . reverse)
    . dropBeforeFrom from
    . Chain.toOldestFirst
  where
    -- Drop everything before the start point
    -- This runs on the list /from oldest to newest/
    dropBeforeFrom :: StreamFrom blk -> [blk] -> [blk]
    dropBeforeFrom StreamFromGenesis       = id
    dropBeforeFrom (StreamFromInclusive p) = dropWhile (isNot p)
    dropBeforeFrom (StreamFromExclusive p) = tail . dropWhile (isNot p)

    -- Drop everything after the end point
    -- This runs on the /reversed/ list (from newest to oldest)
    dropAfterTo :: StreamTo blk -> [blk] -> [blk]
    dropAfterTo StreamToEnd           = id
    dropAfterTo (StreamToInclusive p) = dropWhile (isNot p)
    dropAfterTo (StreamToExclusive p) = tail . dropWhile (isNot p)

    isNot :: Point blk -> blk -> Bool
    p `isNot` b = p /= Block.blockPoint b

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
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (ChainHash (..), ChainUpdate (..),
                     HasHeader, HeaderHash, Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
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
      -> (blk -> a)  -- ^ Provided since `AnchoredFragment` is not a functor
      -> Model blk
      -> AnchoredFragment a
lastK (SecurityParam k) f =
      Fragment.anchorNewest k
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

addBlock :: forall blk. (ProtocolLedgerView blk, LedgerConfigView blk)
         => NodeConfig (BlockProtocol blk) -> blk -> Model blk -> Model blk
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

addBlocks :: forall blk. (ProtocolLedgerView blk, LedgerConfigView blk)
          => NodeConfig (BlockProtocol blk) -> [blk] -> Model blk -> Model blk
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

validate :: forall blk.
           ( ProtocolLedgerView blk
           , LedgerConfigView blk
           )
         => NodeConfig (BlockProtocol blk)
         -> ExtLedgerState blk
         -> Chain blk
         -> Maybe (Chain blk, ExtLedgerState blk)
validate cfg initLedger chain =
      fromEither
    . runExcept
    $ chainExtLedgerState cfg chain initLedger
  where
    fromEither :: Either (ExtValidationError blk) (ExtLedgerState blk)
               -> Maybe (Chain blk, ExtLedgerState blk)
    fromEither (Left _err) = Nothing
    fromEither (Right l)   = Just (chain, l)

chains :: forall blk. (HasHeader blk)
       => Map (HeaderHash blk) blk -> [Chain blk]
chains bs = go Chain.Genesis
  where
    -- Construct chains,
    go :: Chain blk -> [Chain blk]
    go ch | null extensions = [ch]
          | otherwise       = extensions
          -- If we can extend the chain, don't include the chain itself. See
          -- the property "Always Extend".
      where
        extensions :: [Chain blk]
        extensions = concat [go (ch :> b) | b <- succs]

        succs :: [blk]
        succs = Map.elems $
          Map.findWithDefault Map.empty (Chain.headHash ch) fwd

    fwd :: Map (ChainHash blk) (Map (HeaderHash blk) blk)
    fwd = successors (Map.elems bs)

-- Map (HeaderHash blk) blk maps a block's hash to the block itself
successors :: forall blk. HasHeader blk
           => [blk] -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
successors = Map.unionsWith Map.union . map single
  where
    single :: blk -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
    single b = Map.singleton (Block.blockPrevHash b)
                             (Map.singleton (Block.blockHash b) b)

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

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model implementation of the chain DB
--
-- Intended for qualified import
module Test.Ouroboros.Storage.ChainDB.Model (
    Model -- opaque
  , IteratorId
  , CPS.ReaderId
  , LedgerCursorId
    -- * Construction
  , empty
  , addBlock
  , addBlocks
  , addBlockPromise
    -- * Queries
  , currentChain
  , currentLedger
  , currentSlot
  , lastK
  , immutableChain
  , immutableBlockNo
  , immutableSlotNo
  , tipBlock
  , tipPoint
  , getBlock
  , getBlockByPoint
  , getBlockComponentByPoint
  , hasBlock
  , hasBlockByPoint
  , maxSlotNo
  , isOpen
  , invalid
  , getPastLedger
    -- * Iterators
  , stream
  , iteratorNext
  , iteratorClose
    -- * Readers
  , newReader
  , readerInstruction
  , readerForward
  , readerClose
    -- * Ledger Cursors
  , getLedgerCursor
  , ledgerCursorState
  , ledgerCursorMove
    -- * ModelSupportsBlock
  , ModelSupportsBlock (..)
    -- * Exported for testing purposes
  , between
  , blocks
  , volDbBlocks
  , immDbChain
  , futureBlocks
  , validChains
  , initLedger
  , garbageCollectable
  , garbageCollectablePoint
  , garbageCollectableIteratorNext
  , garbageCollect
  , copyToImmDB
  , closeDB
  , reopen
  , wipeVolDB
  , advanceCurSlot
  , chains
  ) where

import           Codec.Serialise (Serialise, serialise)
import           Control.Monad (unless)
import           Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function (on)
import           Data.Functor.Identity (Identity (..))
import           Data.List (isInfixOf, isPrefixOf, sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader, HeaderHash,
                     MaxSlotNo (..), Point, SlotNo, pointSlot)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.MockChain.Chain (Chain (..), ChainUpdate)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.MockChainSel
import           Ouroboros.Consensus.Util (repeatedly)
import qualified Ouroboros.Consensus.Util.AnchoredFragment as Fragment
import           Ouroboros.Consensus.Util.IOLike (MonadSTM)
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     BlockComponent (..), ChainDB, ChainDbError (..),
                     InvalidBlockReason (..), IteratorResult (..),
                     LedgerCursorFailure (..), StreamFrom (..), StreamTo (..),
                     UnknownRange (..), validBounds)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel (olderThanK)

type IteratorId = Int

type LedgerCursorId = Int

-- | Model of the chain DB
data Model blk = Model {
      volDbBlocks     :: Map (HeaderHash blk) blk
      -- ^ The VolatileDB
    , immDbChain      :: Chain blk
      -- ^ The ImmutableDB
    , cps           :: CPS.ChainProducerState blk
    , currentLedger :: ExtLedgerState blk
    , initLedger    :: ExtLedgerState blk
    , iterators     :: Map IteratorId [blk]
    , ledgerCursors :: Map LedgerCursorId (ExtLedgerState blk)
    , invalid       :: WithFingerprint (Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo))
    , currentSlot   :: SlotNo
    , futureBlocks  :: Map SlotNo [blk]
      -- ^ Blocks that were added in a slot < than the 'blockSlot' of the
      -- block. Blocks are added to this map by 'addBlock' and will be removed
      -- from it by 'runScheduledChainSelections'. The blocks are stored in
      -- reverse chronological order under the slot they are scheduled to be
      -- added in.
    , maxSlotNo     :: MaxSlotNo
      -- ^ We can't calculate this from 'blocks' and 'futureBlocks', so we
      -- track it separately. See [MaxSlotNo and future blocks].
    , isOpen        :: Bool
      -- ^ While the model tracks whether it is closed or not, the queries and
      -- other functions in this module ignore this for simplicity. The mock
      -- ChainDB that wraps this model will throw a 'ClosedDBError' whenever
      -- it is used while closed.
    }
  deriving (Generic)

deriving instance
  ( ConsensusProtocol (BlockProtocol blk)
  , LedgerSupportsProtocol           blk
  , Block.StandardHash               blk
  , Show                             blk
  ) => Show (Model blk)

-- [MaxSlotNo and future blocks]
--
-- Consider the following scenario: a future block arrives, but when its time
-- is there to perform chain selection for the block, it is older than @k@ and
-- we ignore it.
--
-- In the real implementation, that future block is added to the VolatileDB
-- directly when it arrives. When its scheduled chain selection is performed,
-- the block is ignored because it is older than @k@.
--
-- In the model, the future block is added to the model at the block's slot,
-- at which point the chain selection for the block ignores the block because
-- it is older than @k@. At this point, the block is removed from
-- 'futureBlocks'.
--
-- Here the real implementation and the model can diverge: in the real
-- implementation, that block is still stored in the VolatileDB and the block
-- affects the result of 'getMaxSlotNo'. In the model, that block is not in
-- the model anymore, not even in 'futureBlocks', so if we were to calculate
-- 'MaxSlotNo' using 'blocks' and 'futureBlocks', we do not take the block
-- into account.

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

immDbBlocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
immDbBlocks Model { immDbChain } = Map.fromList $
    [ (Block.blockHash blk, blk)
    | blk <- Chain.toOldestFirst immDbChain
    ]

blocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
blocks m = volDbBlocks m <> immDbBlocks m

currentChain :: Model blk -> Chain blk
currentChain = CPS.producerChain . cps

getBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Maybe blk
getBlock hash m = Map.lookup hash (blocks m)

hasBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Bool
hasBlock hash = isJust . getBlock hash

getBlockByPoint :: HasHeader blk
                => RealPoint blk -> Model blk
                -> Maybe blk
getBlockByPoint (RealPoint _ hash) = getBlock hash

getBlockComponentByPoint
  :: forall m blk b. (ModelSupportsBlock blk, Monad m)
  => BlockComponent (ChainDB m blk) b
  -> RealPoint blk -> Model blk
  -> Either ChainDbError (Maybe b) -- Just to satify the API
getBlockComponentByPoint blockComponent pt m = Right $
    (`getBlockComponent` blockComponent) <$> getBlockByPoint pt m

hasBlockByPoint :: HasHeader blk
                => Point blk -> Model blk -> Bool
hasBlockByPoint pt = case Block.pointHash pt of
    GenesisHash    -> const False
    BlockHash hash -> hasBlock hash

tipBlock :: Model blk -> Maybe blk
tipBlock = Chain.head . currentChain

tipPoint :: HasHeader blk => Model blk -> Point blk
tipPoint = maybe Block.genesisPoint Block.blockPoint . tipBlock

lastK :: HasHeader a
      => SecurityParam
      -> (blk -> a)  -- ^ Provided since `AnchoredFragment` is not a functor
      -> Model blk
      -> AnchoredFragment a
lastK (SecurityParam k) f =
      Fragment.anchorNewest k
    . Chain.toAnchoredFragment
    . fmap f
    . currentChain

-- | Actual number of blocks that can be rolled back. Equal to @k@, except
-- when:
--
-- * Near genesis, the chain might not be @k@ blocks long yet.
-- * After VolatileDB corruption, the whole chain might be >= @k@ blocks, but
--   the tip of the ImmutableDB might be closer than @k@ blocks away from the
--   current chain's tip.
--
maxActualRollback :: HasHeader blk => SecurityParam -> Model blk -> Word64
maxActualRollback k m =
      fromIntegral
    . length
    . takeWhile (/= immutableTipPoint)
    . map Block.blockPoint
    . Chain.toNewestFirst
    . currentChain
    $ m
  where
    immutableTipPoint = Chain.headPoint (immutableChain k m)

-- | Return the immutable prefix of the current chain.
--
-- This is the longest of the given two chains:
--
-- 1. The current chain with the last @k@ blocks dropped.
-- 2. The chain formed by the blocks in 'immDbChain', i.e., the
--    \"ImmutableDB\". We need to take this case in consideration because the
--    VolatileDB might have been wiped.
--
-- We need this because we do not allow rolling back more than @k@ blocks, but
-- the background thread copying blocks from the VolatileDB to the ImmutableDB
-- might not have caught up yet. This means we cannot use the tip of the
-- ImmutableDB to know the most recent \"immutable\" block.
immutableChain
  :: SecurityParam
  -> Model blk
  -> Chain blk
immutableChain (SecurityParam k) m =
    maxBy
      Chain.length
      (Chain.drop (fromIntegral k) (currentChain m))
      (immDbChain m)
  where
    maxBy f a b
      | f a >= f b = a
      | otherwise  = b

-- | The block number of the most recent \"immutable\" block, i.e. the oldest
-- block we can roll back to. We cannot roll back the block itself.
--
-- Note that this is not necessarily the block at the tip of the ImmutableDB,
-- because the background thread copying blocks to the ImmutableDB might not
-- have caught up.
immutableBlockNo :: HasHeader blk
                 => SecurityParam -> Model blk -> WithOrigin Block.BlockNo
immutableBlockNo k = Chain.headBlockNo . immutableChain k

-- | The slot number of the most recent \"immutable\" block (see
-- 'immutableBlockNo').
--
-- This is used for garbage collection of the VolatileDB, which is done in
-- terms of slot numbers, not in terms of block numbers.
immutableSlotNo :: HasHeader blk
                => SecurityParam
                -> Model blk
                -> WithOrigin SlotNo
immutableSlotNo k = Chain.headSlot . immutableChain k

-- | Get past ledger state
--
-- TODO: To perfectly match the real implementation, we should only return
-- ledger states for blocks within a certain range from the tip; unfortunately,
-- that specific range depends currently on the ledger DB's in-memory
-- representation. Right now we return 'Nothing' only if requesting a 'Point'
-- that doesn't lie on the current chain
getPastLedger :: forall blk. LedgerSupportsProtocol blk
              => TopLevelConfig blk
              -> Point blk -> Model blk -> Maybe (ExtLedgerState blk)
getPastLedger cfg p m@Model{..} =
    case prefix of
      [] | p /= Block.genesisPoint ->
        Nothing
      _otherwise ->
        Just $ refoldLedger cfg prefix initLedger
  where
    prefix :: [blk]
    prefix = reverse
           . dropWhile (\blk -> Block.blockPoint blk /= p)
           . Chain.toNewestFirst
           . currentChain
           $ m

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: ExtLedgerState blk -> Model blk
empty initLedger = Model {
      volDbBlocks   = Map.empty
    , immDbChain    = Chain.Genesis
    , cps           = CPS.initChainProducerState Chain.Genesis
    , currentLedger = initLedger
    , initLedger    = initLedger
    , iterators     = Map.empty
    , ledgerCursors = Map.empty
    , invalid       = WithFingerprint Map.empty (Fingerprint 0)
    , currentSlot   = 0
    , futureBlocks  = Map.empty
    , maxSlotNo     = NoMaxSlotNo
    , isOpen        = True
    }

-- | Advance the 'currentSlot' of the model to the given 'SlotNo' if the
-- current slot of the model < the given 'SlotNo'.
--
-- Besides updating the 'currentSlot', future blocks are also added to the
-- model.
advanceCurSlot
  :: forall blk. (LedgerSupportsProtocol blk, ModelSupportsBlock blk)
  => TopLevelConfig blk
  -> SlotNo  -- ^ The new current slot
  -> Model blk -> Model blk
advanceCurSlot cfg curSlot m =
    advance curSlot $
    repeatedly
      (\(slot, blk) -> addBlock cfg blk . advance slot)
      blksWithSlots
      (m { futureBlocks = after })
  where
    (before, at, after) = Map.splitLookup curSlot (futureBlocks m)
    blksWithSlots =
      [ (slot, blkAtSlot)
      | (slot, blksAtSlot) <- Map.toAscList before
      , blkAtSlot <- reverse blksAtSlot
      ] <>
      zip (repeat curSlot) (reverse (fromMaybe [] at))

    advance :: SlotNo -> Model blk -> Model blk
    advance slot m' = m' { currentSlot = slot `max` currentSlot m' }

addBlock :: forall blk. (LedgerSupportsProtocol blk, ModelSupportsBlock blk)
         => TopLevelConfig blk
         -> blk
         -> Model blk -> Model blk
addBlock cfg blk m
    -- If the block is as old as the tip of the ImmutableDB, i.e. older than
    -- @k@, we ignore it, as we can never switch to it.
  | olderThanK hdr (isEBB hdr) immBlockNo
  = m
    -- If it's an invalid block we've seen before, ignore it.
  | isKnownInvalid blk
  = m
    -- The block is from the future, don't add it now, but remember when to
    -- add it.
  | slot > currentSlot m
  = m {
      futureBlocks  = Map.insertWith (<>) slot [blk] (futureBlocks m)
      -- Imitate the model, see [MaxSlotNo and future blocks]
    , maxSlotNo     = maxSlotNo m `max` MaxSlotNo slot
    }
  | otherwise
  = Model {
      volDbBlocks   = volDbBlocks'
    , immDbChain    = immDbChain m
    , cps           = CPS.switchFork newChain (cps m)
    , currentLedger = newLedger
    , initLedger    = initLedger m
    , iterators     = iterators  m
    , ledgerCursors = ledgerCursors m
    , invalid       = WithFingerprint invalidBlocks' fingerprint'
    , currentSlot   = currentSlot  m
    , futureBlocks  = futureBlocks m
    , maxSlotNo     = maxSlotNo m `max` MaxSlotNo slot
    , isOpen        = True
    }
  where
    secParam = configSecurityParam cfg

    immBlockNo = immutableBlockNo secParam m

    hdr = getHeader blk

    slot = Block.blockSlot blk

    isKnownInvalid b =
      Map.member (Block.blockHash b) (forgetFingerprint (invalid m))

    volDbBlocks' :: Map (HeaderHash blk) blk
    volDbBlocks' = Map.insert (Block.blockHash blk) blk (volDbBlocks m)

    invalidBlocks' :: Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
    candidates     :: [(Chain blk, ExtLedgerState blk)]
    (invalidBlocks', candidates) =
      validChains cfg (initLedger m) (immDbBlocks m <> volDbBlocks')

    -- The fingerprint only changes when there are new invalid blocks
    fingerprint'
      | Map.null $ Map.difference invalidBlocks' invalidBlocks
      = fingerprint
      | otherwise
      = succ fingerprint
    WithFingerprint invalidBlocks fingerprint = invalid m

    immutableChainHashes =
        map Block.blockHash
      . Chain.toOldestFirst
      . immutableChain secParam
      $ m

    extendsImmutableChain :: Chain blk -> Bool
    extendsImmutableChain fork =
      immutableChainHashes `isPrefixOf`
      map Block.blockHash (Chain.toOldestFirst fork)

    newChain  :: Chain blk
    newLedger :: ExtLedgerState blk
    (newChain, newLedger) =
        fromMaybe (currentChain m, currentLedger m)
      . selectChain
          (selectView (configBlock cfg) . getHeader)
          (configConsensus cfg)
          (currentChain m)
      . filter (extendsImmutableChain . fst)
      $ candidates

addBlocks :: (LedgerSupportsProtocol blk, ModelSupportsBlock blk)
          => TopLevelConfig blk
          -> [blk]
          -> Model blk -> Model blk
addBlocks cfg = repeatedly (addBlock cfg)

-- | Wrapper around 'addBlock' that returns an 'AddBlockPromise'.
addBlockPromise
  :: forall m blk. (LedgerSupportsProtocol blk, ModelSupportsBlock blk, MonadSTM m)
  => TopLevelConfig blk
  -> blk
  -> Model blk
  -> (AddBlockPromise m blk, Model blk)
addBlockPromise cfg blk m = (result, m')
  where
    m' = addBlock cfg blk m
    blockWritten = Map.notMember (Block.blockHash blk) (blocks m)
                && Map.member    (Block.blockHash blk) (blocks m')
    result = AddBlockPromise
      { blockWrittenToDisk      = return blockWritten
      , blockProcessed          = return $ tipPoint m'
        -- We currently cannot wait for future blocks
      , chainSelectionPerformed = error "chainSelectionPerformed not supported"
      }

{-------------------------------------------------------------------------------
  Iterators
-------------------------------------------------------------------------------}

stream
  :: HasHeader blk
  => SecurityParam
  -> StreamFrom blk -> StreamTo blk
  -> Model blk
  -> Either ChainDbError
            (Either (UnknownRange blk) IteratorId, Model blk)
stream securityParam from to m = do
    unless (validBounds from to) $ Left (InvalidIteratorRange from to)
    case between securityParam from to m of
      Left  e    -> return (Left e,      m)
      Right blks -> return (Right itrId, m {
          iterators = Map.insert itrId blks (iterators m)
        })
  where
    itrId :: IteratorId
    itrId = Map.size (iterators m) -- we never delete iterators

iteratorNext
  :: forall m blk b. (ModelSupportsBlock blk, Monad m)
  => IteratorId
  -> BlockComponent (ChainDB m blk) b
  -> Model blk
  -> (IteratorResult blk b, Model blk)
iteratorNext itrId blockComponent m =
    case Map.lookup itrId (iterators m) of
      Just []     -> ( IteratorExhausted
                     , m
                     )
      Just (b:bs) -> ( IteratorResult $ getBlockComponent b blockComponent
                     , m { iterators = Map.insert itrId bs (iterators m) }
                     )
      Nothing      -> error "iteratorNext: unknown iterator ID"

getBlockComponent
  :: (ModelSupportsBlock blk, Monad m)
  => blk -> BlockComponent (ChainDB m blk) b -> b
getBlockComponent blk = \case
    GetBlock      -> return blk
    GetRawBlock   -> serialise blk

    GetHeader     -> return $ getHeader blk
    GetRawHeader  -> serialise $ getHeader blk

    GetHash       -> Block.blockHash blk
    GetSlot       -> Block.blockSlot blk
    GetIsEBB      -> isEBB (getHeader blk)
    GetBlockSize  -> fromIntegral $ Lazy.length $ serialise blk
    GetHeaderSize -> fromIntegral $ Lazy.length $ serialise $ getHeader blk

    GetPure a     -> a
    GetApply f bc -> getBlockComponent blk f $ getBlockComponent blk bc

-- We never delete iterators such that we can use the size of the map as the
-- next iterator id.
iteratorClose :: IteratorId -> Model blk -> Model blk
iteratorClose itrId m = m { iterators = Map.insert itrId [] (iterators m) }

{-------------------------------------------------------------------------------
  Readers
-------------------------------------------------------------------------------}

readerExists :: CPS.ReaderId -> Model blk -> Bool
readerExists rdrId = CPS.readerExists rdrId . cps

checkIfReaderExists :: CPS.ReaderId -> Model blk
                    -> a
                    -> Either ChainDbError a
checkIfReaderExists rdrId m a
    | readerExists rdrId m
    = Right a
    | otherwise
    = Left ClosedReaderError

newReader :: HasHeader blk => Model blk -> (CPS.ReaderId, Model blk)
newReader m = (rdrId, m { cps = cps' })
  where
    (cps', rdrId) = CPS.initReader Block.genesisPoint (cps m)

readerInstruction
  :: forall m blk b. (ModelSupportsBlock blk, Monad m)
  => CPS.ReaderId
  -> BlockComponent (ChainDB m blk) b
  -> Model blk
  -> Either ChainDbError
            (Maybe (ChainUpdate blk b), Model blk)
readerInstruction rdrId blockComponent m = checkIfReaderExists rdrId m $
    rewrap $ CPS.readerInstruction rdrId (cps m)
  where
    toB :: blk -> b
    toB blk = getBlockComponent blk blockComponent

    rewrap
      :: Maybe (ChainUpdate blk blk, CPS.ChainProducerState blk)
      -> (Maybe (ChainUpdate blk b), Model blk)
    rewrap Nothing            = (Nothing, m)
    rewrap (Just (upd, cps')) = (Just (toB <$> upd), m { cps = cps' })

readerForward :: HasHeader blk
              => CPS.ReaderId
              -> [Point blk]
              -> Model blk
              -> Either ChainDbError
                        (Maybe (Point blk), Model blk)
readerForward rdrId points m = checkIfReaderExists rdrId m $
    case CPS.findFirstPoint points (cps m) of
      Nothing     -> (Nothing, m)
      Just ipoint -> (Just ipoint, m { cps = cps' })
        where
          cps' = CPS.updateReader rdrId ipoint (cps m)

readerClose :: CPS.ReaderId
            -> Model blk
            -> Model blk
readerClose rdrId m
    | readerExists rdrId m
    = m { cps = CPS.deleteReader rdrId (cps m) }
    | otherwise
    = m


{-------------------------------------------------------------------------------
  Ledger Cursors
-------------------------------------------------------------------------------}

getLedgerCursor :: Model blk -> (LedgerCursorId, Model blk)
getLedgerCursor m@Model { ledgerCursors = lcs, currentLedger } =
    (lcId, m { ledgerCursors = Map.insert lcId currentLedger lcs })
  where
    lcId :: LedgerCursorId
    lcId = Map.size lcs -- we never delete ledger cursors

ledgerCursorState :: LedgerCursorId -> Model blk -> ExtLedgerState blk
ledgerCursorState lcId m
    | Just ledgerState <- Map.lookup lcId (ledgerCursors m)
    = ledgerState
    | otherwise
    = error $ "unknown ledgerCursor: " <> show lcId

ledgerCursorMove
  :: forall blk. LedgerSupportsProtocol blk
  => TopLevelConfig blk
  -> LedgerCursorId
  -> Point blk
  -> Model blk
  -> (Either LedgerCursorFailure (ExtLedgerState blk), Model blk)
ledgerCursorMove cfg lcId pt m@Model { ledgerCursors = lcs }
    | Just ledgerState <- getPastLedger cfg pt m
    = (Right ledgerState, m { ledgerCursors = Map.insert lcId ledgerState lcs })
    | pointSlot pt < immutableSlotNo (configSecurityParam cfg) m
    = (Left PointTooOld, m)
    | otherwise
    = (Left PointNotOnChain, m)

{-------------------------------------------------------------------------------
  ModelSupportsBlock
-------------------------------------------------------------------------------}

-- | Functionality the block needs to support so that it can be used in the
-- 'Model'.
--
-- The real ChainDB takes these as function arguments. For convenience (we
-- don't want to pass around an environment throughout the model and the state
-- machine tests), we bundle them in a testing-only type class.
class ( HasHeader blk
      , GetHeader blk
      , HasHeader (Header blk)
      , Serialise blk
      , Serialise (Header blk)
      ) => ModelSupportsBlock blk where
  isEBB :: Header blk -> IsEBB

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

data ValidationResult blk
  = -- | The chain was valid, the ledger corresponds to the tip of the chain.
    ValidChain (Chain blk) (ExtLedgerState blk)
  | InvalidChain
      (ExtValidationError blk)
      -- ^ The validation error of the invalid block.
      (NonEmpty (RealPoint blk))
      -- ^ The point corresponding to the invalid block is the first in this
      -- list. The remaining elements in the list are the points after the
      -- invalid block.
      (Chain blk)
      -- ^ The valid prefix of the chain.
      (ExtLedgerState blk)
      -- ^ The ledger state corresponding to the tip of the valid prefix of
      -- the chain.

validate :: forall blk. LedgerSupportsProtocol blk
         => TopLevelConfig blk
         -> ExtLedgerState blk
         -> Chain blk
         -> ValidationResult blk
validate cfg initLedger chain =
    go initLedger Genesis (Chain.toOldestFirst chain)
  where
    go :: ExtLedgerState blk  -- ^ Corresponds to the tip of the valid prefix
       -> Chain blk           -- ^ Valid prefix
       -> [blk]               -- ^ Remaining blocks to validate
       -> ValidationResult blk
    go ledger validPrefix bs = case bs of
      []    -> ValidChain validPrefix ledger
      b:bs' -> case runExcept (tickThenApply cfg b ledger) of
        Right ledger' -> go ledger' (validPrefix :> b) bs'
        Left  e       -> InvalidChain e (fmap blockRealPoint (b NE.:| bs'))
                           validPrefix ledger

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

validChains :: forall blk. LedgerSupportsProtocol blk
            => TopLevelConfig blk
            -> ExtLedgerState blk
            -> Map (HeaderHash blk) blk
            -> ( Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
               , [(Chain blk, ExtLedgerState blk)]
               )
validChains cfg initLedger bs =
    foldMap (classify . validate cfg initLedger) $
    -- Note that we sort here to make sure we pick the same chain as the real
    -- chain selection in case there are multiple equally preferable chains
    -- after detecting invalid blocks. For example:
    --
    -- We add the following blocks: B, B', C', A where C' is invalid. Without
    -- sorting here (in the model), this results in the following two
    -- unvalidated chains: A->B and A->B'->C'. After validation, this results
    -- in the following two validated chains: A->B and A->B'. The first of
    -- these two will be chosen.
    --
    -- In the real implementation, we sort the candidate chains before
    -- validation so that in the best case (no invalid blocks) we only have to
    -- validate the most preferable candidate chain. So A->B'->C' is validated
    -- first, which results in the valid chain A->B', which is then chosen
    -- over the equally preferable A->B as it will be the first in the list
    -- after a stable sort.
    sortChains $
    chains bs
  where
    sortChains :: [Chain blk] -> [Chain blk]
    sortChains = sortBy (flip (Fragment.compareAnchoredCandidates cfg `on`
                                 (Chain.toAnchoredFragment . fmap getHeader)))

    classify :: ValidationResult blk
             -> ( Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
                , [(Chain blk, ExtLedgerState blk)]
                )
    classify (ValidChain chain ledger) = (mempty, [(chain, ledger)])
    classify (InvalidChain e invalid chain ledger) =
        ( mkInvalid e invalid
        , [(chain, ledger)]
        )

    mkInvalid :: ExtValidationError blk -> NonEmpty (RealPoint blk)
              -> Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
    mkInvalid e (pt NE.:| after) =
      uncurry Map.insert (fmap (ValidationError e,) (pointToHashAndSlot pt)) $
      Map.fromList $ map (fmap (InChainAfterInvalidBlock pt e,) .
                          pointToHashAndSlot) after

    pointToHashAndSlot (RealPoint s h) = (h, s)

-- Map (HeaderHash blk) blk maps a block's hash to the block itself
successors :: forall blk. HasHeader blk
           => [blk] -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
successors = Map.unionsWith Map.union . map single
  where
    single :: blk -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
    single b = Map.singleton (Block.blockPrevHash b)
                             (Map.singleton (Block.blockHash b) b)

between :: forall blk. HasHeader blk
        => SecurityParam -> StreamFrom blk -> StreamTo blk -> Model blk
        -> Either (UnknownRange blk) [blk]
between k from to m = do
    fork <- errFork
    -- See #871.
    if partOfCurrentChain fork ||
       Fragment.forksAtMostKBlocks (maxActualRollback k m) currentFrag fork
      then return $ Fragment.toOldestFirst fork
           -- We cannot stream from an old fork
      else Left $ ForkTooOld from
  where
    currentFrag :: AnchoredFragment blk
    currentFrag = Chain.toAnchoredFragment (currentChain m)

    partOfCurrentChain :: AnchoredFragment blk -> Bool
    partOfCurrentChain fork =
      map Block.blockPoint (Fragment.toOldestFirst fork) `isInfixOf`
      map Block.blockPoint (Chain.toOldestFirst (currentChain m))

    -- A fragment for each possible chain in the database
    fragments :: [AnchoredFragment blk]
    fragments = map Chain.toAnchoredFragment
              . chains
              . blocks
              $ m

    -- The fork that contained the start and end point, i.e. the fork to
    -- stream from. This relies on the fact that each block uniquely
    -- determines its prefix.
    errFork :: Either (UnknownRange blk) (AnchoredFragment blk)
    errFork = do
      -- The error refers to @to@, because if the list is empty, @to@ was not
      -- found on any chain
      let err = MissingBlock $ case to of
            StreamToInclusive p -> p
            StreamToExclusive p -> p
      -- Note that any chain that contained @to@, must have an identical
      -- prefix because the hashes of the blocks enforce this. So we can just
      -- pick any fork.
      afterTo <- anyFork (map cutOffAfterTo fragments) err
      cutOffBeforeFrom afterTo

    -- Select the first 'Right' in the list, otherwise return the last 'Left'.
    -- If the list is empty, return the error given as second argument.
    --
    -- See 'errFork' for why it doesn't matter which fork we return.
    anyFork :: [Either (UnknownRange blk) (AnchoredFragment blk)]
            ->  UnknownRange blk
            ->  Either (UnknownRange blk) (AnchoredFragment blk)
    anyFork (Right f : _ ) _ = Right f
    anyFork (Left  u : []) _ = Left u
    anyFork (Left  _ : fs) e = anyFork fs e
    anyFork []             e = Left e

    -- If @to@ is on the fragment, remove all blocks after it, including @to@
    -- itself in case of 'StreamToExclusive'. If it is not on the fragment,
    -- return a 'MissingBlock' error.
    cutOffAfterTo :: AnchoredFragment blk
                  -> Either (UnknownRange blk) (AnchoredFragment blk)
    cutOffAfterTo frag = case to of
      StreamToInclusive p
        | Just frag' <- fst <$> Fragment.splitAfterPoint frag (realPointToPoint p)
        -> return frag'
        | otherwise
        -> Left $ MissingBlock p
      StreamToExclusive p
        | Just frag' <- fst <$> Fragment.splitAfterPoint frag (realPointToPoint p)
        -> return $ Fragment.dropNewest 1 frag'
        | otherwise
        -> Left $ MissingBlock p

    -- If @from@ is on the fragment, remove all blocks before it, including
    -- @from@ itself in case of 'StreamFromExclusive'. It it is not on the
    -- fragment, return a 'MissingBlock' error.
    cutOffBeforeFrom :: AnchoredFragment blk
                     -> Either (UnknownRange blk) (AnchoredFragment blk)
    cutOffBeforeFrom frag = case from of
      StreamFromInclusive p
        | Just frag' <- snd <$> Fragment.splitBeforePoint frag (realPointToPoint p)
        -> return frag'
        | otherwise
        -> Left $ MissingBlock p
      StreamFromExclusive p@(BlockPoint s h)
        | Just frag' <- snd <$> Fragment.splitAfterPoint frag p
        -> return frag'
        | otherwise
        -> Left $ MissingBlock (RealPoint s h)
      StreamFromExclusive GenesisPoint
        -> return frag

-- | Should the given block be garbage collected from the VolatileDB?
--
-- Blocks can be garbage collected when their slot number is older than the
-- slot number of the immutable block (the block @k@ blocks after the current
-- tip).
garbageCollectable :: forall blk. HasHeader blk
                   => SecurityParam -> Model blk -> blk -> Bool
garbageCollectable secParam m@Model{..} b =
    -- Note: we don't use the block number but the slot number, as the
    -- VolatileDB's garbage collection is in terms of slot numbers.
    At (Block.blockSlot b) < immutableSlotNo secParam m

-- | Return 'True' when the model contains the block corresponding to the point
-- and the block itself is eligible for garbage collection, i.e. the real
-- implementation might have garbage collected it.
--
-- If the block is not in the model, return 'True', as it has likely been
-- garbage-collected from the model too. Note that we cannot distinguish this
-- case from a block that was never added to the model in the first place.
garbageCollectablePoint :: forall blk. HasHeader blk
                        => SecurityParam -> Model blk -> RealPoint blk -> Bool
garbageCollectablePoint secParam m@Model{..} pt
    | Just blk <- getBlock (realPointHash pt) m
    = garbageCollectable secParam m blk
    | otherwise
    = True

-- | Return 'True' when the next block the given iterator would produced is
-- eligible for garbage collection, i.e. the real implementation might have
-- garbage collected it.
garbageCollectableIteratorNext
  :: forall blk. ModelSupportsBlock blk
  => SecurityParam -> Model blk -> IteratorId -> Bool
garbageCollectableIteratorNext secParam m itId =
    case fst (iteratorNext @Identity itId GetBlock m) of
      IteratorExhausted             -> True -- TODO
      IteratorBlockGCed {}          -> error "model doesn't return IteratorBlockGCed"
      IteratorResult (Identity blk) -> garbageCollectable secParam m blk

garbageCollect :: forall blk. HasHeader blk
               => SecurityParam -> Model blk -> Model blk
garbageCollect secParam m@Model{..} = m
    { volDbBlocks = Map.filter (not . collectable) volDbBlocks
    }
    -- TODO what about iterators that will stream garbage collected blocks?
  where
    collectable :: blk -> Bool
    collectable = garbageCollectable secParam m

-- | Copy all blocks on the current chain older than @k@ to the \"mock
-- ImmutableDB\" ('immDbChain').
--
-- Idempotent.
copyToImmDB :: SecurityParam -> Model blk -> Model blk
copyToImmDB secParam m = m { immDbChain = immutableChain secParam m }

closeDB :: Model blk -> Model blk
closeDB m@Model{..} = m
    { isOpen        = False
    , cps           = cps { CPS.chainReaders = Map.empty }
    , iterators     = Map.empty
    , ledgerCursors = Map.empty
    }

reopen :: Model blk -> Model blk
reopen m = m { isOpen = True }

wipeVolDB
  :: forall blk. LedgerSupportsProtocol blk
  => TopLevelConfig blk
  -> Model blk
  -> (Point blk, Model blk)
wipeVolDB cfg m =
    (tipPoint m', reopen m')
  where
    m' = (closeDB m)
      { volDbBlocks   = Map.empty
      , cps           = CPS.switchFork newChain (cps m)
      , currentLedger = newLedger
        -- Future blocks were in the VolatileDB, so they're now gone
      , futureBlocks  = Map.empty
      , maxSlotNo     = NoMaxSlotNo
      }

    -- Get the chain ending at the ImmutableDB by doing chain selection on the
    -- sole candidate (or none) in the ImmutableDB.
    newChain  :: Chain blk
    newLedger :: ExtLedgerState blk
    (newChain, newLedger) =
        isSameAsImmDbChain
      $ selectChain
          (selectView (configBlock cfg) . getHeader)
          (configConsensus cfg)
          Chain.genesis
      $ snd
      $ validChains cfg (initLedger m) (immDbBlocks m)

    isSameAsImmDbChain = \case
      Nothing
        | Chain.null (immDbChain m)
        -> (Chain.Genesis, initLedger m)
        | otherwise
        -> error "Did not select any chain"
      Just res@(chain, _ledger)
        | toHashes chain == toHashes (immDbChain m)
        -> res
        | otherwise
        -> error "Did not select the ImmutableDB's chain"

    toHashes = map Block.blockHash . Chain.toOldestFirst

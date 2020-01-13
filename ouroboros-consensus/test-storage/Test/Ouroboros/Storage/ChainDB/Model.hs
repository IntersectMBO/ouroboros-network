{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model implementation of the chain DB
--
-- Intended for qualified import
module Test.Ouroboros.Storage.ChainDB.Model (
    Model -- opaque
    -- * Construction
  , empty
  , addBlock
  , addBlocks
    -- * Queries
  , currentChain
  , currentLedger
  , currentSlot
  , lastK
  , tipBlock
  , tipPoint
  , tipBlockNo
  , getBlock
  , getBlockByPoint
  , hasBlock
  , hasBlockByPoint
  , immutableBlockNo
  , maxSlotNo
  , isOpen
  , invalid
  , getPastLedger
    -- * Iterators
  , streamBlocks
  , iteratorNext
  , iteratorNextDeserialised
  , iteratorClose
    -- * Readers
  , readBlocks
  , readerInstruction
  , readerForward
  , readerClose
    -- * Exported for testing purposes
  , between
  , blocks
  , validChains
  , initLedger
  , garbageCollectable
  , garbageCollectablePoint
  , garbageCollectableIteratorNext
  , garbageCollect
  , closeDB
  , reopen
  , advanceCurSlot
  , toDeserialisable
  , chains
  ) where

import           Control.Monad (unless)
import           Control.Monad.Except (runExcept)
import           Data.Bifunctor (first)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           GHC.Generics (Generic)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (BlockNo, pattern BlockPoint,
                     ChainHash (..), pattern GenesisPoint, HasHeader,
                     HeaderHash, MaxSlotNo (..), Point, Serialised (..),
                     SlotNo)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.MockChain.Chain (Chain (..), ChainUpdate)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.MockChainSel
import           Ouroboros.Consensus.Util (repeatedly)
import qualified Ouroboros.Consensus.Util.AnchoredFragment as Fragment
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     Deserialisable (..), InvalidBlockReason (..),
                     IteratorId (..), IteratorResult (..), StreamFrom (..),
                     StreamTo (..), UnknownRange (..), validBounds)

-- | Model of the chain DB
data Model blk = Model {
      blocks        :: Map (HeaderHash blk) blk
    , cps           :: CPS.ChainProducerState blk
    , currentLedger :: ExtLedgerState blk
    , initLedger    :: ExtLedgerState blk
    , iterators     :: Map IteratorId [blk]
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
  ( OuroborosTag (BlockProtocol blk)
  , ProtocolLedgerView          blk
  , Block.StandardHash          blk
  , Show                        blk
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

currentChain :: Model blk -> Chain blk
currentChain = CPS.producerChain . cps

getBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Maybe blk
getBlock hash Model{..} = Map.lookup hash blocks

hasBlock :: HasHeader blk => HeaderHash blk -> Model blk -> Bool
hasBlock hash = isJust . getBlock hash

getBlockByPoint :: HasHeader blk
                => Point blk -> Model blk
                -> Either ChainDbError (Maybe blk)
getBlockByPoint pt = case Block.pointHash pt of
    GenesisHash    -> const $ Left NoGenesisBlock
    BlockHash hash -> Right . getBlock hash

hasBlockByPoint :: HasHeader blk
                => Point blk -> Model blk -> Bool
hasBlockByPoint pt = case Block.pointHash pt of
    GenesisHash    -> const False
    BlockHash hash -> hasBlock hash

tipBlock :: Model blk -> Maybe blk
tipBlock = Chain.head . currentChain

tipPoint :: HasHeader blk => Model blk -> Point blk
tipPoint = maybe Block.genesisPoint Block.blockPoint . tipBlock

tipBlockNo :: HasHeader blk => Model blk -> BlockNo
tipBlockNo = maybe Block.genesisBlockNo Block.blockNo . tipBlock

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

-- | The block number of the most recent \"immutable\" block, i.e. the oldest
-- block we can roll back to. We cannot roll back the block itself.
--
-- In the real implementation this will correspond to the block number of the
-- block at the tip of the Immutable DB.
immutableBlockNo :: HasHeader blk
                 => SecurityParam -> Model blk -> Block.BlockNo
immutableBlockNo (SecurityParam k) =
        Chain.headBlockNo
      . Chain.drop (fromIntegral k)
      . currentChain

-- | The slot number of the most recent \"immutable\" block (see
-- 'immutableBlockNo').
--
-- This is used for garbage collection of the VolatileDB, which is done in
-- terms of slot numbers, not in terms of block numbers.
immutableSlotNo :: HasHeader blk
                => SecurityParam
                -> Model blk
                -> WithOrigin SlotNo
immutableSlotNo (SecurityParam k) =
        Chain.headSlot
      . Chain.drop (fromIntegral k)
      . currentChain

-- | Get past ledger state
--
-- TODO: To perfectly match the real implementation, we should only return
-- ledger states for blocks within a certain range from the tip; unfortunately,
-- that specific range depends currently on the ledger DB's in-memory
-- representation. Right now we return 'Nothing' only if requesting a 'Point'
-- that doesn't lie on the current chain
getPastLedger :: forall blk. ProtocolLedgerView blk
              => NodeConfig (BlockProtocol blk)
              -> Point blk -> Model blk -> Maybe (ExtLedgerState blk)
getPastLedger cfg p m@Model{..} =
    case prefix of
      [] | p /= Block.genesisPoint ->
        Nothing
      _otherwise ->
        -- Re-applying previously validated blocks shouldn't result in an error
        case runExcept $ foldExtLedgerState
                           BlockPreviouslyApplied
                           cfg
                           prefix
                           initLedger of
          Left err -> error $ "getPastLedger: unexpected error " ++ show err
          Right l  -> Just l
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
      blocks        = Map.empty :: Map (HeaderHash blk) blk
    , cps           = CPS.initChainProducerState Chain.Genesis
    , currentLedger = initLedger
    , initLedger    = initLedger
    , iterators     = Map.empty
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
  :: forall blk.
     ( ProtocolLedgerView blk
     , CanSelect (BlockProtocol blk) blk
     )
  => NodeConfig (BlockProtocol blk)
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

addBlock :: forall blk. (
              ProtocolLedgerView blk
              -- Chain selection is normally done on /headers/ only
            , CanSelect (BlockProtocol blk) blk
            )
         => NodeConfig (BlockProtocol blk)
         -> blk
         -> Model blk -> Model blk
addBlock cfg blk m
    -- If the block is as old as the tip of the ImmutableDB, i.e. older than
    -- @k@, we ignore it, as we can never switch to it. TODO what about EBBs?
  | Block.blockNo blk <= immutableBlockNo secParam m
    -- Unless we're adding the first block to the empty chain: the empty chain
    -- has the same block number as the genesis EBB, i.e. 0, so we don't want
    -- to ignore it in this case.
  , not addingGenesisEBBToEmptyDB
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
      blocks        = blocks'
    , cps           = CPS.switchFork newChain (cps m)
    , currentLedger = newLedger
    , initLedger    = initLedger m
    , iterators     = iterators  m
    , invalid       = WithFingerprint invalidBlocks' fingerprint'
    , currentSlot   = currentSlot  m
    , futureBlocks  = futureBlocks m
    , maxSlotNo     = maxSlotNo m `max` MaxSlotNo slot
    , isOpen        = True
    }
  where
    secParam = protocolSecurityParam cfg

    addingGenesisEBBToEmptyDB = tipPoint m == GenesisPoint
                             && Block.blockNo blk == Block.genesisBlockNo

    slot = Block.blockSlot blk

    blocks' :: Map (HeaderHash blk) blk
    blocks' = Map.insert (Block.blockHash blk) blk (blocks m)

    invalidBlocks' :: Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
    candidates     :: [(Chain blk, ExtLedgerState blk)]
    (invalidBlocks', candidates) = validChains cfg (initLedger m) blocks'

    -- The fingerprint only changes when there are new invalid blocks
    fingerprint'
      | Map.null $ Map.difference invalidBlocks' invalidBlocks
      = fingerprint
      | otherwise
      = succ fingerprint
    WithFingerprint invalidBlocks fingerprint = invalid m

    currentChainFrag = Chain.toAnchoredFragment (currentChain m)

    newChain  :: Chain blk
    newLedger :: ExtLedgerState blk
    (newChain, newLedger) =
      fromMaybe (currentChain m, currentLedger m) $
      selectChain cfg (currentChain m) $
      filter
        (Fragment.forksAtMostKBlocks (maxRollbacks secParam) currentChainFrag .
         Chain.toAnchoredFragment . fst)
        candidates

addBlocks :: (ProtocolLedgerView blk, CanSelect (BlockProtocol blk) blk)
          => NodeConfig (BlockProtocol blk)
          -> [blk]
          -> Model blk -> Model blk
addBlocks cfg = repeatedly (addBlock cfg)

{-------------------------------------------------------------------------------
  Iterators
-------------------------------------------------------------------------------}

streamBlocks :: HasHeader blk
             => SecurityParam
             -> StreamFrom blk -> StreamTo blk
             -> Model blk
             -> Either ChainDbError
                       (Either (UnknownRange blk) IteratorId, Model blk)
streamBlocks securityParam from to m = do
    unless (validBounds from to) $ Left (InvalidIteratorRange from to)
    case between securityParam from to m of
      Left  e    -> return (Left e,      m)
      Right blks -> return (Right itrId, m {
          iterators = Map.insert itrId blks (iterators m)
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

iteratorNextDeserialised
  :: forall m blk. (Monad m, HasHeader blk)
  => IteratorId -> Model blk
  -> (IteratorResult (Deserialisable m blk blk), Model blk)
iteratorNextDeserialised itrId m =
    first convert $ iteratorNext itrId m
  where
    convert :: IteratorResult blk -> IteratorResult (Deserialisable m blk blk)
    convert = \case
      IteratorExhausted      -> IteratorExhausted
      IteratorResult blk     -> IteratorResult (toDeserialisable blk)
      IteratorBlockGCed hash -> IteratorBlockGCed hash

toDeserialisable
  :: (Monad m, HasHeader b, HeaderHash blk ~ HeaderHash b)
  => b -> Deserialisable m blk b
toDeserialisable b = Deserialisable
  { serialised         = Serialised mempty -- Currently unused
  , deserialisableSlot = Block.blockSlot b
  , deserialisableHash = Block.blockHash b
  , deserialise        = return b
  }

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
    = Left $ ClosedReaderError rdrId

readBlocks :: HasHeader blk => Model blk -> (CPS.ReaderId, Model blk)
readBlocks m = (rdrId, m { cps = cps' })
  where
    (cps', rdrId) = CPS.initReader Block.genesisPoint (cps m)

readerInstruction
  :: forall blk b. HasHeader blk
  => (blk -> b)
  -> CPS.ReaderId
  -> Model blk
  -> Either ChainDbError
            (Maybe (ChainUpdate blk b), Model blk)
readerInstruction toB rdrId m = checkIfReaderExists rdrId m $
    rewrap $ CPS.readerInstruction rdrId (cps m)
  where
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
  Internal auxiliary
-------------------------------------------------------------------------------}

data ValidationResult blk
  = -- | The chain was valid, the ledger corresponds to the tip of the chain.
    ValidChain (Chain blk) (ExtLedgerState blk)
  | InvalidChain
      (ExtValidationError blk)
      -- ^ The validation error of the invalid block.
      (NonEmpty (Point blk))
      -- ^ The point corresponding to the invalid block is the first in this
      -- list. The remaining elements in the list are the points after the
      -- invalid block.
      (Chain blk)
      -- ^ The valid prefix of the chain.
      (ExtLedgerState blk)
      -- ^ The ledger state corresponding to the tip of the valid prefix of
      -- the chain.

validate :: forall blk. ProtocolLedgerView blk
         => NodeConfig (BlockProtocol blk)
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
      b:bs' -> case runExcept (applyExtLedgerState BlockNotPreviouslyApplied cfg b ledger) of
        Right ledger' -> go ledger' (validPrefix :> b) bs'
        Left  e       -> InvalidChain e (fmap Block.blockPoint (b NE.:| bs'))
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

validChains :: forall blk. (
                 ProtocolLedgerView blk
               , CanSelect (BlockProtocol blk) blk
               )
            => NodeConfig (BlockProtocol blk)
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
                                 Chain.toAnchoredFragment))

    classify :: ValidationResult blk
             -> ( Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
                , [(Chain blk, ExtLedgerState blk)]
                )
    classify (ValidChain chain ledger) = (mempty, [(chain, ledger)])
    classify (InvalidChain e invalid chain ledger) =
        ( mkInvalid e invalid
        , [(chain, ledger)]
        )

    mkInvalid :: ExtValidationError blk -> NonEmpty (Point blk)
              -> Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)
    mkInvalid e (pt NE.:| after) =
      uncurry Map.insert (fmap (ValidationError e,) (pointToHashAndSlot pt)) $
      Map.fromList $ map (fmap (InChainAfterInvalidBlock pt e,) .
                          pointToHashAndSlot) after

    pointToHashAndSlot GenesisPoint    = error "genesis cannot be invalid"
    pointToHashAndSlot BlockPoint {..} = (withHash, atSlot)

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
between (SecurityParam k) from to m = do
    fork <- errFork
    if Fragment.forksAtMostKBlocks k currentFrag fork
      then return $ Fragment.toOldestFirst fork
           -- We cannot stream from an old fork
      else Left $ ForkTooOld from
  where
    currentFrag :: AnchoredFragment blk
    currentFrag = Chain.toAnchoredFragment (currentChain m)

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
        | Just frag' <- fst <$> Fragment.splitAfterPoint frag p
        -> return frag'
        | otherwise
        -> Left $ MissingBlock p
      StreamToExclusive p
        | Just frag' <- fst <$> Fragment.splitAfterPoint frag p
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
        | Just frag' <- snd <$> Fragment.splitBeforePoint frag p
        -> return frag'
        | otherwise
        -> Left $ MissingBlock p
      StreamFromExclusive p
        | Just frag' <- snd <$> Fragment.splitAfterPoint frag p
        -> return frag'
        | otherwise
        -> Left $ MissingBlock p

-- | Is it possible that the given block is no longer in the ChainDB because
-- the garbage collector has collected it?
--
-- Note that blocks on the current chain will always remain in the ChainDB as
-- they are copied to the ImmutableDB.
--
-- Blocks not on the current chain can be garbage collected from the
-- VolatileDB when their slot number is older than the slot number of the
-- immutable block (the block @k@ blocks after the current tip).
garbageCollectable :: forall blk. HasHeader blk
                   => SecurityParam -> Model blk -> blk -> Bool
garbageCollectable secParam m@Model{..} b =
    not onCurrentChain && olderThanImmutableSlotNo
  where
    onCurrentChain = Chain.pointOnChain (Block.blockPoint b) (currentChain m)
    -- Note: we don't use the block number but the slot number, as the
    -- VolatileDB's garbage collection is in terms of slot numbers.
    olderThanImmutableSlotNo = At (Block.blockSlot b) < immutableSlotNo secParam m

-- Return 'True' when the model contains the block corresponding to the point
-- and the block itself is eligible for garbage collection, i.e. the real
-- implementation might have garbage collected it.
--
-- If the block is not in the model, return 'True', as it has likely been
-- garbage-collected from the model too. Note that we cannot distinguish this
-- case from a block that was never added to the model in the first place.
garbageCollectablePoint :: forall blk. HasHeader blk
                        => SecurityParam -> Model blk -> Point blk -> Bool
garbageCollectablePoint secParam m@Model{..} pt
    | BlockHash hash <- Block.pointHash pt
    , Just blk <- getBlock hash m
    = garbageCollectable secParam m blk
    | otherwise
    = True

-- | Return 'True' when the next block the given iterator would produced is
-- eligible for garbage collection, i.e. the real implementation might have
-- garbage collected it.
garbageCollectableIteratorNext
  :: forall blk. HasHeader blk
  => SecurityParam -> Model blk -> IteratorId -> Bool
garbageCollectableIteratorNext secParam m itId =
    case fst (iteratorNext itId m) of
      IteratorExhausted    -> True -- TODO
      IteratorBlockGCed {} -> error "model doesn't return IteratorBlockGCed"
      IteratorResult blk   -> garbageCollectable secParam m blk

garbageCollect :: forall blk. HasHeader blk
               => SecurityParam -> Model blk -> Model blk
garbageCollect secParam m@Model{..} = m
    { blocks = Map.filter (not . collectable) blocks
    }
    -- TODO what about iterators that will stream garbage collected blocks?
  where
    collectable :: blk -> Bool
    collectable = garbageCollectable secParam m

closeDB :: Model blk -> Model blk
closeDB m@Model{..} = m
    { isOpen    = False
    , cps       = cps { CPS.chainReaders = Map.empty }
    , iterators = Map.empty
    }

reopen :: Model blk -> Model blk
reopen m = m { isOpen = True }

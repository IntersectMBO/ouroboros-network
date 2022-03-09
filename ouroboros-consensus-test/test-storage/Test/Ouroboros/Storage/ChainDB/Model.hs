{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
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
    Model
    -- opaque
  , CPS.FollowerId
  , IteratorId
    -- * Construction
  , addBlock
  , addBlockPromise
  , addBlocks
  , empty
    -- * Queries
  , currentChain
  , currentLedger
  , currentSlot
  , futureBlocks
  , getBlock
  , getBlockByPoint
  , getBlockComponentByPoint
  , getIsValid
  , getLedgerDB
  , getMaxSlotNo
  , hasBlock
  , hasBlockByPoint
  , immutableBlockNo
  , immutableChain
  , immutableSlotNo
  , invalid
  , isOpen
  , isValid
  , lastK
  , maxClockSkew
  , tipBlock
  , tipPoint
  , volatileChain
    -- * Iterators
  , iteratorClose
  , iteratorNext
  , stream
    -- * Followers
  , followerClose
  , followerForward
  , followerInstruction
  , newFollower
    -- * ModelSupportsBlock
  , ModelSupportsBlock
    -- * Exported for testing purposes
  , ShouldGarbageCollect (GarbageCollect, DoNotGarbageCollect)
  , advanceCurSlot
  , between
  , blocks
  , chains
  , closeDB
  , copyToImmutableDB
  , garbageCollectable
  , garbageCollectableIteratorNext
  , garbageCollectablePoint
  , immutableDbChain
  , initLedger
  , reopen
  , validChains
  , volatileDbBlocks
  , wipeVolatileDB
  ) where

import           Codec.Serialise (Serialise, serialise)
import           Control.Monad (unless)
import           Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function (on)
import           Data.List (isInfixOf, isPrefixOf, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.MockChain.Chain (Chain (..), ChainUpdate)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS

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

import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     BlockComponent (..), ChainDbError (..),
                     InvalidBlockReason (..), IteratorResult (..),
                     StreamFrom (..), StreamTo (..), UnknownRange (..),
                     validBounds)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel (olderThanK)
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

type IteratorId = Int

-- | Model of the chain DB
data Model blk = Model {
      volatileDbBlocks :: Map (HeaderHash blk) blk
      -- ^ The VolatileDB
    , immutableDbChain :: Chain blk
      -- ^ The ImmutableDB
    , cps              :: CPS.ChainProducerState blk
    , currentLedger    :: ExtLedgerState blk
    , initLedger       :: ExtLedgerState blk
    , iterators        :: Map IteratorId [blk]
    , valid            :: Set (HeaderHash blk)
    , invalid          :: InvalidBlocks blk
    , currentSlot      :: SlotNo
    , maxClockSkew     :: Word64
      -- ^ Max clock skew in terms of slots. A static configuration parameter.
    , isOpen           :: Bool
      -- ^ While the model tracks whether it is closed or not, the queries and
      -- other functions in this module ignore this for simplicity. The mock
      -- ChainDB that wraps this model will throw a 'ClosedDBError' whenever
      -- it is used while closed.
    }
  deriving (Generic)

deriving instance
  ( ConsensusProtocol (BlockProtocol blk)
  , LedgerSupportsProtocol           blk
  , StandardHash                     blk
  , Show                             blk
  ) => Show (Model blk)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

immutableDbBlocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
immutableDbBlocks Model { immutableDbChain } = Map.fromList $
    [ (blockHash blk, blk)
    | blk <- Chain.toOldestFirst immutableDbChain
    ]

blocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
blocks m = volatileDbBlocks m <> immutableDbBlocks m

futureBlocks :: HasHeader blk => Model blk -> Map (HeaderHash blk) blk
futureBlocks m =
    Map.filter ((currentSlot m <) . blockSlot) (volatileDbBlocks m)

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

getBlockComponentByPoint ::
     ModelSupportsBlock blk
  => BlockComponent blk b
  -> RealPoint blk -> Model blk
  -> Either (ChainDbError blk) (Maybe b) -- Just to satify the API
getBlockComponentByPoint blockComponent pt m = Right $
    (`getBlockComponent` blockComponent) <$> getBlockByPoint pt m

hasBlockByPoint :: HasHeader blk
                => Point blk -> Model blk -> Bool
hasBlockByPoint pt = case pointHash pt of
    GenesisHash    -> const False
    BlockHash hash -> hasBlock hash

tipBlock :: Model blk -> Maybe blk
tipBlock = Chain.head . currentChain

tipPoint :: HasHeader blk => Model blk -> Point blk
tipPoint = maybe GenesisPoint blockPoint . tipBlock

getMaxSlotNo :: HasHeader blk => Model blk -> MaxSlotNo
getMaxSlotNo = foldMap (MaxSlotNo . blockSlot) . blocks

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
    . map blockPoint
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
-- 2. The chain formed by the blocks in 'immutableDbChain', i.e., the
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
      (immutableDbChain m)
  where
    maxBy f a b
      | f a >= f b = a
      | otherwise  = b

-- | Return the volatile suffix of the current chain.
--
-- The opposite of 'immutableChain'.
--
-- This is the shortest of the given two chain fragments:
--
-- 1. The last @k@ blocks of the current chain.
-- 2. The suffix of the current chain not part of the 'immutableDbChain', i.e.,
--    the \"ImmutableDB\".
volatileChain
    :: (HasHeader a, HasHeader blk)
    => SecurityParam
    -> (blk -> a)  -- ^ Provided since 'AnchoredFragment' is not a functor
    -> Model blk
    -> AnchoredFragment a
volatileChain k f m =
      Fragment.fromNewestFirst anchor
    . map f
    . takeWhile ((/= immutableTipPoint) . blockPoint)
    . Chain.toNewestFirst
    . currentChain
    $ m
  where
    (immutableTipPoint, anchor) = case Chain.head (immutableChain k m) of
        Nothing -> (GenesisPoint, Fragment.AnchorGenesis)
        Just b  -> (blockPoint b, Fragment.anchorFromBlock (f b))

-- | The block number of the most recent \"immutable\" block, i.e. the oldest
-- block we can roll back to. We cannot roll back the block itself.
--
-- Note that this is not necessarily the block at the tip of the ImmutableDB,
-- because the background thread copying blocks to the ImmutableDB might not
-- have caught up.
immutableBlockNo :: HasHeader blk
                 => SecurityParam -> Model blk -> WithOrigin BlockNo
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

getIsValid :: forall blk. LedgerSupportsProtocol blk
           => Model blk
           -> (RealPoint blk -> Maybe Bool)
getIsValid m = \(RealPoint _ hash) -> if
    | Map.member hash (volatileDbBlocks m) &&
      Set.member hash (valid m)               -> Just True
    | Map.member hash (volatileDbBlocks m) &&
      Map.member hash (invalid m)             -> Just False
    | otherwise                               -> Nothing

isValid :: forall blk. LedgerSupportsProtocol blk
        => RealPoint blk
        -> Model blk
        -> Maybe Bool
isValid = flip getIsValid

getLedgerDB ::
     LedgerSupportsProtocol blk
  => TopLevelConfig blk
  -> Model blk
  -> LedgerDB (ExtLedgerState blk)
getLedgerDB cfg m@Model{..} =
      ledgerDbPrune (SecurityParam (maxActualRollback k m))
    $ ledgerDbPushMany' ledgerDbCfg blks
    $ ledgerDbWithAnchor initLedger
  where
    blks = Chain.toOldestFirst $ currentChain m

    k = configSecurityParam cfg

    ledgerDbCfg = LedgerDbCfg {
          ledgerDbCfgSecParam = k
        , ledgerDbCfg         = ExtLedgerCfg cfg
        }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty
  :: ExtLedgerState blk
  -> Word64   -- ^ Max clock skew in number of blocks
  -> Model blk
empty initLedger maxClockSkew = Model {
      volatileDbBlocks = Map.empty
    , immutableDbChain = Chain.Genesis
    , cps              = CPS.initChainProducerState Chain.Genesis
    , currentLedger    = initLedger
    , initLedger       = initLedger
    , iterators        = Map.empty
    , invalid          = Map.empty
    , valid            = Set.empty
    , currentSlot      = 0
    , maxClockSkew     = maxClockSkew
    , isOpen           = True
    }

-- | Advance the 'currentSlot' of the model to the given 'SlotNo' if the
-- current slot of the model < the given 'SlotNo'.
advanceCurSlot
  :: SlotNo  -- ^ The new current slot
  -> Model blk -> Model blk
advanceCurSlot curSlot m = m { currentSlot = curSlot `max` currentSlot m }

addBlock :: forall blk. (Eq blk, LedgerSupportsProtocol blk)
         => TopLevelConfig blk
         -> blk
         -> Model blk -> Model blk
addBlock cfg blk m = Model {
      volatileDbBlocks = volatileDbBlocks'
    , immutableDbChain = immutableDbChain m
    , cps              = CPS.switchFork newChain (cps m)
    , currentLedger    = newLedger
    , initLedger       = initLedger m
    , iterators        = iterators  m
    , invalid          = invalid'
    , valid            = valid'
    , currentSlot      = currentSlot  m
    , maxClockSkew     = maxClockSkew m
    , isOpen           = True
    }
  where
    secParam = configSecurityParam cfg

    immBlockNo = immutableBlockNo secParam m

    hdr = getHeader blk

    ignoreBlock =
        -- If the block is as old as the tip of the ImmutableDB, i.e. older
        -- than @k@, we ignore it, as we can never switch to it.
        olderThanK hdr (headerToIsEBB hdr) immBlockNo ||
        -- If it's an invalid block we've seen before, ignore it.
        Map.member (blockHash blk) (invalid m)

    volatileDbBlocks' :: Map (HeaderHash blk) blk
    volatileDbBlocks'
        | ignoreBlock
        = volatileDbBlocks m
        | otherwise
        = Map.insert (blockHash blk) blk (volatileDbBlocks m)

    -- @invalid'@ will be a (non-strict) superset of the previous value of
    -- @invalid@, see 'validChains', thus no need to union.
    invalid'   :: InvalidBlocks blk
    candidates :: [(Chain blk, ExtLedgerState blk)]
    (invalid', candidates) =
        validChains cfg m (immutableDbBlocks m <> volatileDbBlocks')

    immutableChainHashes =
        map blockHash
      . Chain.toOldestFirst
      . immutableChain secParam
      $ m

    extendsImmutableChain :: Chain blk -> Bool
    extendsImmutableChain fork =
      immutableChainHashes `isPrefixOf`
      map blockHash (Chain.toOldestFirst fork)

    consideredCandidates = filter (extendsImmutableChain . fst)
      $ candidates

    newChain  :: Chain blk
    newLedger :: ExtLedgerState blk
    (newChain, newLedger) =
        fromMaybe (currentChain m, currentLedger m)
      . selectChain
          (Proxy @(BlockProtocol blk))
          (selectView (configBlock cfg) . getHeader)
          (currentChain m)
      $ consideredCandidates

    -- See note [Getting the valid blocks] just below
    valid' = foldl
               (Chain.foldChain (\s b -> Set.insert (blockHash b) s))
               (valid m)
               (takeWhile (not . Chain.isPrefixOf newChain) (map fst consideredCandidates) ++ [newChain])

-- = Getting the valid blocks
--
-- The chain selection algorithms implemented by the model and by the SUT differ
-- but have the same outcome.We illustrate this with an example. Imagine having
-- the following candidate chains where @v@ represents a valid block and @x@
-- represents an invalid block:
--
-- > C0: vvvvvxxxxx
-- > C1: vvvvvvvx
-- > C2: vvv
--
-- For candidate Cx, we will call CxV the valid prefix and CxI the invalid suffix.
--
-- The chain selection algorithm will run whenever we add a block, although it
-- will only select a new chain when adding a block results in a chain that is
-- longer than the currently selected chain. Note that the chain selection
-- algorithm doesn't know beforehand the validity of the blocks in the
-- candidates. The process it follows will be:
--
-- 1. Sort the chains by 'SelectView'. Note that for Praos this will trivially
-- imply first consider the candidates by length.
--
--    > sortedCandidates == [C0, C1, C2]
--
-- 2. Until a candidate is found to be valid and longer than the currently selected
--    chain, take the head of the (sorted) list of candidates and validate the
--    blocks in it one by one.
--
--    If a block in the candidate is found to be invalid, the candidate is
--    truncated, added back to the list, and the algorithm starts again at step 1.
--    The valid blocks in the candidate are recorded in the set of known-valid
--    blocks, so that the next time they are applied, it is known that applying
--    said block can't fail and therefore some checks can be skipped. The invalid
--    blocks in the candidate are recorded in the set of known-invalid blocks so
--    that they are not applied again.
--
--    The steps on the example are as follows:
--
--    1.  Start with the sorted candidate chains: [C0, C1, C2]
--    2.  Validate first chain C0 resulting in C0V and C0I.
--    3.  Append C0V to the list of remaining candidates: [C1, C2] ++ [C0V]
--    4.  Add the valid blocks to the state:
--        > knownValid = append C0V knownValid
--    5.  Add the invalid blocks to the state:
--        > knownInvalid = append C0I knownInvalid
--    6.  Re-sort list
--        > sortBy `selectView` [C1, C2, C0V] == [C1, C0V, C2]
--    7.  Validate first chain C1 resulting in C1V and C1I.
--    8.  Append C1V to the list of remaining candidates: [C0V, C2] ++ [C1V]
--    9.  Add the valid blocks to the state:
--        > knownValid   = append C1V knownValid
--    10. Add the invalid blocks to the state:
--        > knownInvalid = append C1I knownInvalid
--    11. Re-sort list
--        > sortBy `selectView` [C0V, C2, C1V] == [C1V, C0V, C2]
--    12. Validate first chain C1V, which is fully valid and returned.
--
-- 3. If such a candidate is found, the algorithm will return it as a result.
--    Otherwise, the algorithm will return a 'Nothing'.
--
--    > chainSelection [C0, C1, C2] = Just C1V
--
-- On the other hand, the chain selection on the model takes some shortcuts to
-- achieve the same result:
--
-- 1. 'validChains' will return the list of candidates sorted by 'SelectView' and
--    each candidate is truncated to its valid prefix.
--
--    > validChains [C0, C1, C2] = (invalid == C0I + C1I, candidates == [C0V, C1V, C2])
--
-- 2. 'selectChain' will sort the chains by 'SelectView' but note that now it will
--    use the 'SelectView' of the already truncated candidate.
--
--    > selectChain [C0V, C1V, C2] = listToMaybe (sortBy `selectView` [C0V, C1V, C2])
--    >                            = listToMaybe ([C1V, C0V, C2])
--    >                            = Just C1V
--
--    The selected candidate will be the same one that the chain selection
--    algorithm would choose. However, as the chain selection algorithm will
--    consider the candidates as they were sorted by 'SelectView' on the
--    non-truncated candidates, blocks in 'C0V' are also considered valid by the
--    real algorithm.
--
--    To get as a result a set of valid blocks that mirrors the one from the
--    real algorithm, the model can process the list of candidates returned by
--    'validChains' until it find the one 'selectChain' chose as these will be
--    the ones that the real algorithm would test and re-add to the list once
--    truncated.
--
--    > knownInvalid = append (C0I + C1I) knownInvalid
--    > knownValid   = foldl append knownValid (takeWhile (/= C1V) candidates ++ [C1V])
--
--    Note that the set of known valid blocks is equivalent to the set computed
--    by real algorithm, but the set of known invalid blocks is a superset of
--    the ones known by the real algorithm. See the note
--    Ouroboros.Storage.ChainDB.StateMachine.[Invalid blocks].

addBlocks :: (Eq blk, LedgerSupportsProtocol blk)
          => TopLevelConfig blk
          -> [blk]
          -> Model blk -> Model blk
addBlocks cfg = repeatedly (addBlock cfg)

-- | Wrapper around 'addBlock' that returns an 'AddBlockPromise'.
addBlockPromise
  :: forall m blk. (Eq blk, LedgerSupportsProtocol blk, MonadSTM m)
  => TopLevelConfig blk
  -> blk
  -> Model blk
  -> (AddBlockPromise m blk, Model blk)
addBlockPromise cfg blk m = (result, m')
  where
    m' = addBlock cfg blk m
    blockWritten = Map.notMember (blockHash blk) (blocks m)
                && Map.member    (blockHash blk) (blocks m')
    result = AddBlockPromise
      { blockWrittenToDisk = return blockWritten
      , blockProcessed     = return $ tipPoint m'
      }

{-------------------------------------------------------------------------------
  Iterators
-------------------------------------------------------------------------------}

stream
  :: GetPrevHash blk
  => SecurityParam
  -> StreamFrom  blk
  -> StreamTo    blk
  -> Model       blk
  -> Either (ChainDbError blk)
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

iteratorNext ::
     ModelSupportsBlock blk
  => IteratorId
  -> BlockComponent blk b
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
  :: forall blk b. ModelSupportsBlock blk
  => blk -> BlockComponent blk b -> b
getBlockComponent blk = \case
    GetVerifiedBlock -> blk  -- We don't verify it
    GetBlock         -> blk
    GetRawBlock      -> serialise blk

    GetHeader        -> getHeader blk
    GetRawHeader     -> serialise $ getHeader blk

    GetHash          -> blockHash blk
    GetSlot          -> blockSlot blk
    GetIsEBB         -> headerToIsEBB (getHeader blk)
    GetBlockSize     -> fromIntegral $ Lazy.length $ serialise blk
    GetHeaderSize    -> fromIntegral $ Lazy.length $ serialise $ getHeader blk
    GetNestedCtxt    -> case unnest (getHeader blk) of
                          DepPair nestedCtxt _ -> SomeSecond nestedCtxt
    GetPure a        -> a
    GetApply f bc    -> getBlockComponent blk f $ getBlockComponent blk bc

-- We never delete iterators such that we can use the size of the map as the
-- next iterator id.
iteratorClose :: IteratorId -> Model blk -> Model blk
iteratorClose itrId m = m { iterators = Map.insert itrId [] (iterators m) }

{-------------------------------------------------------------------------------
  Followers
-------------------------------------------------------------------------------}

followerExists :: CPS.FollowerId -> Model blk -> Bool
followerExists flrId = CPS.followerExists flrId . cps

checkIfFollowerExists ::
     CPS.FollowerId
  -> Model blk
  -> a
  -> Either (ChainDbError blk) a
checkIfFollowerExists flrId m a
    | followerExists flrId m
    = Right a
    | otherwise
    = Left ClosedFollowerError

newFollower :: HasHeader blk => Model blk -> (CPS.FollowerId, Model blk)
newFollower m = (flrId, m { cps = cps' })
  where
    (cps', flrId) = CPS.initFollower GenesisPoint (cps m)

followerInstruction ::
     forall blk b. ModelSupportsBlock blk
  => CPS.FollowerId
  -> BlockComponent blk b
  -> Model blk
  -> Either (ChainDbError blk)
            (Maybe (ChainUpdate blk b), Model blk)
followerInstruction flrId blockComponent m = checkIfFollowerExists flrId m $
    rewrap $ CPS.followerInstruction flrId (cps m)
  where
    toB :: blk -> b
    toB blk = getBlockComponent blk blockComponent

    rewrap
      :: Maybe (ChainUpdate blk blk, CPS.ChainProducerState blk)
      -> (Maybe (ChainUpdate blk b), Model blk)
    rewrap Nothing            = (Nothing, m)
    rewrap (Just (upd, cps')) = (Just (toB <$> upd), m { cps = cps' })

followerForward ::
     HasHeader blk
  => CPS.FollowerId
  -> [Point blk]
  -> Model blk
  -> Either (ChainDbError blk) (Maybe (Point blk), Model blk)
followerForward flrId points m = checkIfFollowerExists flrId m $
    case CPS.findFirstPoint points (cps m) of
      Nothing     -> (Nothing, m)
      Just ipoint -> (Just ipoint, m { cps = cps' })
        where
          cps' = CPS.updateFollower flrId ipoint (cps m)

followerClose ::
     CPS.FollowerId
  -> Model blk
  -> Model blk
followerClose flrId m
    | followerExists flrId m
    = m { cps = CPS.deleteFollower flrId (cps m) }
    | otherwise
    = m

{-------------------------------------------------------------------------------
  ModelSupportsBlock
-------------------------------------------------------------------------------}

-- | Functionality the block needs to support so that it can be used in the
-- 'Model'.
class ( HasHeader blk
      , GetHeader blk
      , HasHeader (Header blk)
      , Serialise blk
      , Serialise (Header blk)
      , HasNestedContent Header blk
      ) => ModelSupportsBlock blk

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

type InvalidBlocks blk = Map (HeaderHash blk) (InvalidBlockReason blk, SlotNo)

-- | Result of 'validate', also used internally.
data ValidatedChain blk =
    ValidatedChain
      (Chain blk)           -- ^ Valid prefix
      (ExtLedgerState blk)  -- ^ Corresponds to the tip of the valid prefix
      (InvalidBlocks blk)   -- ^ Invalid blocks encountered while validating
                            -- the candidate chain.

-- | Validate the given 'Chain'.
--
-- The 'InvalidBlocks' in the returned 'ValidatedChain' will be >= the
-- 'invalid' of the given 'Model'.
validate :: forall blk. LedgerSupportsProtocol blk
         => TopLevelConfig blk
         -> Model blk
         -> Chain blk
         -> ValidatedChain blk
validate cfg Model { currentSlot, maxClockSkew, initLedger, invalid } chain =
    go initLedger Genesis (Chain.toOldestFirst chain)
  where
    mkInvalid :: blk -> InvalidBlockReason blk -> InvalidBlocks blk
    mkInvalid b reason =
      Map.singleton (blockHash b) (reason, blockSlot b)

    go :: ExtLedgerState blk  -- ^ Corresponds to the tip of the valid prefix
       -> Chain blk           -- ^ Valid prefix
       -> [blk]               -- ^ Remaining blocks to validate
       -> ValidatedChain blk
    go ledger validPrefix = \case
      -- Return 'mbFinal' if it contains an "earlier" result
      []    -> ValidatedChain validPrefix ledger invalid
      b:bs' -> case runExcept (tickThenApply (ExtLedgerCfg cfg) b ledger) of
        -- Invalid block according to the ledger
        Left e
          -> ValidatedChain
               validPrefix
               ledger
               (invalid <> mkInvalid b (ValidationError e))

        -- Valid block according to the ledger
        Right ledger'

          -- But the block has been recorded as an invalid block. It must be
          -- that it exceeded the clock skew in the past.
          | Map.member (blockHash b) invalid
          -> ValidatedChain validPrefix ledger invalid

          -- Block is in the future and exceeds the clock skew, it is
          -- considered to be invalid
          | blockSlot b > SlotNo (unSlotNo currentSlot + maxClockSkew)
          -> ValidatedChain
               validPrefix
               ledger
               (invalid <>
                mkInvalid b (InFutureExceedsClockSkew (blockRealPoint b)))

          -- Block is in the future but doesn't exceed the clock skew. It will
          -- not be part of the chain, but we have to continue validation,
          -- because the real implementation validates before truncating
          -- future blocks, and we try to detect the same invalid blocks as
          -- the real implementation.
          | blockSlot b > currentSlot
          -> ValidatedChain
               validPrefix
               ledger
               (invalid <>
                findInvalidBlockInTheFuture ledger' bs')

          -- Block not in the future, this is the good path
          | otherwise
          -> go ledger' (validPrefix :> b) bs'

    -- | Take the following chain:
    --
    -- A (valid) -> B (future) -> C (future) -> D (invalid)
    --
    -- where B and C are from the future, but don't exceed the max clock skew,
    -- and D is invalid according to the ledger.
    --
    -- The real implementation would detect that B and C are from the future
    -- (not exceeding clock skew), but it would also find that D is invalid
    -- according to the ledger and record that. This is because the
    -- implementation first validates using the ledger and then does the
    -- future check, restarting chain selection for any invalid candidates and
    -- revalidating them.
    --
    -- To keep things simple, we don't restart chain selection in this model,
    -- so we want to return the 'ValidatedChain' corresponding to A, but we
    -- also have to continue validating the future blocks B and C so that we
    -- encounter D. That's why 'findInvalidBlockInTheFuture' is called in 'go'
    -- when a block from the future is encountered.
    --
    -- Note that ledger validation stops at the first invalid block, so this
    -- function should find 0 or 1 invalid blocks.
    findInvalidBlockInTheFuture
      :: ExtLedgerState blk
      -> [blk]
      -> InvalidBlocks blk
    findInvalidBlockInTheFuture ledger = \case
      []    -> Map.empty
      b:bs' -> case runExcept (tickThenApply (ExtLedgerCfg cfg) b ledger) of
        Left e        -> mkInvalid b (ValidationError e)
        Right ledger'
          | blockSlot b > SlotNo (unSlotNo currentSlot + maxClockSkew)
          -> mkInvalid b (InFutureExceedsClockSkew (blockRealPoint b)) <>
             findInvalidBlockInTheFuture ledger' bs'
          | otherwise
          -> findInvalidBlockInTheFuture ledger' bs'


chains :: forall blk. (GetPrevHash blk)
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
            -> Model blk
            -> Map (HeaderHash blk) blk
            -> (InvalidBlocks blk, [(Chain blk, ExtLedgerState blk)])
validChains cfg m bs =
    foldMap (classify . validate cfg m) $
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
    sortChains $ chains bs
  where
    sortChains :: [Chain blk] -> [Chain blk]
    sortChains =
      sortBy $ flip (
               Fragment.compareAnchoredFragments (configBlock cfg)
          `on` (Chain.toAnchoredFragment . fmap getHeader)
        )

    classify :: ValidatedChain blk
             -> (InvalidBlocks blk, [(Chain blk, ExtLedgerState blk)])
    classify (ValidatedChain chain ledger invalid) =
      (invalid, [(chain, ledger)])

-- Map (HeaderHash blk) blk maps a block's hash to the block itself
successors :: forall blk. GetPrevHash blk
           => [blk]
           -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
successors = Map.unionsWith Map.union . map single
  where
    single :: blk -> Map (ChainHash blk) (Map (HeaderHash blk) blk)
    single b = Map.singleton (blockPrevHash b)
                             (Map.singleton (blockHash b) b)

between :: forall blk. GetPrevHash blk
        => SecurityParam
        -> StreamFrom  blk
        -> StreamTo    blk
        -> Model       blk
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
      map blockPoint (Fragment.toOldestFirst fork) `isInfixOf`
      map blockPoint (Chain.toOldestFirst (currentChain m))

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

    -- If @to@ is on the fragment, remove all blocks after it. If it is not on
    -- the fragment, return a 'MissingBlock' error.
    cutOffAfterTo :: AnchoredFragment blk
                  -> Either (UnknownRange blk) (AnchoredFragment blk)
    cutOffAfterTo frag = case to of
      StreamToInclusive p
        | Just frag' <- fst <$> Fragment.splitAfterPoint frag (realPointToPoint p)
        -> return frag'
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
garbageCollectable secParam m b =
    -- Note: we don't use the block number but the slot number, as the
    -- VolatileDB's garbage collection is in terms of slot numbers.
    NotOrigin (blockSlot b) < immutableSlotNo secParam m

-- | Return 'True' when the model contains the block corresponding to the point
-- and the block itself is eligible for garbage collection, i.e. the real
-- implementation might have garbage collected it.
--
-- If the block is not in the model, return 'True', as it has likely been
-- garbage-collected from the model too. Note that we cannot distinguish this
-- case from a block that was never added to the model in the first place.
garbageCollectablePoint :: forall blk. HasHeader blk
                        => SecurityParam -> Model blk -> RealPoint blk -> Bool
garbageCollectablePoint secParam m pt
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
    case fst (iteratorNext itId GetBlock m) of
      IteratorExhausted    -> True -- TODO
      IteratorBlockGCed {} -> error "model doesn't return IteratorBlockGCed"
      IteratorResult blk   -> garbageCollectable secParam m blk

-- | Delete blocks that are older than the security parameter from the volatile
-- DB. This function assumes that the blocks that will be deleted are copied to
-- the immutable DB.
--
-- If this function collects blocks that are not yet copied to the immutable DB
-- the volatile fragment of the current chain will not be connected to the
-- immutable part of the chain. For this reason, this function should not be
-- used in isolation and is not exported.
--
garbageCollect :: forall blk. HasHeader blk
               => SecurityParam -> Model blk -> Model blk
garbageCollect secParam m@Model{..} = m {
      volatileDbBlocks = Map.filter (not . collectable) volatileDbBlocks
    }
    -- TODO what about iterators that will stream garbage collected blocks?
  where
    collectable :: blk -> Bool
    collectable = garbageCollectable secParam m

data ShouldGarbageCollect = GarbageCollect | DoNotGarbageCollect
  deriving (Eq, Show)

-- | Copy all blocks on the current chain older than @k@ to the \"mock
-- ImmutableDB\" ('immutableDbChain').
--
-- The 'ShouldGarbageCollect' parameter determines if garbage collection should
-- be performed __after__ copying.
--
-- Idempotent.
copyToImmutableDB :: forall blk. HasHeader blk
                  => SecurityParam -> ShouldGarbageCollect -> Model blk -> Model blk
copyToImmutableDB secParam shouldCollectGarbage m =
      garbageCollectIf shouldCollectGarbage
    $ m { immutableDbChain = immutableChain secParam m }
  where
    garbageCollectIf GarbageCollect      = garbageCollect secParam
    garbageCollectIf DoNotGarbageCollect = id

closeDB :: Model blk -> Model blk
closeDB m@Model{..} = m {
      isOpen        = False
    , cps           = cps { CPS.chainFollowers = Map.empty }
    , iterators     = Map.empty
    }

reopen :: Model blk -> Model blk
reopen m = m { isOpen = True }

wipeVolatileDB
  :: forall blk. LedgerSupportsProtocol blk
  => TopLevelConfig blk
  -> Model blk
  -> (Point blk, Model blk)
wipeVolatileDB cfg m =
    (tipPoint m', reopen m')
  where
    m' = (closeDB m) {
        volatileDbBlocks = Map.empty
      , cps              = CPS.switchFork newChain (cps m)
      , currentLedger    = newLedger
      , invalid          = Map.empty
      }

    -- Get the chain ending at the ImmutableDB by doing chain selection on the
    -- sole candidate (or none) in the ImmutableDB.
    newChain  :: Chain blk
    newLedger :: ExtLedgerState blk
    (newChain, newLedger) =
        isSameAsImmutableDbChain
      $ selectChain
          (Proxy @(BlockProtocol blk))
          (selectView (configBlock cfg) . getHeader)
          Chain.genesis
      $ snd
      $ validChains cfg m (immutableDbBlocks m)

    isSameAsImmutableDbChain = \case
      Nothing
        | Chain.null (immutableDbChain m)
        -> (Chain.Genesis, initLedger m)
        | otherwise
        -> error "Did not select any chain"
      Just res@(chain, _ledger)
        | toHashes chain == toHashes (immutableDbChain m)
        -> res
        | otherwise
        -> error "Did not select the ImmutableDB's chain"

    toHashes = map blockHash . Chain.toOldestFirst

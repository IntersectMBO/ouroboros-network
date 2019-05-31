{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
    -- * Iterators
  , streamBlocks
  , iteratorNext
  , iteratorClose
    -- * Readers
  , readBlocks
  , readerInstruction
  , readerForward
    -- * Exported for testing purposes
  , blocks
  ) where

import           Control.Monad (unless)
import           Control.Monad.Except (runExcept)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           GHC.Generics (Generic)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HasHeader,
                     HeaderHash, Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (repeatedly)

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainUpdate (..), IteratorId (..), IteratorResult (..),
                     StreamFrom (..), StreamTo (..), UnknownRange (..),
                     fromNetworkChainUpdate, validBounds)

-- | Model of the chain DB
data Model blk = Model {
      blocks        :: Map (HeaderHash blk) blk
    , cps           :: CPS.ChainProducerState blk
    , currentLedger :: ExtLedgerState blk
    , initLedger    :: ExtLedgerState blk
    , iterators     :: Map IteratorId [blk]
    } deriving (Generic)

deriving instance
  ( OuroborosTag (BlockProtocol blk)
  , UpdateLedger                blk
  , Block.StandardHash          blk
  , Show                        blk
  ) => Show (Model blk)

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
                -> Either (ChainDbError blk) (Maybe blk)
getBlockByPoint pt = case Chain.pointHash pt of
    GenesisHash    -> const $ Left NoGenesisBlock
    BlockHash hash -> Right . getBlock hash

hasBlockByPoint :: HasHeader blk
                => Point blk -> Model blk -> Bool
hasBlockByPoint pt = case Chain.pointHash pt of
    GenesisHash    -> const False
    BlockHash hash -> hasBlock hash

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
    . Fragment.fromChain
    . fmap f
    . currentChain

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
             => SecurityParam
             -> StreamFrom blk -> StreamTo blk
             -> Model blk
             -> Either (ChainDbError blk)
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

-- We never delete iterators such that we can use the size of the map as the
-- next iterator id
iteratorClose :: IteratorId -> Model blk -> Model blk
iteratorClose _itrId m = m

{-------------------------------------------------------------------------------
  Readers
-------------------------------------------------------------------------------}

readBlocks :: HasHeader blk => Model blk -> (CPS.ReaderId, Model blk)
readBlocks m = (rdrId, m { cps = cps' })
  where
    (cps', rdrId) = CPS.initReader Chain.genesisPoint (cps m)

readerInstruction :: forall blk. HasHeader blk
                  => CPS.ReaderId
                  -> Model blk
                  -> (Maybe (ChainUpdate blk blk), Model blk)
readerInstruction rdrId m =
    rewrap $ CPS.readerInstruction rdrId (cps m)
  where
    rewrap :: Maybe (Chain.ChainUpdate blk, CPS.ChainProducerState blk)
           -> (Maybe (ChainUpdate blk blk), Model blk)
    rewrap Nothing            = (Nothing, m)
    rewrap (Just (upd, cps')) = ( Just (fromNetworkChainUpdate upd)
                                , m { cps = cps' }
                                )

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
        => SecurityParam -> StreamFrom blk -> StreamTo blk -> Model blk
        -> Either (UnknownRange blk) [blk]
between (SecurityParam k) from to m = do
    fork <- errFork
    -- Check that we're not streaming from an old fork
    case fork of
      []      -> return ()
      (blk:_)
        | Fragment.pointOnFragment (Block.blockPoint blk) currentFrag
          -- We're on the current chain
        -> return ()
        | Block.blockNo blk > blockNoAtImmDBTip
          -- The fork is recent enough
        -> return ()
        | otherwise
        -> Left $ ForkTooOld from
    return fork
  where
    currentFrag :: AnchoredFragment blk
    currentFrag = Fragment.fromChain (currentChain m)

    -- The 'BlockNo' of the tip of the Immutable DB
    blockNoAtImmDBTip :: BlockNo
    blockNoAtImmDBTip =
        either (const Chain.genesisBlockNo) (pred . Block.blockNo)
      . Fragment.last . Fragment.anchorNewest k
      $ currentFrag

    -- A fragment for each possible chain in the database
    fragments :: [AnchoredFragment blk]
    fragments = map Fragment.fromChain
              . chains
              . blocks
              $ m

    -- The fork that contained the start and end point, i.e. the fork to
    -- stream from.
    errFork :: Either (UnknownRange blk) [blk]
    errFork = anyFork (map cutOffAfterTo fragments) >>=
              cutOffBeforeFrom

    -- Select the first 'Right' in the list, otherwise return the last 'Left'.
    -- If the list is empty, a @'Left' ('MissingBlock' p)@ will be returned
    -- where @p@ correspond to the point in @to@.
    anyFork :: [Either (UnknownRange blk) (AnchoredFragment blk)]
            ->  Either (UnknownRange blk) (AnchoredFragment blk)
    anyFork (Right f : _ ) = Right f
    anyFork (Left  u : []) = Left u
    anyFork (Left  _ : fs) = anyFork fs
    anyFork []             = Left $ MissingBlock $ case to of
      StreamToInclusive p -> p
      StreamToExclusive p -> p

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
                     -> Either (UnknownRange blk) [blk]
    cutOffBeforeFrom frag = case from of
      StreamFromInclusive p
        | blks@(_:_) <- dropWhile (isNot p) $ Fragment.toOldestFirst frag
        -> return blks
        | otherwise
        -> Left $ MissingBlock p
      StreamFromExclusive p
        | Just frag' <- snd <$> Fragment.splitAfterPoint frag p
        -> return $ Fragment.toOldestFirst frag'
        | otherwise
        -> Left $ MissingBlock p

    isNot :: Point blk -> blk -> Bool
    p `isNot` b = p /= Block.blockPoint b

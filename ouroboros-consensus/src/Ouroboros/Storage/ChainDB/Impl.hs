{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs(..)
  , defaultArgs
  , openDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Exception (assert)
import           Control.Monad (unless)
import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict
import           Data.Foldable (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Control.Tracer

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (BlockNo, ChainHash (..),
                     HasHeader (..), HeaderHash, Point (..), StandardHash,
                     blockPoint, castPoint)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisBlockNo, genesisPoint)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (mapMaybeM)

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)

import           Ouroboros.Storage.ChainDB.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.VolDB as VolDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs m blk hdr = forall h1 h2 h3. ChainDbArgs {

      -- Decoders

      cdbDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , cdbDecodeBlock      :: forall s. Decoder s blk
    , cdbDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , cdbDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))

      -- Encoders

    , cdbEncodeBlock      :: blk -> Encoding
    , cdbEncodeHash       :: HeaderHash blk -> Encoding

      -- Error handling

    , cdbErrImmDb         :: ErrorHandling ImmDB.ImmutableDBError m
    , cdbErrVolDb         :: ErrorHandling (VolDB.VolatileDBError (HeaderHash blk)) m
    , cdbErrVolDbSTM      :: ThrowCantCatch (VolDB.VolatileDBError (HeaderHash blk)) (STM m)

      -- HasFS instances

    , cdbHasFSImmDb       :: HasFS m h1
    , cdbHasFSVolDb       :: HasFS m h2
    , cdbHasFSLgrDB       :: HasFS m h3

      -- Policy

    , cdbValidation       :: ImmDB.ValidationPolicy
    , cdbBlocksPerFile    :: Int
    , cdbMemPolicy        :: LgrDB.MemPolicy

      -- Integration

    , cdbNodeConfig       :: NodeConfig (BlockProtocol blk)
    , cdbEpochSize        :: EpochNo -> m EpochSize
    , cdbIsEBB            :: blk -> Maybe (HeaderHash blk)
    , cdbGetHeader        :: blk -> hdr
    , cdbGenesis          :: m (ExtLedgerState blk)
    }

-- | Default arguments for use within IO
--
-- See 'ImmDB.defaultArgs' and 'VolDB.defaultArgs' for a list of which fields
-- are not given a default and must therefore be set explicitly.
defaultArgs :: StandardHash blk => FilePath -> ChainDbArgs IO blk hdr
defaultArgs fp = toChainDbArgs ( ImmDB.defaultArgs fp
                               , VolDB.defaultArgs fp
                               , LgrDB.defaultArgs fp
                               )

-- | Internal: split chain DB args into imm DB and vol DB args
fromChainDbArgs :: ChainDbArgs m blk hdr
                -> ( ImmDB.ImmDbArgs m blk
                   , VolDB.VolDbArgs m blk hdr
                   , LgrDB.LgrDbArgs m blk
                   )
fromChainDbArgs ChainDbArgs{..} = (
      ImmDB.ImmDbArgs {
          immDecodeHash       = cdbDecodeHash
        , immDecodeBlock      = cdbDecodeBlock
        , immEncodeHash       = cdbEncodeHash
        , immErr              = cdbErrImmDb
        , immEpochSize        = cdbEpochSize
        , immValidation       = cdbValidation
        , immIsEBB            = cdbIsEBB
        , immHasFS            = cdbHasFSImmDb
        }
    , VolDB.VolDbArgs {
          volHasFS            = cdbHasFSVolDb
        , volErr              = cdbErrVolDb
        , volErrSTM           = cdbErrVolDbSTM
        , volBlocksPerFile    = cdbBlocksPerFile
        , volEncodeBlock      = cdbEncodeBlock
        , volDecodeBlock      = cdbDecodeBlock
        , volGetHeader        = cdbGetHeader
        }
    , LgrDB.LgrDbArgs {
          lgrNodeConfig       = cdbNodeConfig
        , lgrHasFS            = cdbHasFSLgrDB
        , lgrDecodeLedger     = cdbDecodeLedger
        , lgrDecodeChainState = cdbDecodeChainState
        , lgrDecodeHash       = cdbDecodeHash
        , lgrMemPolicy        = cdbMemPolicy
        , lgrGenesis          = cdbGenesis
        }
    )

-- | Internal: construct chain DB args from imm DB and vol DB
--
-- Useful in 'defaultArgs'
toChainDbArgs :: ( ImmDB.ImmDbArgs m blk
                 , VolDB.VolDbArgs m blk hdr
                 , LgrDB.LgrDbArgs m blk
                 )
              -> ChainDbArgs m blk hdr
toChainDbArgs ( ImmDB.ImmDbArgs{..}
              , VolDB.VolDbArgs{..}
              , LgrDB.LgrDbArgs{..}
              ) = ChainDbArgs{
      -- Decoders
      cdbDecodeHash       = immDecodeHash
    , cdbDecodeBlock      = immDecodeBlock
    , cdbDecodeLedger     = lgrDecodeLedger
    , cdbDecodeChainState = lgrDecodeChainState
      -- Encoders
    , cdbEncodeBlock      = volEncodeBlock
    , cdbEncodeHash       = immEncodeHash
      -- Error handling
    , cdbErrImmDb         = immErr
    , cdbErrVolDb         = volErr
    , cdbErrVolDbSTM      = volErrSTM
      -- HasFS instances
    , cdbHasFSImmDb       = immHasFS
    , cdbHasFSVolDb       = volHasFS
    , cdbHasFSLgrDB       = lgrHasFS
      -- Policy
    , cdbValidation       = immValidation
    , cdbBlocksPerFile    = volBlocksPerFile
    , cdbMemPolicy        = lgrMemPolicy
      -- Integration
    , cdbNodeConfig       = lgrNodeConfig
    , cdbEpochSize        = immEpochSize
    , cdbIsEBB            = immIsEBB
    , cdbGetHeader        = volGetHeader
    , cdbGenesis          = lgrGenesis
    }

{-------------------------------------------------------------------------------
  Internal environment
-------------------------------------------------------------------------------}

data ChainDbEnv m blk hdr = CDB {
      cdbImmDB          :: ImmDB m blk
    , cdbVolDB          :: VolDB m blk hdr
    , cdbLgrDB          :: LgrDB m blk
    , cdbChain          :: TVar m (AnchoredFragment hdr)
    , cdbConfig         :: NodeConfig (BlockProtocol blk)
    , cdbInvalid        :: TVar m (Set (Point blk))
    , cdbHeader         :: blk -> hdr
    , cdbNextIteratorId :: TVar m IteratorId
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

openDB :: forall m blk hdr.
          ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , MonadThrow (STM m)
          , HasHeader hdr
          , HeaderHash hdr ~ HeaderHash blk
          , ProtocolLedgerView blk
          , LedgerConfigView   blk

          , Show hdr
          )
       => ChainDbArgs m blk hdr -> m (ChainDB m blk hdr)
openDB args = do
    immDB   <- ImmDB.openDB argsImmDb
    volDB   <- VolDB.openDB argsVolDb
    lgrDB   <- LgrDB.openDB argsLgrDb
                            immDB
                            (getAnyKnownBlock immDB volDB)
    -- TODO initialise from the ImmutableDB & the VolatileDB
    chain          <- atomically $ newTVar $ Empty genesisPoint
    invalid        <- atomically $ newTVar Set.empty
    nextIteratorId <- atomically $ newTVar $ IteratorId 0
    let env = CDB { cdbImmDB          = immDB
                  , cdbVolDB          = volDB
                  , cdbLgrDB          = lgrDB
                  , cdbChain          = chain
                  , cdbConfig         = cdbNodeConfig args
                  , cdbInvalid        = invalid
                  , cdbHeader         = cdbGetHeader args
                  , cdbNextIteratorId = nextIteratorId
                  }
    return ChainDB {
        addBlock           = cdbAddBlock           env
      , getCurrentChain    = cdbGetCurrentChain    env
      , getCurrentLedger   = cdbGetCurrentLedger   env
      , getTipBlock        = cdbGetTipBlock        env
      , getTipHeader       = cdbGetTipHeader       env
      , getTipPoint        = cdbGetTipPoint        env
      , getBlock           = cdbGetBlock           env
      , getIsFetched       = cdbGetIsFetched       env
      , streamBlocks       = cdbStreamBlocks       env
      , readBlocks         = undefined
      , readHeaders        = undefined
      , knownInvalidBlocks = cdbKnownInvalidBlocks env
      , closeDB            = cdbCloseDB            env
      }
  where
    (argsImmDb, argsVolDb, argsLgrDb) = fromChainDbArgs args

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

cdbCloseDB :: (MonadCatch m, HasHeader blk) => ChainDbEnv m blk hdr -> m ()
cdbCloseDB CDB{..} = do
    -- TODO
    ImmDB.closeDB cdbImmDB
    VolDB.closeDB cdbVolDB

cdbGetIsFetched :: forall m blk hdr. MonadSTM m
                => ChainDbEnv m blk hdr
                -> STM m (Point blk -> Bool)
cdbGetIsFetched CDB{..} = basedOnHash <$> VolDB.getIsMember cdbVolDB
  where
    -- The volatile DB indexes by hash only, not by points. However, it should
    -- not be possible to have two points with the same hash but different
    -- slot numbers.
    basedOnHash :: (HeaderHash blk -> Bool) -> Point blk -> Bool
    basedOnHash f p =
        case Block.pointHash p of
          BlockHash hash -> f hash
          GenesisHash    -> False

cdbGetCurrentChain :: MonadSTM m
                   => ChainDbEnv m blk hdr
                   -> STM m (AnchoredFragment hdr)
cdbGetCurrentChain CDB{..} = readTVar cdbChain

cdbGetCurrentLedger :: MonadSTM m
                    => ChainDbEnv m blk hdr
                    -> STM m (ExtLedgerState blk)
cdbGetCurrentLedger CDB{..} = LgrDB.getCurrentState cdbLgrDB

cdbGetTipPoint :: ( MonadSTM m
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> STM m (Point blk)
cdbGetTipPoint = fmap (Block.castPoint . Fragment.headPoint)
               . cdbGetCurrentChain

cdbGetTipBlock :: ( MonadCatch m
                  , MonadSTM m
                  , HasHeader blk
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> m (Maybe blk)
cdbGetTipBlock cdb@CDB{..} = do
    tipPoint <- atomically $ cdbGetTipPoint cdb
    if tipPoint == genesisPoint
      then return Nothing
      else Just <$> getAnyKnownBlock cdbImmDB cdbVolDB tipPoint

cdbGetTipHeader :: ( MonadSTM m
                   , HasHeader hdr
                   )
                => ChainDbEnv m blk hdr
                -> m (Maybe hdr)
cdbGetTipHeader CDB{..} =
    eitherToMaybe . Fragment.head <$> atomically (readTVar cdbChain)
  where
    eitherToMaybe = either (const Nothing) Just

cdbKnownInvalidBlocks :: MonadSTM m
                      => ChainDbEnv m blk hdr
                      -> STM m (Set (Point blk))
cdbKnownInvalidBlocks CDB{..} = readTVar cdbInvalid

cdbGetBlock :: ( MonadCatch m
               , HasHeader blk
               )
            => ChainDbEnv m blk hdr
            -> Point blk -> m (Maybe blk)
cdbGetBlock CDB{..} = getAnyBlock cdbImmDB cdbVolDB

cdbStreamBlocks :: ( MonadCatch m
                   , MonadSTM   m
                   , MonadThrow (STM m)
                   , HasHeader blk
                   )
                => ChainDbEnv m blk hdr
                -> StreamFrom blk -> StreamTo blk
                -> m (Either (UnknownRange blk) (Iterator m blk))
cdbStreamBlocks CDB{..} = implStreamBlocks cdbImmDB cdbVolDB makeNewIteratorId
  where
    makeNewIteratorId = atomically $ do
      newIteratorId <- readTVar cdbNextIteratorId
      modifyTVar' cdbNextIteratorId succ
      return newIteratorId

-- | Auxiliary data type for 'cdbAddBlock' for a chain with a ledger that
-- corresponds to it.
--
-- INVARIANT:
-- > for (x :: ChainAndLedger blk hdr),
-- >   Fragment.headPoint (_chain x) == LgrDB.currentPoint (_ledger x)
data ChainAndLedger blk hdr = ChainAndLedger
  { _chain  :: !(AnchoredFragment hdr)
    -- ^ Chain fragment
  , _ledger :: !(LgrDB.LedgerDB blk)
    -- ^ Ledger corresponding to '_chain'
  }

mkChainAndLedger
    :: ( HasHeader blk
       , HasHeader hdr
       , HeaderHash blk ~ HeaderHash hdr
       , UpdateLedger blk
       )
    => AnchoredFragment hdr -> LgrDB.LedgerDB blk
    -> ChainAndLedger blk hdr
mkChainAndLedger c l =
    assert (castPoint (Fragment.headPoint c) == LgrDB.currentPoint l) $
    ChainAndLedger c l


-- | TODO
--
-- TODO add @blockSlot b <= currentSlot@ as a precondition (guaranteed by the
-- ChainSyncClient)
-- TODO ^ incorporate ClockSkew
--
--
-- = Constructing candidate fragments
--
-- The VolatileDB keeps a \"successors\" map in memory, telling us the hashes
-- of the known successors of any block, but it does not keep /headers/ in
-- memory, which are needed to construct candidate fargments. We try to reuse
-- the headers from the current chain fragment where possible, but it will not
-- contain all needed headers. This means that we will need to read some
-- blocks from disk and extract their headers. Under normal circumstances this
-- does not matter too much; although this will be done every time we add a
-- block, the expected number of headers to read from disk is very small:
--
-- * None if we stay on the current chain and this is just the next block
-- * A handful if we stay on the current chain and the block we just received
--   was a missing block and we already received some of its successors
-- * A handful if we switch to a short fork
--
-- This is expensive only
--
-- * on startup: in this case we need to read at least @k@ blocks from the
--   VolatileDB, and possibly more if there are some other chains in the
--   VolatileDB starting from the tip of the ImmutableDB
-- * when we switch to a distant fork
--
-- This cost is currently deemed acceptable.
--
-- TODO: It might be possible with some low-level hackery to avoid reading the
-- whole block and read only the header directly.
--
-- TODO: We might want to add some microbenchmarking here to measure the cost.
cdbAddBlock :: forall m blk hdr.
               ( MonadSTM   m
               , MonadCatch m
               , HasHeader blk
               , HasHeader hdr
               , HeaderHash hdr ~ HeaderHash blk
               , ProtocolLedgerView blk

               , Show hdr
               )
            => ChainDbEnv m blk hdr
            -> blk -> m ()
cdbAddBlock cdb@CDB{..} b = do
    -- TODO check that we don't get an invalid block (cdbInvalid)?

    (isMember, curChain, tipPoint, ledgerDB) <- atomically $
      (,,,) <$> VolDB.getIsMember    cdbVolDB
            <*> cdbGetCurrentChain   cdb
            <*> cdbGetTipPoint       cdb
            <*> LgrDB.getCurrent     cdbLgrDB
    let blockNoAtImmDBTip = getBlockNoAtImmDBTip curChain
        curChainAndLedger = mkChainAndLedger curChain ledgerDB

    -- We follow the steps from section "## Adding a block" in ChainDB.md

    -- ### Ignore
    unless (blockNo b <= blockNoAtImmDBTip || isMember (blockHash b)) $ do

      -- Write the block to the VolatileDB in all other cases
      VolDB.putBlock cdbVolDB b

      trace $ "Added " <> show (blockPoint b) <> " to the VolatileDB"

      -- We need to get these after adding the block to the VolatileDB
      (isMember', predecessor, succsOf) <- atomically $
         (,,) <$> VolDB.getIsMember    cdbVolDB
              <*> VolDB.getPredecessor cdbVolDB
              <*> VolDB.getSuccessors  cdbVolDB

      -- The block @b@ fits onto the end of our current chain
      if | pointHash tipPoint == blockPrevHash b ->
           -- ### Add to current chain
           addToCurrentChain succsOf curChainAndLedger

         | Just hashes <- VolDB.isReachable predecessor isMember'
             (getPointAtImmDBTip curChain) (blockPoint b) ->
           -- ### Switch to a fork
           switchToAFork succsOf curChainAndLedger hashes

         | otherwise ->
           -- ### Store but don't change the current chain

           -- We have already stored the block in the VolatileDB
           trace "Store but don't change"

      -- TODO Note that we may have extended the chain, but have not trimmed
      -- it to @k@ blocks/headers. That is the job of the background thread,
      -- which will first copy the blocks/headers to trim (from the end of the
      -- fragment) from the VolatileDB to the ImmutableDB.

  where
    trace :: String -> m ()
    trace = traceWith nullTracer

    -- | The BlockNo of the first header (the oldest) on the current chain
    -- fragment is the BlockNo of the ImmutableDB tip + 1, so we just subtract
    -- 1. If the current chain is empty, use the BlockNo of Genesis.
    --
    -- TODO what if the VolatileDB was corrupted?
    getBlockNoAtImmDBTip :: AnchoredFragment hdr -> BlockNo
    getBlockNoAtImmDBTip =
      either (const genesisBlockNo) (pred . blockNo) . Fragment.last

    getPointAtImmDBTip :: AnchoredFragment hdr -> Point blk
    getPointAtImmDBTip = castPoint . Fragment.anchorPoint

    -- | PRECONDITION: the header @hdr@ (and block @b@) fit onto the end of
    -- the current chain.
    addToCurrentChain :: HasCallStack
                      => (Maybe (HeaderHash blk) -> Set (HeaderHash blk))
                      -> ChainAndLedger blk hdr
                         -- ^ The current chain and ledger
                      -> m ()
    addToCurrentChain succsOf curChainAndLedger@(ChainAndLedger curChain _) =
        assert (Fragment.validExtension curChain hdr) $ do
          trace "Adding to the current chain"
          let suffixesAfterB = VolDB.candidates succsOf (blockPoint b)

          candidates <- case NE.nonEmpty suffixesAfterB of
            -- If there are no suffixes after @b@, just use the fragment
            -- that ends in @b@ as the sole candidate.
            Nothing              -> return $ (curChain :> hdr) NE.:| []
            Just suffixesAfterB' -> do
              let initCache =
                    Map.insert (blockHash b) hdr (cacheHeaders curChain)
              flip evalStateT initCache $ forM suffixesAfterB' $ \hashes -> do
                hdrs <- mapM getKnownHeaderThroughCache $ NE.toList hashes
                let suffixChain = Fragment.fromOldestFirst
                      (Fragment.headPoint curChain) (hdr : hdrs)
                return $ joinSuffix curChain suffixChain

          chainSelection curChainAndLedger candidates $ \case
            Nothing                -> return ()
            Just newChainAndLedger -> do
              _updated <- atomically $ trySwitchTo newChainAndLedger
              -- TODO what should we do when the chain has changed in the
              -- meantime and this fails? If we call 'cdbAddBlock' again, we
              -- will end up in Ignore.
              return ()
      where
        hdr = cdbHeader b
        joinSuffix prefix suffix = fromMaybe (error "invalid suffix")
                                 $ Fragment.join prefix suffix

    -- | TODO
    switchToAFork :: HasCallStack
                  => (Maybe (HeaderHash blk) -> Set (HeaderHash blk))
                  -> ChainAndLedger blk hdr
                     -- ^ The current chain (anchored at @i@) and ledger
                  -> NonEmpty (HeaderHash blk)
                     -- ^ An uninterrupted path of hashes @(i,b]@.
                  -> m ()
    switchToAFork succsOf curChainAndLedger@(ChainAndLedger curChain _) hashes = do
        trace "Switching to a fork"
        let suffixesAfterB = VolDB.candidates succsOf (blockPoint b)
            i              = castPoint $ getPointAtImmDBTip curChain
            initCache      = Map.insert (blockHash b) hdr (cacheHeaders curChain)
        candidates <- flip evalStateT initCache $
          case NE.nonEmpty suffixesAfterB of
            -- If there are no suffixes after @b@, just use the fragment that
            -- ends in @b@ as the sole candidate.
            Nothing              -> (NE.:| []) <$> constructFork i hashes []
            Just suffixesAfterB' -> mapM (constructFork i hashes . NE.toList)
                                         suffixesAfterB'

        chainSelection curChainAndLedger candidates $ \case
          Nothing                -> return ()
          Just newChainAndLedger -> do
            _updated <- atomically $ trySwitchTo newChainAndLedger
            -- TODO what should we do when the chain has changed in the
            -- meantime and this fails? If we call 'cdbAddBlock' again, we
            -- will end up in Ignore.
            return ()
      where
        hdr = cdbHeader b

    -- | Try to swap the current (chain) fragment with the given candidate
    -- fragment. The 'LgrDB.LedgerDB' is updated in the same transaction.
    --
    -- Note that the current chain might have changed in the meantime. We only
    -- switch when the new chain is preferred over the current chain, in which
    -- case we return 'True'.
    trySwitchTo :: ChainAndLedger blk hdr  -- ^ Chain and ledger to switch to
                -> STM m Bool
    trySwitchTo (ChainAndLedger newChain newLedger) = do
      curChain <- readTVar cdbChain
      if preferCandidate cdbConfig curChain newChain
        then do
          -- The current chain might be longer than @k@ headers because it
          -- might still contain some old headers that have not yet been
          -- written to the ImmutableDB. We must make sure to prepend these
          -- headers to the new chain so that we still know what the full
          -- chain is.
          let (p1, _p2, _s1, s2) = fromMaybe
                (error "new chain doesn't intersect with the current chain") $
                Fragment.intersect curChain newChain
              newChain' = fromMaybe
                (error "postcondition of intersect violated") $
                Fragment.join p1 s2
          writeTVar cdbChain newChain'
          LgrDB.setCurrent cdbLgrDB newLedger
          return True
        else
          return False

    -- | TODO
    --
    -- PRECONDITION: the candidates must be anchored at the same point as the
    -- current chain.
    chainSelection
        :: HasCallStack
        => ChainAndLedger blk hdr           -- ^ The current chain and ledger
        -> NonEmpty (AnchoredFragment hdr)  -- ^ Candidates
        -> (Maybe (ChainAndLedger blk hdr) -> m r)
           -- ^ Called with the (valid) chain and corresponding LedgerDB that
           -- was selected, or 'Nothing' if there is no valid chain preferred
           -- over the current chain.
        -> m r
    chainSelection curChainAndLedger@(ChainAndLedger curChain _) candidates k =
        assert (all ((== Fragment.anchorPoint curChain) . Fragment.anchorPoint)
                    candidates) $
        trace ("Candidates: " <> show candidates) >>
        -- First filter so that only candidates preferred over our current
        -- chain remain. This filtering is much cheaper than validation so
        -- doing it before validation makes sense. This is safe because two
        -- filtering operations permute.
        case NE.nonEmpty $
             NE.filter (preferCandidate cdbConfig curChain) candidates of
          -- No candidates preferred over our current chain
          Nothing -> k Nothing

          -- If there's only one candidate. We can remember the validated
          -- ledger state.
          Just (cand NE.:| []) ->
            validateCandidate curChainAndLedger cand >>= \case
              Nothing                -> k Nothing  -- TODO trace something?
              Just newChainAndLedger -> k (Just newChainAndLedger)

          -- If there are multiple candidates, validate them, but don't remember
          -- the resulting ledger states, because that would take too much
          -- memory. Instead, after choosing a candidate, revalidate again. This
          -- time it will be faster, as some checks can be omitted.
          --
          Just cands -> do
            -- Note that we're extracting the validated chains, which might be
            -- shorter than the given chains. E.g., when the last block of a
            -- candidate chain is invalid, the chain without the last block is
            -- returned.
            validCands <- mapMaybeM
              (fmap (fmap _chain) . validateCandidate curChainAndLedger)
              (NE.toList cands)
            -- Now select the "best" among the candidate fragments. Important:
            -- in 'selectFragment', we first filter out candidate fragments
            -- that are not preferred over the current chain, because
            -- validation might have trimmed some chains. TODO only do this
            -- for trimmed chains?
            case selectFragment cdbConfig curChain validCands of
              Nothing   -> k Nothing
              Just cand -> revalidateChain curChainAndLedger cand >>= (k . Just)

    -- | Validate a fragment and return a 'ChainAndLedger' for it, i.e. a
    -- validated fragment along with a ledger corresponding to its tip (most
    -- recent block).
    --
    -- PRECONDITION: the candidate fragment must have the same anchor point as
    -- the current chain fragment.
    --
    -- PRECONDITION: the candidate fragment must contain as least as many
    -- blocks as the current chain fragment.
    --
    -- If all blocks in the fragment are valid, then the fragment in the
    -- returned 'ChainAndLedger' is the same as the given candidate fragment.
    --
    -- If a block in the fragment is invalid, then the fragment in the
    -- returned 'ChainAndLedger' is a prefix of the given candidate fragment
    -- (upto the last valid block), if that fragment is still preferred
    -- ('preferCandidate') over the current chain, if not, 'Nothing' is
    -- returned.
    validateCandidate :: HasCallStack
                      => ChainAndLedger blk hdr  -- ^ Current chain and ledger
                      -> AnchoredFragment hdr    -- ^ Candidate fragment
                      -> m (Maybe (ChainAndLedger blk hdr))
    validateCandidate (ChainAndLedger curChain curLedger) candidate =
      LgrDB.validate cdbLgrDB curLedger rollback newBlocks >>= \case
        LgrDB.InvalidBlockInPrefix _e pt -> do
          -- TODO trace
          atomically $ modifyTVar' cdbInvalid (Set.insert pt)
          return Nothing
        LgrDB.PushSuffix (LgrDB.InvalidBlock _e pt ledger') -> do
          -- TODO trace
          atomically $ modifyTVar' cdbInvalid (Set.insert pt)
          let lastValid  = castPoint $ LgrDB.currentPoint ledger'
              candidate' = fromMaybe
                (error "cannot rollback to point on fragment") $
                Fragment.rollback lastValid candidate
          assert (preferCandidate cdbConfig curChain candidate' ) $
            return $ Just $ mkChainAndLedger candidate' ledger'
        LgrDB.PushSuffix (LgrDB.ValidBlocks ledger') ->
          return $ Just $ mkChainAndLedger candidate ledger'
      where
        (rollback, candSuffix) = candidateRollbackAndSuffix curChain candidate
        newBlocks = map toHdrOrBlk (Fragment.toOldestFirst candSuffix)

    -- | Variant of 'validateCandidate' for when the chain fragment already
    -- has been validated with 'validateCandidate'.
    --
    -- The same preconditions apply as for 'validateCandidate'.
    revalidateChain :: HasCallStack
                    => ChainAndLedger blk hdr  -- ^ Current chain and ledger
                    -> AnchoredFragment hdr    -- ^ Candidate fragment
                    -> m (ChainAndLedger blk hdr)
    revalidateChain (ChainAndLedger curChain curLedger) candidate =
        LgrDB.revalidate cdbLgrDB curLedger rollback newBlocks >>= \case
          LgrDB.PushSuffix (LgrDB.ValidBlocks ledger') ->
            return $ mkChainAndLedger candidate ledger'
      where
        (rollback, candSuffix) = candidateRollbackAndSuffix curChain candidate
        newBlocks = map toHdrOrBlk (Fragment.toOldestFirst candSuffix)

    -- | Find the intersection point between the current chain and the
    -- candidate fragment. Return the number of blocks to roll the /current
    -- chain/ back and the suffix of the candidate fragment.
    --
    -- PRECONDITION: the candidate fragment must have the same anchor point as
    -- the current chain fragment.
    --
    -- PRECONDITION: the candidate fragment must contain as least as many
    -- blocks as the current chain fragment.
    --
    -- POSTCONDITION: @(n, suffix)@: rolling back the current chain (dropping
    -- the newest @n@) and then joining the resulting fragment and the
    -- @suffix@ will be equal to the given candidate fragment.
    candidateRollbackAndSuffix
      :: AnchoredFragment hdr  -- ^ Current chain
      -> AnchoredFragment hdr  -- ^ Candidate chain
      -> (Word64, AnchoredFragment hdr)
         -- ^ How many blocks to roll back and the candidate suffix
    candidateRollbackAndSuffix curChain candChain =
      case Fragment.intersect curChain candChain of
        Just (curChainPrefix, candPrefix, curChainSuffix, candSuffix)
          | Fragment.anchorPoint curChainPrefix ==
            Fragment.anchorPoint candPrefix
          , let rollback = Fragment.length curChainSuffix
          , Fragment.length candSuffix >= rollback
          -> (fromIntegral rollback, candSuffix)
        -- Precondition violated.
        _ -> error $ "invalid candidate fragment: " <> show candChain

    -- | In case the @hdr@ matches the block we're adding (@b@), return that,
    -- so we avoid reading it from disk again (when passed to
    -- 'LgrDB.validate').
    toHdrOrBlk :: hdr -> Either hdr blk
    toHdrOrBlk hdr
      | castPoint (blockPoint hdr) == blockPoint b = Right b
      | otherwise                                  = Left  hdr

    -- | Build a cache from the headers in the fragment.
    cacheHeaders :: AnchoredFragment hdr -> Map (HeaderHash blk) hdr
    cacheHeaders =
      foldl' (\m hdr -> Map.insert (blockHash hdr) hdr m) Map.empty .
      Fragment.toNewestFirst

    -- | Check whether the header for the hash is in the cache, if not, get
    -- the corresponding header from the VolatileDB and store it in the cache.
    -- The header (block) must exist in the VolatileDB.
    getKnownHeaderThroughCache :: HeaderHash blk
                               -> StateT (Map (HeaderHash blk) hdr) m hdr
    getKnownHeaderThroughCache hash = gets (Map.lookup hash) >>= \case
      Just hdr -> return hdr
      Nothing  -> do
        hdr <- lift $ VolDB.getKnownHeader cdbVolDB hash
        modify (Map.insert hash hdr)
        return hdr

    -- | We have a new block @b@ that doesn't fit onto the current chain, but
    -- there is an unbroken path from the tip of the ImmutableDB (@i@ = the
    -- anchor point of the current chain) to @b@. We also have a suffix @s@ of
    -- hashes that starts after @b@.
    --
    -- We will try to construct a fragment @f@ for the fork such that:
    -- * @f@ is anchored at @i@
    -- * @f@ starts with the headers corresponding to the hashes of @(i,b]@
    -- * The next header in @f@ is the header for @b@
    -- * Finally, @f@ ends with the headers corresponding to the hashes
    --   @(b,?]@ of the suffix @s@.
    --
    -- Note that we need to read the headers corresponding to the hashes
    -- @(i,b]@ and @(b,?]@ from disk. It is likely that many of these headers
    -- are actually on the current chain, so when possible, we reuse these
    -- headers instead of reading them from disk.
    constructFork
      :: Point hdr                  -- ^ Tip of ImmutableDB @i@
      -> NonEmpty (HeaderHash blk)  -- ^ Hashes of @(i,b]@
      -> [HeaderHash blk]           -- ^ Suffix @s@, hashes of @(b,?]@
      -> StateT (Map (HeaderHash blk) hdr)
                 m (AnchoredFragment hdr)
         -- ^ Fork, anchored at @i@, contains (the header of) @b@ and ends
         -- with the suffix @s@.
    constructFork i hashes suffixHashes
      = fmap (Fragment.fromOldestFirst i)
      $ mapM getKnownHeaderThroughCache
      $ NE.toList hashes <> suffixHashes

{-------------------------------------------------------------------------------
  Lower level functionality

  These are functions that don't require all parts of the ChainDB to have
  been initialized
-------------------------------------------------------------------------------}

-- | Stream blocks
--
-- TODO handle corruption and recovery
-- TODO test that EBBs are handled correctly
-- TODO the ImmDB.Iterator isn't guaranteed to be closed in case of an
-- exception, we would need something like ResourceT for that.
--
-- = Start & end point
--
-- The start point can either be in the ImmutableDB (on our chain) or in the
-- VolatileDB (on our chain or on a recent fork). We first check whether it is
-- in the VolatileDB, if not, we check if it is in the ImmutableDB (see
-- \"Garbage collection\" for why this order is important). Similarly for the
-- end point.
--
-- If a bound can't be found in the ChainDB, an 'UnknownRange' error is
-- returned.
--
-- When the bounds are nonsensical, e.g.,
-- > StreamFromExclusive (Point { pointSlot = SlotNo 3 , .. }
-- > StreamToInclusive   (Point { pointSlot = SlotNo 3 , .. }
-- An 'InvalidIteratorRange' exception is thrown.
--
-- = Paths of blocks
--
-- To stream blocks from the ImmutableDB we can simply use the iterators
-- offered by the ImmutableDB.
--
-- To stream blocks from the VolatileDB we have to construct a path of block
-- hashes backwards through the VolatileDB, starting from the end point using
-- 'getPredecessor' until we get to the start point, genesis, or we get to a
-- block that is not in the VolatileDB. Then, for each hash in the path, we
-- can ask the VolatileDB for the corresponding block.
--
-- If the path through the VolatileDB is incomplete, we will first have to
-- stream blocks from the ImmutableDB and then switch to the path through the
-- VolatileDB. We only allow the tip of the ImmutableDB to be the switchover
-- point between the two DBs. In other words, the incomplete path through the
-- VolatileDB must fit onto the tip of the ImmutableDB. This must be true at
-- the time of initialising the iterator, but does not have to hold during the
-- whole lifetime of the iterator. If it doesn't fit on it, it means the path
-- forked off more than @k@ blocks in the past and blocks belonging to it are
-- more likely to go missing because of garbage-collection (see the next
-- paragraph). In that case, we return 'ForkTooOld'.
--
-- = Garbage collection
--
-- We have to be careful about the following: as our chain grows, blocks from
-- our chain will be copied to the ImmutableDB in the background. After a
-- while, old blocks will be garbage-collected from the VolatileDB. Blocks
-- that were part of the current chain will be in the ImmutableDB, but blocks
-- that only lived on forks will be gone forever.
--
-- This means that blocks that were part of the VolatileDB when the iterator
-- was initialised might no longer be part of the VolatileDB when we come to
-- the point that the iterator will try to read them. When this is noticed, we
-- will try to open an iterator from the ImmutableDB to obtain the blocks that
-- have moved over. However, this will only work if they were and are part of
-- the current chain, otherwise they will have been deleted from the
-- VolatileDB without being copied to the ImmutableDB.
--
-- This iterator is opened with an open upper bound and will be used to stream
-- blocks until the path has been fully streamed, the iterator is exhausted,
-- or a block doesn't match the expected hash. In the latter two cases, we
-- switch back to the VolatileDB. If the block is missing from the VolatileDB,
-- we will switch back to streaming from the ImmutableDB. If that fails, we
-- switch back to the VolatileDB. To avoid eternally switching between the two
-- DBs, we only switch back to the VolatileDB if the stream from the
-- ImmutableDB has made progress, i.e. streamed at least one block with the
-- expected hash. If no block was streamed from the ImmutableDB, not even the
-- first one, we know for sure that that block isn't part of the VolatileDB
-- (the reason we switch to the ImmutableDB) and isn't part of the ImmutableDB
-- (no block was streamed). In that case, we return 'IteratorBlockGCed' and
-- stop the stream.
--
-- Note that the open upper bound doesn't allow us to include blocks in the
-- stream that are copied to the ImmutableDB after opening this iterator, as
-- the bound of the iterator is fixed upon initialisation. These newly added
-- blocks will be included in the stream because we will repeatedly open new
-- ImmutableDB iterators (as long as we make progress).
--
-- = Bounds checking
--
-- The ImmutableDB is slot-based instead of point-based, which means that
-- before we know whether a block in the ImmutableDB matches a given point, we
-- must first read that block to obtain its hash, after which we can then
-- verify whether it matches the hash of the point. This is important for the
-- start and end bounds (both points) of a stream: we must first read the
-- blocks corresponding to the bounds to be sure the range is valid. Note that
-- these reads happen before the first call to 'iteratorNext' (which will
-- trigger a second read of the first block).
--
-- Note that when streaming to an /exclusive/ bound, the block corresponding
-- to that bound ('Point') must exist in the ChainDB.
--
-- = Costs
--
-- Opening an iterator has some costs:
--
-- * When blocks have to be streamed from the ImmutableDB: as discussed in
--   \"Bounds checking\", the blocks corresponding to the bounds have to be
--   read from disk.
--
-- * When blocks have to be streamed both from the ImmutableDB and the
--   VolatileDB, the blocks corresponding to the two bound will have to be
--   read upfront, as described in the previous bullet point. Since the tip of
--   the ImmutableDB must be the switchover point between the two, it will be
--   the upper bound.
--
-- In summary:
--
-- * Only streaming from the VolatileDB: 0 blocks read upfront.
-- * Only streaming from the ImmutableDB: 2 blocks read upfront.
-- * Streaming from both the ImmutableDB and the VolatileDB: 2 blocks read
--   upfront.
--
-- Additionally, when we notice during streaming that a block is no longer in
-- the VolatileDB, we try to see whether it can be streamed from the
-- ImmutableDB instead. Opening such an iterator (with an exclusive bound) has
-- the cost of reading (but not parsing) one extra block from disk, in
-- addition to the block(s) we are actually interested in. This can happen
-- multiple times. See #548.
implStreamBlocks :: forall m blk hdr.
                    ( MonadCatch m
                    , MonadSTM   m
                    , MonadThrow (STM m)
                    , HasHeader blk
                    )
                 => ImmDB m blk
                 -> VolDB m blk hdr
                 -> m IteratorId   -- ^ How to make a new 'IteratorId'
                 -> StreamFrom blk -> StreamTo blk
                 -> m (Either (UnknownRange blk) (Iterator m blk))
implStreamBlocks immDB volDB makeNewIteratorId from to = do
    unless (validBounds from to) $
      throwM $ InvalidIteratorRange from to
    runExceptT start
  where
    start :: ExceptT (UnknownRange blk) m (Iterator m blk)
    start = do
      path <- lift $ atomically $ VolDB.computePathSTM volDB from to
      case path of
        VolDB.NotInVolDB        _hash           -> streamFromImmDB
        VolDB.PartiallyInVolDB  predHash hashes -> streamFromBoth predHash hashes
        VolDB.CompletelyInVolDB hashes          -> case NE.nonEmpty hashes of
          Just hashes' -> lift $ streamFromVolDB hashes'
          Nothing      -> lift $ emptyIterator

    streamFromVolDB :: NonEmpty (HeaderHash blk) -> m (Iterator m blk)
    streamFromVolDB = createIterator . InVolDB from

    streamFromImmDB :: ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromImmDB = streamFromImmDBHelper True

    streamFromImmDBHelper :: Bool -- ^ Check the hash of the upper bound
                          -> ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromImmDBHelper checkUpperBound = do
        -- First check whether the block in the ImmDB at the end bound has the
        -- correct hash.
        when checkUpperBound $
          lift (ImmDB.getBlockWithPoint immDB endPoint) >>= \case
            Just _  -> return ()
            Nothing -> throwError $ MissingBlock endPoint
        -- 'ImmDB.streamBlocksFrom' will check the hash of the block at the
        -- start bound.
        immIt <- ExceptT $ ImmDB.streamBlocksFrom immDB from
        lift $ createIterator $ InImmDB from immIt (StreamTo to)
      where
        endPoint = case to of
          StreamToInclusive pt -> pt
          StreamToExclusive pt -> pt

    -- | If we have to stream from both the ImmutableDB and the VolatileDB, we
    -- only allow the (current) tip of the ImmutableDB to be the switchover
    -- point between the two DBs. If not, this would mean we have to stream a
    -- fork that forks off more than @k@ blocks in the past, in which case the
    -- risk of blocks going missing due to GC increases. So we refuse such a
    -- stream.
    streamFromBoth :: HeaderHash blk
                   -> [HeaderHash blk]
                   -> ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromBoth predHash hashes = lift (ImmDB.getBlockAtTip immDB) >>= \case
        -- The ImmutableDB is empty
        Nothing -> throwError $ ForkTooOld from
        -- The incomplete path fits onto the tip of the ImmutableDB.
        Just blk | blockHash blk == predHash -> case NE.nonEmpty hashes of
          Just hashes' -> stream (blockPoint blk) hashes'
          -- The path is actually empty, but the exclusive upper bound was in
          -- the VolatileDB. Just stream from the ImmutableDB without checking
          -- the upper bound (which might not be in the ImmutableDB)
          Nothing      -> streamFromImmDBHelper False
        -- The incomplete path doesn't fit onto the tip of the ImmutableDB.
        -- Note that since we have constructed the incomplete path through the
        -- VolatileDB, blocks might have moved from the VolatileDB to the
        -- ImmutableDB so that the tip of the ImmutableDB has changed. Either
        -- the path used to fit onto the tip but the tip has changed, or the
        -- path simply never fitted onto the tip.
        Just blk -> case dropWhile (/= blockHash blk) hashes of
          -- The current tip is not in the path, this means that the path
          -- never fitted onto the tip of the ImmutableDB. We refuse this
          -- stream.
          []                    -> throwError $ ForkTooOld from
          -- The current tip is in the path, with some hashes after it, this
          -- means that some blocks in our path have moved from the VolatileDB
          -- to the ImmutableDB. We can shift the switchover point to the
          -- current tip.
          _tipHash:hash:hashes' -> stream (blockPoint blk) (hash NE.:| hashes')
          -- The current tip is the end of the path, this means we can
          -- actually stream everything from just the ImmutableDB. No need to
          -- check the hash at the upper bound again.
          [_tipHash]            -> streamFromImmDBHelper False

      where
        stream pt hashes' = do
          let immEnd = SwitchToVolDBFrom (StreamToInclusive pt) hashes'
          immIt <- ExceptT $ ImmDB.streamBlocksFrom immDB from
          lift $ createIterator $ InImmDB from immIt immEnd

    emptyIterator :: m (Iterator m blk)
    emptyIterator = createIterator Closed

    createIterator :: IteratorState m blk -> m (Iterator m blk)
    createIterator itState = do
      iteratorId <- makeNewIteratorId
      varItState <- newTVarM itState
      return Iterator {
          iteratorNext  = implIteratorNext  varItState immDB volDB
        , iteratorClose = implIteratorClose varItState
        , iteratorId    = iteratorId
        }

    implIteratorClose :: TVar m (IteratorState m blk) -> m ()
    implIteratorClose varItState = atomically (readTVar varItState) >>= \case
      Closed                 -> return ()
      InImmDB _ immIt _      -> do
        ImmDB.iteratorClose immIt
        atomically $ writeTVar varItState Closed
      InImmDBRetry _ immIt _ -> do
        ImmDB.iteratorClose immIt
        atomically $ writeTVar varItState Closed
      InVolDB {}             ->
        atomically $ writeTVar varItState Closed

-- | Possible states of an iterator.
--
-- When streaming solely from the ImmutableDB ('InImmDB' where 'InImmDBEnd' is
-- /not/ 'SwitchToVolDBFrom'): we will remain in this state until we are done,
-- and end up in 'Closed'.
--
-- When streaming solely from the VolatileDB ('InVolDB'): when
-- 'VolDB.getBlock' returns 'Nothing', i.e. the block is missing from the
-- VolatileDB and might have moved to the ImmutableDB: we switch to the
-- 'InImmDBRetry' state, unless we just come from that state, in that case,
-- return 'IteratorBlockGCed' and close the iterator.
--
-- When streaming from the ImmutableDB with a planned switchover to the
-- VolatileDB ('InImmDB' where 'InImmDBEnd' is 'SwitchToVolDBFrom') and we
-- have reached the end of the ImmutableDB iterator (exhausted or upper bound
-- is reached): we switch to the 'InVolDB' state.
--
-- In the 'InImmDBRetry' state, we distinguish two cases:
--
-- 1. We have just switched to it because a block was missing from the
--    VolatileDB. We have an iterator that could stream this block from the
--    ImmutableDB (if it was indeed moved to the ImmutableDB). If the streamed
--    block matches the expected hash, we continue. If not, or if the iterator
--    is immediately exhausted, then the block is missing and we return
--    'IteratorBlockGCed' and close the iterator.
--
-- 2. We have successfully streamed one or more blocks from the ImmutableDB
--    that were previously part of the VolatileDB. When we now encounter a
--    block of which the hash does not match the expected hash or when the
--    iterator is exhausted, we switch back to the 'InVolDB' state.
--
data IteratorState m blk
  = InImmDB
      (StreamFrom blk)
      (ImmDB.Iterator (HeaderHash blk) m blk)
      (InImmDBEnd blk)
    -- ^ Streaming from the ImmutableDB.
    --
    -- Invariant: an ImmutableDB iterator opened using the 'StreamFrom'
    -- parameter as lower bound will yield the same next block as the iterator
    -- stored as parameter. There is one difference, which is exactly the
    -- reason for keeping track of this 'StreamFrom': if the latter iterator
    -- (the parameter) is exhausted and blocks have been appended to the end
    -- of the ImmutableDB since it was originally opened, the new iterator can
    -- include them in its stream.
    --
    -- Invariant: the iterator is not exhausted.
  | InVolDB
      (StreamFrom blk)
      (NonEmpty (HeaderHash blk))
    -- ^ Streaming from the VolatileDB.
    --
    -- The (non-empty) list of hashes is the path to follow through the
    -- VolatileDB.
    --
    -- Invariant: if the blocks corresponding to the hashes have been moved to
    -- the ImmutableDB, it should be possible to stream these blocks from the
    -- ImmutableDB by starting an iterator using the 'StreamFrom' parameter.
    -- Note that the hashes of these blocks still have to be checked against
    -- the hashes in the path, because the blocks might not have been part of
    -- the current chain, in which case they will not be in the ImmutableDB.
  | InImmDBRetry
      (StreamFrom blk)
      (ImmDB.Iterator (HeaderHash blk) m blk)
      (NonEmpty (HeaderHash blk))
    -- ^ When streaming blocks (a list of hashes) from the VolatileDB, we
    -- noticed a block was missing from the VolatileDB. It may have moved to
    -- the ImmutableDB since we initialised the iterator (and built the path),
    -- so we'll try if we can stream it from the ImmutableDB.
    --
    -- Invariants: invariants of 'InImmDB' + invariant of 'InVolDB'.

  | Closed

-- | Determines if/when to stop streaming from the ImmutableDB and what to do
-- afterwards.
data InImmDBEnd blk
  = StreamAll
    -- ^ Don't stop streaming until the iterator is exhausted.
  | StreamTo          (StreamTo blk)
    -- ^ Stream to the upper bound.
  | SwitchToVolDBFrom (StreamTo blk)  (NonEmpty (HeaderHash blk))
    -- ^ Stream to the upper bound. Afterwards, start streaming the path (the
    -- second parameter) from the VolatileDB.

implIteratorNext :: forall m blk hdr.
                    ( MonadCatch m
                    , MonadSTM   m
                    , HasHeader blk
                    )
                 => TVar m (IteratorState m blk)
                 -> ImmDB m blk
                 -> VolDB m blk hdr
                 -> m (IteratorResult blk)
implIteratorNext varItState immDB volDB =
    atomically (readTVar varItState) >>= \case
      Closed ->
        return IteratorExhausted
      InImmDB continueAfter immIt immEnd ->
        nextInImmDB continueAfter immIt immEnd
      InImmDBRetry continueAfter immIt immHashes ->
        nextInImmDBRetry (Just continueAfter) immIt immHashes
      InVolDB continueAfter volHashes ->
        nextInVolDB continueAfter volHashes
  where
    -- | Read the next block while in the 'InVolDB' state.
    nextInVolDB :: StreamFrom blk
                   -- ^ In case the block corresponding to the first hash in
                   -- the path is missing from the VolatileDB, we can use this
                   -- lower bound to try to stream it from the ImmutableDB (if
                   -- the block indeed has been moved there).
                -> NonEmpty (HeaderHash blk)
                -> m (IteratorResult blk)
    nextInVolDB continueFrom (hash NE.:| hashes) =
      VolDB.getBlock volDB hash >>= \case
        -- Block is missing
        Nothing -> do
            -- Try if we can stream a block from the ImmutableDB that was
            -- previously in the VolatileDB. This will only work if the block
            -- was part of the current chain, otherwise it will not have been
            -- copied to the ImmutableDB.
            --
            -- This call cannot throw a 'ReadFutureSlotError' or a
            -- 'ReadFutureEBBError' because if the block is missing, it /must/
            -- have been garbage-collected, which means that its slot was
            -- older than the slot of the tip of the ImmutableDB.
            immIt <- ImmDB.streamBlocksFromUnchecked immDB continueFrom
            nextInImmDBRetry Nothing immIt (hash NE.:| hashes)

        -- Block is there
        Just blk | Just hashes' <- NE.nonEmpty hashes -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes')
          return $ IteratorResult blk
        -- No more hashes, so we can stop
        Just blk -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult blk

    -- | Read the next block while in the 'InImmDB' state.
    nextInImmDB :: StreamFrom blk
                -> ImmDB.Iterator (HeaderHash blk) m blk
                -> InImmDBEnd blk
                -> m (IteratorResult blk)
    nextInImmDB continueFrom immIt immEnd = do
      immRes <- selectResult immEnd <$> ImmDB.iteratorNext    immIt
                                    <*> ImmDB.iteratorHasNext immIt
      case immRes of
        NotDone blk -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InImmDB continueFrom' immIt immEnd)
          return $ IteratorResult blk
        -- True indicates that this is the last element in the stream
        DoneAfter blk | SwitchToVolDBFrom _ hashes <- immEnd -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes)
          return $ IteratorResult blk
        DoneAfter blk -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult blk
        Done | SwitchToVolDBFrom _ hashes <- immEnd ->
          nextInVolDB continueFrom hashes
        Done -> do
          -- No need to switch to the VolatileDB, so we can stop
          atomically $ writeTVar varItState Closed
          return IteratorExhausted

    -- | Read the next block while in the 'InImmDBRetry' state.
    --
    -- We try to stream blocks that we suspect are now in the ImmutableDB
    -- because they are no longer in the VolatileDB. We don't know this for
    -- sure, so we must check whether they match the expected hashes.
    nextInImmDBRetry :: Maybe (StreamFrom blk)
                        -- ^ 'Nothing' iff the iterator was just opened and
                        -- nothing has been streamed from it yet. This is used
                        -- to avoid switching right back to the VolatileDB if
                        -- we came from there.
                     -> ImmDB.Iterator (HeaderHash blk) m blk
                     -> NonEmpty (HeaderHash blk)
                     -> m (IteratorResult blk)
    nextInImmDBRetry mbContinueFrom immIt (hash NE.:| hashes) =
      selectResult StreamAll <$> ImmDB.iteratorNext    immIt
                             <*> ImmDB.iteratorHasNext immIt >>= \case
        NotDone blk | blockHash blk == hash -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState $ case NE.nonEmpty hashes of
            Nothing      -> Closed
            Just hashes' -> InImmDBRetry continueFrom' immIt hashes'
          return $ IteratorResult blk

        DoneAfter blk | blockHash blk == hash -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState $ case NE.nonEmpty hashes of
            Nothing      -> Closed
            Just hashes' -> InVolDB continueFrom' hashes'
          return $ IteratorResult blk

        -- Hash mismatch or 'Done'
        _ -> case mbContinueFrom of
          -- We just switched to this state and the iterator was just opened.
          -- The block must be GC'ed, since we opened the iterator because it
          -- was missing from the VolatileDB and now it is not in the
          -- ImmutableDB either.
          Nothing -> do
            atomically $ writeTVar varItState Closed
            return $ IteratorBlockGCed hash

          -- We have already streamed something from the iterator. We can try
          -- looking in the VolatileDB again. If we hadn't streamed something
          -- yet, switching to the VolatileDB would be pointless as we just
          -- came from there.
          Just continueFrom -> nextInVolDB continueFrom (hash NE.:| hashes)

    -- | Return a 'Done' based on the 'InImmDBEnd'. See the documentation of
    -- 'Done'. The 'Bool' argument should be the result of
    -- 'ImmDB.iteratorHasNext' and indicates whether the iterator is able to
    -- stream more blocks ('True') or whether it is exhausted ('False') after
    -- returning the last result.
    --
    -- We're doing this because we're streaming from the ImmutableDB with an
    -- open upper bound, because the ImmutableDB doesn't support streaming to
    -- an exclusive upper bound.
    selectResult :: InImmDBEnd blk
                 -> ImmDB.IteratorResult (HeaderHash blk) blk
                 -> Bool  -- ^ has the iterator a next element
                 -> Done blk
    selectResult immEnd itRes hasNext = case itRes of
        ImmDB.IteratorResult _ blk -> select blk
        ImmDB.IteratorEBB  _ _ blk -> select blk
        ImmDB.IteratorExhausted    -> Done
      where
        select blk = case immEnd of
          StreamAll
            | hasNext             -> NotDone   blk
            | otherwise           -> DoneAfter blk
          StreamTo          to'   -> checkUpperBound blk to'
          SwitchToVolDBFrom to' _ -> checkUpperBound blk to'
        checkUpperBound blk = \case
          StreamToExclusive pt
            | pt == blockPoint blk -> Done
            | hasNext              -> NotDone   blk
            | otherwise            -> DoneAfter blk
          StreamToInclusive pt
            | pt == blockPoint blk -> DoneAfter blk
            | hasNext              -> NotDone   blk
            | otherwise            -> DoneAfter blk

-- | Auxiliary data type used for 'selectResult' in 'implIteratorNext'.
data Done blk
  = Done
    -- ^ We're done with the iterator, either it is exhausted or we reached
    -- its upper bound.
  | DoneAfter blk
    -- ^ We're done with the iterator, but have to return this last block. We
    -- must have reached its upper /inclusive/ bound.
  | NotDone     blk
    -- ^ We're not done yet with the iterator and have to return this block.
    -- We know the iterator is not exhausted, but this does not mean that the
    -- next block returned by it will be included in the stream as it might
    -- correspond to the exclusive upper bound.

{-------------------------------------------------------------------------------
  Unifying interface over the immutable DB and volatile DB, but independent
  of the ledger DB. These functions therefore do not require the entire
  Chain DB to have been initialized.
-------------------------------------------------------------------------------}

-- | Wrapper around 'getAnyBlock' for blocks we know should exist
--
-- If the block does not exist, this indicates disk failure.
getAnyKnownBlock :: forall m blk hdr. (MonadCatch m, HasHeader blk)
                 => ImmDB m blk
                 -> VolDB m blk hdr
                 -> Point blk
                 -> m blk
getAnyKnownBlock immDB volDB p = do
    mBlock <- mustExist p <$> getAnyBlock immDB volDB p
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get a block from either the immutable DB or volatile DB
--
-- Returns 'Nothing' if the block is unknown.
-- Throws 'NoGenesisBlockException' if the 'Point' refers to the genesis block.
getAnyBlock :: forall m blk hdr. (MonadCatch m, HasHeader blk)
            => ImmDB m blk
            -> VolDB m blk hdr
            -> Point blk
            -> m (Maybe blk)
getAnyBlock immDB volDB p = case pointHash p of
    GenesisHash    -> throwM $ NoGenesisBlock @blk
    BlockHash hash -> do
      -- Note: to determine whether a block is in the ImmutableDB, we can look
      -- at the slot of its tip, which we'll call @immTipSlot@. If the slot of
      -- the requested point > @immTipSlot@, then the block will not be in the
      -- ImmutableDB but in the VolatileDB. However, there is a race condition
      -- here: if between the time we got @immTipSlot@ and the time we look up
      -- the block in the VolatileDB the block was moved from the VolatileDB
      -- to the ImmutableDB, and it was deleted from the VolatileDB, we won't
      -- find the block, even though it is in the ChainDB.
      --
      -- Therefore, we first query the VolatileDB and if the block is not in
      -- it, then we can get @immTipSlot@ and compare it to the slot of the
      -- requested point. If the slot <= @immTipSlot@ it /must/ be in the
      -- ImmutableDB (no race condition here).
      mbVolBlock <- VolDB.getBlock volDB hash
      case mbVolBlock of
        Just block -> return $ Just block
        Nothing    -> do
          -- ImmDB will throw an exception if we ask for a block past the tip
          immTipSlot <- ImmDB.getSlotNoAtTip immDB
          if pointSlot p > immTipSlot
            -- It's not supposed to be in the ImmutableDB and the VolatileDB
            -- didn't contain it, so return 'Nothing'.
            then return Nothing
            else ImmDB.getBlockWithPoint immDB p

mustExist :: Point blk
          -> Maybe blk
          -> Either (ChainDbFailure blk) blk
mustExist p Nothing  = Left  $ ChainDbMissingBlock p
mustExist _ (Just b) = Right $ b

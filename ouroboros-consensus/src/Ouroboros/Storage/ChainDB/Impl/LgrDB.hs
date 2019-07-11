{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Thin wrapper around the LedgerDB
module Ouroboros.Storage.ChainDB.Impl.LgrDB (
    LgrDB -- opaque
  , LedgerDB
    -- * Initialization
  , LgrDbArgs(..)
  , defaultArgs
  , openDB
  , reopen
    -- * Wrappers
  , getCurrent
  , setCurrent
  , getCurrentState
  , currentPoint
  , takeSnapshot
  , trimSnapshots
  , getDiskPolicy
    -- * Validation
  , validate
  , ValidateResult
    -- * Garbage collect points of previously applied blocks
  , garbageCollectPrevApplied
    -- * Re-exports
  , MemPolicy
  , DiskPolicy (..)
  , DiskSnapshot
  , LedgerDB.SwitchResult (..)
  , LedgerDB.PushManyResult (..)
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except (runExcept)
import           Data.Bifunctor (second)
import           Data.Foldable (foldl')
import           Data.Functor ((<&>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Control.Tracer

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, Point,
                     SlotNo, StandardHash, blockPoint, castPoint)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS (hasFsErr),
                     createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (FsError, MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.DiskPolicy (DiskPolicy (..))
import           Ouroboros.Storage.LedgerDB.InMemory (Apply (..), RefOrVal (..))
import qualified Ouroboros.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Storage.LedgerDB.MemPolicy (MemPolicy)
import           Ouroboros.Storage.LedgerDB.OnDisk (DiskSnapshot,
                     NextBlock (..), StreamAPI (..))
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      conf           :: Conf m blk
    , varDB          :: TVar m (LedgerDB blk)
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied :: TVar m (Set (Point blk))
      -- ^ INVARIANT: this set contains only points that are in the
      -- VolatileDB.
      --
      -- The VolatileDB might contain invalid blocks, these will not be in
      -- this set.
      --
      -- When a garbage-collection is performed on the VolatileDB, the points
      -- of the blocks eligible for garbage-collection should be removed from
      -- this set.
    , args           :: LgrDbArgs m blk
      -- ^ The arguments used to open the 'LgrDB'. Needed for 'reopen'ing the
      -- 'LgrDB'.
    }

-- | Shorter synonym for the instantiated 'LedgerDB.LedgerDB'.
type LedgerDB blk = LedgerDB.LedgerDB (ExtLedgerState blk) (Point blk)

-- | Shorter synonym for the instantiated 'LedgerDbConf'.
type Conf m blk =
  LedgerDbConf m (ExtLedgerState blk) (Point blk) blk (ExtValidationError blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs m blk = forall h. LgrDbArgs {
      lgrNodeConfig       :: NodeConfig (BlockProtocol blk)
    , lgrHasFS            :: HasFS m h
    , lgrDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , lgrDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))
    , lgrDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , lgrEncodeLedger     :: LedgerState blk                -> Encoding
    , lgrEncodeChainState :: ChainState (BlockProtocol blk) -> Encoding
    , lgrEncodeHash       :: HeaderHash blk                 -> Encoding
    , lgrMemPolicy        :: MemPolicy
    , lgrDiskPolicy       :: DiskPolicy m
    , lgrGenesis          :: m (ExtLedgerState blk)
    }

-- | Default arguments
--
-- The following arguments must still be defined:
--
-- * 'lgrNodeConfig'
-- * 'lgrDecodeLedger'
-- * 'lgrDecodeChainState'
-- * 'lgrDecodeHash'
-- * 'lgrEncodeLedger'
-- * 'lgrEncodeChainState'
-- * 'lgrEncodeHash'
-- * 'lgrMemPolicy'
-- * 'lgrGenesis'
defaultArgs :: FilePath -> LgrDbArgs IO blk
defaultArgs fp = LgrDbArgs {
      lgrHasFS            = ioHasFS $ MountPoint (fp </> "ledger")
      -- Fields without a default
    , lgrNodeConfig       = error "no default for lgrNodeConfig"
    , lgrDecodeLedger     = error "no default for lgrDecodeLedger"
    , lgrDecodeChainState = error "no default for lgrDecodeChainState"
    , lgrDecodeHash       = error "no default for lgrDecodeHash"
    , lgrEncodeLedger     = error "no default for lgrEncodeLedger"
    , lgrEncodeChainState = error "no default for lgrEncodeChainState"
    , lgrEncodeHash       = error "no default for lgrEncodeHash"
    , lgrMemPolicy        = error "no default for lgrMemPolicy"
    , lgrDiskPolicy       = error "no default for lgrDiskPolicy"
    , lgrGenesis          = error "no default for lgrGenesis"
    }

-- | Open the ledger DB
openDB :: forall m blk.
          ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , ProtocolLedgerView blk
          )
       => LgrDbArgs m blk
       -- ^ Stateless initializaton arguments
       -> ImmDB m blk
       -- ^ Reference to the immutable DB
       --
       -- After reading a snapshot from disk, the ledger DB will be brought
       -- up to date with tip of the immutable DB. The corresponding ledger
       -- state can then be used as the starting point for chain selection in
       -- the ChainDB driver.
       -> (Point blk -> m blk)
       -- ^ Read a block from disk
       --
       -- The block may be in the immutable DB or in the volatile DB; the ledger
       -- DB does not know where the boundary is at any given point.
       -> Tracer m (LedgerDB.InitLog (Point blk))
       -> m (LgrDB m blk)
openDB args@LgrDbArgs{..} immDB getBlock tracer = do
    createDirectoryIfMissing lgrHasFS True []
    db <- initFromDisk args lgrDbConf immDB tracer
    (varDB, varPrevApplied) <- atomically $
      (,) <$> newTVar db <*> newTVar Set.empty
    return LgrDB {
        conf           = lgrDbConf
      , varDB          = varDB
      , varPrevApplied = varPrevApplied
      , args           = args
      }
  where
    apply :: blk
          -> ExtLedgerState blk
          -> Either (ExtValidationError blk) (ExtLedgerState blk)
    apply = runExcept .: applyExtLedgerState lgrNodeConfig

    reapply :: blk
            -> ExtLedgerState blk
            -> ExtLedgerState blk
    reapply b l = case apply b l of  -- TODO skip some checks, see #440
      Left  e  -> error $ "reapply failed: " <> show e
      Right l' -> l'

    lgrDbConf = LedgerDbConf {
        ldbConfGenesis = lgrGenesis
      , ldbConfApply   = apply
      , ldbConfReapply = reapply
      , ldbConfResolve = getBlock
      }

reopen :: ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , ProtocolLedgerView blk
          )
       => LgrDB  m blk
       -> ImmDB  m blk
       -> Tracer m (LedgerDB.InitLog (Point blk))
       -> m ()
reopen LgrDB{..} immDB tracer = do
    db <- initFromDisk args conf immDB tracer
    atomically $ writeTVar varDB db

initFromDisk :: ( MonadST    m
                , MonadCatch m
                , HasHeader blk
                )
             => LgrDbArgs m blk
             -> Conf      m blk
             -> ImmDB     m blk
             -> Tracer    m (LedgerDB.InitLog (Point blk))
             -> m (LedgerDB blk)
initFromDisk args@LgrDbArgs{..} lgrDbConf immDB tracer = wrapFailure args $ do
    (initLog, db) <-
      LedgerDB.initLedgerDB
        lgrHasFS
        (decodeExtLedgerState lgrDecodeLedger lgrDecodeChainState)
        (Block.decodePoint lgrDecodeHash)
        lgrMemPolicy
        lgrDbConf
        (streamAPI immDB)
    traceWith tracer initLog
    return db

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getCurrent :: MonadSTM m
           => LgrDB m blk -> STM m (LedgerDB blk)
getCurrent LgrDB{..} = readTVar varDB

getCurrentState :: MonadSTM m
                => LgrDB m blk -> STM m (ExtLedgerState blk)
getCurrentState LgrDB{..} = LedgerDB.ledgerDbCurrent <$> readTVar varDB

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent :: MonadSTM m
           => LgrDB m blk -> LedgerDB blk -> STM m ()
setCurrent LgrDB{..} = writeTVar varDB

currentPoint :: UpdateLedger blk
             => LedgerDB blk -> Point blk
currentPoint = ledgerTipPoint
             . ledgerState
             . LedgerDB.ledgerDbCurrent

takeSnapshot :: (MonadSTM m, MonadThrow m, StandardHash blk, Typeable blk)
             => LgrDB m blk
             -> m (DiskSnapshot, Point blk)
takeSnapshot lgrDB@LgrDB{ args = args@LgrDbArgs{..} } = wrapFailure args $ do
    ledgerDB <- atomically $ getCurrent lgrDB
    second tipToPoint <$> LedgerDB.takeSnapshot
      lgrHasFS
      (encodeExtLedgerState lgrEncodeLedger lgrEncodeChainState)
      (Block.encodePoint lgrEncodeHash)
      ledgerDB

trimSnapshots :: (MonadThrow m, StandardHash blk, Typeable blk)
              => LgrDB m blk
              -> m [DiskSnapshot]
trimSnapshots LgrDB{ args = args@LgrDbArgs{..} } = wrapFailure args $
    LedgerDB.trimSnapshots lgrHasFS lgrDiskPolicy

getDiskPolicy :: LgrDB m blk -> DiskPolicy m
getDiskPolicy LgrDB{ args = LgrDbArgs{..} } = lgrDiskPolicy

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

type ValidateResult blk =
  LedgerDB.SwitchResult (ExtValidationError blk) (ExtLedgerState blk) (Point blk) 'False

validate :: forall m blk.
            ( MonadSTM m
            , ProtocolLedgerView blk
            , HasCallStack
            )
         => LgrDB m blk
         -> LedgerDB blk
            -- ^ This is used as the starting point for validation, not the one
            -- in the 'LgrDB'.
         -> Word64  -- ^ How many blocks to roll back
         -> [Header blk]
         -> m (ValidateResult blk)
validate LgrDB{..} ledgerDB numRollbacks = \hdrs -> do
    blocks <- toBlocks hdrs <$> atomically (readTVar varPrevApplied)
    res <- LedgerDB.ledgerDbSwitch conf numRollbacks blocks ledgerDB
    atomically $ modifyTVar' varPrevApplied $
      addPoints (validBlockPoints res (map headerPoint hdrs))
    return res
  where
    toBlocks :: [Header blk] -> Set (Point blk)
             -> [(Apply 'False, RefOrVal (Point blk) blk)]
    toBlocks hdrs prevApplied =
      [ ( if Set.member (headerPoint hdr) prevApplied
          then Reapply else Apply
        , toRefOrVal (Left hdr) )
      | hdr <- hdrs ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk
                     -> ([Point blk] -> [Point blk])
    validBlockPoints = \case
      LedgerDB.PushSuffix (LedgerDB.ValidBlocks _)              -> id
      LedgerDB.PushSuffix (LedgerDB.InvalidBlock _ lastValid _) ->
        takeWhile (/= lastValid)
      LedgerDB.InvalidBlockInPrefix _ lastValid                 ->
        takeWhile (/= lastValid)

    addPoints :: [Point blk] -> Set (Point blk)
              -> Set (Point blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Stream API to the immutable DB
-------------------------------------------------------------------------------}

streamAPI :: forall m blk. (MonadCatch m, HasHeader blk)
          => ImmDB m blk -> StreamAPI m (Point blk) blk
streamAPI immDB = StreamAPI streamAfter
  where
    streamAfter :: Tip (Point blk)
                -> (Maybe (m (NextBlock (Point blk) blk)) -> m a)
                -> m a
    streamAfter tip k = do
      slotNoAtTip <- ImmDB.getSlotNoAtTip immDB
      if Block.pointSlot (tipToPoint tip) > slotNoAtTip
        then k Nothing
        else bracket
          (ImmDB.streamBlocksAfter immDB (tipToPoint tip))
          ImmDB.iteratorClose
          (k . Just . getNext)

    getNext :: ImmDB.Iterator (HeaderHash blk) m blk
            -> m (NextBlock (Point blk) blk)
    getNext itr = ImmDB.iteratorNext itr <&> \case
      ImmDB.IteratorExhausted    -> NoMoreBlocks
      ImmDB.IteratorResult _ blk -> NextBlock (Block.blockPoint blk, blk)
      ImmDB.IteratorEBB  _ _ blk -> NextBlock (Block.blockPoint blk, blk)

{-------------------------------------------------------------------------------
  Garbage collect points of previously applied blocks
-------------------------------------------------------------------------------}

-- | Remove all points with a slot less than or equal to the given slot from
-- the set of previously applied points.
garbageCollectPrevApplied :: MonadSTM m
                          => LgrDB m blk
                          -> SlotNo
                          -> STM m ()
garbageCollectPrevApplied LgrDB{..} slotNo = modifyTVar' varPrevApplied $
    Set.filter ((<= slotNo) . Block.pointSlot)

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap exceptions that may indicate disk failure in a 'ChainDbFailure'
-- exception using the 'LgrDbFailure' constructor.
wrapFailure :: forall m blk x. (MonadThrow m, StandardHash blk, Typeable blk)
            => LgrDbArgs m blk -> m x -> m x
wrapFailure LgrDbArgs{ lgrHasFS = hasFS } k =
    EH.catchError (hasFsErr hasFS) k rethrow
  where
    rethrow :: FsError -> m x
    rethrow err = throwM $ LgrDbFailure @blk err

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

toRefOrVal :: (HasHeader blk, HasHeader (Header blk))
           => Either (Header blk) blk -> RefOrVal (Point blk) blk
toRefOrVal (Left  hdr) = Ref (castPoint (blockPoint hdr))
toRefOrVal (Right blk) = Val (blockPoint blk) blk

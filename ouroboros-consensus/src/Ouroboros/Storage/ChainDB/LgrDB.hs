{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Ouroboros.Storage.ChainDB.LgrDB (
    LgrDB -- opaque
    -- * Initialization
  , LgrDbArgs(..)
  , defaultArgs
  , openDB
    -- * Re-exports
  , MemPolicy
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Control.Monad (when)
import           Control.Monad.Except (runExcept)
import           Data.Maybe (isJust)
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, Point)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)

import           Ouroboros.Storage.LedgerDB.Conf
import qualified Ouroboros.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Storage.LedgerDB.MemPolicy (MemPolicy)
import           Ouroboros.Storage.LedgerDB.OnDisk (NextBlock (..),
                     StreamAPI (..))
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Storage.ChainDB.API (StreamFrom (..))
import           Ouroboros.Storage.ChainDB.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.ImmDB as ImmDB

-- | Thin wrapper around the ledger database
data LgrDB m blk = LedgerDB {
      nodeConfig :: NodeConfig (BlockProtocol blk)
    , varDB      :: TVar m (LedgerDB.LedgerDB (ExtLedgerState blk) (Point blk))
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs m blk = forall h. LgrDbArgs {
      lgrNodeConfig       :: NodeConfig (BlockProtocol blk)
    , lgrHasFS            :: HasFS m h
    , lgrDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , lgrDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))
    , lgrDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , lgrMemPolicy        :: MemPolicy
    , lgrGenesis          :: m (ExtLedgerState blk)
    }

-- | Default arguments
--
-- The following arguments must still be defined:
--
-- * lgrNodeConfig
-- * lgrDecodeLedger
-- * lgrDecodeChainState
-- * lgrDecodeHash
-- * lgrMemPolicy
-- * lgrGenesis
defaultArgs :: FilePath -> LgrDbArgs IO blk
defaultArgs fp = LgrDbArgs {
      lgrHasFS            = ioHasFS $ MountPoint (fp </> "ledger")
      -- Fields without a default
    , lgrNodeConfig       = error "no default for lgrNodeConfig"
    , lgrDecodeLedger     = error "no default for lgrDecodeLedger"
    , lgrDecodeChainState = error "no default for lgrDecodeChainState"
    , lgrDecodeHash       = error "no default for lgrDecodeHash"
    , lgrMemPolicy        = error "no default for lgrMemPolicy"
    , lgrGenesis          = error "no default for lgrGenesis"
    }

-- | Open the ledger DB
openDB :: forall m blk.
          ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , ProtocolLedgerView blk
          , LedgerConfigView   blk
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
       -> m (LgrDB m blk)
openDB LgrDbArgs{..} immDB getBlock = do
    (_initLog, db) <-
      LedgerDB.initLedgerDB
        lgrHasFS
        (decodeExtLedgerState lgrDecodeLedger lgrDecodeChainState)
        (Block.decodePoint (Block.decodeChainHash lgrDecodeHash))
        lgrMemPolicy
        lgrDbConf
        (streamAPI immDB)
    -- TODO: Do something with the init log (log warnings/errors)
    varDB <- atomically $ newTVar db
    return $ LedgerDB lgrNodeConfig varDB
  where
    apply :: blk
          -> ExtLedgerState blk
          -> Either (ExtValidationError blk) (ExtLedgerState blk)
    apply = runExcept .: applyExtLedgerState lgrNodeConfig

    reapply :: blk
            -> ExtLedgerState blk
            -> ExtLedgerState blk
    reapply b l = case apply b l of  -- TODO skip some checks
      Left  e  -> error $ "reapply failed: " <> show e
      Right l' -> l'

    lgrDbConf = LedgerDbConf {
        ldbConfGenesis = lgrGenesis
      , ldbConfApply   = apply
      , ldbConfReapply = reapply
      , ldbConfResolve = getBlock
      }

{-------------------------------------------------------------------------------
  Stream API to the immutable DB
-------------------------------------------------------------------------------}

streamAPI :: forall m blk. (MonadCatch m, MonadSTM m, HasHeader blk)
          => ImmDB m blk -> StreamAPI m (Point blk) blk
streamAPI immDB = StreamAPI streamAfter
  where
    streamAfter :: Tip (Point blk)
                -> (Maybe (m (NextBlock (Point blk) blk)) -> m a)
                -> m a
    streamAfter tip k = bracket
      (ImmDB.streamBlocks immDB (StreamFromExclusive (tipToPoint tip)))
      ImmDB.iteratorClose
      $ \itr -> do
        first <- ImmDB.iteratorNext itr
        case first of
          ImmDB.IteratorExhausted -> k Nothing
          _                       -> do
            varNext <- atomically $ newTVar (Just (toNextBlock first))
            k $ Just $ getNext itr varNext

    -- The TVar stores the first block we got from the iterator when we had to
    -- check whether it was empty or not. The TVar will only be filled once,
    -- all subsequent calls will see an empty TVar and get the next block from
    -- the iterator.
    getNext :: ImmDB.Iterator (HeaderHash blk) m blk
            -> TVar m (Maybe (NextBlock (Point blk) blk))
            -> m (NextBlock (Point blk) blk)
    getNext itr varNext = do
      mbNext <- atomically $ do
        mbNext <- readTVar varNext
        when (isJust mbNext) $ writeTVar varNext Nothing
        return mbNext
      case mbNext of
        Just next -> return next
        Nothing   -> toNextBlock <$> ImmDB.iteratorNext itr

    toNextBlock :: ImmDB.IteratorResult (HeaderHash blk) blk
                -> NextBlock (Point blk) blk
    toNextBlock res = case res of
      ImmDB.IteratorExhausted    -> NoMoreBlocks
      ImmDB.IteratorResult _ blk -> NextBlock (Block.blockPoint blk, blk)
      ImmDB.IteratorEBB  _ _ blk -> NextBlock (Block.blockPoint blk, blk)

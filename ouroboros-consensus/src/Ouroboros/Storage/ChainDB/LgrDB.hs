{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Ouroboros.Storage.ChainDB.LgrDB (
    LgrDB -- opaque
  , LedgerDB
    -- * Initialization
  , LgrDbArgs(..)
  , defaultArgs
  , openDB
    -- * Wrappers
  , getCurrent
  , setCurrent
  , getCurrentState
  , currentPoint
    -- * Validation
  , validate
  , revalidate
  , ValidateResult
  , RevalidateResult
    -- * Re-exports
  , MemPolicy
  , LedgerDB.SwitchResult (..)
  , LedgerDB.PushManyResult (..)
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Control.Monad.Except (runExcept)
import           Data.Functor ((<&>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, Point,
                     blockPoint, castPoint)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS, createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (MountPoint (..))
import           Ouroboros.Storage.FS.IO (ioHasFS)

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.InMemory (Apply (..), RefOrVal (..))
import qualified Ouroboros.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Storage.LedgerDB.MemPolicy (MemPolicy)
import           Ouroboros.Storage.LedgerDB.OnDisk (NextBlock (..),
                     StreamAPI (..))
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Storage.ChainDB.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.ImmDB as ImmDB

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      conf           :: Conf m blk
    , varDB          :: TVar m (LedgerDB blk)
    , varPrevApplied :: TVar m (Set (HeaderHash blk))
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
    createDirectoryIfMissing lgrHasFS True []
    (_initLog, db) <-
      LedgerDB.initLedgerDB
        lgrHasFS
        (decodeExtLedgerState lgrDecodeLedger lgrDecodeChainState)
        (Block.decodePoint (Block.decodeChainHash lgrDecodeHash))
        lgrMemPolicy
        lgrDbConf
        (streamAPI immDB)
    -- TODO: Do something with the init log (log warnings/errors)
    (varDB, varPrevApplied) <- atomically $
      (,) <$> newTVar db <*> newTVar Set.empty
    return LgrDB {
        conf           = lgrDbConf
      , varDB          = varDB
      , varPrevApplied = varPrevApplied
      }
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
  Wrappers
-------------------------------------------------------------------------------}

getCurrent :: MonadSTM m
           => LgrDB m blk -> STM m (LedgerDB blk)
getCurrent LgrDB{..} = readTVar varDB

getCurrentState :: MonadSTM m
                => LgrDB m blk -> STM m (ExtLedgerState blk)
getCurrentState LgrDB{..} = LedgerDB.ledgerDbCurrent <$> readTVar varDB

setCurrent :: MonadSTM m
           => LgrDB m blk -> LedgerDB blk -> STM m ()
setCurrent LgrDB{..} = writeTVar varDB

currentPoint :: UpdateLedger blk
             => LedgerDB blk -> Point blk
currentPoint = ledgerTipPoint
             . ledgerState
             . LedgerDB.ledgerDbCurrent

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

type ValidateResult blk =
  LedgerDB.SwitchResult (ExtValidationError blk) (ExtLedgerState blk) (Point blk) 'False

type RevalidateResult blk =
  LedgerDB.SwitchResult (ExtValidationError blk) (ExtLedgerState blk) (Point blk) 'True

validate :: ( Monad m
            , ProtocolLedgerView blk
            , HasHeader hdr
            , HeaderHash blk ~ HeaderHash hdr
            , HasCallStack
            )
         => LgrDB m blk
         -> LedgerDB blk
            -- ^ This is used as the starting point for validation, not the one
            -- in the 'LgrDB'.
         -> Word64  -- ^ How many blocks to roll back
         -> [Either hdr blk]
            -- ^ A @hdr@ is passed by-reference, a @blk@ by-value.
         -> m (ValidateResult blk)
validate LgrDB {..} ledgerDB numRollbacks hdrOrBlks =
    -- TODO look in the varPrevApplied and update after validation
    LedgerDB.ledgerDbSwitch conf numRollbacks bs ledgerDB
  where
    bs = [(Apply, toRefOrVal hdrOrBlk) | hdrOrBlk <- hdrOrBlks]

revalidate :: ( Monad m
              , ProtocolLedgerView blk
              , HasHeader hdr
              , HeaderHash blk ~ HeaderHash hdr
              , HasCallStack
              )
          => LgrDB m blk
          -> LedgerDB blk
             -- ^ This is used as the starting point for validation, not the one
             -- in the 'LgrDB'.
          -> Word64  -- ^ How many blocks to roll back
          -> [Either hdr blk]
             -- ^ A @hdr@ is passed by-reference, a @blk@ by-value.
          -> m (RevalidateResult blk)
revalidate LgrDB {..} ledgerDB numRollbacks hdrOrBlks =
    -- TODO check with varPrevApplied?
    LedgerDB.ledgerDbSwitch conf numRollbacks bs ledgerDB
  where
    bs = [(Reapply, toRefOrVal hdrOrBlk) | hdrOrBlk <- hdrOrBlks]

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
  Auxiliary
-------------------------------------------------------------------------------}

toRefOrVal :: ( HasHeader hdr
              , HasHeader blk
              , HeaderHash blk ~ HeaderHash hdr
              )
           => Either hdr blk -> RefOrVal (Point blk) blk
toRefOrVal (Left  hdr) = Ref (castPoint (blockPoint hdr))
toRefOrVal (Right blk) = Val (blockPoint blk) blk

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Mock (
    -- * Types
    Err (..)
  , ID (..)
  , Mock (..)
  , ValueHandle (..)
  , ValueHandleStatus (..)
  , emptyMock
    -- * Type classes
  , ApplyDiff (..)
  , DiffSize (..)
  , EmptyValues (..)
  , HasOps
  , KeysSize (..)
  , LookupKeys (..)
  , LookupKeysRange (..)
  , MakeDiff (..)
  , ValuesLength (..)
    -- * State monad to run the mock in
  , MockState (..)
  , runMockState
    -- * Mocked @'BackingStore'@ operations
  , mBSClose
  , mBSCopy
  , mBSInitFromCopy
  , mBSInitFromValues
  , mBSVHClose
  , mBSVHRangeRead
  , mBSVHRead
  , mBSValueHandle
  , mBSWrite
  , mGuardBSClosed
  , mGuardBSVHClosed
  ) where

import           Control.Monad
import           Control.Monad.Except (ExceptT (..), MonadError (throwError),
                     runExceptT)
import           Control.Monad.State (MonadState, State, StateT (StateT), gets,
                     modify, runState)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Block.Abstract (SlotNo, WithOrigin (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as BS

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Mock vs = Mock {
    backingValues :: vs
  , backingSeqNo  :: WithOrigin SlotNo
  , copies        :: Map BS.BackingStorePath (WithOrigin SlotNo, vs)
  , isClosed      :: Bool
    -- | Track whether value handles have been closed.
  , valueHandles  :: Map ID ValueHandleStatus
    -- | The next id to use if a new value handle is opened.
  , nextId        :: ID
  }
  deriving stock (Show, Eq)

data ValueHandleStatus = Open | ClosedByStore | ClosedByHandle
  deriving stock (Show, Eq)

data ValueHandle values = ValueHandle {
    getId  :: ID
  , values :: values
  , seqNo  :: WithOrigin SlotNo
  }
  deriving stock Show

instance Eq (ValueHandle vs) where
  x == y = getId x == getId y

instance Ord (ValueHandle vs) where
  x <= y = getId x < getId y

-- | An ID for a mocked value handle.
newtype ID = ID Word
  deriving stock (Show, Eq, Ord)
  deriving newtype Num

-- | An empty mock state.
emptyMock :: EmptyValues vs => Mock vs
emptyMock = Mock {
    backingValues = emptyValues
  , backingSeqNo  = Origin
  , copies        = Map.empty
  , isClosed      = False
  , valueHandles  = Map.empty
  , nextId        = 0
  }

data Err =
    ErrBackingStoreClosed
  | ErrBackingStoreValueHandleClosed
  | ErrCopyPathAlreadyExists
  | ErrCopyPathDoesNotExist
  | ErrNonMonotonicSeqNo (WithOrigin SlotNo) (WithOrigin SlotNo)
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Type classes
-------------------------------------------------------------------------------}

-- | Abstract over interactions between values, keys and diffs.
class ( EmptyValues vs, ApplyDiff vs d, LookupKeysRange ks vs
      , LookupKeys ks vs, ValuesLength vs, MakeDiff vs d
      , DiffSize d, KeysSize ks
      ) => HasOps ks vs d

class EmptyValues vs where
  emptyValues :: vs

class ApplyDiff vs d where
  applyDiff :: vs -> d -> vs

class LookupKeysRange ks vs where
  lookupKeysRange :: Maybe ks -> Int -> vs -> vs

class LookupKeys ks vs where
  lookupKeys :: ks -> vs -> vs

class ValuesLength vs where
  valuesLength :: vs -> Int

class MakeDiff vs d where
  diff :: vs -> vs -> d

class DiffSize d where
  diffSize :: d -> Int

class KeysSize ks where
  keysSize :: ks -> Int

{-------------------------------------------------------------------------------
  State monad to run the mock in
-------------------------------------------------------------------------------}

-- | State within which the mock runs.
newtype MockState ks vs d a =
    MockState (ExceptT Err (State (Mock vs)) a)
  deriving stock     Functor
  deriving newtype ( Applicative
                   , Monad
                   , MonadState (Mock vs)
                   , MonadError Err
                   )

runMockState ::
     MockState ks vs d a
  -> Mock vs
  -> (Either Err a, Mock vs)
runMockState (MockState t) = runState . runExceptT $ t

{------------------------------------------------------------------------------
  Mocked @'BackingStore'@ operations
------------------------------------------------------------------------------}

mBSInitFromValues ::
     forall vs m. (MonadState (Mock vs) m)
  => WithOrigin SlotNo
  -> vs
  -> m ()
mBSInitFromValues sl vs = modify (\m -> m {
    backingValues = vs
  , backingSeqNo  = sl
  , isClosed      = False
  })

mBSInitFromCopy ::
     forall vs m. (MonadState (Mock vs) m, MonadError Err m)
  => BS.BackingStorePath
  ->  m ()
mBSInitFromCopy bsp = do
  cps <- gets copies
  case Map.lookup bsp cps of
    Nothing       -> throwError ErrCopyPathDoesNotExist
    Just (sl, vs) -> modify (\m -> m {
        backingValues = vs
      , backingSeqNo  = sl
  , isClosed      = False
      })

-- | Throw an error if the backing store has been closed, which prevents any
-- other operations from succeeding.
mGuardBSClosed :: (MonadState (Mock vs) m, MonadError Err m) => m ()
mGuardBSClosed = do
  closed <- gets isClosed
  when closed $
    throwError ErrBackingStoreClosed

-- | Close the backing store.
mBSClose :: (MonadState (Mock vs) m, MonadError Err m) => m ()
mBSClose = do
  mGuardBSClosed
  modify (\m -> m {
      isClosed = True
    , valueHandles = fmap (const ClosedByStore) (valueHandles m)
    })

-- | Copy the contents of the backing store to the given path.
mBSCopy :: (MonadState (Mock vs) m, MonadError Err m) => BS.BackingStorePath ->  m ()
mBSCopy bsp = do
  mGuardBSClosed
  cps <- gets copies
  when (bsp `Map.member` cps) $
    throwError ErrCopyPathAlreadyExists
  modify (\m -> m {
      copies = Map.insert bsp (backingSeqNo m, backingValues m) (copies m)
    })

-- | Open a new value handle, which captures the state of the backing store
-- at the time of opening the handle.
mBSValueHandle ::
     (MonadState (Mock vs) m, MonadError Err m)
  => m (WithOrigin SlotNo, ValueHandle vs)
mBSValueHandle = do
  mGuardBSClosed
  vs <- gets backingValues
  seqNo <- gets backingSeqNo
  nxt <- gets nextId
  let
    vh = ValueHandle nxt vs seqNo
  modify (\m -> m {
      valueHandles = Map.insert nxt Open (valueHandles m)
    , nextId = nxt + 1
    })

  pure (seqNo, vh)

-- | Write a diff to the backing store.
mBSWrite ::
     (MonadState (Mock vs) m, MonadError Err m, ApplyDiff vs d)
  => SlotNo
  -> d
  -> m ()
mBSWrite sl d = do
  mGuardBSClosed
  vs <- gets backingValues
  seqNo <- gets backingSeqNo
  when (seqNo > NotOrigin sl) $
    throwError $ ErrNonMonotonicSeqNo (NotOrigin sl) seqNo
  modify (\m -> m {
      backingValues = applyDiff vs d
    , backingSeqNo = NotOrigin sl
    })

-- | Throw an error if the required backing store value handle has been closed.
mGuardBSVHClosed ::
     (MonadState (Mock vs) m, MonadError Err m)
  => ValueHandle vs
  -> m ()
mGuardBSVHClosed vh = do
  vhs <- gets valueHandles
  case Map.lookup (getId vh) vhs of
    Nothing -> error "Value handle not found"
    Just status ->
      case status of
        ClosedByStore  -> throwError ErrBackingStoreClosed
        ClosedByHandle -> throwError ErrBackingStoreValueHandleClosed
        _              -> pure ()

-- | Close a backing store value handle.
mBSVHClose ::
     (MonadState (Mock vs) m, MonadError Err m)
  => ValueHandle vs
  -> m ()
mBSVHClose vh = do
  mGuardBSClosed
  mGuardBSVHClosed vh
  vhs <- gets valueHandles
  modify (\m -> m {
    valueHandles = Map.adjust (const ClosedByHandle) (getId vh) vhs
  })

-- | Perform a range read on a backing store value handle.
mBSVHRangeRead ::
     (MonadState (Mock vs) m, MonadError Err m, LookupKeysRange ks vs)
  => ValueHandle vs
  -> BS.RangeQuery ks
  -> m vs
mBSVHRangeRead vh BS.RangeQuery{BS.rqPrev, BS.rqCount} = do
  mGuardBSClosed
  mGuardBSVHClosed vh
  let
    vs = values vh
  pure $ lookupKeysRange rqPrev rqCount vs

-- | Perform a regular read on a backing store value handle
mBSVHRead ::
     (MonadState (Mock vs) m, MonadError Err m, LookupKeys ks vs)
  => ValueHandle vs
  -> ks
  -> m vs
mBSVHRead vh ks = do
  mGuardBSClosed
  mGuardBSVHClosed vh
  let vs = values vh
  pure $ lookupKeys ks vs

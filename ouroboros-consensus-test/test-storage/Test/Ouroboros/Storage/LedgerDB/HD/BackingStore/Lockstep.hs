{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Lockstep (
    -- * Facilitate running the tests in @'IO'@ or @'IOSim'@.
    IOLikeMonad (..)
  , IOLikeMonadC (..)
  , RealMonad
  , unIOLikeMonad
    -- * Model state
  , BackingStoreState (..)
  , maxOpenValueHandles
  ) where

import           Control.Monad
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Constraint
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable

import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Gen)
import           Test.QuickCheck.StateModel
import           Test.QuickCheck.StateModel.Lockstep as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Defaults as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Op as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Op.SumProd as Lockstep

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Storage.FS.API hiding (Handle)
import           Ouroboros.Consensus.Storage.FS.API.Types hiding (Handle)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as BS
import           Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB as LMDB
                     (DbErr (..))
import           Ouroboros.Consensus.Util.IOLike hiding (MonadMask (..), handle)

import           Test.Util.Orphans.Slotting.Arbitrary ()
import           Test.Util.Orphans.ToExpr ()

import qualified Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Mock as Mock
import           Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Mock (Err (..),
                     Mock (..), MockValueHandle (..), runMockState)
import           Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Registry

{-------------------------------------------------------------------------------
  Facilitate running the tests in @'IO'@ or @'IOSim'@.

  TODO: put this in a separate module?
-------------------------------------------------------------------------------}

-- This wrapper allows us to run the tests both in @'IO'@ and @'IOSim'@, without
-- having to duplicate code for both @'IO'@ and @'IOSim'@.
data IOLikeMonad m a where
  RealIO :: IO a -> IOLikeMonad IO a
  SimIO  :: IOSim s a -> IOLikeMonad (IOSim s) a

-- | Retrieve the wrapped @'IOLike'@ monad.
unIOLikeMonad :: IOLikeMonad m a -> m a
unIOLikeMonad (RealIO x) = x
unIOLikeMonad (SimIO x)  = x

-- | Create a wrapper @'IOLike'@ monad.
class IOLikeMonadC m where
  ioLikeMonad :: m a -> IOLikeMonad m a

instance IOLikeMonadC IO where
  ioLikeMonad x = RealIO x

instance IOLikeMonadC (IOSim s) where
  ioLikeMonad x = SimIO x

instance (Functor m, IOLikeMonadC m) => Functor (IOLikeMonad m) where
  fmap f x = ioLikeMonad $ fmap f (unIOLikeMonad x)

instance (Applicative m, IOLikeMonadC m) =>Applicative (IOLikeMonad m) where
  x <*> y = ioLikeMonad $ unIOLikeMonad x <*> unIOLikeMonad y
  pure = ioLikeMonad . pure

instance (Monad m, IOLikeMonadC m) => Monad (IOLikeMonad m) where
  m >>= fm = ioLikeMonad $ unIOLikeMonad m >>= unIOLikeMonad . fm

-- | Since the tests do not return any types specific to the underlying
-- @'IOLike'@ monad, @'Realized' ('IOLikeMonad' m)@ behaves just like
-- @'Realized' 'IO'@.
type instance Realized (IOLikeMonad m) a = a

{-------------------------------------------------------------------------------
  @'Values'@ wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for preventing nonsenical pattern matches.
--
-- A logical step is to have the @'BSVHRangeRead'@ and @'BSVHRead'@ actions
-- declare that the result of the action should be something of type @'vs'@.
-- However, this means that in theory @'vs'@ could be instantiated to any type
-- (like @'Handle'@). Consequentially, if we match on a value that is returned
-- by running an action, we would always have to match on the case where it is a
-- result of running @'BSVHRangeRead'@ and @'BSVHRead'@ as well, even if the
-- return type is @'Handle'@, which we don't expect to use as our @vs@ type. As
-- such, we define this wrapper to prevent having to match on this nonsensical
-- case.
newtype Values vs = Values {unValues :: vs}
  deriving stock (Show, Eq, Ord, Typeable)
  deriving newtype QC.Arbitrary

{-------------------------------------------------------------------------------
  Model state
-------------------------------------------------------------------------------}

data BackingStoreState ks vs d = BackingStoreState {
    bssMock  :: Mock vs
  , bssStats :: Stats ks vs d
  }
  deriving (Show, Eq)

initState :: Mock.EmptyValues vs  => BackingStoreState ks vs d
initState = BackingStoreState {
    bssMock  = Mock.emptyMock
  , bssStats = initStats
  }

-- | Maximum number of LMDB readers that can be active at a time.
--
-- 32 is an arbitrary number of readers. We can increase or decrease this at
-- will.
--
-- TODO: should the LMDB backing store keep track of the number of open value
-- handles itself, and throw a custom error if the maximum is exceeded?
maxOpenValueHandles :: Int
maxOpenValueHandles = 32

{-------------------------------------------------------------------------------
  @'StateModel'@ and @'RunModel'@ instances
-------------------------------------------------------------------------------}

type RealEnv m ks vs d = (
    SomeHasFS m
  , BS.BackingStore m ks vs d
  , HandleRegistry m (BS.BackingStoreValueHandle m ks vs)
  )

type RealMonad m ks vs d = ReaderT (RealEnv m ks vs d) (IOLikeMonad m)

type BSAct ks vs d a =
  Action
    (Lockstep (BackingStoreState ks vs d))
    (Either Err a)
type BSVar ks vs d a =
  ModelVar (BackingStoreState ks vs d) a

instance ( Show ks, Show vs, Show d
         , Eq ks, Eq vs, Eq d
         , Typeable ks, Typeable vs, Typeable d
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , Mock.HasOps ks vs d
         ) => StateModel (Lockstep (BackingStoreState ks vs d)) where
  data Action (Lockstep (BackingStoreState ks vs d)) a where
    BSClose       :: BSAct ks vs d ()
    BSCopy        :: BS.BackingStorePath
                  -> BSAct ks vs d ()
    BSValueHandle :: BSAct ks vs d (WithOrigin SlotNo, Handle)
    BSWrite       :: SlotNo
                  -> d
                  -> BSAct ks vs d ()
    BSVHClose     :: BSVar ks vs d Handle
                  -> BSAct ks vs d ()
    BSVHRangeRead :: BSVar ks vs d Handle
                  -> BS.RangeQuery ks
                  -> BSAct ks vs d (Values vs)
    BSVHRead      :: BSVar ks vs d Handle
                  -> ks
                  -> BSAct ks vs d (Values vs)

  initialState        = Lockstep.initialState initState
  nextState           = Lockstep.nextState
  precondition st act = Lockstep.precondition st act
                        && modelPrecondition (getModel st) act
  arbitraryAction     = Lockstep.arbitraryAction
  shrinkAction        = Lockstep.shrinkAction

deriving stock instance (Show ks, Show vs, Show d)
                     => Show (LockstepAction (BackingStoreState ks vs d) a)
deriving stock instance (Eq ks, Eq vs, Eq d)
                     => Eq (LockstepAction (BackingStoreState ks vs d) a)

instance ( Show ks, Show vs, Show d
         , Eq ks, Eq vs, Eq d
         , Typeable ks, Typeable vs, Typeable d
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , IOLike m
         , Mock.HasOps ks vs d
         , IOLikeMonadC m
         ) => RunModel
                (Lockstep (BackingStoreState ks vs d))
                (RealMonad m ks vs d) where
  perform       = \_st -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @(RealMonad m ks vs d))

-- | Custom precondition that prevents errors in the @'LMDB'@ backing store due
-- to exceeding the maximum number of LMDB readers.
--
-- See @'maxOpenValueHandles'@.
modelPrecondition ::
       BackingStoreState ks vs d
    -> LockstepAction (BackingStoreState ks vs d) a
    -> Bool
modelPrecondition (BackingStoreState mock _stats) action = case action of
    BSCopy _      -> canOpenReader
    BSValueHandle -> canOpenReader
    _             -> True
  where
    canOpenReader         = Map.size openValueHandles < maxOpenValueHandles
    openValueHandles      = Map.filter not (valueHandles mock)

{-------------------------------------------------------------------------------
  @'InLockstep'@ instance
-------------------------------------------------------------------------------}

type BSVal ks vs d a = ModelValue (BackingStoreState ks vs d) a
type BSObs ks vs d a = Observable (BackingStoreState ks vs d) a

instance ( Show ks, Show vs, Show d
         , Eq ks, Eq vs, Eq d
         , Typeable ks, Typeable vs, Typeable d
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , Mock.HasOps ks vs d
         ) => InLockstep (BackingStoreState ks vs d) where

  data instance ModelValue (BackingStoreState ks vs d) a where
    MValueHandle :: MockValueHandle vs -> BSVal ks vs d Handle

    MErr    :: Err
            -> BSVal ks vs d Err
    MSlotNo :: WithOrigin SlotNo
            -> BSVal ks vs d (WithOrigin SlotNo)
    MValues :: vs
            -> BSVal ks vs d (Values vs)
    MUnit   :: ()
            -> BSVal ks vs d ()

    MEither :: Either (BSVal ks vs d a) (BSVal ks vs d b)
            -> BSVal ks vs d (Either a b)
    MPair   :: (BSVal ks vs d a, BSVal ks vs d b)
            -> BSVal ks vs d (a, b)

  data instance Observable (BackingStoreState ks vs d) a where
    OValueHandle :: BSObs ks vs d Handle
    OValues :: (Show a, Eq a, Typeable a) => a -> BSObs ks vs d (Values a)
    OId     :: (Show a, Eq a, Typeable a) => a -> BSObs ks vs d a
    OEither :: Either (BSObs ks vs d a) (BSObs ks vs d b)
            -> BSObs ks vs d (Either a b)
    OPair   :: (BSObs ks vs d a, BSObs ks vs d b) -> BSObs ks vs d (a, b)

  observeModel :: BSVal ks vs d a -> BSObs ks vs d a
  observeModel = \case
    MValueHandle _ -> OValueHandle
    MErr x         -> OId x
    MSlotNo x      -> OId x
    MValues x      -> OValues x
    MUnit x        -> OId x
    MEither x      -> OEither $ bimap observeModel observeModel x
    MPair x        -> OPair   $ bimap observeModel observeModel x

  modelNextState :: forall a.
       LockstepAction (BackingStoreState ks vs d) a
    -> ModelLookUp (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d -> (BSVal ks vs d a, BackingStoreState ks vs d)
  modelNextState action lookUp (BackingStoreState mock stats) =
      auxStats $ runMock lookUp action mock
    where
      auxStats ::
           (BSVal ks vs d a, Mock vs)
        -> (BSVal ks vs d a, BackingStoreState ks vs d)
      auxStats (result, state') =
          ( result
          , BackingStoreState state' $ updateStats action lookUp result stats
          )

  type ModelOp (BackingStoreState ks vs d) = Op

  usedVars ::
       LockstepAction (BackingStoreState ks vs d) a
    -> [AnyGVar (ModelOp (BackingStoreState ks vs d))]
  usedVars = \case
    BSClose           -> []
    BSCopy _          -> []
    BSValueHandle     -> []
    BSWrite _ _       -> []
    BSVHClose h       -> [SomeGVar h]
    BSVHRangeRead h _ -> [SomeGVar h]
    BSVHRead h _      -> [SomeGVar h]

  arbitraryWithVars ::
       ModelFindVariables (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d
    -> Gen (Any (LockstepAction (BackingStoreState ks vs d)))
  arbitraryWithVars = arbitraryBackingStoreAction

  shrinkWithVars ::
       ModelFindVariables (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d
    -> LockstepAction (BackingStoreState ks vs d) a
    -> [Any (LockstepAction (BackingStoreState ks vs d))]
  shrinkWithVars = shrinkBackingStoreAction

  tagStep ::
       (BackingStoreState ks vs d, BackingStoreState ks vs d)
    -> LockstepAction (BackingStoreState ks vs d) a
    -> BSVal ks vs d a
    -> [String]
  tagStep (_before, BackingStoreState _ after) action val =
    map show $ tagBSAction after action val

deriving stock instance (Show ks, Show vs, Show d) => Show (BSVal ks vs d a)

deriving stock instance (Show ks, Show vs, Show d) => Show (BSObs ks vs d a)
deriving stock instance (Eq ks, Eq vs, Eq d) => Eq (BSObs ks vs d a)

{-------------------------------------------------------------------------------
  @'RunLockstep'@ instance
-------------------------------------------------------------------------------}

instance ( Show ks, Show vs, Show d
         , Eq ks, Eq vs, Eq d
         , Typeable ks, Typeable vs, Typeable d
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , IOLike m
         , Mock.HasOps ks vs d
         , IOLikeMonadC m
         ) => RunLockstep (BackingStoreState ks vs d) (RealMonad m ks vs d) where
  observeReal ::
       Proxy (RealMonad m ks vs d)
    -> LockstepAction (BackingStoreState ks vs d) a
    -> Realized (RealMonad m ks vs d) a
    -> BSObs ks vs d a
  observeReal _proxy = \case
    BSClose           -> OEither . bimap OId OId
    BSCopy _          -> OEither . bimap OId OId
    BSValueHandle     -> OEither . bimap OId (OPair . bimap OId (const OValueHandle))
    BSWrite _ _       -> OEither . bimap OId OId
    BSVHClose _       -> OEither . bimap OId OId
    BSVHRangeRead _ _ -> OEither . bimap OId (OValues . unValues)
    BSVHRead _ _      -> OEither . bimap OId (OValues . unValues)

  showRealResponse ::
       Proxy (RealMonad m ks vs d)
    -> LockstepAction (BackingStoreState ks vs d) a
    -> Maybe (Dict (Show (Realized (RealMonad m ks vs d) a)))
  showRealResponse _proxy = \case
    BSClose           -> Just Dict
    BSCopy _          -> Just Dict
    BSValueHandle     -> Nothing
    BSWrite _ _       -> Just Dict
    BSVHClose _       -> Just Dict
    BSVHRangeRead _ _ -> Just Dict
    BSVHRead _ _      -> Just Dict

{-------------------------------------------------------------------------------
  Interpreter against the model
-------------------------------------------------------------------------------}

runMock ::
     Mock.HasOps ks vs d
  => ModelLookUp (BackingStoreState ks vs d)
  -> Action (Lockstep (BackingStoreState ks vs d)) a
  -> Mock vs
  -> ( BSVal ks vs d a
     , Mock vs
     )
runMock lookUp = \case
    BSClose            ->
      wrap MUnit . runMockState Mock.mBSClose
    BSCopy bsp         ->
      wrap MUnit . runMockState (Mock.mBSCopy bsp)
    BSValueHandle      ->
      wrap mBSValueHandle . runMockState Mock.mBSValueHandle
    BSWrite sl d       ->
      wrap MUnit . runMockState (Mock.mBSWrite sl d)
    BSVHClose h        ->
      wrap MUnit . runMockState (Mock.mBSVHClose (getHandle $ lookUp h))
    BSVHRangeRead h rq ->
      wrap MValues . runMockState (Mock.mBSVHRangeRead (getHandle $ lookUp h) rq)
    BSVHRead h ks      ->
      wrap MValues . runMockState (Mock.mBSVHRead (getHandle $ lookUp h) ks)
  where
    wrap ::
         (a -> BSVal ks vs d b)
      -> (Either Err a, Mock vs)
      -> (BSVal ks vs d (Either Err b), Mock vs)
    wrap f = first (MEither . bimap MErr f)

    mBSValueHandle ::
         (WithOrigin SlotNo, MockValueHandle vs)
      -> BSVal ks vs d (WithOrigin SlotNo, Handle)
    mBSValueHandle (sl, h) = MPair (MSlotNo sl, MValueHandle h)

    getHandle :: BSVal ks vs d Handle -> MockValueHandle vs
    getHandle (MValueHandle h) = h

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

arbitraryBackingStoreAction ::
     forall ks vs d.
     ( Eq ks, Eq vs, Eq d, Typeable vs
     , QC.Arbitrary ks, QC.Arbitrary vs
     , QC.Arbitrary (BS.RangeQuery ks)
     , Mock.MakeDiff vs d
     )
  => ModelFindVariables (BackingStoreState ks vs d)
  -> BackingStoreState ks vs d
  -> Gen (Any (LockstepAction (BackingStoreState ks vs d)))
arbitraryBackingStoreAction findVars (BackingStoreState mock _stats) =
    QC.frequency $
         withoutVars
      ++ case findVars (Proxy @(Either Err (WithOrigin SlotNo, Handle))) of
          []   -> []
          vars -> withVars (QC.elements vars)
  where
    withoutVars :: [(Int, Gen (Any (LockstepAction (BackingStoreState ks vs d))))]
    withoutVars = [
        (1, pure $ Some BSClose)
      , (5, fmap Some $ BSCopy <$> genBackingStorePath)
      , (5, pure $ Some BSValueHandle)
      , (5, fmap Some $ BSWrite <$> genSlotNo <*> genDiff)
      ]

    withVars ::
         Gen (BSVar ks vs d (Either Err (WithOrigin SlotNo, Handle)))
      -> [(Int, Gen (Any (LockstepAction (BackingStoreState ks vs d))))]
    withVars genVar = [
          (2, fmap Some $ BSVHClose <$> (fhandle <$> genVar))
        , (10, fmap Some $ BSVHRangeRead <$> (fhandle <$> genVar) <*> QC.arbitrary)
        , (10, fmap Some $ BSVHRead <$> (fhandle <$> genVar) <*> QC.arbitrary)
        ]
      where
        fhandle ::
             GVar Op (Either Err (WithOrigin SlotNo, Handle))
          -> GVar Op Handle
        fhandle = mapGVar (\op -> OpSnd `OpComp` OpRight `OpComp` op)

    genBackingStorePath :: Gen BS.BackingStorePath
    genBackingStorePath = do
      file <- genBSPFile
      pure . BS.BackingStorePath . mkFsPath $ ["copies", file]

    -- Generate a file name for a copy of the backing store contents. We keep
    -- the set of possible file names small, such that errors (i.e., file alread
    -- exists) occur most of the time.
    genBSPFile :: Gen String
    genBSPFile = QC.elements ["a", "b", "c", "d"]

    -- Generate a slot number that is close before, at, or after the backing
    -- store's current slot number. A
    genSlotNo :: Gen SlotNo
    genSlotNo = do
        n :: Int <- QC.choose (-5, 5)
        pure $ maybe 0 (+ fromIntegral n) (withOriginToMaybe seqNo)
      where
        seqNo = backingSeqNo mock

    -- Generate valid diffs most of the time, and generate fully arbitrary
    -- (probably invalid) diffs some of the time.
    genDiff :: Gen d
    genDiff = QC.frequency [
        (9, Mock.diff (backingValues mock) <$> QC.arbitrary)
      --TODO: enable @, (1, QC.arbitrary)@
      ]

{-------------------------------------------------------------------------------
  Shrinker
-------------------------------------------------------------------------------}

shrinkBackingStoreAction ::
       forall ks vs d a.
       ( Typeable vs, Eq ks, Eq vs, Eq d
       , QC.Arbitrary d, QC.Arbitrary (BS.RangeQuery ks), QC.Arbitrary ks
       )
    => ModelFindVariables (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d
    -> LockstepAction (BackingStoreState ks vs d) a
    -> [Any (LockstepAction (BackingStoreState ks vs d))]
shrinkBackingStoreAction _findVars (BackingStoreState _mock _) = \case
  BSWrite sl d ->
       [Some $ BSWrite sl d'  | d' <- QC.shrink d]
    ++ [Some $ BSWrite sl' d  | sl' <- QC.shrink sl]
  BSVHRangeRead h rq ->
    [Some $ BSVHRangeRead h rq' | rq' <- QC.shrink rq]
  BSVHRead h ks ->
    [Some $ BSVHRead h ks' | ks' <- QC.shrink ks]
  _ -> []

{-------------------------------------------------------------------------------
  Interpret @'Op'@ against @'ModelValue'@
-------------------------------------------------------------------------------}

instance InterpretOp Op (ModelValue (BackingStoreState ks vs d)) where
  intOp OpId         = Just
  intOp OpFst        = \case MPair   x -> Just (fst x)
  intOp OpSnd        = \case MPair   x -> Just (snd x)
  intOp OpLeft       = \case MEither x -> either Just (const Nothing) x
  intOp OpRight      = \case MEither x -> either (const Nothing) Just x
  intOp (OpComp g f) = intOp g <=< intOp f

{-------------------------------------------------------------------------------
  Interpreter for implementation (@'RealMonad'@)
-------------------------------------------------------------------------------}

runIO ::
     forall m ks vs d a. (IOLike m, IOLikeMonadC m) =>
     LockstepAction (BackingStoreState ks vs d) a
  -> LookUp (RealMonad m ks vs d)
  -> RealMonad m ks vs d (Realized (RealMonad m ks vs d) a)
runIO action lookUp = ReaderT $ \(sfhs, bs, rr) ->
    ioLikeMonad $ aux sfhs bs rr action
  where
    aux ::
         SomeHasFS m
      -> BS.BackingStore m ks vs d
      -> HandleRegistry m (BS.BackingStoreValueHandle m ks vs)
      -> LockstepAction (BackingStoreState ks vs d) a
      -> m a
    aux sfhs bs rr = \case
        BSClose            -> catchErr $
          BS.bsClose bs
        BSCopy bsp         -> catchErr $
          BS.bsCopy bs sfhs bsp
        BSValueHandle      -> catchErr $
          BS.bsValueHandle bs >>= mapM (registerHandle rr)
        BSWrite sl d       -> catchErr $
          BS.bsWrite bs sl d
        BSVHClose h        -> catchErr $
          readHandle rr (lookUp' h) >>= \vh -> BS.bsvhClose vh
        BSVHRangeRead h rq -> catchErr $ Values <$>
          (readHandle rr (lookUp' h) >>= \vh -> BS.bsvhRangeRead vh rq)
        BSVHRead h ks      -> catchErr $ Values <$>
          (readHandle rr (lookUp' h) >>= \vh -> BS.bsvhRead vh ks)
      where
        lookUp' :: BSVar ks vs d x -> Realized (RealMonad m ks vs d) x
        lookUp' = lookUpGVar (Proxy @(RealMonad m ks vs d)) lookUp

instance InterpretOp Op (WrapRealized (IOLikeMonad m)) where
  intOp = intOpRealizedId intOpId

catchErr :: forall m a. IOLike m => m a -> m (Either Err a)
catchErr act = catches (Right <$> act)
    [mkHandler fromTVarExn, mkHandler fromDbErr]

{-------------------------------------------------------------------------------
  Statistics and tagging
-------------------------------------------------------------------------------}

data Stats ks vs d = Stats {
    -- | Slots that value handles were created in
    handleSlots         :: Map (MockValueHandle vs) (WithOrigin SlotNo)
    -- | Slots in which writes were performed
  , writeSlots          :: Map SlotNo Int
    -- | A value handle was created before a write, and read after the write
  , readAfterWrite      :: Bool
    -- | A value handle was created before a write, and range read after the
    -- write
  , rangeReadAfterWrite :: Bool
    -- | Actions that caused a @'ErrBackingStoreClosed'@ error to be thrown
  , bsClosedThrown      :: Set TagAction
  }
  deriving stock (Show, Eq)


initStats :: Stats ks vs d
initStats = Stats {
    handleSlots         = Map.empty
  , writeSlots          = Map.empty
  , readAfterWrite      = False
  , rangeReadAfterWrite = False
  , bsClosedThrown      = Set.empty
  }

updateStats ::
     forall ks vs d a. Mock.HasOps ks vs d
  => LockstepAction (BackingStoreState ks vs d) a
  -> ModelLookUp (BackingStoreState ks vs d)
  -> BSVal ks vs d a
  -> Stats ks vs d
  -> Stats ks vs d
updateStats action lookUp result stats@Stats{handleSlots, writeSlots, bsClosedThrown} =
      updateHandleSlots
    . updateWriteSlots
    . updateReadAfterWrite
    . updateRangeReadAfterWrite
    . updateBSClosedThrown
    $ stats
  where
    getHandle :: BSVal ks vs d Handle -> MockValueHandle vs
    getHandle (MValueHandle h) = h

    updateHandleSlots :: Stats ks vs d -> Stats ks vs d
    updateHandleSlots s = case (action, result) of
      (BSValueHandle, MEither (Right (MPair (MSlotNo sl, MValueHandle h))))
        -> s {handleSlots = Map.insert h sl handleSlots}
      _ -> s

    updateWriteSlots :: Stats ks vs d -> Stats ks vs d
    updateWriteSlots s = case (action, result) of
      (BSWrite sl d, MEither (Right (MUnit ())))
        | 1 <= Mock.diffSize d
        -> s {writeSlots = Map.insert sl (Mock.diffSize d) writeSlots}
      _ -> s

    updateReadAfterWrite :: Stats ks vs d -> Stats ks vs d
    updateReadAfterWrite s = case (action, result) of
      (BSVHRead h _, MEither (Right (MValues vs)))
        | h' <- getHandle $ lookUp h
        , Just wosl <- Map.lookup h' handleSlots
        , Just (sl, _) <- Map.lookupMax writeSlots
        , wosl < at sl
        , 1 <= Mock.valuesLength vs
        -> s {readAfterWrite = True}
      _ -> s

    updateRangeReadAfterWrite :: Stats ks vs d -> Stats ks vs d
    updateRangeReadAfterWrite s = case (action, result) of
      (BSVHRangeRead h _, MEither (Right (MValues vs)))
        | h' <- getHandle $ lookUp h
        , Just wosl <- Map.lookup h' handleSlots
        , Just (sl, _) <- Map.lookupMax writeSlots
        , wosl < at sl
        , 1 <= Mock.valuesLength vs
        -> s {rangeReadAfterWrite = True}
      _ -> s

    updateBSClosedThrown :: Stats ks vs d -> Stats ks vs d
    updateBSClosedThrown s = case (action, result) of
      (_, MEither (Left (MErr ErrBackingStoreClosed)))
        -> s { bsClosedThrown = Set.insert (tAction action) bsClosedThrown}
      _ -> s

data TagAction =
    TBSClose
  | TBSCopy
  | TBSValueHandle
  | TBSWrite
  | TBSVHClose
  | TBSVHRangeRead
  | TBSVHRead
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Identify actions by their constructor.
tAction :: LockstepAction (BackingStoreState ks vs d) a -> TagAction
tAction = \case
  BSClose           -> TBSClose
  BSCopy _          -> TBSCopy
  BSValueHandle     -> TBSValueHandle
  BSWrite _ _       -> TBSWrite
  BSVHClose _       -> TBSVHClose
  BSVHRangeRead _ _ -> TBSVHRangeRead
  BSVHRead _ _      -> TBSVHRead

data Tag =
    -- | A value handle is created before a write, and read after the write. The
    -- write should not affect the result of the read.
    ReadAfterWrite
    -- | A value handle is created before a write, and read after the write. The
    -- write should not affect the result of the read.
  | RangeReadAfterWrite
  | AllActionsErrorBecauseBackingStoreIsClosed
  deriving (Show)

tagBSAction ::
     Stats ks vs d
  -> LockstepAction (BackingStoreState ks vs d) a
  -> BSVal ks vs d a
  -> [Tag]
tagBSAction Stats{readAfterWrite, rangeReadAfterWrite, bsClosedThrown} _ _ =
    globalTags
  where
    globalTags = mconcat [
        [ ReadAfterWrite
        | readAfterWrite
        ]
      , [ RangeReadAfterWrite
        | rangeReadAfterWrite
        ]
      , [ AllActionsErrorBecauseBackingStoreIsClosed
        | Set.fromList [minBound .. maxBound] == bsClosedThrown
        ]
      ]

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

mkHandler ::
     (IOLike m, Exception e)
  => (e -> Maybe Err)
  -> Handler m (Either Err a)
mkHandler fhandler = Handler $
  \e -> maybe (throwIO e) (return . Left) (fhandler e)

-- | Map LMDB errors to mock errors.
fromDbErr :: LMDB.DbErr -> Maybe Err
fromDbErr = \case
  DbErrStr _                  -> Nothing
  DbErrNoDbState              -> Nothing
  DbErrNonMonotonicSeq wo wo' -> Just $ ErrNonMonotonicSeqNo wo wo'
  DbErrInitialisingNonEmpty _ -> Nothing
  DbErrNoValueHandle _        -> Just ErrBSVHDoesNotExist
  DbErrBadRead                -> Nothing
  DbErrBadRangeRead           -> Nothing
  DbErrDirExists _            -> Just ErrCopyPathAlreadyExists
  DbErrDirDoesntExist _       -> Nothing
  DbErrDirIsNotLMDB _         -> Nothing
  DbErrClosed                 -> Just ErrBackingStoreClosed

-- | Map InMemory (i.e., @TVarBackingStore@) errors to mock errors.
fromTVarExn :: BS.TVarBackingStoreExn -> Maybe Err
fromTVarExn = \case
  BS.TVarBackingStoreClosedExn              -> Just ErrBackingStoreClosed
  BS.TVarBackingStoreValueHandleClosedExn   -> Just ErrBSVHDoesNotExist
  BS.TVarBackingStoreDirectoryExists        -> Just ErrCopyPathAlreadyExists
  BS.TVarBackingStoreNonMonotonicSeq wo wo' -> Just $ ErrNonMonotonicSeqNo wo wo'
  BS.TVarBackingStoreDeserialiseExn _       -> Nothing
  BS.TVarIncompleteDeserialiseExn           -> Nothing

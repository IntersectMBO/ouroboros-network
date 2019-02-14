{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans      #-}
module Test.Ouroboros.Storage.ImmutableDB.StateMachine
    ( tests
    , showLabelledExamples
    , showLabelledErrorExamples
    ) where

import           Prelude hiding (elem, notElem)

import           Control.Monad (when, unless, forM_)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Except (ExceptT(..), runExceptT)
import           Control.Monad.State (MonadState, State, StateT, runState,
                                      evalStateT, get, gets, modify, put, lift)

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.Functor.Classes (Eq1, Show1)
import           Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe, isJust, isNothing)
import           Data.Proxy (Proxy(..))
import           Data.TreeDiff (Expr(App))
import           Data.TreeDiff.Class (ToExpr(..))
import           Data.Typeable (Typeable)

import qualified Generics.SOP as SOP

import           GHC.Generics (Generic, Generic1, from)
import           GHC.Stack (HasCallStack, callStack)

import           System.Random (getStdRandom, randomR)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Pretty (ppShow)

import           Ouroboros.Consensus.Util (lastMaybe)
import qualified Ouroboros.Consensus.Util.Classify as C

import           Ouroboros.Network.Block (Slot(..))

import           Ouroboros.Storage.FS.API (HasFS(..))
import           Ouroboros.Storage.FS.API.Types (FsPath, FsError(..),
                                                 FsErrorType(..), sameFsError)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS, runSimFS, simHasFS)
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.ImmutableDB.Util (renderFile)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.FS.Sim.Error (SimErrorFS, runSimErrorFS,
                                                      mkSimErrorHasFS, Errors,
                                                      withErrors)
import           Test.Ouroboros.Storage.ImmutableDB.Model
import           Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes)
import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES
import           Test.Ouroboros.Storage.ImmutableDB.TestBlock hiding (tests)
import           Test.Ouroboros.Storage.Util (collects, tryImmDB)

import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE


{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
--
-- 'Cmd' will be instantiated to:
--
-- > Cmd (IterRef m r)
-- > Cmd (Iterator m)
--
-- Where @m@ can be 'PureM', 'RealM', or 'RealErrM', and @r@ can be 'Symbolic'
-- or 'Concrete'.
data Cmd it
  = GetBinaryBlob     EpochSlot
  | AppendBinaryBlob  RelativeSlot TestBlock
  | StartNewEpoch     EpochSize
  | StreamBinaryBlobs (Maybe EpochSlot) (Maybe EpochSlot)
  | IteratorNext      it
  | IteratorClose     it
  | Corruption        (Corruption it)
  deriving (Generic, Show, Functor, Foldable, Traversable)

deriving instance SOP.Generic         (Cmd it)
deriving instance SOP.HasDatatypeInfo (Cmd it)

-- | Simulate corruption of some files of the database.
--
-- Includes a list of all open iterators, which should be closed before
-- the corruption is \"executed\". Otherwise, we might delete a file that
-- is still opened by an iterator.
data Corruption it = MkCorruption Corruptions [it]
  deriving (Generic, Show, Functor, Foldable, Traversable)

-- | A 'Cmd' together with 'Errors'.
--
-- When executing the 'Cmd', these 'Errors' are passed to 'SimErrorFS' to
-- simulate file system errors thrown at the 'HasFS' level. When 'Nothing', no
-- errors will be thrown.
data CmdErr it = CmdErr
  { _cmdErr :: Maybe Errors
  , _cmd    :: Cmd it
  } deriving (Generic, Functor, Foldable, Traversable)

instance Show it => Show (CmdErr it) where
  showsPrec d (CmdErr mbErrors cmd) = showParen (d > 10) $
      showString constr . showsPrec 11 cmd
    where
      constr = case mbErrors of
        Nothing  -> "Cmd "
        Just err -> "Cmd " <> show err <> " "

-- | Return type for successful database operations.
data Success it
  = Unit         ()
  | Blob         (Maybe ByteString)
  | Epoch        Epoch
  | Iter         it
  | IterResult   IteratorResult
  | ReopenResult ReopenResult
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Run the command against the given database.
run :: (HasCallStack, Monad m)
    => (ImmutableDB m -> Corruption (Iterator m) -> m (Success (Iterator m)))
       -- ^ How to run a 'Corruption' command.
    -> ImmutableDB m
    -> Cmd (Iterator m)
    -> m (Success (Iterator m))
run runCorruption db cmd = case cmd of
  GetBinaryBlob eps     -> Blob       <$> getBinaryBlob db eps
  AppendBinaryBlob s b  -> Unit       <$> appendBinaryBlob db s (testBlockToBuilder b)
  StartNewEpoch epsz    -> Epoch      <$> startNewEpoch db epsz
  StreamBinaryBlobs s e -> Iter       <$> streamBinaryBlobs db s e
  IteratorNext it       -> IterResult <$> iteratorNext it
  IteratorClose it      -> Unit       <$> iteratorClose it
  Corruption corr       -> runCorruption db corr

-- | The result of reopening the database after corruption, used to test
-- recovery code.
data ReopenResult = MkReopenResult
  { _lastValidBlobAfterRestore :: Maybe EpochSlot
    -- ^ The location of the last valid blob that was returned after reopening
    -- the database with the 'RestoreToLastValidEpoch' policy.
  , _nextEpochSlotAfterRestore :: EpochSlot
    -- ^ The result of 'getNextEpochSlot' after reopening the database with
    -- the 'RestoreToLastValidEpoch' policy.
  } deriving (Eq)

-- | A 'Show' instance that is readable in test output
instance Show ReopenResult where
  show (MkReopenResult lvbl nes) =
    "(_lastValidBlobAfterRestore: "  <> show lvbl <>
    ", _nextEpochSlotAfterRestore: " <> show nes  <> ")"


{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp it = Resp { getResp :: Either ImmutableDBError (Success it) }
  deriving (Functor, Foldable, Traversable)

-- | The 'Eq' instance for 'Resp' uses 'sameImmutableDBError'.
instance Eq it => Eq (Resp it) where
  Resp (Left  e) == Resp (Left  e') = sameImmutableDBError e e'
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

-- | Don't print the callstack and parameters in the 'ImmutableDBError', just
-- the 'FsErrorType' or the constructor name.
instance Show it => Show (Resp it) where
  show (Resp resp) = "(Resp " <> str <> ")"
    where
      str = case resp of
        Left (UnexpectedError (FileSystemError fse)) -> show (fsErrorType fse)
        Left (UnexpectedError ue)                    -> cmdName (from ue)
        Left (UserError ue _)                        -> show ue
        Right success                                -> show success

-- | The monad used to run pure model/mock implementation of the database.
type PureM = ExceptT ImmutableDBError (State DBModel)

-- | The type of the pure model/mock implementation of the database.
type ModelDBPure = ImmutableDB PureM

-- | The monad used to run the real database.
type RealM = SimFS IO

-- | The type of the real database.
type RealDB = ImmutableDB RealM

-- | The monad used to run the real database when simulating errors.
type RealErrM = SimErrorFS IO

-- | The type of the real database when simulating errors.
type RealDBErr = ImmutableDB RealErrM


-- | Run a command against the pure model
runPure :: DBModel
        -> ModelDBPure
        -> Cmd (Iterator PureM)
        -> (Resp (Iterator PureM), DBModel)
runPure dbm mdb cmd =
    first Resp $ runState (runExceptT (run runCorruption mdb cmd)) dbm
  where
    runCorruption :: ModelDBPure -> Corruption (Iterator PureM)
                  -> PureM (Success (Iterator PureM))
    runCorruption _ (MkCorruption corrs its) = do
      mapM_ iteratorClose its
      modify $ simulateCorruptions corrs
      _lastValidBlobAfterRestore <- gets getLastBlobLocation
      _nextEpochSlotAfterRestore <- gets lookupNextEpochSlot
      return $ ReopenResult $ MkReopenResult {..}

{-------------------------------------------------------------------------------
  Collect arguments
-------------------------------------------------------------------------------}

-- | Collect all iterators created. For example, @t@ could be 'Cmd' or 'CmdErr'.
iters :: Traversable t => t it -> [it]
iters = toList


{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Concrete or symbolic references to a real (or model) iterator
type IterRef m = Reference (Opaque (Iterator m))

-- | Mapping between iterator references and mocked iterators
type KnownIters m = RefEnv (Opaque (Iterator m)) (Iterator PureM)

-- | Execution model
data Model m r = Model
  { dbModel     :: DBModel
    -- ^ A model of the database, used as state for the 'HasImmutableDB'
    -- instance of 'ModelDB'.
  , mockDB      :: ModelDBPure
    -- ^ A handle to the mocked database.
  , knownIters  :: KnownIters m r
    -- ^ Store a mapping between iterator references and mocked iterators.
  , simulatedError :: Maybe FsError
    -- ^ Record the simulated error ('FsError') that was thrown. As soon as a
    -- simulated 'FsError' was thrown, the test will stop.
  } deriving (Show, Generic)

-- | Initial model
initModel :: DBModel -> ModelDBPure -> Model m r
initModel dbModel mockDB = Model
    { knownIters  = RE.empty
    , simulatedError = Nothing
    , ..
    }

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Functor t, Eq1 r) => Model m r -> At t m r -> t (Iterator PureM)
toMock Model {..} (At t) = fmap (knownIters RE.!) t

-- | Step the mock semantics
--
-- We cannot step the whole Model here (see 'lockstep', below)
step :: Eq1 r => Model m r -> At Cmd m r -> (Resp (Iterator PureM), DBModel)
step model@Model{..} cmd = runPure dbModel mockDB (toMock model cmd)

-- | Step the mock semantics with a 'CmdErr'
--
-- In the case of a simulated error, we need the actual response to know how
-- to update the model (see 'lockstep', below).
stepErr :: Eq1 r => Model m r -> At CmdErr m r -> Resp (Iterator PureM)
stepErr model (At (CmdErr mbErrors cmd)) = case (mbErrors, resp) of
    -- No simulated errors, just step
    (Nothing, _) -> resp
    -- Even though an error was simulated, another error was thrown before it.
    -- Note that the error is UnexpectedError iff it is simulated. In this
    -- case, return the expected result without a simulated error
    (Just _, Resp (Left (UserError {}))) -> resp
    -- An error was simulated and will be thrown, so mock an error.
    (Just _, _) -> Resp (Left mockedError)
  where
    (resp, _) = step model (At cmd)


{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- | Instantiate functor @t@ to @t ('IterRef' m r)@.
--
-- Needed because we need to (partially) apply @'At' t m@ to @r@.
newtype At t m r = At { unAt :: t (IterRef m r) }
  deriving (Generic)

-- | Don't print the 'At' constructor.
instance Show (t (IterRef m r)) => Show (At t m r) where
  show = show . unAt

deriving instance Eq1 r => Eq (At Resp m r)

deriving instance Generic1          (At Cmd m)
deriving instance Rank2.Foldable    (At Cmd m)
deriving instance Rank2.Functor     (At Cmd m)
deriving instance Rank2.Traversable (At Cmd m)

deriving instance Generic1          (At CmdErr m)
deriving instance Rank2.Foldable    (At CmdErr m)
deriving instance Rank2.Functor     (At CmdErr m)
deriving instance Rank2.Traversable (At CmdErr m)

deriving instance Generic1          (At Resp m)
deriving instance Rank2.Foldable    (At Resp m)


{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event m r = Event
  { eventBefore   :: Model     m r
  , eventCmdErr   :: At CmdErr m r
  , eventAfter    :: Model     m r
  , eventMockResp :: Resp (Iterator PureM)
  } deriving (Show)

eventCmd :: Event m r -> At Cmd m r
eventCmd = At . _cmd . unAt . eventCmdErr

eventMockCmd :: Eq1 r => Event m r -> Cmd (Iterator PureM)
eventMockCmd ev@Event {..} = toMock eventBefore (eventCmd ev)


-- | Construct an event
lockstep :: (Show1 r, Eq1 r)
         => Model     m r
         -> At CmdErr m r
         -> At Resp   m r
         -> Event     m r
lockstep model@Model {..} cmdErr@(At (CmdErr mbErrors cmd)) (At resp) = Event
    { eventBefore   = model
    , eventCmdErr   = cmdErr
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model (At cmd)
    newIters = RE.fromList $ zip (iters resp) (iters mockResp)
    model' = model
      { dbModel        = dbModel'
      , knownIters     = knownIters `RE.union` newIters
      , simulatedError = simulatedErr
      }
    -- Record the simulated error if one was thrown.
    simulatedErr = case (mbErrors, getResp resp) of
        (Just _, Left (UnexpectedError (FileSystemError fse))) -> Just fse
        _                                                      -> Nothing

-- | The error thrown when mocking a simulated 'FileSystemError'.
mockedError :: HasCallStack => ImmutableDBError
mockedError = UnexpectedError $ FileSystemError $ FsError
  { fsErrorType   = FsIllegalOperation
  , fsErrorPath   = ["<mockedError>"]
  , fsErrorString = "mocked error"
  , fsErrorStack  = callStack
  , fsLimitation  = False
  }

isMockedError :: ImmutableDBError -> Bool
isMockedError = sameImmutableDBError mockedError

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}


-- | Generate a 'Cmd'.
generator :: Model m Symbolic -> Gen (At Cmd m Symbolic)
generator Model {..} = At <$> frequency
    [ (1, GetBinaryBlob <$> frequency [ (10, genEpochSlotInThePast)
                                      , (1, genEpochSlotInTheFuture)
                                      , (1, genEpochSlot) ])
    , (2, StartNewEpoch <$> genEpochSize)
    , (3, do
            relSlot <- genRelSlot
            return $ AppendBinaryBlob relSlot (TestBlock relSlot))
    , (1, frequency
            -- An iterator with a random and likely invalid range,
            [ (1, StreamBinaryBlobs <$> (Just <$> genEpochSlot)
                                    <*> (Just <$> genEpochSlot))
            -- An iterator that is more likely to be valid. A
            -- SlotGreaterThanEpochSizeError is still possible.
            , (2, do
                    start <- genEpochSlotInThePast
                    end   <- genEpochSlotInThePast `suchThat` (>= start)
                    return $ StreamBinaryBlobs (Just start) (Just end))
            -- Same as above, but with possible empty bounds
            , (2, do
                    start <- genEpochSlotInThePast
                    end   <- genEpochSlotInThePast `suchThat` (>= start)
                    mbStart <- elements [Nothing, Just start]
                    mbEnd   <- elements [Nothing, Just end]
                    return $ StreamBinaryBlobs mbStart mbEnd)
            ])
      -- Only if there are iterators can we generate commands that manipulate
      -- them.
    , (if Map.null dbmIterators then 0 else 4,
       do
         iter <- elements $ RE.keys knownIters
         frequency [ (4, return $ IteratorNext  iter)
                   , (1, return $ IteratorClose iter) ])
      -- Only if there are files on disk can we generate commands that corrupt
      -- them.
    , (if null dbFiles then 0 else 2, Corruption <$> genCorruption)
    ]
  where
    DBModel {..} = dbModel
    currentEpochSize = CES.lastEpochSize dbmCumulEpochSizes

    EpochSlot maxEpoch nextSlot = lookupNextEpochSlot dbModel

    genEpochSize = choose (1, 20)

    -- + x because we also want relative slots that are greater than the epoch
    -- size
    genRelSlot = RelativeSlot <$> choose (0, currentEpochSize + 2)

    genEpochSlot = EpochSlot <$> arbitrary <*> genRelSlot

    genEpochSlotInThePast = do
      epoch <- choose (0, maxEpoch)
      slot  <- if epoch == maxEpoch
               then choose (0, getRelativeSlot nextSlot - 1)
               else arbitrary
      return (EpochSlot epoch (RelativeSlot slot))

    genEpochSlotInTheFuture = do
      epoch <- choose (maxEpoch, maxBound)
      slot  <- if epoch == maxEpoch
               then choose (getRelativeSlot nextSlot, maxBound)
               else arbitrary
      return (EpochSlot epoch (RelativeSlot slot))

    genCorruption = MkCorruption <$>
      generateCorruptions (NE.fromList dbFiles) <*> pure openIterators

    openIterators = RE.keys knownIters

    dbFiles = getDBFiles dbModel

getEpochsOnDisk :: DBModel -> [Epoch]
getEpochsOnDisk DBModel {..} = [0..CES.lastEpoch dbmCumulEpochSizes]

getDBFiles :: DBModel -> [FsPath]
getDBFiles dbModel = getEpochsOnDisk dbModel >>= \epoch ->
    [ dbFolder <> renderFile "index" epoch
    , dbFolder <> renderFile "epoch" epoch
    ]

-- | Generate a 'CmdErr' without 'Errors'
generatorNoErr :: Model m Symbolic -> Maybe (Gen (At CmdErr m Symbolic))
generatorNoErr m =
    Just $ At . CmdErr Nothing . unAt <$> generator m

-- | Generate a 'CmdErr' that can have 'Errors'
generatorErr :: Model m Symbolic -> Maybe (Gen (At CmdErr m Symbolic))
generatorErr m@Model {..}
    | isJust simulatedError  -- If an error was thrown, stop testing
    = Nothing
    | otherwise
    = Just $ At <$> do
      cmd <- genModel
      frequency
        -- We want to make some progress
        [ (4, return $ CmdErr Nothing cmd)
          -- TODO Don't simulate an error during corruption, because we don't
          -- want an error to happen while we corrupt a file. We could test
          -- what happens when an error is thrown during recovery.
        , (if isCorruption cmd then 0 else 1
          , CmdErr <$> (Just <$> arbitrary) <*> return cmd)
        ]
  where
    genModel = unAt <$> generator m
    isCorruption Corruption {} = True
    isCorruption _             = False


{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker :: Model m Symbolic -> At CmdErr m Symbolic -> [At CmdErr m Symbolic]
shrinker m (At (CmdErr mbErrors cmd)) = fmap At $
    [ CmdErr mbErrors' cmd  | mbErrors' <- shrink mbErrors ] ++
    [ CmdErr mbErrors  cmd' | At cmd'   <- shrinkCmd m (At cmd) ]

-- | Shrink a 'Cmd'.
shrinkCmd :: Model m Symbolic -> At Cmd m Symbolic -> [At Cmd m Symbolic]
shrinkCmd Model {..} (At cmd) = fmap At $ case cmd of
    AppendBinaryBlob relSlot _ ->
      [ AppendBinaryBlob relSlot' (TestBlock relSlot')
      | relSlot' <- shrinkRelativeSlot relSlot]
    StartNewEpoch epochSize ->
      [StartNewEpoch epochSize' | epochSize' <- shrink epochSize
                                , epochSize' > 0]
    StreamBinaryBlobs mbStart mbEnd ->
      [ StreamBinaryBlobs mbStart' mbEnd
      | mbStart' <- shrinkMbEpochSlot mbStart] ++
      [ StreamBinaryBlobs mbStart  mbEnd'
      | mbEnd'   <- shrinkMbEpochSlot mbEnd]
    GetBinaryBlob epochSlot ->
      [GetBinaryBlob epochSlot' | epochSlot' <- shrinkEpochSlot epochSlot]
    IteratorNext  {} -> []
    IteratorClose {} -> []
    Corruption corr ->
      [Corruption corr' | corr' <- shrinkCorruption corr]
  where
    shrinkRelativeSlot :: RelativeSlot -> [RelativeSlot]
    shrinkRelativeSlot = genericShrink
    shrinkMbEpochSlot :: Maybe EpochSlot -> [Maybe EpochSlot]
    shrinkMbEpochSlot Nothing   = []
    shrinkMbEpochSlot (Just es) = Nothing : map Just (shrinkEpochSlot es)
    shrinkEpochSlot :: EpochSlot -> [EpochSlot]
    shrinkEpochSlot (EpochSlot epoch relSlot) =
      [EpochSlot epoch' relSlot  | epoch'   <- shrink epoch] ++
      [EpochSlot epoch  relSlot' | relSlot' <- shrinkRelativeSlot relSlot]
    shrinkCorruption (MkCorruption corrs _) =
      [ MkCorruption corrs' (RE.keys knownIters)
      | corrs' <- shrinkCorruptions corrs]

{-------------------------------------------------------------------------------
  The final state machine
-------------------------------------------------------------------------------}


-- | Mock a response
--
-- We do this by running the pure semantics and then generating mock
-- references for any new handles.
mock :: Typeable m
     => Model           m Symbolic
     -> At CmdErr       m Symbolic
     -> GenSym (At Resp m Symbolic)
mock model cmdErr = At <$> traverse (const genSym) resp
  where
    resp = stepErr model cmdErr

precondition :: Model m Symbolic -> At CmdErr m Symbolic -> Logic
precondition Model {..} (At (CmdErr _ cmd)) =
    Boolean (isNothing simulatedError) .&&
    forall (iters cmd) (`elem` RE.keys knownIters) .&&
    case cmd of
      Corruption corr ->
        forall (corruptionFiles corr) (`elem` getDBFiles dbModel)
      _ -> Top
  where
    corruptionFiles (MkCorruption corrs _) = map snd $ NE.toList corrs

transition :: (Show1 r, Eq1 r)
           => Model m r -> At CmdErr m r -> At Resp m r -> Model m r
transition model cmdErr = eventAfter . lockstep model cmdErr

postcondition :: Model m Concrete
              -> At CmdErr m Concrete
              -> At Resp m Concrete
              -> Logic
postcondition model cmdErr@(At (CmdErr mbErrors _)) resp = case unAt resp of
    -- Check that the UnexpectedError that was simulated
    Resp (Left (UnexpectedError {}))
      | Just _ <- mbErrors -> Top

    -- Report an unexpected success when a simulated error was supposed to be
    -- thrown
    Resp (Right _)
      | Just _ <- mbErrors ->
      Bot .// "postconditionErr expected failure"

    -- Otherwise: expected success or expected error. Even when simulating an
    -- error, it is possible that we expect another error to be thrown, which
    -- can happen before the simulated is thrown.
    _ -> toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

toResp :: (Typeable m, MonadMask n)
       => n (Success (Iterator m)) -> n (At Resp m Concrete)
toResp n = At . fmap (reference . Opaque) . Resp <$> tryImmDB n

-- | Ignores the 'Errors'
semantics :: (MonadMask m, Typeable m)
          => HasFS m -> ImmutableDB m -> At CmdErr m Concrete
          -> m (At Resp m Concrete)
semantics hasFS db (At (CmdErr _ cmd)) =
    toResp $ run (semanticsCorruption hasFS) db (opaque <$> cmd)

semanticsErr :: HasFS RealErrM -> RealDBErr -> At CmdErr RealErrM Concrete
             -> RealErrM (At Resp RealErrM Concrete)
semanticsErr hasFS db (At cmdErr) = case opaque <$> cmdErr of
    CmdErr Nothing _         -> semantics hasFS db (At cmdErr)
    CmdErr (Just errors) cmd -> At . fmap (reference . Opaque) <$> do
      res <- withErrors errors $
        tryImmDB $ run (semanticsCorruption hasFS) db cmd
      case res of
        Left _ -> return (Resp res)
        -- When by coincidence no error was thrown, force an error to be
        -- thrown, as we need the outcome to be deterministic. The mocked
        -- response for this command must match the actual command. When
        -- constructing the mocked response, we don't know yet whether the
        -- simulated error will be thrown in practice or not. We assume it is
        -- thrown, so we must make sure here it is actually thrown.
        Right _ -> do
          when (unexpectedErrorShouldCloseDB cmd) $ closeDB db
          return $ Resp (Left forcedError)

semanticsCorruption :: MonadMask m
                    => HasFS m -> ImmutableDB m
                    -> Corruption (Iterator m)
                    -> m (Success (Iterator m))
semanticsCorruption hasFS db (MkCorruption corrs its) = do
    mapM_ iteratorClose its
    closeDB db
    forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file

    -- Record the error thrown using the 'ValidateAllEpochs' policy
    let validatePol = ValidateAllEpochs $ testBlockEpochFileParser' hasFS
    validateRes      <- tryImmDB $ reopen db validatePol
    _validationError <- case validateRes of
          Left e  -> return $ Just e
          -- Close the DB again, otherwise the next reopen is a no-op.
          Right _ -> closeDB db $> Nothing

    -- Now try to restore to the last valid epoch slot.
    let restorePol = RestoreToLastValidEpoch $ testBlockEpochFileParser' hasFS
    _lastValidBlobAfterRestore <- reopen db restorePol
    _nextEpochSlotAfterRestore <- getNextEpochSlot db
    return $ ReopenResult $ MkReopenResult {..}


forcedFsError :: HasCallStack => FsError
forcedFsError = FsError
  { fsErrorType   = FsIllegalOperation
  , fsErrorPath   = ["<forcedError>"]
  , fsErrorString = "forced error"
  , fsErrorStack  = callStack
  , fsLimitation  = False
  }

forcedError :: HasCallStack => ImmutableDBError
forcedError = UnexpectedError $ FileSystemError forcedFsError

-- | Should an 'UnexpectedError' thrown during the 'Cmd' close the database?
--
-- Yes for write operations, no for read operations.
unexpectedErrorShouldCloseDB :: Cmd it -> Bool
unexpectedErrorShouldCloseDB cmd = case cmd of
    AppendBinaryBlob  {} -> True
    StartNewEpoch     {} -> True
    GetBinaryBlob     {} -> False
    StreamBinaryBlobs {} -> False
    IteratorNext      {} -> False
    IteratorClose     {} -> False
    Corruption        {} -> True

-- | The state machine proper
sm :: HasFS RealM
   -> RealDB
   -> DBModel
   -> ModelDBPure
   -> StateMachine (Model RealM) (At CmdErr RealM) RealM (At Resp RealM)
sm hasFS db dbm mdb = StateMachine
  { initModel     = initModel dbm mdb
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = generatorNoErr
  , shrinker      = shrinker
  , semantics     = semantics hasFS db
  , mock          = mock
  , invariant     = Nothing
  , distribution  = Nothing
  }

-- | The state machine with errors
smErr :: HasFS RealErrM
      -> RealDBErr
      -> DBModel
      -> ModelDBPure
      -> StateMachine (Model RealErrM) (At CmdErr RealErrM) RealErrM (At Resp RealErrM)
smErr hasFS db dbm mdb = StateMachine
  { initModel     = initModel dbm mdb
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = generatorErr
  , shrinker      = shrinker
  , semantics     = semanticsErr hasFS db
  , mock          = mock
  , invariant     = Nothing
  , distribution  = Nothing
  }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

validate :: forall m. Monad m
         => Model m Concrete -> ImmutableDB m
         -> m ()
validate Model {..} db = flip evalStateT dbModel $ do
    itDB    <- runDB    $ streamBinaryBlobs db     Nothing Nothing
    itModel <- runModel $ streamBinaryBlobs mockDB Nothing Nothing
    let loop = do
          dbRes    <- runDB    $ iteratorNext itDB
          modelRes <- runModel $ iteratorNext itModel
          case (dbRes, modelRes) of
            -- The database should be a prefix of the model, so this is
            -- fine
            (IteratorExhausted, _) -> return ()
            (IteratorResult {},  IteratorResult {})
              | dbRes == modelRes -> loop
            _ -> fail $ "Mismatch between database (" <> show dbRes <>
                        ") and model (" <> show modelRes <> ")"
    loop
  where
    runDB :: m a -> StateT DBModel m a
    runDB = lift

    runModel :: PureM a -> StateT DBModel m a
    runModel m = do
      s <- get
      case runState (runExceptT m) s of
        (Left e,  _)  -> fail $ prettyImmutableDBError e
        (Right a, s') -> put s' >> return a


{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

data Tag
  = TagGetBinaryBlobJust

  | TagGetBinaryBlobNothing

  | TagReadFutureSlotError

  | TagSlotGreaterThanEpochSizeError

  | TagAppendToSlotInThePastError

  | TagGetFromEmptyEpoch

  | TagEmptyEpochsInARow Int

  | TagIteratorStartsOnEmptySlot

  | TagIteratorStartsInEmptyEpoch

  | TagIteratorStreamedNBlobs Int

  | TagIteratorWithoutBounds

  | TagCorruption

  | TagErrorDuringAppendBinaryBlob

  | TagErrorDuringStartNewEpoch

  | TagErrorDuringGetBinaryBlob

  | TagErrorDuringStreamBinaryBlobs

  | TagErrorDuringIteratorNext

  | TagErrorDuringIteratorClose

  deriving (Show, Eq)


-- | Predicate on events
type EventPred m = C.Predicate (Event m Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful :: (    Event m Symbolic
                -> Success (Iterator PureM)
                -> Either Tag (EventPred m)
              )
           -> EventPred m
successful f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  _ ) -> Right $ successful f
    Resp (Right ok) -> f ev ok

-- | Convenience combinator for creating classifiers for failed commands
failed :: (    Event m Symbolic
            -> ImmutableDBError
            -> Either Tag (EventPred m)
          )
       -> EventPred m
failed f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  e) -> f ev e
    Resp (Right _) -> Right $ failed f

-- | Convenience combinator for creating classifiers for commands failed with
-- a 'UserError'
failedUserError :: (    Event m Symbolic
                     -> UserError
                     -> Either Tag (EventPred m)
                   )
                -> EventPred m
failedUserError f = failed $ \ev e -> case e of
    UserError ue _ -> f ev ue
    _              -> Right $ failedUserError f



-- | Convenience combinator for creating classifiers for commands during which
-- a generated FsError was thrown
errorExpected :: (Event m Symbolic -> Either Tag (EventPred m))
              -> EventPred m
errorExpected f = C.predicate $ \ev ->
    case eventMockResp ev of
      Resp (Left  e) | isMockedError e -> f ev
      Resp _ -> Right $ errorExpected f


-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: forall m. [Event m Symbolic] -> [Tag]
tag = C.classify
    [ tagGetBinaryBlobJust
    , tagGetBinaryBlobNothing
    , tagReadFutureSlotError
    , tagSlotGreaterThanEpochSizeError
    , tagAppendToSlotInThePastError
    , tagGetFromEmptyEpoch
    , tagEmptyEpochsInARow 0
    , tagIteratorStartsOnEmptySlot
    , tagIteratorStartsInEmptyEpoch
    , tagIteratorStreamedNBlobs Map.empty
    , tagIteratorWithoutBounds
    , tagCorruption
    , tagErrorDuringAppendBinaryBlob
    , tagErrorDuringStartNewEpoch
    , tagErrorDuringGetBinaryBlob
    , tagErrorDuringStreamBinaryBlobs
    , tagErrorDuringIteratorNext
    , tagErrorDuringIteratorClose
    ]
  where
    tagGetBinaryBlobJust :: EventPred m
    tagGetBinaryBlobJust = successful $ \ev r -> case r of
      Blob (Just _) | GetBinaryBlob {} <- unAt $ eventCmd ev ->
        Left TagGetBinaryBlobJust
      _ -> Right tagGetBinaryBlobJust

    tagGetBinaryBlobNothing :: EventPred m
    tagGetBinaryBlobNothing = successful $ \ev r -> case r of
      Blob Nothing | GetBinaryBlob {} <- unAt $ eventCmd ev ->
        Left TagGetBinaryBlobNothing
      _ -> Right tagGetBinaryBlobNothing

    tagReadFutureSlotError :: EventPred m
    tagReadFutureSlotError = failedUserError $ \_ e -> case e of
      ReadFutureSlotError {} -> Left TagReadFutureSlotError
      _ -> Right tagReadFutureSlotError

    tagSlotGreaterThanEpochSizeError :: EventPred m
    tagSlotGreaterThanEpochSizeError = failedUserError $ \_ e -> case e of
      SlotGreaterThanEpochSizeError {} -> Left TagSlotGreaterThanEpochSizeError
      _ -> Right tagSlotGreaterThanEpochSizeError

    tagAppendToSlotInThePastError :: EventPred m
    tagAppendToSlotInThePastError = failedUserError $ \_ e -> case e of
      AppendToSlotInThePastError {} -> Left TagAppendToSlotInThePastError
      _ -> Right tagAppendToSlotInThePastError

    tagGetFromEmptyEpoch :: EventPred m
    tagGetFromEmptyEpoch = successful $ \ev r -> case r of
      Blob Nothing
        | At (GetBinaryBlob (EpochSlot epoch _)) <- eventCmd ev
          -- No blobs start in the given epoch
        , not $ any ((epoch ==) .  _epoch) $ Map.keys $ dbmBlobs
        $ dbModel $ eventBefore ev
        -> Left  TagGetFromEmptyEpoch
      _ -> Right tagGetFromEmptyEpoch

    tagEmptyEpochsInARow :: Int -> EventPred m
    tagEmptyEpochsInARow before = C.Predicate
      { C.predApply = \ev -> case eventMockResp ev of
          Resp (Right _)
            | At (StartNewEpoch {}) <- eventCmd ev
            -> Right $ tagEmptyEpochsInARow (before + 1)
          _ -> Right $ tagEmptyEpochsInARow 0
      , C.predFinish = case before > 0 of
          True  -> Just $ TagEmptyEpochsInARow before
          False -> Nothing
      }

    tagIteratorStartsOnEmptySlot :: EventPred m
    tagIteratorStartsOnEmptySlot = successful $ \ev _ -> case eventCmd ev of
      At (StreamBinaryBlobs (Just start) _)
        | Map.notMember start $ dbmBlobs $ dbModel $ eventBefore ev
        -> Left  TagIteratorStartsOnEmptySlot
      _ -> Right tagIteratorStartsOnEmptySlot

    tagIteratorStartsInEmptyEpoch :: EventPred m
    tagIteratorStartsInEmptyEpoch = successful $ \ev _ -> case eventCmd ev of
      At (StreamBinaryBlobs (Just (EpochSlot epoch _)) _)
        | not $ any ((epoch ==) .  _epoch) $ Map.keys $ dbmBlobs
        $ dbModel $ eventBefore ev
        -> Left  TagIteratorStartsInEmptyEpoch
      _ -> Right tagIteratorStartsInEmptyEpoch

    tagIteratorStreamedNBlobs :: Map (Iterator PureM) Int
                              -> EventPred m
    tagIteratorStreamedNBlobs blobsStreamed = C.Predicate
      { C.predApply = \ev -> case eventMockResp ev of
          Resp (Right (IterResult (IteratorResult {})))
            | IteratorNext it <- eventMockCmd ev
            -> Right $ tagIteratorStreamedNBlobs $
               Map.insertWith (+) it 1 blobsStreamed
          _ -> Right $ tagIteratorStreamedNBlobs blobsStreamed
      , C.predFinish = do
          -- Find the entry with the highest value, i.e. the iterator that has
          -- streamed the most blobs
          (_, streamed) <- listToMaybe $ sortBy (flip compare `on` snd) $
                           Map.toList blobsStreamed
          return $ TagIteratorStreamedNBlobs streamed
      }

    tagIteratorWithoutBounds :: EventPred m
    tagIteratorWithoutBounds = successful $ \ev _ -> case eventCmd ev of
      At (StreamBinaryBlobs Nothing Nothing) -> Left TagIteratorWithoutBounds
      _ -> Right tagIteratorWithoutBounds

    tagCorruption :: EventPred m
    tagCorruption = C.Predicate
      { C.predApply = \ev -> case eventCmd ev of
          At (Corruption {}) -> Left  TagCorruption
          _                  -> Right tagCorruption
      , C.predFinish = Nothing
      }

    tagErrorDuringAppendBinaryBlob :: EventPred m
    tagErrorDuringAppendBinaryBlob = errorExpected $ \ev -> case eventCmd ev of
      At (AppendBinaryBlob {}) -> Left TagErrorDuringAppendBinaryBlob
      _ -> Right tagErrorDuringAppendBinaryBlob

    tagErrorDuringStartNewEpoch :: EventPred m
    tagErrorDuringStartNewEpoch = errorExpected $ \ev -> case eventCmd ev of
      At (StartNewEpoch {}) -> Left TagErrorDuringStartNewEpoch
      _ -> Right tagErrorDuringStartNewEpoch

    tagErrorDuringGetBinaryBlob :: EventPred m
    tagErrorDuringGetBinaryBlob = errorExpected $ \ev -> case eventCmd ev of
      At (GetBinaryBlob {}) -> Left TagErrorDuringGetBinaryBlob
      _ -> Right tagErrorDuringGetBinaryBlob

    tagErrorDuringStreamBinaryBlobs :: EventPred m
    tagErrorDuringStreamBinaryBlobs = errorExpected $ \ev -> case eventCmd ev of
      At (StreamBinaryBlobs {}) -> Left TagErrorDuringStreamBinaryBlobs
      _ -> Right tagErrorDuringStreamBinaryBlobs

    tagErrorDuringIteratorNext :: EventPred m
    tagErrorDuringIteratorNext = errorExpected $ \ev -> case eventCmd ev of
      At (IteratorNext {}) -> Left TagErrorDuringIteratorNext
      _ -> Right tagErrorDuringIteratorNext

    tagErrorDuringIteratorClose :: EventPred m
    tagErrorDuringIteratorClose = errorExpected $ \ev -> case eventCmd ev of
      At (IteratorClose {}) -> Left TagErrorDuringIteratorClose
      _ -> Right tagErrorDuringIteratorClose


-- | Step the model using a 'QSM.Command' (i.e., a command associated with
-- an explicit set of variables)
execCmd :: Model m Symbolic
        -> QSM.Command (At CmdErr m) (At Resp m)
        -> Event m Symbolic
execCmd model (QSM.Command cmdErr resp _vars) = lockstep model cmdErr resp

-- | 'execCmds' is just the repeated form of 'execCmd'
execCmds :: forall m
          . Model m Symbolic
         -> QSM.Commands (At CmdErr m) (At Resp m) -> [Event m Symbolic]
execCmds model = \(QSM.Commands cs) -> go model cs
  where
    go :: Model m Symbolic -> [QSM.Command (At CmdErr m) (At Resp m)]
       -> [Event m Symbolic]
    go _ []        = []
    go m (c : cs) = let ev = execCmd m c in ev : go (eventAfter ev) cs


{-------------------------------------------------------------------------------
  Required instances

  The 'ToExpr' constraints come from "Data.TreeDiff".
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd m) where
  cmdName (At cmd) = constrName cmd
  cmdNames (_ :: Proxy (At Cmd m r)) =
    constrNames (Proxy @(Cmd (IterRef m r)))

instance CommandNames (At CmdErr m) where
  cmdName (At (CmdErr _ cmd)) = constrName cmd
  cmdNames (_ :: Proxy (At CmdErr m r)) =
    constrNames (Proxy @(Cmd (IterRef m r)))

instance Show (Iterator PureM) where
  show it = "<iterator " <> show (iteratorID it) <> ">"

instance Show ModelDBPure where
  show _ = "<ModelDB>"

instance ToExpr Slot where
  toExpr (Slot w) = App "Slot" [toExpr w]

instance ToExpr EpochSlot
instance ToExpr RelativeSlot
instance ToExpr IteratorID
instance ToExpr CumulEpochSizes
instance ToExpr IteratorModel
instance ToExpr DBModel

instance ToExpr FsError where
  toExpr fsError = App (show fsError) []

instance ToExpr ModelDBPure where
  toExpr db = App (show db) []

instance ToExpr (Iterator PureM) where
  toExpr it = App (show it) []

instance ToExpr (Model m Concrete)

-- Orphan instance to store 'Iterator's in a 'Map'.
instance Ord (Iterator m) where
  compare = compare `on` iteratorID

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

-- | Show minimal examples for each of the generated tags
--
showLabelledExamples'
  :: Maybe Int
  -- ^ Seed
  -> Int
  -- ^ Number of tests to run to find examples
  -> (Tag -> Bool)
  -- ^ Tag filter (can be @const True@)
  -> (    ImmutableDB m
       -> DBModel
       -> ModelDBPure
       -> StateMachine (Model m) (At CmdErr m) m (At Resp m)
     )
  -- ^ The state machine
  -> IO ()
showLabelledExamples' mbReplay numTests focus stateMachine = do
    replaySeed <- case mbReplay of
      Nothing   -> getStdRandom (randomR (1,999999))
      Just seed -> return seed

    labelledExamplesWith (stdArgs { replay     = Just (mkQCGen replaySeed, 0)
                                  , maxSuccess = numTests
                                  }) $
      forAllShrinkShow (QSM.generateCommands smUnused Nothing)
                       (QSM.shrinkCommands   smUnused)
                       ppShow $ \cmds ->
        collects (filter focus . tag . execCmds (QSM.initModel smUnused) $ cmds) $
          property True
  where
    (dbm, mdb) = mkDBModel
    smUnused = stateMachine dbUnused dbm mdb


showLabelledExamples :: IO ()
showLabelledExamples =
    showLabelledExamples' Nothing 1000 nonError (sm hasFSRealM)
  where
    nonError TagErrorDuringAppendBinaryBlob = False
    nonError TagErrorDuringStartNewEpoch    = False
    nonError TagErrorDuringGetBinaryBlob    = False
    nonError _                              = True

showLabelledErrorExamples :: IO ()
showLabelledErrorExamples =
    showLabelledExamples' Nothing 1000 (const True) (smErr hasFSRealErrM)

prop_sequential :: Property
prop_sequential = forAllCommands smUnused Nothing $ \cmds ->
    QCM.monadic (ioProperty . runSimIO) $ do
      (db, Nothing) <- QCM.run $
        openDB hasFS EH.monadCatch dbFolder 0 (Map.singleton 0 firstEpochSize)
          NoValidation
      let sm' = sm hasFS db dbm mdb
      (hist, model, res) <- runCommands sm' cmds
      lastBlobLocation <- QCM.run $ do
        closeDB db
        lastBlobLocation <- reopen db NoValidation
        validate model db
        closeDB db
        return lastBlobLocation

      let lastBlobLocationModel = getLastBlobLocation $ dbModel model

      QCM.monitor $ counterexample ("lastBlobLocation: " <> show lastBlobLocation)
      QCM.monitor $ counterexample ("lastBlobLocationModel: " <> show lastBlobLocationModel)

      prettyCommands sm' hist
        $ tabulate "Tags" (map show $ tag (execCmds (QSM.initModel sm') cmds))
        $ res === Ok .&&. lastBlobLocationModel === lastBlobLocation
  where
    (dbm, mdb) = mkDBModel
    smUnused = sm hasFS dbUnused dbm mdb
    hasFS = hasFSRealM


prop_sequential_errors :: Property
prop_sequential_errors = forAllCommands smUnused Nothing $ \cmds ->
    QCM.monadic (ioProperty . runSimErrorIO) $ do
      (db, Nothing) <- QCM.run $
        openDB hasFS EH.monadCatch dbFolder 0 (Map.singleton 0 firstEpochSize)
          NoValidation
      let sm' = smErr hasFS db dbm mdb
      (hist, model, res) <- runCommands sm' cmds
      let events = execCmds (QSM.initModel sm') cmds
          mbLastEvent = lastMaybe events
      reopenedEpoch <- QCM.run $ do
        checkIfDBClosedWhenNecessary db mbLastEvent
        -- If we don't close all (open) iterators, we may still have open
        -- handles to database files that have to be removed while
        -- reopening/restoring the database.
        closeIterators model
        let recPol = RestoreToLastValidEpoch $ testBlockEpochFileParser' hasFS
        _lastBlobLocation <- reopen db recPol -- TODO check
        reopenedEpoch <- getCurrentEpoch db
        validate model db
        closeDB db
        return reopenedEpoch

      let (errSimulated, errDuring, errForced, errMsg) = case mbLastEvent of
            Nothing -> let none = "no commands" in (none, none, none, none)
            Just ev -> case simulatedError model of
              Nothing  -> ("no", "no error", "no error", "no error")
              Just fsErr -> ("yes", cmdName (eventCmd ev),
                             if sameFsError fsErr forcedFsError
                             then "forced" else "unforced",
                             fsErrorString fsErr)
          lastModelEpoch   = CES.lastEpoch $ dbmCumulEpochSizes $ dbModel model
          epochsRolledBack = lastModelEpoch - reopenedEpoch
          reopenedEpochStr
            | reopenedEpoch > 3 = ">3"
            | otherwise         = show reopenedEpoch

      prettyCommands sm' hist
        $ tabulate "Tags"                   (map show (tag events))
        $ tabulate "Error simulated"        [errSimulated]
        $ tabulate "Error simulated during" [errDuring]
        $ tabulate "Error forced"           [errForced]
        $ tabulate "Error msg"              [errMsg]
        $ tabulate "Reopened epoch"         [reopenedEpochStr]
        $ tabulate "Epochs rolled back"     [show epochsRolledBack]
        $ res === Ok .&&. epochsRolledBack <= 1
  where
    (dbm, mdb) = mkDBModel
    smUnused = smErr hasFS dbUnused dbm mdb
    hasFS = hasFSRealErrM

    checkIfDBClosedWhenNecessary db mbLastEvent = do
      open <- isOpen db
      case mbLastEvent of
        Nothing -> return ()
        Just ev
          | isJust $ simulatedError (eventAfter ev)
          , unexpectedErrorShouldCloseDB (unAt (eventCmd ev))
          -> when open $
             fail "Database is still open while it should be closed"
          | otherwise
          -> unless open $
             fail "Database is closed while it should be open"

closeIterators :: Monad m => Model m Concrete -> m ()
closeIterators Model {..} =
  mapM_ (iteratorClose . opaque) (RE.keys knownIters)

tests :: TestTree
tests = testGroup "ImmutableDB q-s-m"
    [ testProperty "sequential" prop_sequential
    , testProperty "sequential with errors" prop_sequential_errors
    ]

runSimIO :: SimFS IO a -> IO a
runSimIO m = fst <$> runSimFS m Mock.empty

runSimErrorIO :: SimErrorFS IO a -> IO a
runSimErrorIO m = fst <$> runSimErrorFS m Mock.empty mempty

firstEpochSize :: EpochSize
firstEpochSize = 10

dbFolder :: FsPath
dbFolder = ["test"]

mkDBModel :: MonadState DBModel m
          => (DBModel, ImmutableDB (ExceptT ImmutableDBError m))
mkDBModel = openDBModel EH.exceptT firstEpochSize

dbUnused :: ImmutableDB m
dbUnused = error "semantics and DB used during command generation"

hasFSRealM :: HasFS RealM
hasFSRealM = simHasFS EH.monadCatch

hasFSRealErrM :: HasFS RealErrM
hasFSRealErrM = mkSimErrorHasFS hasFSRealM


-- TODO use condense?

{-------------------------------------------------------------------------------
  generics-sop auxiliary
-------------------------------------------------------------------------------}

cmdConstrInfo :: Proxy (Cmd it)
              -> SOP.NP SOP.ConstructorInfo (SOP.Code (Cmd it))
cmdConstrInfo = SOP.constructorInfo . SOP.datatypeInfo

constrName :: forall it. Cmd it -> String
constrName a =
    SOP.hcollapse $ SOP.hliftA2 go (cmdConstrInfo p) (SOP.unSOP (SOP.from a))
  where
    go :: SOP.ConstructorInfo a -> SOP.NP SOP.I a -> SOP.K String a
    go nfo _ = SOP.K $ SOP.constructorName nfo

    p = Proxy @(Cmd it)

constrNames :: Proxy (Cmd it) -> [String]
constrNames p =
    SOP.hcollapse $ SOP.hmap go (cmdConstrInfo p)
  where
    go :: SOP.ConstructorInfo a -> SOP.K String a
    go nfo = SOP.K $ SOP.constructorName nfo

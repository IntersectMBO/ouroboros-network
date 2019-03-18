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
    ) where

import           Prelude hiding (elem, notElem)

import           Control.Monad (forM_)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.State (MonadState, State, StateT, evalStateT,
                     get, gets, lift, modify, put, runState)

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
import           Data.Maybe (listToMaybe)
import           Data.Proxy (Proxy (..))
import           Data.TreeDiff (Expr (App))
import           Data.TreeDiff.Class (ToExpr (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import qualified Generics.SOP as SOP

import           GHC.Generics (Generic, Generic1)
import           GHC.Stack (HasCallStack)

import           System.Random (getStdRandom, randomR)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Pretty (ppShow)

import qualified Ouroboros.Consensus.Util.Classify as C

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.FS.API (HasFS (..))
import           Ouroboros.Storage.FS.API.Types (FsError (..), FsPath)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes,
                     EpochSlot (..), RelativeSlot (..))
import qualified Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES
import           Ouroboros.Storage.ImmutableDB.Util (renderFile)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH


import           Test.Ouroboros.Storage.FS.Sim.Error (Errors, mkSimErrorHasFS,
                     withErrors)
import           Test.Ouroboros.Storage.ImmutableDB.Model
import           Test.Ouroboros.Storage.ImmutableDB.TestBlock hiding (tests)
import           Test.Ouroboros.Storage.Util (collects, tryImmDB)

import           Test.Util.Orphans.Arbitrary ()
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
  = GetBinaryBlob     SlotNo
  | AppendBinaryBlob  SlotNo TestBlock
  | StreamBinaryBlobs (Maybe SlotNo) (Maybe SlotNo)
  | IteratorNext      it
  | IteratorClose     it
  | Reopen            ValidationPolicy (Maybe TruncateFrom)
  | Corruption        Corruption
  deriving (Generic, Show, Functor, Foldable, Traversable)

deriving instance SOP.Generic         (Cmd it)
deriving instance SOP.HasDatatypeInfo (Cmd it)

-- | Simulate corruption of some files of the database.
newtype Corruption = MkCorruption Corruptions
  deriving (Generic, Show)

-- | A 'Cmd' together with 'Errors'.
--
-- When executing the 'Cmd', these 'Errors' are passed to 'SimErrorFS' to
-- simulate file system errors thrown at the 'HasFS' level. When 'Nothing', no
-- errors will be thrown.
data CmdErr it = CmdErr
  { _cmdErr   :: Maybe Errors
  , _cmd      :: Cmd it
  , _cmdIters :: [it]
    -- ^ A list of all open iterators. For some commands, e.g., corrupting the
    -- database or simulating errors, we need to close and reopen the
    -- database, which almost always requires truncation of the database.
    -- During truncation we might need to delete a file that is still opened
    -- by an iterator. As this is not allowed by the MockFS implementation, we
    -- first close all open iterators in these cases.
    --
    -- See #328
  } deriving (Generic, Functor, Foldable, Traversable)

instance Show it => Show (CmdErr it) where
  showsPrec d (CmdErr mbErrors cmd its) = showParen (d > 10) $
      showString constr . showsPrec 11 cmd . showString " " .
      shows its
    where
      constr = case mbErrors of
        Nothing  -> "Cmd "
        Just err -> "Cmd " <> show err <> " "

-- | Return type for successful database operations.
data Success it
  = Unit         ()
  | Blob         (Maybe ByteString)
  | EpochNo        EpochNo
  | Iter         it
  | IterResult   IteratorResult
  | LastBlob     (Maybe SlotNo)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Run the command against the given database.
run :: (HasCallStack, Monad m)
    => (ImmutableDB m -> Corruption -> m (Success (Iterator m)))
       -- ^ How to run a 'Corruption' command.
    -> [Iterator m]
    -> ImmutableDB m
    -> Cmd (Iterator m)
    -> m (Success (Iterator m))
run runCorruption its db cmd = case cmd of
  GetBinaryBlob     s   -> Blob       <$> getBinaryBlob db s
  AppendBinaryBlob  s b -> Unit       <$> appendBinaryBlob db s (testBlockToBuilder b)
  StreamBinaryBlobs s e -> Iter       <$> streamBinaryBlobs db s e
  IteratorNext  it      -> IterResult <$> iteratorNext it
  IteratorClose it      -> Unit       <$> iteratorClose it
  Reopen valPol mbTrunc -> do
    mapM_ iteratorClose its
    closeDB db
    LastBlob <$> reopen db valPol mbTrunc
  Corruption corr       -> do
    mapM_ iteratorClose its
    runCorruption db corr


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
  show resp = "(Resp " <> str <> ")"
    where
      str = case getResp resp of
        Right success                                -> show success
        Left (UserError ue _)                        -> show ue
        Left (UnexpectedError (FileSystemError fse)) -> show (fsErrorType fse)
        Left (UnexpectedError ue)                    ->
          prettyImmutableDBError (UnexpectedError ue)

-- | The monad used to run pure model/mock implementation of the database.
type PureM = ExceptT ImmutableDBError (State DBModel)

-- | The type of the pure model/mock implementation of the database.
type ModelDBPure = ImmutableDB PureM

-- | Run a command against the pure model
runPure :: DBModel
        -> ModelDBPure
        -> CmdErr (Iterator PureM)
        -> (Resp (Iterator PureM), DBModel)
runPure dbm mdb (CmdErr mbErrors cmd its) =
    first Resp $ flip runState dbm $ do
      resp <- runExceptT $ run runCorruption its mdb cmd
      case (mbErrors, resp) of
        -- No simulated errors, just step
        (Nothing, _) -> return resp
        -- Even though an error will be simulated, a user error will be thrown
        -- before the simulated error can be thrown.
        (Just _, Left (UserError {})) -> return resp
        -- An error will be simulated and thrown (not here, but in the real
        -- implementation). To mimic what the implementation will do, we only
        -- have to close the iterators, as the truncation during the reopening
        -- of the database will erase any changes.
        (Just _, _) -> do
          -- Note that we reset the state to the initial state (@dbm@), as we
          -- don't want the result of executing the @cmd@ successfully.
          modify $ const dbm { dbmIterators = mempty }
          gets (Right . LastBlob . getLastBlobLocation)
  where
    runCorruption :: ModelDBPure -> Corruption
                  -> PureM (Success (Iterator PureM))
    runCorruption _ (MkCorruption corrs) = do
      modify $ simulateCorruptions corrs
      gets (LastBlob . getLastBlobLocation)

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
  { dbModel    :: DBModel
    -- ^ A model of the database, used as state for the 'HasImmutableDB'
    -- instance of 'ModelDB'.
  , mockDB     :: ModelDBPure
    -- ^ A handle to the mocked database.
  , knownIters :: KnownIters m r
    -- ^ Store a mapping between iterator references and mocked iterators.
  } deriving (Show, Generic)

-- | Initial model
initModel :: DBModel -> ModelDBPure -> Model m r
initModel dbModel mockDB = Model
    { knownIters  = RE.empty
    , ..
    }

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Functor t, Eq1 r) => Model m r -> At t m r -> t (Iterator PureM)
toMock Model {..} (At t) = fmap (knownIters RE.!) t

-- | Step the mock semantics
step :: Eq1 r
     => Model m r -> At CmdErr m r -> (Resp (Iterator PureM), DBModel)
step model@Model{..} cmdErr = runPure dbModel mockDB (toMock model cmdErr)


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
lockstep model@Model {..} cmdErr (At resp) = Event
    { eventBefore   = model
    , eventCmdErr   = cmdErr
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmdErr
    newIters = RE.fromList $ zip (iters resp) (iters mockResp)
    model' = model
      { dbModel    = dbModel'
      , knownIters = knownIters `RE.union` newIters
      }

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

-- | Generate a 'CmdErr'
generator :: Model m Symbolic -> Gen (At CmdErr m Symbolic)
generator m@Model {..} = do
    _cmd    <- unAt <$> generateCmd m
    _cmdErr <- if noErrorFor _cmd
       then return Nothing
       else frequency
          -- We want to make some progress
          [ (4, return Nothing)
          , (1, Just <$> arbitrary)
          ]
    let _cmdIters = RE.keys knownIters
    return $ At CmdErr {..}
  where
    -- Don't simulate an error during corruption, because we don't want an
    -- error to happen while we corrupt a file.
    noErrorFor Corruption {} = True
    noErrorFor Reopen {}     = True
    noErrorFor _             = False
    -- TODO simulate errors during recovery?


-- | Generate a 'Cmd'.
generateCmd :: Model m Symbolic -> Gen (At Cmd m Symbolic)
generateCmd Model {..} = At <$> frequency
    [ (1, GetBinaryBlob <$> frequency
            [ (if empty then 0 else 10, genSlotInThePast)
            , (1,  genSlotInTheFuture)
            , (1,  arbitrary) ])
    , (3, do
            slot <- arbitrary -- TODO can create many empty epochs
            return $ AppendBinaryBlob slot (TestBlock slot))
    , (1, frequency
            -- An iterator with a random and likely invalid range,
            [ (1, StreamBinaryBlobs <$> (Just <$> arbitrary)
                                    <*> (Just <$> arbitrary))
            -- An iterator that is more likely to be valid.
            , (if empty then 0 else 2, do
                    start <- genSlotInThePast
                    end   <- genSlotInThePast `suchThat` (>= start)
                    return $ StreamBinaryBlobs (Just start) (Just end))
            -- Same as above, but with possible empty bounds
            , (if empty then 0 else 2, do
                    start <- genSlotInThePast
                    end   <- genSlotInThePast `suchThat` (>= start)
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
    , (1, Reopen <$> genValPol <*> genMbTrunc)

    , (if null dbFiles then 0 else 2, Corruption <$> genCorruption)
    ]
  where
    DBModel {..} = dbModel

    empty = dbmNextSlot == 0

    genSlotInThePast
      | empty     = discard
      | otherwise = SlotNo <$> choose @Word64 (0, unSlotNo dbmNextSlot - 1)

    genSlotInTheFuture = SlotNo <$> choose @Word64 (unSlotNo dbmNextSlot, maxBound)

    genCorruption = MkCorruption <$> generateCorruptions (NE.fromList dbFiles)

    dbFiles = getDBFiles dbModel

    genValPol = elements [ValidateMostRecentEpoch, ValidateAllEpochs]

    -- TODO generate slots in the future?
    genMbTrunc
      | empty
      = return Nothing
      | otherwise
      = frequency
        [ (1, return Nothing)
        , (4, Just . TruncateFrom <$> genSlotInThePast)
        ]

-- | Return the files that the database with the given model would have
-- created. For each epoch an index and epoch file, except for the last epoch:
-- only an epoch but no index file.
getDBFiles :: DBModel -> [FsPath]
getDBFiles DBModel {..}
    | null dbmChain
    = []
    | 0 <- lastEpoch
    = lastEpochFiles
    | otherwise
    = lastEpochFiles <> epochsBeforeLastFiles
  where
    lastEpoch = CES.lastEpoch dbmCumulEpochSizes
    lastEpochFiles = [renderFile "epoch" lastEpoch]
    epochsBeforeLastFiles = [0..lastEpoch-1] >>= \epoch ->
      [ renderFile "index" epoch
      , renderFile "epoch" epoch
      ]


{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker :: Model m Symbolic -> At CmdErr m Symbolic -> [At CmdErr m Symbolic]
shrinker m@Model {..} (At (CmdErr mbErrors cmd _)) = fmap At $
    [ CmdErr mbErrors' cmd  its' | mbErrors' <- shrink mbErrors ] ++
    [ CmdErr mbErrors  cmd' its' | At cmd'   <- shrinkCmd m (At cmd) ]
  where
    its' = RE.keys knownIters

-- | Shrink a 'Cmd'.
shrinkCmd :: Model m Symbolic -> At Cmd m Symbolic -> [At Cmd m Symbolic]
shrinkCmd Model {..} (At cmd) = fmap At $ case cmd of
    AppendBinaryBlob slot _ ->
      [ AppendBinaryBlob slot' (TestBlock slot')
      | slot' <- shrink slot ]
    StreamBinaryBlobs mbStart mbEnd ->
      [ StreamBinaryBlobs mbStart' mbEnd
      | mbStart' <- shrinkMbSlot mbStart] ++
      [ StreamBinaryBlobs mbStart  mbEnd'
      | mbEnd'   <- shrinkMbSlot mbEnd]
    GetBinaryBlob slot ->
      [GetBinaryBlob slot' | slot' <- shrink slot]
    IteratorNext  {} -> []
    IteratorClose {} -> []
    Reopen valPol mbTruncateFrom ->
      [ Reopen valPol' mbTruncateFrom
      | valPol' <- shrinkValPol valPol ] ++
      [ Reopen valPol  mbTruncateFrom'
      | mbTruncateFrom' <- shrinkMbTruncateFrom mbTruncateFrom ]
    Corruption corr ->
      [Corruption corr' | corr' <- shrinkCorruption corr]
  where
    shrinkMbSlot :: Maybe SlotNo -> [Maybe SlotNo]
    shrinkMbSlot Nothing  = []
    shrinkMbSlot (Just s) = Nothing : map Just (shrink s)
    shrinkCorruption (MkCorruption corrs) =
      [ MkCorruption corrs'
      | corrs' <- shrinkCorruptions corrs]
    shrinkValPol ValidateAllEpochs = [ValidateMostRecentEpoch]
    shrinkValPol _                 = []
    shrinkMbTruncateFrom Nothing  = []
    shrinkMbTruncateFrom (Just _) = [Nothing]

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
    (resp, _dbm) = step model cmdErr

precondition :: Model m Symbolic -> At CmdErr m Symbolic -> Logic
precondition Model {..} (At (CmdErr { _cmd = cmd })) =
   forall (iters cmd) (`elem` RE.keys knownIters) .&&
    case cmd of
      Corruption corr ->
        forall (corruptionFiles corr) (`elem` getDBFiles dbModel)
      Reopen _ (Just (TruncateFrom slot)) ->
        slot .< dbmNextSlot dbModel
      _ -> Top
  where
    corruptionFiles (MkCorruption corrs) = map snd $ NE.toList corrs

transition :: (Show1 r, Eq1 r)
           => Model m r -> At CmdErr m r -> At Resp m r -> Model m r
transition model cmdErr = eventAfter . lockstep model cmdErr

postcondition :: Model m Concrete
              -> At CmdErr m Concrete
              -> At Resp m Concrete
              -> Logic
postcondition model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

semantics :: TVar IO Errors
          -> HasFS IO h
          -> ImmutableDB IO
          -> At CmdErr IO Concrete
          -> IO (At Resp IO Concrete)
semantics errorsVar hasFS db (At cmdErr) =
    At . fmap (reference . Opaque) . Resp <$> case opaque <$> cmdErr of

      CmdErr Nothing       cmd its -> tryImmDB $
        run (semanticsCorruption hasFS) its db cmd

      CmdErr (Just errors) cmd its -> do
        nextSlot <- getNextSlot db
        res      <- withErrors errorsVar errors $ tryImmDB $
          run (semanticsCorruption hasFS) its db cmd
        case res of
          -- If the command resulted in a 'UserError', we didn't even get the
          -- chance to run into a simulated error. Just return this error.
          Left (UserError {})       -> return res

          -- We encountered a simulated error
          Left (UnexpectedError {}) -> truncateAndReopen nextSlot its

          -- TODO track somewhere which/how many errors were *actually* thrown

          -- By coincidence no error was thrown, try to mimic what would have
          -- happened if the error was thrown, so that we stay in sync with
          -- the model.
          Right suc                 ->
            truncateAndReopen nextSlot (its <> iters suc)
            -- Note that we might have created an iterator, make sure to close
            -- it as well
  where
    truncateAndReopen fromSlot its = tryImmDB $ do
      -- Close all open iterators as we will perform truncation
      mapM_ iteratorClose its
      -- Close the database in case no errors occurred and it wasn't
      -- closed already. This is idempotent anyway.
      closeDB db

      -- TODO validate the database and check that if reopen succeeds, it
      -- happened at some point in the right interval

      -- Reopen the database, but truncate it to the point right before
      -- the error
      let trunc = Just (TruncateFrom fromSlot)
      LastBlob <$> reopen db ValidateMostRecentEpoch trunc

semanticsCorruption :: MonadCatch m
                    => HasFS m h
                    -> ImmutableDB m
                    -> Corruption
                    -> m (Success (Iterator m))
semanticsCorruption hasFS db (MkCorruption corrs) = do
    closeDB db
    forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file

    -- Record the error thrown using the 'ValidateAllEpochs' policy
    validateRes     <- tryImmDB $ reopen db ValidateAllEpochs Nothing
    validationError <- case validateRes of
          Left e  -> return $ Just e
          -- Close the DB again, otherwise the next reopen is a no-op.
          Right _ -> closeDB db $> Nothing

    -- Now try to restore to the last valid epoch slot using the recovery
    -- strategy suggested by the error.
    let mbTruncateFrom = validationError >>= extractTruncateFrom

    LastBlob <$> reopen db ValidateAllEpochs mbTruncateFrom

-- | The state machine proper
sm :: TVar IO Errors
   -> HasFS IO h
   -> ImmutableDB IO
   -> DBModel
   -> ModelDBPure
   -> StateMachine (Model IO) (At CmdErr IO) IO (At Resp IO)
sm errorsVar hasFS db dbm mdb = StateMachine
  { initModel     = initModel dbm mdb
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = Just . generator
  , shrinker      = shrinker
  , semantics     = semantics errorsVar hasFS db
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
            (IteratorExhausted, IteratorExhausted)
              -> return ()
            (IteratorResult {}, IteratorResult {})
              | dbRes == modelRes
              -> loop
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

  | TagAppendToSlotInThePastError

  | TagReadFutureSlotError

  | TagInvalidIteratorRangeError

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

-- | Convenience combinator for creating classifiers for commands for which an
-- error is simulated.
simulatedError :: (Event m Symbolic -> Either Tag (EventPred m))
               -> EventPred m
simulatedError f = C.predicate $ \ev ->
    case (_cmdErr (unAt (eventCmdErr ev)), getResp (eventMockResp ev)) of
      (Just _, Right _) -> f ev
      _                 -> Right $ simulatedError f


-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: forall m. [Event m Symbolic] -> [Tag]
tag = C.classify
    [ tagGetBinaryBlobJust
    , tagGetBinaryBlobNothing
    , tagAppendToSlotInThePastError
    , tagReadFutureSlotError
    , tagInvalidIteratorRangeError
    , tagIteratorStreamedNBlobs Map.empty
    , tagIteratorWithoutBounds
    , tagCorruption
    , tagErrorDuringAppendBinaryBlob
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

    tagAppendToSlotInThePastError :: EventPred m
    tagAppendToSlotInThePastError = failedUserError $ \_ e -> case e of
      AppendToSlotInThePastError {} -> Left TagAppendToSlotInThePastError
      _                             -> Right tagAppendToSlotInThePastError

    tagReadFutureSlotError :: EventPred m
    tagReadFutureSlotError = failedUserError $ \_ e -> case e of
      ReadFutureSlotError {} -> Left TagReadFutureSlotError
      _                      -> Right tagReadFutureSlotError

    tagInvalidIteratorRangeError :: EventPred m
    tagInvalidIteratorRangeError = failedUserError $ \_ e -> case e of
      InvalidIteratorRangeError {} -> Left TagInvalidIteratorRangeError
      _                            -> Right tagInvalidIteratorRangeError

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
    tagErrorDuringAppendBinaryBlob = simulatedError $ \ev -> case eventCmd ev of
      At (AppendBinaryBlob {}) -> Left TagErrorDuringAppendBinaryBlob
      _                        -> Right tagErrorDuringAppendBinaryBlob

    tagErrorDuringGetBinaryBlob :: EventPred m
    tagErrorDuringGetBinaryBlob = simulatedError $ \ev -> case eventCmd ev of
      At (GetBinaryBlob {}) -> Left TagErrorDuringGetBinaryBlob
      _                     -> Right tagErrorDuringGetBinaryBlob

    tagErrorDuringStreamBinaryBlobs :: EventPred m
    tagErrorDuringStreamBinaryBlobs = simulatedError $ \ev -> case eventCmd ev of
      At (StreamBinaryBlobs {}) -> Left TagErrorDuringStreamBinaryBlobs
      _                         -> Right tagErrorDuringStreamBinaryBlobs

    tagErrorDuringIteratorNext :: EventPred m
    tagErrorDuringIteratorNext = simulatedError $ \ev -> case eventCmd ev of
      At (IteratorNext {}) -> Left TagErrorDuringIteratorNext
      _                    -> Right tagErrorDuringIteratorNext

    tagErrorDuringIteratorClose :: EventPred m
    tagErrorDuringIteratorClose = simulatedError $ \ev -> case eventCmd ev of
      At (IteratorClose {}) -> Left TagErrorDuringIteratorClose
      _                     -> Right tagErrorDuringIteratorClose


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
    go _ []       = []
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
  cmdName (At (CmdErr { _cmd = cmd }) ) = constrName cmd
  cmdNames (_ :: Proxy (At CmdErr m r)) =
    constrNames (Proxy @(Cmd (IterRef m r)))

instance Show (Iterator PureM) where
  show it = "<iterator " <> show (iteratorID it) <> ">"

instance Show ModelDBPure where
  show _ = "<ModelDB>"

instance ToExpr SlotNo where
  toExpr (SlotNo w) = App "SlotNo" [toExpr w]

instance ToExpr EpochNo
instance ToExpr EpochSize
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
  -> (   DBModel
      -> ModelDBPure
      -> StateMachine (Model m) (At CmdErr m) m (At Resp m)
     )
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
    smUnused = stateMachine dbm mdb

showLabelledExamples :: IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000 (const True) $
    sm (error "errorsVar unused") hasFsUnused dbUnused

prop_sequential :: Property
prop_sequential = forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
    let test :: TVar IO Errors -> HasFS IO h -> QC.PropertyM IO (
                    QSM.History (At CmdErr IO) (At Resp IO)
                  , Reason
                  , Maybe SlotNo
                  , Maybe SlotNo
                  )
        test errorsVar hasFS = do
          let parser = testBlockEpochFileParser' hasFS
          (db, Nothing) <- QC.run $
            openDB hasFS EH.monadCatch fixedGetEpochSize ValidateMostRecentEpoch
              parser
          let sm' = sm errorsVar hasFS db dbm mdb
          (hist, model, res) <- runCommands sm' cmds
          lastBlobLocation <- QC.run $ do
            closeDB db
            lastBlobLocation <- reopen db ValidateAllEpochs Nothing
            validate model db
            closeDB db
            return lastBlobLocation

          let lastBlobLocationModel = getLastBlobLocation $ dbModel model

          QC.monitor $ counterexample ("lastBlobLocation: "      <> show lastBlobLocation)
          QC.monitor $ counterexample ("lastBlobLocationModel: " <> show lastBlobLocationModel)

          return (hist, res, lastBlobLocationModel, lastBlobLocation)

    fsVar     <- QC.run $ atomically (newTVar Mock.empty)
    errorsVar <- QC.run $ atomically (newTVar mempty)
    (hist, res, lastBlobLocationModel, lastBlobLocation) <-
      test errorsVar (mkSimErrorHasFS EH.monadCatch fsVar errorsVar)
    prettyCommands smUnused hist
      $ tabulate "Tags" (map show $ tag (execCmds (QSM.initModel smUnused) cmds))
      $ res === Ok .&&. lastBlobLocationModel === lastBlobLocation
  where
    (dbm, mdb) = mkDBModel
    smUnused   = sm (error "errorsVar unused") hasFsUnused dbUnused dbm mdb


tests :: TestTree
tests = testGroup "ImmutableDB q-s-m"
    [ testProperty "sequential" prop_sequential
    ]


fixedEpochSize :: EpochSize
fixedEpochSize = 10

fixedGetEpochSize :: Monad m => EpochNo -> m EpochSize
fixedGetEpochSize _ = return fixedEpochSize

mkDBModel :: MonadState DBModel m
          => (DBModel, ImmutableDB (ExceptT ImmutableDBError m))
mkDBModel = openDBModel EH.exceptT (const fixedEpochSize)

dbUnused :: ImmutableDB m
dbUnused = error "semantics and DB used during command generation"

hasFsUnused :: HasFS m h
hasFsUnused = error "HasFS only used during execution"

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

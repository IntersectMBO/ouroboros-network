{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Consensus.ResourceRegistry (
    tests
  ) where

import           Prelude hiding (elem)

import           Control.Monad.Except
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Classes
import           Data.Kind (Type)
import           Data.List (delete, sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.TreeDiff (defaultExprViaShow)
import           Data.Typeable
import           GHC.Generics (Generic, Generic1)
import qualified Generics.SOP as SOP

import           Control.Monad.Class.MonadTimer

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import           Test.StateMachine
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty hiding (after)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Test.Util.QSM
import           Test.Util.SOP

tests :: TestTree
tests = testGroup "ResourceRegistry" [
      testProperty "sequential" prop_sequential
    ]

{-------------------------------------------------------------------------------
  Mock implementaton
-------------------------------------------------------------------------------}

-- | Mock thread IDs record thread pedigree
--
-- > [t]           top-level thread t
-- > [t, t']       child t' of top-level thread t
-- > [t, t', t'']  child t'' of thread t', itself a child of t
--
-- NOTE: All thread IDs will be unique. If both threads 1 and 2 spawn a child,
-- they would be @[1,3]@ and @[2,4]@.
type MockThread = [Int]

-- | Threads and their subthreads
--
-- Once created, threads are never removed from this map. Instead, when they are
-- killed their 'alive' status is set to 'False'.
newtype MockThreads = MTs { mockThreadsMap :: Map Int MockState }
  deriving (Show, Generic)

-- | State of a mock thread
data MockState = MS {
      alive :: Bool
    , kids  :: MockThreads
    }
  deriving (Show, Generic)

-- | All known threads, and whether or not they are alive
--
-- TODO: Perhaps it would be better to have an invariant that in 'MockThreads'
-- threads must be recorded as dead if any of their parents are, rather than
-- computing that here.
mockThreads :: MockThreads -> [(MockThread, Bool)]
mockThreads = go [] True
  where
    go :: [Int] -> Bool -> MockThreads -> [(MockThread, Bool)]
    go prefix parentAlive =
        concatMap aux . Map.toList . mockThreadsMap
      where
        aux :: (Int, MockState) -> [(MockThread, Bool)]
        aux (tid, MS{..}) =
            (t, parentAlive && alive) : go t alive' kids
          where
            t :: [Int]
            t = prefix ++ [tid]

            alive' :: Bool
            alive' = parentAlive && alive

mockLiveThreads :: MockThreads -> [MockThread]
mockLiveThreads = map fst . filter snd . mockThreads

alterThreadF :: forall m. MonadError Err m
             => MockThread
             -> (Maybe MockState -> m MockState)
             -> MockThreads -> m MockThreads
alterThreadF [] _ _ =
    error "alterThreadF: invalid thread"
alterThreadF [t] f (MTs m) =
    MTs <$> Map.alterF (fmap Just . f) t m
alterThreadF thread@(t:ts) f (MTs m) =
    MTs <$> Map.alterF (fmap Just . f') t m
  where
    f' :: Maybe MockState -> m MockState
    f' Nothing   = throwError $ ErrInvalidThread (show thread)
    f' (Just ms) = (\kids' -> ms { kids = kids' }) <$>
                     alterThreadF ts f (kids ms)

-- Create thread with the given ID
mockFork :: MockThread -> MockThreads -> Except Err MockThreads
mockFork t = alterThreadF t $ \case
    Just _  -> error "fork: thread already exists (bug in runMock)"
    Nothing -> return newState
  where
    newState :: MockState
    newState = MS {
          alive = True
        , kids  = MTs Map.empty
        }

mockKill :: MockThread -> MockThreads -> Except Err MockThreads
mockKill t = alterThreadF t $ \case
    Nothing -> throwError $ ErrInvalidThread (show t)
    Just st -> return st { alive = False }

data Mock = Mock {
      nextId  :: Int
    , threads :: MockThreads
    , links   :: Map MockThread (Link MockThread)
    }
  deriving (Show, Generic)

emptyMock :: Mock
emptyMock = Mock {
      nextId  = 1
    , threads = MTs Map.empty
    , links   = Map.empty
    }

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- | Should we link a new thread to its parent?
--
-- If yes, we need some information about that parent.
data Link a = LinkFromParent a | DontLink
  deriving (Show, Functor, Generic)

data Cmd t =
    -- | Fork a new top-level thread
    --
    -- We don't allow linking here, because we don't want an exception in one
    -- of these threads to kill the thread running the tests.
    Fork

    -- | Fork a child thread
  | ForkFrom t (Link ())

    -- | Cause a thread to terminate normally
  | Terminate t

    -- | Cause a thread to terminate abnormally
  | Crash t

    -- | Get all live threads
  | LiveThreads
  deriving (Show, Functor, Foldable, Traversable, Generic)

data Success t =
    Unit ()
  | Spawned t
  | Threads [t]
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Err =
    ErrTimeout
  | ErrInvalidThread String
  deriving (Show, Eq)

newtype Resp t = Resp (Either Err (Success t))
  deriving (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  "Up-to" comparison for responses

  This is used in 'postcondition'.
-------------------------------------------------------------------------------}

normalize :: Resp MockThread -> Resp MockThread
normalize (Resp r) = Resp $ aux <$> r
  where
    aux :: Success MockThread -> Success MockThread
    aux (Unit ())    = Unit ()
    aux (Spawned t)  = Spawned t
    aux (Threads ts) = Threads (sort ts)

{-------------------------------------------------------------------------------
  Run against the mock implementation
-------------------------------------------------------------------------------}

runMock :: Cmd MockThread -> Mock -> (Resp MockThread, Mock)
runMock cmd m@Mock{..} =
    case runExcept (go cmd) of
      Left  err           -> (Resp (Left err), m)
      Right (success, m') -> (Resp (Right success), m')
  where
    go :: Cmd MockThread -> Except Err (Success MockThread, Mock)
    go Fork                = createThread DontLink           [nextId]
    go (ForkFrom t linked) = createThread (const t <$> linked) (t ++ [nextId])
    go (Terminate t)       = (\x -> (Unit (), m { threads = x })) <$> mockKill t threads
    go (Crash t)           = (\x -> (Unit (), m { threads = x })) <$> killAll  t threads
    go LiveThreads         = return (Threads $ mockLiveThreads threads, m)

    createThread :: Link MockThread -- Thread to link to (if any)
                 -> MockThread -> Except Err (Success MockThread, Mock)
    createThread shouldLink t = do
        threads' <- mockFork t threads
        return (
            Spawned t
          , m { nextId  = succ nextId
              , threads = threads'
              , links   = Map.insert t shouldLink links
              }
          )

    killAll :: MockThread -> MockThreads -> Except Err MockThreads
    killAll t =
        mockKill t >=> killParent (Map.findWithDefault DontLink t links)
      where
        killParent :: Link MockThread -> MockThreads -> Except Err MockThreads
        killParent DontLink            = return
        killParent (LinkFromParent t') = killAll t'

{-------------------------------------------------------------------------------
  Run in IO (possibly simulated)
-------------------------------------------------------------------------------}

data TestThread m = TestThread {
      -- | The underlying 'Thread'
      testThread   :: Thread m ()

      -- | Parent thread this thread is linked to (if any)
    , threadLinked :: Link (TestThread m)

      -- | Send the thread instructions (see 'ThreadInstr')
    , threadComms  :: TQueue m (QueuedInstr m)
    }

-- | Instructions to a thread
--
-- The type argument indicates the result of the instruction
data ThreadInstr m :: Type -> Type where
  -- | Have the thread spawn a child thread
  ThreadFork :: Link () -> ThreadInstr m (TestThread m)

  -- | Have the thread terminate normally
  ThreadTerminate :: ThreadInstr m ()

  -- | Raise an exception in the thread
  ThreadCrash :: ThreadInstr m ()

-- | Instruction along with an MVar for the result
data QueuedInstr m = forall a. QueuedInstr (ThreadInstr m a) (StrictMVar m a)

runInThread :: IOLike m => TestThread m -> ThreadInstr m a -> m a
runInThread TestThread{..} instr = do
    result <- uncheckedNewEmptyMVar (error "no result yet")
    atomically $ writeTQueue threadComms (QueuedInstr instr result)
    takeMVar result

instance (IOLike m) => Show (TestThread m) where
  show TestThread{..} = "<Thread " ++ show (threadId testThread) ++ ">"

instance (IOLike m) => Eq (TestThread m) where
  (==) = (==) `on` (threadId . testThread)

-- | Create a new thread in the given registry
--
-- In order to be able to see which threads are alive, we have threads
-- register and unregister themselves. We do not reuse the registry for this,
-- to avoid circular reasoning in the tests.
newThread :: forall m. IOLike m
          => StrictTVar m [TestThread m]
          -> ResourceRegistry m
          -> Link (TestThread m)
          -> m (TestThread m)
newThread alive parentReg = \shouldLink -> do
    comms      <- atomically $ newTQueue
    spawned    <- uncheckedNewEmptyMVar (error "no thread spawned yet")

    thread <- forkThread parentReg "newThread" $
                withRegistry $ \childReg ->
                  threadBody childReg spawned comms
    case shouldLink of
      LinkFromParent _ -> linkToRegistry thread
      DontLink         -> return ()

    let testThread :: TestThread m
        testThread = TestThread {
                         testThread   = thread
                       , threadLinked = shouldLink
                       , threadComms  = comms
                       }

    -- Make sure to register thread before starting it
    atomically $ modifyTVar alive (testThread:)
    putMVar spawned testThread
    return testThread
  where
    threadBody :: ResourceRegistry m
               -> StrictMVar m (TestThread m)
               -> TQueue m (QueuedInstr m)
               -> m ()
    threadBody childReg spawned comms = do
        us <- readMVar spawned
        loop us `finally` (atomically $ modifyTVar alive (delete us))
      where
        loop :: TestThread m -> m ()
        loop us = do
          QueuedInstr instr result <- atomically $ readTQueue comms
          case instr of
            ThreadFork linked -> do
              child <- newThread alive childReg (const us <$> linked)
              putMVar result child
              loop us
            ThreadTerminate -> do
              putMVar result ()
            ThreadCrash -> do
              putMVar result ()
              error "crashing"

runIO :: forall m. (IOLike m, MonadTimer m)
      => StrictTVar m [TestThread m]
      -> ResourceRegistry m
      -> Cmd (TestThread m) -> m (Resp (TestThread m))
runIO alive reg cmd = catchEx $ timeout 1 $
    case cmd of
      Fork ->
        Spawned <$> newThread alive reg DontLink
      ForkFrom thread shouldLink -> do
        Spawned <$> runInThread thread (ThreadFork shouldLink)
      Terminate thread -> do
        runInThread thread ThreadTerminate
        Unit <$> waitForTermination thread
      Crash thread -> do
        runInThread thread ThreadCrash
        Unit <$> waitForTermination thread
      LiveThreads ->
        atomically $ Threads <$> readTVar alive
  where
    catchEx :: m (Maybe (Success a)) -> m (Resp a)
    catchEx = fmap (Resp . maybe (Left ErrTimeout) Right)

    -- For the thread and all of its linked parents to have terminated
    waitForTermination :: TestThread m -> m ()
    waitForTermination t = do
        result <- try $ waitThread (testThread t)
        case (result, threadLinked t) of
          (Left (_ :: SomeException), LinkFromParent t') ->
            waitForTermination t'
          _otherwise ->
            return ()

{-------------------------------------------------------------------------------
  QSM wrappers
-------------------------------------------------------------------------------}

newtype At m f r = At (f (Reference (TestThread m) r))

deriving instance (Show1 r, IOLike m) => Show (At m Cmd  r)
deriving instance (Show1 r, IOLike m) => Show (At m Resp r)

{-------------------------------------------------------------------------------
  Relate model to IO
-------------------------------------------------------------------------------}

-- TODO: Use RefEnv?
type Refs m r = [(Reference (TestThread m) r, MockThread)]

(!) :: (Eq k, Show k) => [(k, a)] -> k -> a
env ! r = case lookup r env of
            Just a  -> a
            Nothing -> error $ "Unknown reference: " ++ show r

data Model m r = Model Mock (Refs m r)
  deriving (Show, Generic)

initModel :: Model m r
initModel = Model emptyMock []

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

toMock :: forall m f r. (Functor f, Eq1 r, Show1 r, IOLike m)
       => Model m r -> At m f r -> f MockThread
toMock (Model _ hs) (At fr) = (hs !) <$> fr

step :: (Eq1 r, Show1 r, IOLike m)
     => Model m r -> At m Cmd r -> (Resp MockThread, Mock)
step m@(Model mock _) c = runMock (toMock m c) mock

data Event m r = Event {
      before   :: Model  m     r
    , cmd      :: At     m Cmd r
    , after    :: Model  m     r
    , mockResp :: Resp MockThread
    }

lockstep :: (Eq1 r, Show1 r, IOLike m)
         => Model m      r
         -> At    m Cmd  r
         -> At    m Resp r
         -> Event m      r
lockstep m@(Model _ hs) c (At resp) = Event {
      before   = m
    , cmd      = c
    , after    = Model mock' (hs <> hs')
    , mockResp = resp'
    }
  where
    (resp', mock') = step m c
    hs' = zip (newHandles resp) (newHandles resp')

    newHandles :: Resp r -> [r]
    newHandles (Resp (Left _))            = []
    newHandles (Resp (Right (Unit ())))   = []
    newHandles (Resp (Right (Spawned t))) = [t]
    newHandles (Resp (Right (Threads _))) = []

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

generator :: forall m. Model m Symbolic -> Maybe (Gen (At m Cmd Symbolic))
generator (Model _ hs) = Just $ QC.oneof $ concat [
      withoutHandle
    , if null hs then [] else withHandle (QC.elements (map fst hs))
    ]
  where
    withoutHandle :: [Gen (At m Cmd Symbolic)]
    withoutHandle = [
          fmap At $ return Fork
        , fmap At $ return LiveThreads
        ]

    withHandle :: Gen (Reference (TestThread m) Symbolic)
               -> [Gen (At m Cmd Symbolic)]
    withHandle pickThread = [
          fmap At $ Terminate <$> pickThread
        , fmap At $ Crash     <$> pickThread
        , fmap At $ ForkFrom  <$> pickThread <*> genLink
        ]

    genLink :: Gen (Link ())
    genLink = aux <$> QC.arbitrary
      where
        aux :: Bool -> Link ()
        aux True  = LinkFromParent ()
        aux False = DontLink

shrinker :: Model m Symbolic -> At m Cmd Symbolic -> [At m Cmd Symbolic]
shrinker _ _ = []

{-------------------------------------------------------------------------------
  QSM required instances
-------------------------------------------------------------------------------}

instance SOP.Generic         (Cmd t)
instance SOP.HasDatatypeInfo (Cmd t)

deriving instance Generic1 (At m Cmd)
deriving instance Generic1 (At m Resp)

instance CommandNames (At m Cmd) where
  cmdName  (At cmd) = constrName cmd
  cmdNames _        = constrNames (Proxy @(Cmd ()))

instance Rank2.Foldable    (At m Cmd)
instance Rank2.Functor     (At m Cmd)
instance Rank2.Traversable (At m Cmd)

instance Rank2.Foldable (At m Resp)

instance ToExpr MockState
instance ToExpr MockThreads
instance ToExpr Mock
instance ToExpr (Link MockThread)
instance ToExpr (Model IO Concrete)

instance (IOLike m) => ToExpr (TestThread m) where
  toExpr = defaultExprViaShow

{-------------------------------------------------------------------------------
  QSM toplevel
-------------------------------------------------------------------------------}

semantics :: (IOLike m, MonadTimer m, Typeable m)
          => StrictTVar m [TestThread m]
          -> ResourceRegistry m
          -> At m Cmd Concrete -> m (At m Resp Concrete)
semantics alive reg (At c) =
    (At . fmap reference) <$>
      runIO alive reg (concrete <$> c)

transition :: (Eq1 r, Show1 r, IOLike m)
           => Model m r -> At m Cmd r -> At m Resp r -> Model m r
transition m c = after . lockstep m c

precondition :: forall m. (IOLike m)
             => Model m Symbolic -> At m Cmd Symbolic -> Logic
precondition (Model mock hs) (At c) =
    forall (toList c) checkRef
  where
    checkRef :: Reference (TestThread m) Symbolic -> Logic
    checkRef r =
        case lookup r hs of
          Nothing -> Bot
          Just r' -> r' `member` mockLiveThreads (threads mock)

postcondition :: (IOLike m)
              => Model m      Concrete
              -> At    m Cmd  Concrete
              -> At    m Resp Concrete
              -> Logic
postcondition m c r =
    normalize (toMock (after e) r) .== normalize (mockResp e)
  where
    e = lockstep m c r

symbolicResp :: (IOLike m, Typeable m)
             => Model m     Symbolic
             -> At    m Cmd Symbolic
             -> GenSym (At m Resp Symbolic)
symbolicResp m c = At <$> traverse (const genSym) resp
  where
    (resp, _mock') = step m c

sm :: (IOLike m, MonadTimer m, Typeable m)
   => StrictTVar m [TestThread m]
   -> ResourceRegistry m
   -> StateMachine (Model m) (At m Cmd) m (At m Resp)
sm alive reg = StateMachine {
      initModel     = initModel
    , transition    = transition
    , precondition  = precondition
    , postcondition = postcondition
    , invariant     = Nothing
    , generator     = generator
    , shrinker      = shrinker
    , semantics     = semantics alive reg
    , mock          = symbolicResp
    , cleanup       = noCleanup
    }

prop_sequential :: QC.Property
prop_sequential = forAllCommands (sm unused unused) Nothing prop_sequential'

prop_sequential' :: QSM.Commands (At IO Cmd) (At IO Resp) -> QC.Property
prop_sequential' cmds = QC.monadicIO $ do
    alive <- liftIO $ uncheckedNewTVarM []
    reg   <- liftIO $ unsafeNewRegistry
    let sm' = sm alive reg
    (hist, _model, res) <- runCommands sm' cmds
    prettyCommands sm' hist
      $ checkCommandNames cmds
      $ res QC.=== Ok

unused :: a
unused = error "not used during command generation"

{-------------------------------------------------------------------------------
  For running things from ghci
-------------------------------------------------------------------------------}

_forkCount :: QSM.Commands (At IO Cmd) (At IO Resp)
_forkCount = example (sm unused unused) $ do
    run' $ At $ Fork
    run' $ At $ LiveThreads

_forkKillCount :: QSM.Commands (At IO Cmd) (At IO Resp)
_forkKillCount = example (sm unused unused) $ do
    [t] <- run $ At $ Fork
    run' $ At $ Terminate t
    run' $ At $ LiveThreads

_forkFromKillCount :: QSM.Commands (At IO Cmd) (At IO Resp)
_forkFromKillCount = example (sm unused unused) $ do
    [t] <- run $ At $ Fork
    run' $ At $ ForkFrom t DontLink
    run' $ At $ Terminate t
    run' $ At $ LiveThreads

_invalidForkFrom :: QSM.Commands (At IO Cmd) (At IO Resp)
_invalidForkFrom = example (sm unused unused) $ do
    [t] <- run $ At $ Fork
    run' $ At $ Terminate t
    run' $ At $ ForkFrom t DontLink

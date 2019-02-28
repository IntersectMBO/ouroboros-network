{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.VolatileDB.StateMachine (tests) where

import           Prelude

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor (first)
import qualified Data.Binary as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import           Data.Functor.Classes
import           Data.Kind (Type)
import           Data.TreeDiff (ToExpr)
import           Data.TreeDiff.Class
import           GHC.Generics
import           GHC.Stack
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Storage.FS.API
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.Impl (openDB)

import           Test.Ouroboros.Storage.Util
import           Test.Ouroboros.Storage.VolatileDB.Model

newtype At t (r :: Type -> Type) = At t
  deriving (Generic)

-- | Alias for 'At'
type (:@) t r = At t r

data Success
  = Unit ()
  | Blob (Maybe ByteString)
  | Bl   Bool
  deriving (Eq, Show)

newtype Resp = Resp {getResp :: Either (VolatileDBError MyBlockId) Success}
    deriving (Show, Eq)

data Cmd
    = IsOpen
    | Close
    | ReOpen
    | GetBlock MyBlockId
    | PutBlock MyBlockId
    | GarbageCollect MyBlockId
    deriving (Show)

deriving instance Generic1          (At Cmd)
deriving instance Rank2.Foldable    (At Cmd)
deriving instance Rank2.Functor     (At Cmd)
deriving instance Rank2.Traversable (At Cmd)
deriving instance Show1 r => Show   (Cmd :@ r)

deriving instance Generic1        (At Resp)
deriving instance Rank2.Foldable  (At Resp)
deriving instance Show1 r => Show (Resp :@ r)

instance Show (Model r) where
    show = show . toShow


deriving instance Show (ModelShow r)
deriving instance Generic (DBModel MyBlockId)
deriving instance Generic (ModelShow r)
deriving instance Generic Slot
deriving instance ToExpr Slot
deriving instance ToExpr (DBModel MyBlockId)
deriving instance ToExpr (ModelShow r)


instance ToExpr (Model r) where
    toExpr = toExpr . toShow

instance CommandNames (At Cmd) where
    cmdName (At cmd) = case cmd of
        GetBlock _       -> "GetBlock"
        PutBlock _       -> "PutBlock"
        GarbageCollect _ -> "GarbageCollect"
        IsOpen           -> "IsOpen"
        Close            -> "Close"
        ReOpen           -> "ReOpen"
    cmdNames _ = ["not", "suported", "yet"]

data Model (r :: Type -> Type) = Model
  { dbModel :: DBModel MyBlockId
    -- ^ A model of the database, used as state for the 'HasImmutableDB'
    -- instance of 'ModelDB'.
  , mockDB  :: ModelDBPure
    -- ^ A handle to the mocked database.
  } deriving (Generic)

data ModelShow (r :: Type -> Type) = Model'
  { msdbModel        :: DBModel MyBlockId
  }

toShow :: Model r -> ModelShow r
toShow (Model dbm _) = Model' dbm

type PureM = ExceptT (VolatileDBError MyBlockId) (State (DBModel MyBlockId))
type ModelDBPure = VolatileDB MyBlockId PureM

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.

data Event r = Event
  { eventBefore   :: Model  r
  , eventCmd      :: At Cmd r
  , eventAfter    :: Model  r
  , eventMockResp :: Resp
  } deriving (Show)

lockstep :: forall r.
            Model   r
         -> At Cmd  r
         -> At Resp r
         -> Event   r
lockstep model@Model {..} (At cmd) (At _resp) = Event
    { eventBefore   = model
    , eventCmd      = At cmd
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model (At cmd)
    model' = model {dbModel = dbModel'}

-- | Key property of the model is that we can go from real to mock responses
toMock :: Model r -> At t r -> t
toMock _ (At t) = t

step :: Model r -> At Cmd r -> (Resp, DBModel MyBlockId)
step model@Model{..} cmd = runPure dbModel mockDB (toMock model cmd)

runPure :: DBModel MyBlockId
        -> ModelDBPure
        -> Cmd
        -> (Resp, DBModel MyBlockId)
runPure dbm mdb cmd =
    first Resp $ runState (runExceptT $ runDB mdb cmd) dbm

runDB :: (HasCallStack, Monad m)
      => VolatileDB MyBlockId m
      -> Cmd
      -> m Success
runDB db cmd = case cmd of
    GetBlock bid -> Blob <$> getBlock db bid
    PutBlock bid -> Unit <$> putBlock db bid (BS.lazyByteString $ Binary.encode $ toBlock bid)
    GarbageCollect bid -> Unit <$> garbageCollect db (toSlot bid)
    IsOpen -> Bl <$> isOpenDB db
    Close -> Unit <$> closeDB db
    ReOpen -> Unit <$> reOpenDB db


sm :: MonadCatch m => VolatileDB MyBlockId m -> DBModel MyBlockId -> ModelDBPure -> StateMachine Model (At Cmd) m (At Resp)
sm db dbm vdb = StateMachine {
        initModel     = initModelImpl dbm vdb
      , transition    = transitionImpl
      , precondition  = preconditionImpl
      , postcondition = postconditionImpl
      , generator     = generatorImpl
      , shrinker      = shrinkerImpl
      , semantics     = semanticsImpl db
      , mock          = mockImpl
      , invariant     = Nothing
      , distribution  = Nothing
    }

initModelImpl :: DBModel MyBlockId -> ModelDBPure -> Model r
initModelImpl dbm vdm = Model {
      dbModel = dbm
    , mockDB = vdm
    }

transitionImpl :: Model r -> At Cmd r -> At Resp r -> Model r
transitionImpl model cmd = eventAfter . lockstep model cmd

preconditionImpl :: Model Symbolic -> At Cmd Symbolic -> Logic
preconditionImpl m@Model {..} (At cmd) =
    Not (knownLimitation m (At cmd))

postconditionImpl :: Model Concrete -> At Cmd Concrete -> At Resp Concrete -> Logic
postconditionImpl model cmd resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmd resp

generatorImpl :: Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorImpl Model {..} = Just $ At <$> do
    sl <- arbitrary
    frequency
        [ (3, return $ GetBlock sl)
        , (3, return $ PutBlock sl)
        , (1, return $ GarbageCollect sl)
        , (1, return $ IsOpen)
        , (1, return $ Close)
        , (1, return $ ReOpen)
        ]

shrinkerImpl :: Model Symbolic -> At Cmd Symbolic -> [At Cmd Symbolic]
shrinkerImpl _ _ = []

semanticsImpl :: MonadCatch m => VolatileDB MyBlockId m -> At Cmd Concrete -> m (At Resp Concrete)
semanticsImpl m (At cmd) = At . Resp <$> tryVolDB (runDB m cmd)

mockImpl :: Model Symbolic -> At Cmd Symbolic -> GenSym (At Resp Symbolic)
mockImpl model cmd = At <$> return mockResp
    where
        (mockResp, _dbModel') = step model cmd

knownLimitation :: Model Symbolic -> Cmd :@ Symbolic -> Logic
knownLimitation model (At cmd) = case cmd of
    GetBlock bid -> isLimitation (latestGarbaged $ dbModel model) (toSlot bid)
    PutBlock bid -> isLimitation (latestGarbaged $ dbModel model) (toSlot bid)
    GarbageCollect _sl -> Bot
    IsOpen -> Bot
    Close -> Bot
    ReOpen -> Bot
    where
        isLimitation :: (Show slot, Ord slot) => Maybe slot -> slot -> Logic
        isLimitation Nothing _sl       = Bot
        isLimitation (Just slot') slot = slot' .>  slot

mkDBModel :: MonadState (DBModel MyBlockId) m => (DBModel MyBlockId, VolatileDB MyBlockId (ExceptT (VolatileDBError MyBlockId) m))
mkDBModel = openDBModel EH.exceptT

prop_sequential :: Property
prop_sequential =
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: HasFS IO h -> PropertyM IO (History (At Cmd) (At Resp), Reason)
            test hasFS = do
              db <- run $ openDB hasFS EH.monadCatch ["test-volatile"] myParser 7 toSlot
              let sm' = sm db dbm vdb
              (hist, _model, res) <- runCommands sm' cmds
              run $ closeDB db
              return (hist, res)

        fsVar <- run $ atomically (newTVar Mock.empty)
        (hist, res) <- test (simHasFS EH.monadCatch fsVar)
        prettyCommands smUnused hist $
            checkCommandNames cmds (res === Ok)
    where
        (dbm, vdb) = mkDBModel
        smUnused = sm (error "semantics and DB used during command generation") dbm vdb

tests :: TestTree
tests = testGroup "Volatile" [
        testProperty "q-s-m" $ prop_sequential
    ]

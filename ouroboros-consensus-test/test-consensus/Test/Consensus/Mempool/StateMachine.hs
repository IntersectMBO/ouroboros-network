{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Consensus.Mempool.StateMachine (stateMachine) where

import           Data.Functor.Classes (Eq1, Show1)
import           Data.Kind (Type)

import           Data.TreeDiff.Class (ToExpr)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Monadic as QCM
import           Test.StateMachine (Reference, StateMachine (StateMachine))
import           Test.StateMachine.ConstructorName
                     (CommandNames (cmdName, cmdNames))
import           Test.StateMachine.Logic (Logic (Top), (.==))
import           Test.StateMachine.Sequential (checkCommandNames,
                     forAllCommands, prettyCommands, runCommands)
import           Test.StateMachine.Types (GenSym, Reason (Ok), Symbolic,
                     concrete, genSym, noCleanup, reference)
import qualified Test.StateMachine.Types as SMT
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.StateMachine.Types.References (Concrete)

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

data Mock = Mock
    deriving (Eq, Show, Generic, ToExpr)

-- | A model of the mempool.
--
-- Mock functions operate on this model.
data Model (r :: Type -> Type) = Model
    { modelMock :: Mock }
    deriving (Eq, Show, Generic)

initModel :: Model r
initModel = Model
    { modelMock = Mock }

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data Cmd r =
    TryAddTxs -- TODO should this be parametrized over arbitrary transactions, or
              -- be [GenTx blk]? Using a mock transaction seems to correspond to
              -- the idea of using mock handles. However this will require an
              -- elaboration function from mock transactions to GenTx blk.
              --
              -- We might want to use GenTx blk as we can use the transaction
              -- application function. And we can also generalize over blocks
              -- and run the mempool tests for different eras. I don't know if
              -- testing the mempool for different eras makes sense though.
              --
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Successful command responses
data SuccessfulResponse tx ref =
    TryAddTxsSuccess (TryAddTxsResult tx)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data TryAddTxsResult tx = TryAddTxsResult {
      valid       :: [tx]
    , invalid     :: [tx]
    , unprocessed :: [tx]
    }
  deriving (Show, Eq)

newtype Resp tx ref = Resp (SuccessfulResponse tx ref)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- At the moment we do not need references to data returned by the mempool operations.
newtype At f r = At (f (Reference ActualRef r))

deriving instance Show (f (Reference ActualRef r)) => Show (At f r)

-- | TODO: for now we use a mock reference as a placeholder for mock references
-- that might be used in the tests.
data MockRef = MockRef
  deriving (Eq, Show)

-- | Placeholder for actual references, ie, references returned by the mempool.
data ActualRef = ActualRef
  deriving (Eq, Show)

runMock :: Cmd MockRef -> Mock -> (Resp tx MockRef, Mock)
runMock TryAddTxs Mock = (result, Mock)
  where
    result = Resp $ TryAddTxsSuccess TryAddTxsResult
      { valid       = [] -- TODO
      , invalid     = [] -- TODO
      , unprocessed = [] -- TODO
      }

{-------------------------------------------------------------------------------
  Command generation
-------------------------------------------------------------------------------}

generator :: Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generator _ = Just $ fmap At $ pure TryAddTxs -- TODO

shrinker :: Model Symbolic -> At Cmd Symbolic -> [At Cmd Symbolic]
shrinker _m (At _c) = [] -- TODO

{-------------------------------------------------------------------------------
  State machine
-------------------------------------------------------------------------------}

-- | Mapping from reference commands or responses to mock commands or responses.
-- In these tests @f@ will be Either 'Cmd' or 'Resp'.
toMock :: (Functor f, Eq1 r) => Model r -> At f r -> f MockRef
toMock _m (At fr) = const MockRef <$> fr
  -- TODO at the moment the mock and actual references are isomorphic to ().
  -- There is nothing to do at the moment.

step :: Eq1 r => Model r -> At Cmd r -> (Resp tx MockRef, Mock)
step m c = runMock (toMock m c) (modelMock m)

-- https://github.com/advancedtelematic/quickcheck-state-machine/blob/3cbf466db302235afdea91dac06a57b8fe0f0b4d/src/Test/StateMachine/Types.hs#L62

data Event tx r = Event {
      before   :: Model  r
    , command  :: At Cmd r
    , mockResp :: Resp tx MockRef
    , after    :: Model r
    }

deriving instance (Show1 r, Show tx, Show (Model r)) => Show (Event tx r)

lockstep ::
     Eq1 r
  => Model r
  -> At Cmd r
  -> At (Resp tx) r
  -> Event tx r
lockstep m c (At _resp) = Event {
  -- If we would be linking the response referring to 'r' with the mock
  -- response, we would use '_resp' here.
      before = m
    , command = c
    , mockResp = resp'
    , after = Model mock'
    }
  where
    (resp', mock') = step m c

transition :: Eq1 r => Model r -> At Cmd r -> At (Resp tx) r -> Model r
transition m c = after . lockstep m c

symbolicSemantics ::
     Model Symbolic
  -> At Cmd Symbolic
  -> GenSym (At (Resp tx) Symbolic)
symbolicSemantics m c = At <$> traverse (const genSym) resp
  where
    (resp, _mock') = step m c

-- | TODO Here is where we call the actual mempool commands
semantics :: forall tx . At Cmd Concrete -> IO (At (Resp tx) Concrete)
semantics (At c) = (At . fmap reference) <$> runIO (concrete <$> c)
  where
    runIO :: Cmd ActualRef -> IO (Resp tx ActualRef)
    runIO (TryAddTxs) = pure result
      where
        result = Resp $ TryAddTxsSuccess TryAddTxsResult
          { valid       = [] -- TODO
          , invalid     = [] -- TODO
          , unprocessed = [] -- TODO
          }

precondition :: Model Symbolic -> At Cmd Symbolic -> Logic
precondition _m (At _c) = Top

postcondition ::
     (Eq tx, Show tx)
  => Model        Concrete
  -> At Cmd       Concrete
  -> At (Resp tx) Concrete
  -> Logic
postcondition m c r = toMock (after e) r .== mockResp e
  where
    e = lockstep m c r

stateMachine :: (Eq tx, Show tx) => StateMachine Model (At Cmd) IO (At (Resp tx))
stateMachine = StateMachine
  { SMT.initModel     = initModel
  , SMT.transition    = transition
  , SMT.precondition  = precondition
  , SMT.postcondition = postcondition
  , SMT.generator     = generator
  , SMT.shrinker      = shrinker
  , SMT.semantics     = semantics
  , SMT.mock          = symbolicSemantics
  , SMT.invariant     = Nothing
  , SMT.cleanup       = noCleanup
  }

prop_sequential :: QC.Property
prop_sequential =
    forAllCommands (stateMachine @()) Nothing $ \cmds ->
      QCM.monadicIO $ do
        (hist, _model, res) <- runCommands (stateMachine @()) cmds
        prettyCommands stateMachine hist
          $ checkCommandNames cmds
          $ res QC.=== Ok

{-------------------------------------------------------------------------------
  Instances required to run the state machine
-------------------------------------------------------------------------------}

instance Functor f => Rank2.Functor (At f) where
  fmap = \f (At x) -> At $ fmap (lift f) x
    where
    lift :: (r x -> r' x) -> SMT.Reference x r -> SMT.Reference x r'
    lift f (SMT.Reference x) = SMT.Reference (f x)

instance Foldable f => Rank2.Foldable (At f) where
  foldMap = \f (At x) -> foldMap (lift f) x
    where
      lift :: (r x -> m) -> SMT.Reference x r -> m
      lift f (SMT.Reference x) = f x

instance Traversable t => Rank2.Traversable (At t) where
  traverse = \f (At x) -> At <$> traverse (lift f) x
    where
      lift :: Functor f
           => (r x -> f (r' x)) -> SMT.Reference x r -> f (SMT.Reference x r')
      lift f (SMT.Reference x) = SMT.Reference <$> f x

instance CommandNames (At Cmd) where
  cmdName  (At TryAddTxs{}) = "TryAddTxs"

  cmdNames _ = ["TryAddTxs"]

instance ToExpr (Model Concrete)

{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Consensus.Mempool.StateMachine (
    prop_sequential
  , stateMachine
  ) where

import           Data.Functor.Classes (Eq1, Show1)
import           Data.Kind (Type)

import           Data.TreeDiff.Class (ToExpr)
import           GHC.Generics (Generic, Generic1)
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (DoNotIntervene))
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

-- SUT
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Text.Read (Lexeme (Char))

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

-- | A model of the mempool.
--
-- Mock functions operate on this model.
data Model (r :: Type -> Type) = Model
    deriving (Eq, Show, Generic)

initModel :: Model r
initModel = Model

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- We call the commands or actions of the state machine "events": this captures
-- the fact that the change of ledger state at the tip of the chain is an event
-- (external to the mempool).
--
-- The mempool is generalized over blocks, so there is not much we can do about
-- it.
data Event blk (r :: Type -> Type) = TryAddTxs [GenTx blk]
  deriving stock (Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving stock instance Show (GenTx blk) => Show (Event blk r)

-- | Successful command responses
data SuccessfulResponse blk (ref :: Type -> Type) =
    TryAddTxsSuccess (TryAddTxsResult blk)
  deriving stock Generic1
  deriving anyclass Rank2.Foldable

deriving stock instance (Show (GenTx blk)) => Show (SuccessfulResponse blk ref)

data TryAddTxsResult blk = TryAddTxsResult {
      valid       :: [GenTx blk]
    , invalid     :: [GenTx blk]
    , unprocessed :: [GenTx blk]
    }

deriving stock instance Show (GenTx blk) => Show (TryAddTxsResult blk)


newtype Response blk ref = Response (SuccessfulResponse blk ref)
  deriving stock Generic1
  deriving anyclass Rank2.Foldable

deriving stock instance (Show (GenTx blk), Show (SuccessfulResponse blk ref)) => Show (Response blk ref)


-- | TODO: for now we use a mock reference as a placeholder for mock references
-- that might be used in the tests.
data MockRef = MockRef
  deriving (Eq, Show)

-- | Placeholder for actual references, ie, references returned by the mempool.
data ActualRef = ActualRef
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Command generation
-------------------------------------------------------------------------------}

generator ::
     QC.Arbitrary (GenTx blk)
  => Model Symbolic -> Maybe (Gen (Event blk Symbolic))
generator _ = Just $ fmap TryAddTxs QC.arbitrary

shrinker :: Model Symbolic -> Event blk Symbolic -> [Event blk Symbolic]
shrinker _model _event = [] -- TODO

{-------------------------------------------------------------------------------
  State machine
-------------------------------------------------------------------------------}

transition :: Model r -> Event blk r -> (Response blk) r -> Model r
transition _model _event _response = Model

-- | TODO Here is where we call the actual mempool commands
semantics :: forall blk idx . Mempool IO blk idx -> Event blk Concrete -> IO ((Response blk) Concrete)
semantics mempool (TryAddTxs txs) = do
  (processed, toProcess) <- tryAddTxs mempool DoNotIntervene txs -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
  pure $ Response
       $ TryAddTxsSuccess
         TryAddTxsResult
            { valid       = [] -- TODO
            , invalid     = [] -- TODO
            , unprocessed = [] -- TODO
            }

precondition :: Model Symbolic -> Event blk Symbolic -> Logic
precondition _model _event = Top

postcondition ::
     (Eq tx, Show tx)
  => Model        Concrete
  -> Event    blk Concrete
  -> Response tx  Concrete
  -> Logic
postcondition _model _event _response = Top

mock  :: forall blk tx . Model Symbolic -> Event blk Symbolic -> GenSym (Response tx Symbolic)
mock _model _event = pure
                     $ Response
                     $ TryAddTxsSuccess
                       TryAddTxsResult
                       { valid       = [] -- TODO
                       , invalid     = [] -- TODO
                       , unprocessed = [] -- TODO
                       }

stateMachine ::
     (Eq blk, Show blk, QC.Arbitrary (GenTx blk))
  => Mempool IO blk idx -> StateMachine Model (Event blk) IO (Response blk)
stateMachine mempool = StateMachine
  { SMT.initModel     = initModel
  , SMT.transition    = transition
  , SMT.precondition  = precondition
  , SMT.postcondition = postcondition
  , SMT.generator     = generator
  , SMT.shrinker      = shrinker
  , SMT.semantics     = semantics mempool
  , SMT.mock          = mock
  , SMT.invariant     = Nothing
  , SMT.cleanup       = noCleanup
  }

prop_sequential ::
     ( Eq blk
     , Show blk
     , Show (GenTx blk)
     , Show (SuccessfulResponse blk Symbolic)
     , Show (SuccessfulResponse blk Concrete)
     , QC.Arbitrary (GenTx blk)
     )
  => IO (Mempool IO blk idx)
  -> QC.Property
prop_sequential mkMempool =
    forAllCommands (stateMachine err) Nothing $ \cmds ->
      QCM.monadicIO $ do
        mempool <- QCM.run mkMempool
        (hist, _model, res) <- runCommands (stateMachine mempool) cmds
        let x :: Char
            x = hist
        prettyCommands (stateMachine mempool) hist -- TODO: check if we need the mempool in 'prettyCommands'
          $ checkCommandNames cmds
          $ res QC.=== Ok
  where
    err = error "The actual mempool should not needed when generating commands."

{-------------------------------------------------------------------------------
  Instances required to run the state machine
-------------------------------------------------------------------------------}

instance ToExpr (Model Concrete)

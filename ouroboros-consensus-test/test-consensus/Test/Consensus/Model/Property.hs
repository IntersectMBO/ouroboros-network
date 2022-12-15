{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

module Test.Consensus.Model.Property (prop_Model_accept_all_non_conficting_txs) where

import           Control.Exception (SomeException (..))
import           Control.Monad.IOSim (Failure (FailureException), IOSim,
                     ppEvents, runSimTrace, traceEvents, traceM, traceResult)
import           Control.Monad.State (evalStateT, runStateT)
import           Control.Tracer (Tracer (Tracer))
import           Test.Consensus.Model.Mempool (Action (..),
                     ConcreteMempool (..), MempoolModel (..), RunMonad (..))
import           Test.QuickCheck (Gen, Property, Testable, counterexample,
                     property, within)
import           Test.QuickCheck.DynamicLogic (DL, action, anyActions_,
                     forAllDL_, getModelStateDL)
import           Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import           Test.QuickCheck.Monadic (PropertyM, assert, monadic')
import           Test.QuickCheck.StateModel (Actions, runActions)

prop_Model_accept_all_non_conficting_txs :: Property
prop_Model_accept_all_non_conficting_txs =
    within 50000000 $
        forAllDL_ accept_all_non_conflicting_txs prop_MemPool

prop_MemPool :: Actions MempoolModel -> Property
prop_MemPool actions = property $
    runIOSimProp $ do
        _ <- runActions actions
        assert True

accept_all_non_conflicting_txs :: DL MempoolModel ()
accept_all_non_conflicting_txs = do
    anyActions_
    getModelStateDL >>= \case
        MempoolModel{transactions} ->
            eventually (HasValidatedTxs transactions)
        _ -> pure ()
  where
    eventually a = action (Wait 10) >> action a

--

-- * Utilities for `IOSim`

--

-- | Specialised runner similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Monadic.html#runSTGen>.
runIOSimProp :: Testable a => (forall s. PropertyM (RunMonad (IOSim s)) a) -> Gen Property
runIOSimProp p = do
    Capture eval <- capture
    let tr = runSimTrace $  (runMonad $ eval $ monadic' p) `evalStateT` ConcreteMempool{theMempool = Nothing, tracer = Tracer traceM}
        traceDump = ppEvents $ traceEvents tr
        logsOnError = counterexample ("trace:\n" <> traceDump)
    case traceResult False tr of
        Right x ->
            pure $ logsOnError x
        Left (FailureException (SomeException ex)) -> do
            pure $ counterexample (show ex) $ logsOnError $ property False
        Left ex ->
            pure $ counterexample (show ex) $ logsOnError $ property False

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.StateMachine.Step (
    Step
  , feed
  , stepCommands
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Test.StateMachine.Sequential
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2

newtype Step model cmd m resp a = Step {
  _unStep :: StateT (model Symbolic)
    (ReaderT (StateMachine model cmd m resp)
    ((WriterT [Command cmd resp]) GenSym)) a
} deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState (model Symbolic)
    , MonadWriter [Command cmd resp]
    , MonadReader (StateMachine model cmd m resp))

feed :: Rank2.Foldable resp => cmd Symbolic
     -> Step model cmd m resp (Command cmd resp)
feed cmd = do
  sm <- ask
  model <- get
  resp <- Step $ lift $ lift $ lift $ mock sm model cmd
  let command = Command cmd resp (getUsedVars resp)
  tell [command]
  put (transition sm model cmd resp)
  pure command

stepCommands :: StateMachine model cmd m resp
             -> Step model cmd m resp a
             -> Commands cmd resp
stepCommands sm (Step action) =
  Commands (evalGenSym
                (execWriterT
                 (runReaderT
                  (evalStateT action (initModel sm))
                  sm))
                newCounter)
  where
    evalGenSym action' counter =
      let (x, _) = runGenSym action' counter
      in x

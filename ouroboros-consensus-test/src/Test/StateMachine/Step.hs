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
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2

newtype Step model cmd m resp a = Step {
  _unStep :: StateT (model QSM.Symbolic)
    (ReaderT (QSM.StateMachine model cmd m resp)
    ((WriterT [QSM.Command cmd resp]) QSM.GenSym)) a
} deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState (model QSM.Symbolic)
    , MonadWriter [QSM.Command cmd resp]
    , MonadReader (QSM.StateMachine model cmd m resp))

feed :: Rank2.Foldable resp => cmd QSM.Symbolic
     -> Step model cmd m resp (QSM.Command cmd resp)
feed cmd = do
  sm <- ask
  model <- get
  resp <- Step $ lift $ lift $ lift $ QSM.mock sm model cmd
  let command = QSM.Command cmd resp (QSM.getUsedVars resp)
  tell [command]
  put (QSM.transition sm model cmd resp)
  pure command

stepCommands :: QSM.StateMachine model cmd m resp
             -> Step model cmd m resp a
             -> QSM.Commands cmd resp
stepCommands sm (Step action) =
  QSM.Commands (evalGenSym
                (execWriterT
                 (runReaderT
                  (evalStateT action (QSM.initModel sm))
                  sm))
                QSM.newCounter)
  where
    evalGenSym action' counter =
      let (x, _) = QSM.runGenSym action' counter
      in x

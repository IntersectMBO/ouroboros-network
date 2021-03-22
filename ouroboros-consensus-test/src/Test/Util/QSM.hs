{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.QSM (
    Example
    -- opaque
  , example
  , run
  , run'
  ) where

import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Data.Typeable

import qualified Test.StateMachine.Logic as Logic
import           Test.StateMachine.Sequential
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2

data Example cmd a =
    Done a
  | Run (cmd Symbolic) ([Var] -> Example cmd a)
  | Fail String

instance Functor (Example cmd) where
  fmap = liftM

instance Applicative (Example cmd) where
  pure  = Done
  (<*>) = ap

instance Monad (Example cmd) where
  return         = pure
  Done a   >>= f = f a
  Run c k  >>= f = Run c (k >=> f)
  Fail err >>= _ = Fail err

instance Fail.MonadFail (Example cmd) where
  fail = Fail

-- | Run a command, and capture its references
run :: Typeable a => cmd Symbolic -> Example cmd [Reference a Symbolic]
run cmd = Run cmd (Done . map (Reference . Symbolic))

-- | Run a command, ignoring its references
run' :: cmd Symbolic -> Example cmd ()
run' cmd = Run cmd (\_vars -> Done ())

example :: forall model cmd m resp. (Rank2.Foldable resp, Show (cmd Symbolic))
        => StateMachine model cmd m resp
        -> Example cmd ()
        -> Commands cmd resp
example sm =
    Commands . fst . flip runGenSym newCounter . go (initModel sm)
  where
    go :: model Symbolic -> Example cmd () -> GenSym [Command cmd resp]
    go _ (Done ())   = return []
    go _ (Fail err)  = error $ "example: " ++ err
    go m (Run cmd k) = do
        case Logic.logic (precondition sm m cmd) of
          Logic.VFalse counterexample ->
            error $ "Invalid command " ++ show cmd ++ ": " ++ show counterexample
          Logic.VTrue -> do
            resp <- mock sm m cmd

            let m' :: model Symbolic
                m' = transition sm m cmd resp

                vars :: [Var]
                vars = getUsedVars resp

                cmd' :: Command cmd resp
                cmd' = Command cmd resp vars

            (cmd' :) <$> go m' (k vars)

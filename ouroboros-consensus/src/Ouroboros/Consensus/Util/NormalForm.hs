{-# LANGUAGE MultiParamTypeClasses #-}

-- | Check whether a value is in normal form.
--
-- Intended for qualified import.
--
-- > import qualified Ouroboros.Consensus.Util.NormalForm as NF
module Ouroboros.Consensus.Util.NormalForm
  ( NormalForm (..)
  , NFCheck
  , checkResult
  , checkMultiple
    -- * Helpers
  , standard
  , tvar
  , tvar'
  , tmvar
  , tmvar'
  , tvarMap
  , ignore
  ) where

import           Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import           Data.Map.Strict (Map)
import           GHC.Stack

import           Control.Monad.Class.MonadSTM

import qualified Cardano.Prelude as Cardano (isNormalForm)


{-------------------------------------------------------------------------------
  NormalForm class
-------------------------------------------------------------------------------}

-- TODO update documentation

-- | Check whether a value of type @a@ is in normal form.
--
-- Use 'standard' (based on 'Cardano.isNormalForm') to implement instances.
--
-- We parameterise over @m@ and have @m Check@ as return type instead of
-- simply @IO Bool@ (or @ExceptT String IO ()@) for the following reason:
-- 'Cardano.isNormalForm' does not support 'TVar's and certainly not the
-- 'TVar's we use that are parameterised over @m@. In order to check whether a
-- 'TVar's is in normal form, we first have to read its contents, but since
-- @m@ is abstract (and thus not 'IO'), this must happen in @m@.
class NormalForm m a where
  custom :: a -> m NFCheck

{-------------------------------------------------------------------------------
  Check
-------------------------------------------------------------------------------}

-- | The result of a 'isNormalForm' check, see 'NormalForm'.
--
-- Instead of returning @IO Bool@, we like to include an error message and
-- make it composable, hence this newtype.
newtype NFCheck = NFCheck { runNFCheck :: ExceptT CallStack IO () }

-- | Return the result of a 'NFCheck'
checkResult :: NFCheck -> IO (Either CallStack ())
checkResult = runExceptT . runNFCheck

instance Semigroup NFCheck where
  NFCheck c1 <> NFCheck c2 = NFCheck (c1 *> c2)

instance Monoid NFCheck where
  mempty  = NFCheck $ return ()
  mappend = (<>)

-- | Combine multiple checks into one.
checkMultiple :: Monad m => [m NFCheck] -> m NFCheck
checkMultiple = fmap mconcat . sequence

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Use this for standard types, not for 'TVar's or 'TMVar's.
standard :: (Monad m, HasCallStack) => a -> m NFCheck
standard x = return $ NFCheck $ do
    normal <- lift $ Cardano.isNormalForm $! x
    if normal
      then return ()
      else throwError callStack

-- | Use this helper for 'TVar's. The given function is used to check the
-- value inside the 'TVar'.
tvar :: (MonadSTM m, HasCallStack)
     => TVar m a -> (a -> m NFCheck) -> m NFCheck
tvar var f = do
    contents <- atomically $ readTVar var
    withFrozenCallStack $ f contents

-- | Same as 'tvar', but uses 'standard' as the function.
tvar' :: (MonadSTM m, HasCallStack) => TVar m a -> m NFCheck
tvar' var = tvar var standard

-- | Use this helper for 'TMVar's. The given function is used to check the
-- value inside the 'TMVar'.
--
-- If the 'TMVar' is empty, the checks passes.
tmvar :: (MonadSTM m, HasCallStack)
      => TMVar m a -> (a -> m NFCheck) -> m NFCheck
tmvar var f = do
    contents <- atomically $ tryReadTMVar var
    case contents of
      Just x  -> withFrozenCallStack $ f x
      Nothing -> return mempty

-- | Same as 'tmvar', but uses 'standard' as the function.
tmvar' :: (MonadSTM m, HasCallStack) => TMVar m a -> m NFCheck
tmvar' var = tmvar var standard

-- | Helper for a 'Map' containing 'TVar's, 'standard' is used to check the
-- contents of the 'TVar's.
tvarMap :: MonadSTM m => Map k (TVar m v) -> m NFCheck
tvarMap _ = return mempty -- TODO

-- | Ignore the given argument and assume it is always in normal form or that
-- we simply don't care.
ignore :: Monad m => a -> m NFCheck
ignore _ = return mempty

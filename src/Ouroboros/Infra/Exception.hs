{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Infra.Exception (
    wrapExceptions
  , wrapExceptionsIO
  ) where

import           Data.List (intercalate)
import           Data.Proxy
import           GHC.Stack
import           UnliftIO

import           Ouroboros.Infra.Util.HList

{-------------------------------------------------------------------------------
  Wrap exceptions
-------------------------------------------------------------------------------}

wrapExceptions :: (MonadUnliftIO m, HasCallStack, All Show as, IsList as)
               => Fn as (m b) -> HList as -> m b
wrapExceptions fn args =
    catch (applyFn fn args) $ \e ->
      throwIO $ WrappedException callStack (collapse (Proxy @Show) show args) e

wrapExceptionsIO :: forall m as b.
                    (MonadUnliftIO m, HasCallStack, All Show as, IsList as)
                 => Fn as (IO b) -> HList as -> m b
wrapExceptionsIO fn =
    wrapExceptions (afterFn (isList :: SList as) (liftIO :: IO b -> m b) fn)

data WrappedException = WrappedException CallStack [String] SomeException

instance Show WrappedException where
  show (WrappedException cs as e) = unlines [
        "Exception " ++ displayException e
      , "Arguments [" ++ intercalate ", " as ++ "]"
      , prettyCallStack cs
      ]

instance Exception WrappedException

{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Util.Exception (catchAlsoLinked) where

import qualified Control.Exception as E
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util.IOLike

-- | Catch @e@ as well as @'ExceptionInLinkedThread' _ (ex :: e)@.
catchAlsoLinked ::
     (MonadCatch m, Exception e)
  => m a
  -> (e -> m a)
  -> m a
catchAlsoLinked ma handler =
    ma `catches`
      [ Handler handler
      , Handler $ \case
          (ExceptionInLinkedThread _ (E.fromException -> Just ex)) -> handler ex
          ex                                                       -> throwIO ex
      ]

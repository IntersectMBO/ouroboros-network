-- |

module LedgerOnDisk.Util where


import Control.Monad.Except
import Control.Monad.STM

runExceptTAtomically  :: (MonadIO m, MonadError e m) => ExceptT e STM a -> m a
runExceptTAtomically (ExceptT stm) = liftIO (atomically stm) >>= either throwError pure

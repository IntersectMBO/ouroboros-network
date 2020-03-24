{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | A shim layer for `Win32-network`'s `IOManager`
--
module System.IOManager
  ( WithIOManager
  , IOManager (..)
  , withIOManager
  ) where


#if defined(mingw32_HOST_OS)
import System.Win32.Types (HANDLE)
import qualified System.Win32.Async.IOManager as Win32.Async
import Network.Socket (Socket)
#endif

-- | On Windows 'AssociateWithIOCP' holds
-- `System.Win32.Async.IOManager.associateWithIOCompletionPort';
-- on other platforms 'AssociateWithIOCP' can run over any type, and thus is
-- guaranteed to be no-op.
--
#if defined(mingw32_HOST_OS)
newtype IOManager = IOManager {
    associateWithIOCP :: Either HANDLE Socket -> IO ()
  }
#else
newtype IOManager = IOManager {
    associateWithIOCP :: forall hole. hole -> IO ()
  }
#endif


type WithIOManager = forall a. (IOManager -> IO a) -> IO a


-- | 'withIOManger' must be called only once at the top level.  We wrap the
-- 'associateWithIOCompletionPort' in a newtype wrapper since it will be
-- carried arround through the application.
--
withIOManager :: WithIOManager
#if defined(mingw32_HOST_OS)
withIOManager = \f ->
  Win32.Async.withIOManager $
    \iocp -> f (IOManager $ \fd -> Win32.Async.associateWithIOCompletionPort fd iocp)
#else
withIOManager = \f -> f (IOManager $ \_ -> pure ())
#endif

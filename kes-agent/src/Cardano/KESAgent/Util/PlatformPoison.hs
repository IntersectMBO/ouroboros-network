{-# LANGUAGE CPP #-}

module Cardano.KESAgent.Util.PlatformPoison
where

-- | Throw an error if the host platform is Windows. On other platforms, this
-- is a no-op.
-- This is needed because while the `kes-agent` packages will build cleanly on
-- Windows, essential functionality does not work or cannot be tested. Hence, in
-- order to avoid surprising failures when trying to run the code on Windows,
-- we place 'poisonWindows' calls in any functions that are not supported on
-- Windows to generate an early and loud failure.
poisonWindows :: Monad m => m ()
#if defined(mingw32_HOST_OS)
poisonWindows = error "This functionality is not supported on Windows"
#else
poisonWindows = return()
#endif

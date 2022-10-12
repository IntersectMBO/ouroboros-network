module Cardano.KESAgent.Tests.Util
where

import Control.Concurrent.MVar

type Lock = MVar ()

withLock :: Lock -> IO a -> IO a
withLock lock = withMVar lock . const

newLock :: IO Lock
newLock = newMVar ()



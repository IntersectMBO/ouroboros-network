{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | A shim layer for `Win32-network`'s `IOManager`
--
module Ouroboros.Network.IOManager (module X) where

import           System.IOManager as X

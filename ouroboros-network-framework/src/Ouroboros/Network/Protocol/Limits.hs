-- | Common values for size and time limits used by ourobors-network.
module Ouroboros.Network.Protocol.Limits where

import           Control.Monad.Class.MonadTime

-- TODO: better limits


largeByteLimit :: Word
largeByteLimit = 2500000

smallByteLimit :: Word
smallByteLimit = 0xffff

shortWait :: Maybe DiffTime
shortWait = Just 10

longWait :: Maybe DiffTime
longWait = Just 60

waitForever :: Maybe DiffTime
waitForever = Nothing


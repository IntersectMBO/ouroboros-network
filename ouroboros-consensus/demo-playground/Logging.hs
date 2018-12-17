{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logging (
    LogEvent -- opaque
  , logChain
  , showNetworkTraffic
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Semigroup ((<>))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Testing.ConcreteBlock

import           Ouroboros.Consensus.Util.Condense

data LogEvent = LogEvent {
    msg    :: String
  }

showNetworkTraffic :: TBQueue LogEvent -> IO ()
showNetworkTraffic q = forever $ do
    LogEvent{..} <- atomically $ readTBQueue q
    -- Add an extra newline to help readability.
    putStrLn $ msg <> "\n"

instance Condense BlockHeader where
    condense BlockHeader{..} =
      "{hash: " <> condense headerHash
                <> ", blockNo: "
                <> condense headerBlockNo
                <> "}"

logChain :: Condense [b]
         => TBQueue LogEvent
         -> Chain b
         -> IO ()
logChain loggingQueue chain = do
    let m = "Adopted chain: " <> condense (Chain.toOldestFirst chain)
    atomically $ writeTBQueue loggingQueue $ LogEvent m

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Condense BlockNo
deriving instance Condense ConcreteHeaderHash

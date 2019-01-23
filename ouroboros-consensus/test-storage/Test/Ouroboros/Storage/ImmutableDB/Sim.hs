{-# LANGUAGE OverloadedStrings #-}
module Test.Ouroboros.Storage.ImmutableDB.Sim ( demoScript ) where

{-- | An example interaction with the immutable database.
--}

import           Control.Monad.Catch (MonadMask)

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           GHC.Stack (HasCallStack)

import           Ouroboros.Storage.ImmutableDB.API


demoScript :: (HasCallStack, MonadMask m)
           => (Epoch -> Map Epoch EpochSize -> m (ImmutableDB m))
           -> m [Maybe ByteString]
demoScript openDB = withDB (openDB 0 (Map.singleton 0 10)) $ \db -> do
      -- Append some blob in the DB
      appendBinaryBlob db 0 "haskell"
      appendBinaryBlob db 1 "nice"
      appendBinaryBlob db 5 "cardano"
      appendBinaryBlob db 7 "blockchain"
      _ <- startNewEpoch db 10
      appendBinaryBlob db 3 "test"

      -- Retrieve some blobs
      q0 <- getBinaryBlob db (EpochSlot 0 0)
      q1 <- getBinaryBlob db (EpochSlot 0 1)
      q2 <- getBinaryBlob db (EpochSlot 0 5)
      q3 <- getBinaryBlob db (EpochSlot 1 3)

      return [q0, q1, q2, q3]

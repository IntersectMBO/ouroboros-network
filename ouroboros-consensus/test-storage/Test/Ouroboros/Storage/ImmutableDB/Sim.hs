{-# LANGUAGE OverloadedStrings #-}
module Test.Ouroboros.Storage.ImmutableDB.Sim ( demoScript ) where

{-- | An example interaction with the immutable database.
--}

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Except (ExceptT(..), runExceptT)

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           GHC.Stack (HasCallStack)

import           Ouroboros.Storage.ImmutableDB.API


demoScript :: (HasCallStack, MonadMask m)
           => (    Epoch
                -> Map Epoch EpochSize
                -> m (Either ImmutableDBError (ImmutableDB m))
              )
           -> m (Either ImmutableDBError [Maybe ByteString])
demoScript openDB = withDB (openDB 0 (Map.singleton 0 10)) $ \db ->
    runExceptT $ do
      -- Append some blob in the DB
      ExceptT $ appendBinaryBlob db 0 "haskell"
      ExceptT $ appendBinaryBlob db 1 "nice"
      ExceptT $ appendBinaryBlob db 5 "cardano"
      ExceptT $ appendBinaryBlob db 7 "blockchain"
      _ <- ExceptT $ startNewEpoch db 10
      ExceptT $ appendBinaryBlob db 3 "test"

      -- Retrieve some blobs
      q0 <- ExceptT $ getBinaryBlob db (EpochSlot 0 0)
      q1 <- ExceptT $ getBinaryBlob db (EpochSlot 0 1)
      q2 <- ExceptT $ getBinaryBlob db (EpochSlot 0 5)
      q3 <- ExceptT $ getBinaryBlob db (EpochSlot 1 3)

      return [q0, q1, q2, q3]

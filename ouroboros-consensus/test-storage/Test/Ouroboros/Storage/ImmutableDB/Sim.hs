{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Ouroboros.Storage.ImmutableDB.Sim
    ( demoScript ) where

{-- | An example interaction with the immutable data store using the
      mock FS.
--}

import           Data.ByteString (ByteString)

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Except

import           GHC.Stack
import qualified System.IO as IO

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.ImmutableDB


demoScript :: ( MonadMask m
              , MonadSTM m
              , HasCallStack
              , HasFSE m
              ) => m (Either ImmutableDBError [ByteString])
demoScript = runExceptT $ do
    liftFsError $ do
      createDirectoryIfMissing True ["demo"]
      -- Create some files
      h1 <- hOpen ["demo", "epoch-006.dat"] IO.WriteMode
      h2 <- hOpen ["demo", "index-006.dat"] IO.WriteMode
      hClose h1
      hClose h2
    ExceptT $ withDB ["demo"] 7 $ \db -> runExceptT $ do
      -- Append some blob in the DB
      ExceptT $ appendBinaryBlob db (7, 0) "haskell"
      ExceptT $ appendBinaryBlob db (7, 1) "nice"
      ExceptT $ appendBinaryBlob db (7, 5) "cardano"
      ExceptT $ appendBinaryBlob db (7, 7) "blockchain"
      ExceptT $ appendBinaryBlob db (8, 3) "test"

      -- Retrieve some blobs
      q0 <- ExceptT $ getBinaryBlob db (7, 0)
      q1 <- ExceptT $ getBinaryBlob db (7, 1)
      q2 <- ExceptT $ getBinaryBlob db (7, 5)
      q3 <- ExceptT $ getBinaryBlob db (8, 3)

      return [q0, q1, q2, q3]

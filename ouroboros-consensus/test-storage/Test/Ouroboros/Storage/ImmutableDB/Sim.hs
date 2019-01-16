{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Ouroboros.Storage.ImmutableDB.Sim
    ( demoScript ) where

{-- | An example interaction with the immutable data store using the
      mock FS.
--}

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as C8

import           Control.Exception (assert)
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
              ) => m (Either ImmutableDBError ())
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
        ExceptT $ appendBinaryBlob db (7, RelativeSlot 0) (BS.byteString . C8.pack $ "haskell")
        ExceptT $ appendBinaryBlob db (7, RelativeSlot 1) (BS.byteString . C8.pack $ "nice")
        ExceptT $ appendBinaryBlob db (7, RelativeSlot 5) (BS.byteString . C8.pack $ "cardano")
        ExceptT $ appendBinaryBlob db (7, RelativeSlot 7) (BS.byteString . C8.pack $ "blockchain")
        ExceptT $ appendBinaryBlob db (8, RelativeSlot 3) (BS.byteString . C8.pack $ "test")

        -- Retrieve some blobs
        q0 <- ExceptT $ getBinaryBlob db (7, RelativeSlot 0)
        q1 <- ExceptT $ getBinaryBlob db (7, RelativeSlot 1)
        q2 <- ExceptT $ getBinaryBlob db (7, RelativeSlot 5)
        q3 <- ExceptT $ getBinaryBlob db (8, RelativeSlot 3)

        assert (q0 == "haskell" &&
                q1 == "nice" &&
                q2 == "cardano" &&
                q3 == "test"
               ) $ return ()

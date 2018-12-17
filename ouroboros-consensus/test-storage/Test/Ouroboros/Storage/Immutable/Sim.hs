{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Ouroboros.Storage.Immutable.Sim
    ( demoScript ) where

{-- | An example interaction with the immutable data store using the
      mock FS.
--}

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map

import           Control.Exception (assert)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Except

import           GHC.Stack
import qualified System.IO as IO

import           Ouroboros.Network.MonadClass
import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.Immutable.DB


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
    let epochSizes = Map.fromList $ (7, 10) : zip [0..6] (repeat 0)
    ExceptT $ withDB ["demo"] 7 epochSizes $ \db -> runExceptT $ do
        -- Append some blob in the DB
        ExceptT $ appendBinaryBlob db 0 (BS.byteString . C8.pack $ "haskell")
        ExceptT $ appendBinaryBlob db 1 (BS.byteString . C8.pack $ "nice")
        ExceptT $ appendBinaryBlob db 5 (BS.byteString . C8.pack $ "cardano")
        ExceptT $ appendBinaryBlob db 7 (BS.byteString . C8.pack $ "blockchain")
        ExceptT $ startNewEpoch db 10
        ExceptT $ appendBinaryBlob db 3 (BS.byteString . C8.pack $ "test")

        -- Retrieve some blobs
        q0 <- ExceptT $ getBinaryBlob db (EpochSlot 7 0)
        q1 <- ExceptT $ getBinaryBlob db (EpochSlot 7 1)
        q2 <- ExceptT $ getBinaryBlob db (EpochSlot 7 5)
        q3 <- ExceptT $ getBinaryBlob db (EpochSlot 8 3)

        assert (q0 == Just "haskell" &&
                q1 == Just "nice" &&
                q2 == Just "cardano" &&
                q3 == Just "test"
               ) $ return ()

{-# LANGUAGE OverloadedStrings #-}
module Ouroboros.Consensus.Node.Recovery
  ( createMarkerOnCleanShutdown
  , hasCleanShutdownMarker
  , createCleanShutdownMarker
  , removeCleanShutdownMarker
  ) where

import           Control.Monad (unless, when)

import           Ouroboros.Consensus.Node.Exit (ExitReason (..), toExitReason)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API (HasFS, doesFileExist,
                     removeFile, withFile)
import           Ouroboros.Consensus.Storage.FS.API.Types (AllowExisting (..),
                     FsPath, OpenMode (..), mkFsPath)

-- | The path to the /clean shutdown marker file/.
cleanShutdownMarkerFile :: FsPath
cleanShutdownMarkerFile = mkFsPath ["clean"]

-- | When the given action terminates with a /clean/ exception, create the
-- /clean shutdown marker file/.
--
-- NOTE: we assume the action (i.e., the node itself) never terminates without
-- an exception.
--
-- A /clean/ exception is an exception for 'exceptionRequiresRecovery' returns
-- 'False'.
createMarkerOnCleanShutdown
  :: IOLike m
  => HasFS m h
  -> m a  -- ^ Action to run
  -> m a
createMarkerOnCleanShutdown mp = onExceptionIf
    (not . exceptionRequiresRecovery)
    (createCleanShutdownMarker mp)

-- | Return 'True' when 'cleanShutdownMarkerFile' exists.
hasCleanShutdownMarker
  :: HasFS m h
  -> m Bool
hasCleanShutdownMarker hasFS =
    doesFileExist hasFS cleanShutdownMarkerFile

-- | Create the 'cleanShutdownMarkerFile'.
--
-- Idempotent.
createCleanShutdownMarker
  :: IOLike m
  => HasFS m h
  -> m ()
createCleanShutdownMarker hasFS = do
    alreadyExists <- hasCleanShutdownMarker hasFS
    unless alreadyExists $
      withFile hasFS cleanShutdownMarkerFile (WriteMode MustBeNew) $ \_h ->
        return ()

-- | Remove 'cleanShutdownMarkerFile'.
--
-- Will throw an 'FsResourceDoesNotExist' error when it does not exist.
removeCleanShutdownMarker
  :: HasFS m h
  -> m ()
removeCleanShutdownMarker hasFS =
    removeFile hasFS cleanShutdownMarkerFile

-- | Return 'True' if the given exception indicates that recovery of the
-- database is required on the next startup.
exceptionRequiresRecovery :: SomeException -> Bool
exceptionRequiresRecovery e = case toExitReason e of
    DatabaseCorruption -> True
    _                  -> False

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

onExceptionIf
  :: (IOLike m, Exception e)
  => (e -> Bool)  -- ^ Predicate to selection exceptions
  -> m ()         -- ^ Exception handler
  -> m a
  -> m a
onExceptionIf p h m = m `catch` \e -> do
    when (p e) h
    throwM e

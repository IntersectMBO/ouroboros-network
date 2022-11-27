{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.Recovery (
    LastShutDownWasClean (..)
  , createCleanShutdownMarker
  , hasCleanShutdownMarker
  , removeCleanShutdownMarker
  , runWithCheckedDB
  ) where

import           Control.Monad (unless, when)
import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)

import           Ouroboros.Consensus.Block (StandardHash)
import           Ouroboros.Consensus.Node.Exit (ExitReason (..), toExitReason)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB
import           Ouroboros.Consensus.Storage.FS.API (HasFS, doesFileExist,
                     removeFile, withFile)
import           Ouroboros.Consensus.Storage.FS.API.Types (AllowExisting (..),
                     FsPath, OpenMode (..), mkFsPath)

-- | The path to the /clean shutdown marker file/.
cleanShutdownMarkerFile :: FsPath
cleanShutdownMarkerFile = mkFsPath ["clean"]

-- | Did the ChainDB already have existing clean-shutdown marker on disk?
newtype LastShutDownWasClean = LastShutDownWasClean Bool
  deriving (Eq, Show)

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
exceptionRequiresRecovery ::
     forall blk. (StandardHash blk, Typeable blk)
  => Proxy blk
  -> SomeException
  -> Bool
exceptionRequiresRecovery pb e = case toExitReason pb e of
    DatabaseCorruption -> True
    _                  -> False

-- | A bracket function that manages the clean-shutdown marker on disk.
--
-- - If the marker is missing on startup, then ChainDB initialization will
--   revalidate the database contents.
--
-- - If the OS kills the nodes, then we don't have the opportunity to write out
--   the marker file, which is fine, since we want the next startup to do
--   revalidation.
--
-- - If initialization was cleanly interrupted (eg SIGINT), then we leave the
--   marker the marker in the same state as it was at the beginning of said
--   initialization.
--
-- - At the end of a successful initialization, we remove the marker and install
--   a shutdown handler that writes the marker except for certain exceptions
--   (see 'exceptionRequiresRecovery') that indicate corruption, for which we
--   want the next startup to do revalidation.
runWithCheckedDB
  :: forall a m h blk. (IOLike m, StandardHash blk, Typeable blk)
  => Proxy blk
  -> HasFS m h
  -> (LastShutDownWasClean -> (ChainDB m blk -> m a -> m a) -> m a)
  -> m a
runWithCheckedDB pb hasFS body = do
    -- When we shut down cleanly, we create a marker file so that the next
    -- time we start, we know we don't have to validate the contents of the
    -- whole ChainDB. When we shut down with an exception indicating
    -- corruption or something going wrong with the file system, we don't
    -- create this marker file so that the next time we start, we do a full
    -- validation.
    wasClean <- hasCleanShutdownMarker hasFS

    removeMarkerOnUncleanShutdown wasClean
      $ body
          (LastShutDownWasClean wasClean)
          (\_cdb runWithInitializedChainDB -> createMarkerOnCleanShutdown $ do
            -- ChainDB initialization has finished by the time we reach this
            -- point. We remove the marker so that a SIGKILL will cause an unclean
            -- shutdown.
            when wasClean $ removeCleanShutdownMarker hasFS
            runWithInitializedChainDB
          )
  where
    -- | If there is a unclean exception during ChainDB initialization, we want
    -- to remove the marker file, so we install this handler.
    --
    -- It is OK to also wrap this handler around code that runs after ChainDB
    -- initialization, because the condition on this handler is the opposite of
    -- the condition in the @createMarkerOnCleanShutdown@ handler.
    removeMarkerOnUncleanShutdown wasClean = if not wasClean then id else onExceptionIf
      (exceptionRequiresRecovery pb)
      (removeCleanShutdownMarker hasFS)

    -- | If a clean exception terminates the running node after ChainDB
    -- initialization, we want to create the marker file.
    --
    -- NOTE: we assume the action (i.e., the node itself) never terminates without
    -- an exception.
    createMarkerOnCleanShutdown = onExceptionIf
      (not . exceptionRequiresRecovery pb)
      (createCleanShutdownMarker hasFS)

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
    throwIO e

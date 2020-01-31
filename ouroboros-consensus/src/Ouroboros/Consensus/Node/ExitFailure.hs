{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Node.ExitFailure
  ( -- * Pre-defined ExitFailures
    ExitFailure
  , defaultExitFailure
  , configurationError
  , noNetwork
  , restartWithRecovery
  , wrongDatabase
  , diskFull
  , insufficientPermissions
    -- * Get the ExitFailure of an Exception
  , toExitFailure
  ) where

import           Control.Exception (SomeException, fromException)

import           Control.Monad.Class.MonadAsync (ExceptionInLinkedThread (..))

import           Ouroboros.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Storage.FS.API.Types (FsError (..), FsErrorType (..))
import           Ouroboros.Storage.ImmutableDB.Types (ImmutableDBError)
import qualified Ouroboros.Storage.ImmutableDB.Types as ImmDB
import           Ouroboros.Storage.VolatileDB.Types (VolatileDBError)
import qualified Ouroboros.Storage.VolatileDB.Types as VolDB

import           Ouroboros.Consensus.Node.DbMarker (DbMarkerError)
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron
                     (PBftLeaderCredentialsError)

{-------------------------------------------------------------------------------
  Pre-defined ExitFailures
-------------------------------------------------------------------------------}

type ExitFailure = Int

-- | Something went wrong, just restart the node.
defaultExitFailure :: ExitFailure
defaultExitFailure = 1

-- | Something is wrong with the node configuration, the user should check it.
--
-- For example, for PBFT, it could be that the block signing key and the
-- delegation certificate do not match.
configurationError :: ExitFailure
configurationError = 2

-- | There is a problem with the network connection, the user should
-- investigate.
--
-- TODO We're not yet returning this.
noNetwork :: ExitFailure
noNetwork = 3

-- | Something went wrong with the database, restart the node with recovery
-- enabled.
restartWithRecovery :: ExitFailure
restartWithRecovery = 4

-- | We were unable to open the database, probably the user is using the wrong
-- directory. See 'DbMarkerError' for details.
wrongDatabase :: ExitFailure
wrongDatabase = 5

-- | The disk is full, make some space before restarting the node.
diskFull :: ExitFailure
diskFull = 6

-- | The database folder doesn't have the right permissions.
insufficientPermissions :: ExitFailure
insufficientPermissions = 7

{-------------------------------------------------------------------------------
  Get the ExitFailure of an Exception
-------------------------------------------------------------------------------}

-- | Return the 'ExitFailure' (to be used in the @ExitFailure@ constructor of
-- 'System.Exit.ExitCode') for the given 'SomeException'. Defaults to
-- 'defaultExitFailure'.
toExitFailure :: SomeException -> ExitFailure
toExitFailure e
    | Just (ExceptionInLinkedThread _ e') <- fromException e
    = toExitFailure e'
    | Just (_ :: DbMarkerError) <- fromException e
    = wrongDatabase
    | Just (e' :: ChainDbFailure) <- fromException e
    = case e' of
        ImmDbFailure ue -> immDbUnexpectedError ue
        VolDbFailure ue -> volDbUnexpectedError ue
        LgrDbFailure fe -> fsError fe
        _               -> restartWithRecovery
    | Just (_ :: PBftLeaderCredentialsError) <- fromException e
    = configurationError

    -- The three exceptions below will always be wrapped in a
    -- 'ChainDbFailure', but we include them just in case.
    | Just (e' :: ImmutableDBError) <- fromException e
    = case e' of
        ImmDB.UnexpectedError ue -> immDbUnexpectedError ue
        _                        -> defaultExitFailure
    | Just (e' :: VolatileDBError) <- fromException e
    = case e' of
        VolDB.UnexpectedError ue -> volDbUnexpectedError ue
        _                        -> defaultExitFailure
    | Just (e' :: FsError) <- fromException e
    = fsError e'

    | otherwise
    = defaultExitFailure
  where
    immDbUnexpectedError :: ImmDB.UnexpectedError -> ExitFailure
    immDbUnexpectedError = \case
      ImmDB.FileSystemError fe -> fsError fe
      _                        -> restartWithRecovery

    volDbUnexpectedError :: VolDB.UnexpectedError -> ExitFailure
    volDbUnexpectedError = \case
      VolDB.FileSystemError fe -> fsError fe
      _                        -> restartWithRecovery

    fsError :: FsError -> ExitFailure
    fsError FsError { fsErrorType } = case fsErrorType of
      FsDeviceFull              -> diskFull
      FsInsufficientPermissions -> insufficientPermissions
      _                         -> restartWithRecovery

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Node.Exit
  ( -- * ExitFailure
    ExitFailure
  , exitReasontoExitFailure
    -- * ExitReason
  , ExitReason (..)
  , toExitReason
  ) where

import           Control.Exception (AsyncException (..), SomeException,
                     fromException)

import           Control.Monad.Class.MonadAsync (ExceptionInLinkedThread (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError (..),
                     FsErrorType (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Types
                     (ImmutableDBError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Types as ImmDB
import           Ouroboros.Consensus.Storage.VolatileDB.Error (VolatileDBError)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Error as VolatileDB

import           Ouroboros.Consensus.Node.DbMarker (DbMarkerError)

{-------------------------------------------------------------------------------
  ExitFailure
-------------------------------------------------------------------------------}

-- | The exit code to return when terminating with an exception.
--
-- To be used in the @ExitFailure@ constructor of 'System.Exit.ExitCode'.
--
-- Note that a node will never turn shut down itself, it is meant to run
-- forever, so it will always terminate with an 'ExitFailure'.
type ExitFailure = Int

-- | Convert an 'ExitReason' to an 'ExitFailure'.
exitReasontoExitFailure :: ExitReason -> ExitFailure
exitReasontoExitFailure = \case
    -- Some action should be taken before restarting in the cases below.
    ConfigurationError      -> 3
    WrongDatabase           -> 4
    DiskFull                -> 5
    InsufficientPermissions -> 6
    NoNetwork               -> 7

    -- The node can simply be restarted in the cases below.
    --
    -- NOTE: Database corruption is handled automically: when the node is
    -- restarted, it will do a full validation pass.
    Killed                  -> 1
    DatabaseCorruption      -> 2
    Other                   -> 2

{-------------------------------------------------------------------------------
  ExitReason
-------------------------------------------------------------------------------}

-- | The reason of shutting down
data ExitReason =
    -- | The node process was killed, by the @kill@ command, @CTRL-C@ or some
    -- other means. This is normal way for a user to terminate the node
    -- process. The node can simply be restarted.
    Killed

    -- | Something is wrong with the node configuration, the user should check it.
    --
    -- For example, for PBFT, it could be that the block signing key and the
    -- delegation certificate do not match.
  | ConfigurationError

    -- | We were unable to open the database, probably the user is using the
    -- wrong directory. See 'DbMarkerError' for details.
  | WrongDatabase

    -- | The disk is full, make some space before restarting the node.
  | DiskFull

    -- | The database folder doesn't have the right permissions.
  | InsufficientPermissions

    -- | There is a problem with the network connection, the user should
    -- investigate.
    --
    -- TODO We're not yet returning this.
  | NoNetwork

    -- | Something went wrong with the database, restart the node with
    -- recovery enabled.
  | DatabaseCorruption

    -- | Some exception was thrown. The node should just be restarted.
  | Other

-- | Return the 'ExitReason' for the given 'SomeException'. Defaults to
-- 'Other'.
toExitReason :: SomeException -> ExitReason
toExitReason e
    | Just (e' :: AsyncException) <- fromException e
    = case e' of
        ThreadKilled  -> Killed
        UserInterrupt -> Killed
        _             -> Other

    | Just (ExceptionInLinkedThread _ e') <- fromException e
    = toExitReason e'
    | Just (_ :: DbMarkerError) <- fromException e
    = WrongDatabase
    | Just (e' :: ChainDbFailure) <- fromException e
    = case e' of
        ImmDbFailure ue -> immDbUnexpectedError ue
        LgrDbFailure fe -> fsError fe
        _               -> DatabaseCorruption

    | Just (e' :: VolatileDBError) <- fromException e
    = case e' of
        VolatileDB.UnexpectedError ue -> volatileDbUnexpectedError ue
        _                             -> Other
    -- The two exceptions below will always be wrapped in a
    -- 'ChainDbFailure', but we include them just in case.
    | Just (e' :: ImmutableDBError) <- fromException e
    = case e' of
        ImmDB.UnexpectedError ue -> immDbUnexpectedError ue
        _                        -> Other
    | Just (e' :: FsError) <- fromException e
    = fsError e'

    | otherwise
    = Other
  where
    immDbUnexpectedError :: ImmDB.UnexpectedError -> ExitReason
    immDbUnexpectedError = \case
      ImmDB.FileSystemError fe -> fsError fe
      _                        -> DatabaseCorruption

    volatileDbUnexpectedError :: VolatileDB.UnexpectedError -> ExitReason
    volatileDbUnexpectedError = \case
      VolatileDB.FileSystemError fe -> fsError fe
      _                             -> DatabaseCorruption

    fsError :: FsError -> ExitReason
    fsError FsError { fsErrorType } = case fsErrorType of
      FsDeviceFull              -> DiskFull
      FsInsufficientPermissions -> InsufficientPermissions
      _                         -> DatabaseCorruption

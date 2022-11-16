{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.Exit (
    -- * ExitFailure
    ExitFailure
  , exitReasontoExitFailure
    -- * ExitReason
  , ExitReason (..)
  , toExitReason
  ) where

import           Control.Exception (AsyncException (..), SomeException,
                     fromException)
import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadAsync (ExceptionInLinkedThread (..))

import           Ouroboros.Consensus.Block (StandardHash)

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError (..),
                     FsErrorType (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDBError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDBError)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

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
toExitReason ::
     forall blk. (Typeable blk, StandardHash blk)
  => Proxy blk
  -> SomeException
  -> ExitReason
toExitReason pb e
    | Just (e' :: AsyncException) <- fromException e
    = case e' of
        ThreadKilled  -> Killed
        UserInterrupt -> Killed
        _             -> Other

    | Just (ExceptionInLinkedThread _ e') <- fromException e
    = toExitReason pb e'
    | Just (_ :: DbMarkerError) <- fromException e
    = WrongDatabase
    | Just (e' :: ChainDbFailure blk) <- fromException e
    = case e' of
        LgrDbFailure fe -> fsError fe
        _               -> DatabaseCorruption

    | Just (e' :: VolatileDBError blk) <- fromException e
    = case e' of
        VolatileDB.UnexpectedFailure uf -> volatileDbUnexpectedFailure uf
        _                               -> Other
    -- The two exceptions below will always be wrapped in a
    -- 'ChainDbFailure', but we include them just in case.
    | Just (e' :: ImmutableDBError blk) <- fromException e
    = case e' of
        ImmutableDB.UnexpectedFailure uf -> immutableDbUnexpectedFailure uf
        _                                -> Other
    | Just (e' :: FsError) <- fromException e
    = fsError e'

    | otherwise
    = Other
  where
    immutableDbUnexpectedFailure :: ImmutableDB.UnexpectedFailure blk -> ExitReason
    immutableDbUnexpectedFailure = \case
      ImmutableDB.FileSystemError fe -> fsError fe
      _                              -> DatabaseCorruption

    volatileDbUnexpectedFailure :: VolatileDB.UnexpectedFailure blk -> ExitReason
    volatileDbUnexpectedFailure = \case
      VolatileDB.FileSystemError fe -> fsError fe
      _                             -> DatabaseCorruption

    fsError :: FsError -> ExitReason
    fsError FsError { fsErrorType } = case fsErrorType of
      FsDeviceFull              -> DiskFull
      FsInsufficientPermissions -> InsufficientPermissions
      _                         -> DatabaseCorruption

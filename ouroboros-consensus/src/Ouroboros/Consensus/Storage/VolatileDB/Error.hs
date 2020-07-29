{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
-- | Errors that can be thrown by the VolatileDB
--
-- At the edge of the API: errors are visible to the user, while the concrete
-- details of the errors are related to the implementation.
module Ouroboros.Consensus.Storage.VolatileDB.Error (
    VolatileDBError (..)
  , UserError (..)
  , UnexpectedError (..)
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Exception (Exception (..), SomeException)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable (Typeable, eqT)

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, sameFsError)

-- | Errors which might arise when working with this database.
data VolatileDBError =
    -- | An error thrown because of incorrect usage of the VolatileDB
    -- by the user.
    UserError UserError

    -- | An unexpected error thrown because something went wrong.
  | UnexpectedError UnexpectedError
  deriving (Show)

instance Exception VolatileDBError where
    displayException = show

instance Eq VolatileDBError where
    (==) = sameVolatileDBError

sameVolatileDBError :: VolatileDBError -> VolatileDBError -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (UserError ue1, UserError ue2)             -> sameUserError ue1 ue2
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    _                                          -> False

data UserError =
    -- | The VolatileDB was closed. In case it was automatically closed
    -- because an unexpected error was thrown during a read operation or any
    -- exception was thrown during a write operation, that exception is
    -- embedded.
    ClosedDBError (Maybe SomeException)
  deriving (Show)

sameUserError :: UserError -> UserError -> Bool
sameUserError e1 e2 = case (e1, e2) of
    (ClosedDBError mbEx1, ClosedDBError mbEx2) -> (show <$> mbEx1) == (show <$> mbEx2)

data UnexpectedError =
    FileSystemError FsError

    -- | A block in failed to parse
  | forall blk. (Typeable blk, StandardHash blk) =>
      ParseFailure (Point blk) CBOR.DeserialiseFailure

    -- | When parsing a block we got some trailing data
  | forall blk. (Typeable blk, StandardHash blk) =>
      TrailingData (Point blk) Lazy.ByteString

    -- | Block missing
    --
    -- This exception gets thrown when a block that we /know/ should exist in
    -- the DB (for example, because its hash exists in the volatile DB's
    -- successor index) nonetheless was not found
    --
    -- This exception will be thrown by the @getKnown*@ functions.
  | forall blk. (Typeable blk, StandardHash blk) =>
      MissingBlock (Proxy blk) (HeaderHash blk)

deriving instance Show UnexpectedError

sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2
    (FileSystemError {}, _) -> False
    (ParseFailure (pt1 :: Point blk1) df1, ParseFailure (pt2 :: Point blk2) df2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> pt1 == pt2 && df1 == df2
    (ParseFailure {}, _) -> False
    (TrailingData (pt1 :: Point blk1) td1, TrailingData (pt2 :: Point blk2) td2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> pt1 == pt2 && td1 == td2
    (TrailingData {}, _) -> False
    (MissingBlock (Proxy :: Proxy blk1) h1, MissingBlock (Proxy :: Proxy blk2) h2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> h1 == h2
    (MissingBlock {}, _) -> False

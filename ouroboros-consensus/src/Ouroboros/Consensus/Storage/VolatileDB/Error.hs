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

import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, FsPath,
                     sameFsError)

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

    -- | A block failed to parse
  | forall blk. (Typeable blk, StandardHash blk) =>
      ParseError FsPath (RealPoint blk) CBOR.DeserialiseFailure

    -- | When parsing a block we got some trailing data
  | forall blk. (Typeable blk, StandardHash blk) =>
      TrailingDataError FsPath (RealPoint blk) Lazy.ByteString

    -- | Block missing
    --
    -- This exception gets thrown when a block that we /know/ it should be in
    -- the VolatileDB, nonetheless was not found.
    --
    -- This exception will be thrown by @getKnownBlockComponent@.
  | forall blk. (Typeable blk, StandardHash blk) =>
      MissingBlockError (Proxy blk) (HeaderHash blk)

deriving instance Show UnexpectedError

sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError e1 e2 = case (e1, e2) of
    (FileSystemError fs1,
     FileSystemError fs2) -> sameFsError fs1 fs2
    (FileSystemError {}, _) -> False

    (ParseError fp1 (pt1 :: RealPoint blk1) df1,
     ParseError fp2 (pt2 :: RealPoint blk2) df2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> fp1 == fp2 && pt1 == pt2 && df1 == df2
    (ParseError {}, _) -> False

    (TrailingDataError fp1 (pt1 :: RealPoint blk1) td1,
     TrailingDataError fp2 (pt2 :: RealPoint blk2) td2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> fp1 == fp2 && pt1 == pt2 && td1 == td2
    (TrailingDataError {}, _) -> False

    (MissingBlockError (Proxy :: Proxy blk1) h1,
     MissingBlockError (Proxy :: Proxy blk2) h2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> h1 == h2
    (MissingBlockError {}, _) -> False

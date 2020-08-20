{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
-- | Errors that can be thrown by the ImmutableDB
--
-- At the edge of the API: errors are visible to the user, while the concrete
-- details of the errors are related to the implementation.
module Ouroboros.Consensus.Storage.ImmutableDB.Error (
    ImmutableDBError (..)
  , sameImmutableDBError
  , UserError (..)
  , sameUserError
  , throwUserError
  , UnexpectedError (..)
  , sameUnexpectedError
  , throwUnexpectedError
  , MissingBlock (..)
  , missingBlockPoint
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Exception (Exception (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.List.NonEmpty (NonEmpty)
import           Data.Typeable (Typeable, eqT)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike (MonadThrow (throwM))

import           Ouroboros.Consensus.Storage.Common (StreamFrom, StreamTo)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, FsPath,
                     sameFsError)
import           Ouroboros.Consensus.Storage.FS.CRC (CRC)

-- | Errors that might arise when working with this database.
data ImmutableDBError
  = UserError       UserError CallStack
    -- ^ An error thrown because of incorrect usage of the immutable database
    -- by the user.
  | UnexpectedError UnexpectedError
    -- ^ An unexpected error thrown because something went wrong on a lower
    -- layer.
  deriving (Generic, Show)

instance Exception ImmutableDBError where
  displayException = \case
      UserError {} ->
        "ImmutableDB incorrectly used, indicate of a bug"
      UnexpectedError (FileSystemError fse) ->
        displayException fse
      UnexpectedError {} ->
        "The ImmutableDB got corrupted, full validation will be enabled for the next startup"

-- | Check if two 'ImmutableDBError's are equal while ignoring their
-- 'CallStack's.
sameImmutableDBError :: ImmutableDBError -> ImmutableDBError -> Bool
sameImmutableDBError e1 e2 = case (e1, e2) of
    (UserError ue1 _,     UserError ue2 _)     -> sameUserError ue1 ue2
    (UserError {},        _)                   -> False
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    (UnexpectedError {},  _)                   -> False

data UserError =
    -- | When trying to append a new block, it was not newer than the current
    -- tip, i.e., the slot was older than or equal to the current tip's slot.
    --
    -- The 'RealPoint' corresponds to the new block and the 'Point' to the
    -- current tip.
    forall blk. (Typeable blk, StandardHash blk) =>
      AppendBlockNotNewerThanTipError (RealPoint blk) (Point blk)

    -- | When trying to read a block with the given point, its slot was newer
    -- than the current tip.
    --
    -- The 'RealPoint' is the requested block and the 'Point' to the current
    -- tip.
  | forall blk. (Typeable blk, StandardHash blk) =>
      ReadBlockNewerThanTipError (RealPoint blk) (Point blk)

    -- | When the chosen iterator range was invalid, i.e. the @start@ (first
    -- parameter) came after the @end@ (second parameter).
  | forall blk. (Typeable blk, StandardHash blk) =>
      InvalidIteratorRangeError (StreamFrom blk) (StreamTo blk)

    -- | When performing an operation on a closed DB that is only allowed when
    -- the database is open.
  | ClosedDBError

    -- | When performing an operation on an open DB that is only allowed when
    -- the database is closed.
  | OpenDBError

deriving instance Show UserError

throwUserError :: (MonadThrow m, HasCallStack) => UserError -> m a
throwUserError e = throwM $ UserError e callStack

sameUserError :: UserError -> UserError -> Bool
sameUserError e1 e2 = case (e1, e2) of
    (AppendBlockNotNewerThanTipError (pt1 :: RealPoint blk1) ct1,
     AppendBlockNotNewerThanTipError (pt2 :: RealPoint blk2) ct2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> pt1 == pt2 && ct1 == ct2
    (AppendBlockNotNewerThanTipError {}, _) -> False
    (ReadBlockNewerThanTipError (pt1 :: RealPoint blk1) ct1,
     ReadBlockNewerThanTipError (pt2 :: RealPoint blk2) ct2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> pt1 == pt2 && ct1 == ct2
    (ReadBlockNewerThanTipError {}, _) -> False
    (InvalidIteratorRangeError (f1 :: StreamFrom blk1) t1,
     InvalidIteratorRangeError (f2 :: StreamFrom blk2) t2) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> f1 == f2 && t1 == t2
    (InvalidIteratorRangeError {}, _) -> False
    (ClosedDBError, ClosedDBError) -> True
    (ClosedDBError, _) -> True
    (OpenDBError, OpenDBError) -> True
    (OpenDBError, _) -> True

data UnexpectedError =
    -- | An IO operation on the file-system threw an error.
    FileSystemError FsError -- An FsError already stores the callstack

    -- | When loading an epoch or index file, its contents did not pass
    -- validation.
  | InvalidFileError FsPath String CallStack

    -- | A missing epoch or index file.
  | MissingFileError FsPath CallStack

    -- | There was a checksum mismatch when reading the block with the given
    -- point. The first 'CRC' is the expected one, the second one the actual
    -- one.
  | forall blk. (Typeable blk, StandardHash blk) =>
      ChecksumMismatchError (RealPoint blk) CRC CRC FsPath CallStack

    -- | A block failed to parse
  | forall blk. (Typeable blk, StandardHash blk) =>
      ParseError FsPath (RealPoint blk) CBOR.DeserialiseFailure

    -- | When parsing a block we got some trailing data
  | forall blk. (Typeable blk, StandardHash blk) =>
      TrailingDataError FsPath (RealPoint blk) Lazy.ByteString

    -- | Block missing
    --
    -- This exception gets thrown when a block that we /know/ it should be in
    -- the ImmutableDB, nonetheless was not found.
    --
    -- This exception will be thrown by the @getKnown*@ functions.
  | forall blk. (Typeable blk, StandardHash blk) =>
      MissingBlockError (MissingBlock blk)

deriving instance Show UnexpectedError

-- | Check if two 'UnexpectedError's are equal while ignoring their
-- 'CallStack's.
sameUnexpectedError :: UnexpectedError -> UnexpectedError -> Bool
sameUnexpectedError ue1 ue2 = case (ue1, ue2) of
    (FileSystemError fse1,
     FileSystemError fse2) -> sameFsError fse1 fse2
    (FileSystemError {}, _) -> False

    (InvalidFileError p1 m1 _,
     InvalidFileError p2 m2 _) -> p1 == p2 && m1 == m2
    (InvalidFileError {}, _) -> False

    (MissingFileError p1 _,
     MissingFileError p2 _) -> p1 == p2
    (MissingFileError {},       _) -> False

    (ChecksumMismatchError (pt1 :: RealPoint blk1) e1 a1 p1 _,
     ChecksumMismatchError (pt2 :: RealPoint blk2) e2 a2 p2 _) ->
       case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> pt1 == pt2 && e1 == e2 && a1 == a2 && p1 == p2
    (ChecksumMismatchError {}, _) -> False

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

    (MissingBlockError (mb1 :: MissingBlock blk1),
     MissingBlockError (mb2 :: MissingBlock blk2)) ->
      case eqT @blk1 @blk2 of
        Nothing   -> False
        Just Refl -> mb1 == mb2
    (MissingBlockError {}, _) -> False

throwUnexpectedError :: MonadThrow m => UnexpectedError -> m a
throwUnexpectedError = throwM . UnexpectedError

-- | This type can be part of an exception, but also returned as part of an
-- 'Either', because it can be expected in some cases.
data MissingBlock blk
    -- | There is no block in the slot of the given point.
  = EmptySlot (RealPoint blk)
    -- | The block and/or EBB in the slot of the given point have a different
    -- hash.
  | WrongHash (RealPoint blk) (NonEmpty (HeaderHash blk))
  deriving (Eq, Show, Generic)

-- | Return the 'RealPoint' of the block that was missing.
missingBlockPoint :: MissingBlock blk -> RealPoint blk
missingBlockPoint (EmptySlot pt)   = pt
missingBlockPoint (WrongHash pt _) = pt

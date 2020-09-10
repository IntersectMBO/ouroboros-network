{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.Orphans () where

import           Data.Maybe (isJust)
import           Data.Typeable (eqT)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CallStack

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbError,
                     ChainDbFailure)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.Common (StreamFrom)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, sameFsError)
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDBError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB.API (VolatileDBError)
import qualified Ouroboros.Consensus.Storage.VolatileDB.API as VolatileDB

{-------------------------------------------------------------------------------
  PrettyCallStack
-------------------------------------------------------------------------------}

-- | NOTE: all 'PrettyCallStack' are equal to each other.
--
-- This is useful for testing, when comparing error types that embed a
-- 'PrettyCallStack'. The call stack will differ in practice, i.e., model vs
-- implementation.
instance Eq PrettyCallStack where
  _ == _ = True

{-------------------------------------------------------------------------------
  FS
-------------------------------------------------------------------------------}

instance Eq FsError where
  (==) = sameFsError

{-------------------------------------------------------------------------------
  VolatileDB
-------------------------------------------------------------------------------}

deriving instance Eq VolatileDBError

instance Eq VolatileDB.ApiMisuse where
  VolatileDB.ClosedDBError mbEx1 == VolatileDB.ClosedDBError mbEx2 =
      -- The exceptions can differ, we only care about the presence of one.
      isJust mbEx1 == isJust mbEx2

instance Eq VolatileDB.UnexpectedFailure where
  e1 == e2 = case (e1, e2) of
      (VolatileDB.FileSystemError fs1,
       VolatileDB.FileSystemError fs2) -> fs1 == fs2
      (VolatileDB.FileSystemError {}, _) -> False

      (VolatileDB.ParseError fp1 (pt1 :: RealPoint blk1) df1,
       VolatileDB.ParseError fp2 (pt2 :: RealPoint blk2) df2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> fp1 == fp2 && pt1 == pt2 && df1 == df2
      (VolatileDB.ParseError {}, _) -> False

      (VolatileDB.TrailingDataError fp1 (pt1 :: RealPoint blk1) td1,
       VolatileDB.TrailingDataError fp2 (pt2 :: RealPoint blk2) td2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> fp1 == fp2 && pt1 == pt2 && td1 == td2
      (VolatileDB.TrailingDataError {}, _) -> False

      (VolatileDB.MissingBlockError (Proxy :: Proxy blk1) h1,
       VolatileDB.MissingBlockError (Proxy :: Proxy blk2) h2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> h1 == h2
      (VolatileDB.MissingBlockError {}, _) -> False

{-------------------------------------------------------------------------------
  ImmutableDB
-------------------------------------------------------------------------------}

deriving instance Eq ImmutableDBError

instance Eq ImmutableDB.ApiMisuse where
  e1 == e2 = case (e1, e2) of
      (ImmutableDB.AppendBlockNotNewerThanTipError (pt1 :: RealPoint blk1) ct1,
       ImmutableDB.AppendBlockNotNewerThanTipError (pt2 :: RealPoint blk2) ct2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> pt1 == pt2 && ct1 == ct2
      (ImmutableDB.AppendBlockNotNewerThanTipError {}, _) -> False
      (ImmutableDB.InvalidIteratorRangeError (f1 :: StreamFrom blk1) t1,
       ImmutableDB.InvalidIteratorRangeError (f2 :: StreamFrom blk2) t2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> f1 == f2 && t1 == t2
      (ImmutableDB.InvalidIteratorRangeError {}, _) -> False
      (ImmutableDB.ClosedDBError,
       ImmutableDB.ClosedDBError) -> True
      (ImmutableDB.ClosedDBError, _) -> True
      (ImmutableDB.OpenDBError,
       ImmutableDB.OpenDBError) -> True
      (ImmutableDB.OpenDBError, _) -> True

instance Eq ImmutableDB.UnexpectedFailure where
  ue1 == ue2 = case (ue1, ue2) of
      (ImmutableDB.FileSystemError fse1,
       ImmutableDB.FileSystemError fse2) -> sameFsError fse1 fse2
      (ImmutableDB.FileSystemError {}, _) -> False

      (ImmutableDB.InvalidFileError p1 m1 _,
       ImmutableDB.InvalidFileError p2 m2 _) -> p1 == p2 && m1 == m2
      (ImmutableDB.InvalidFileError {}, _) -> False

      (ImmutableDB.MissingFileError p1 _,
       ImmutableDB.MissingFileError p2 _) -> p1 == p2
      (ImmutableDB.MissingFileError {},       _) -> False

      (ImmutableDB.ChecksumMismatchError (pt1 :: RealPoint blk1) e1 a1 p1 _,
       ImmutableDB.ChecksumMismatchError (pt2 :: RealPoint blk2) e2 a2 p2 _) ->
         case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> pt1 == pt2 && e1 == e2 && a1 == a2 && p1 == p2
      (ImmutableDB.ChecksumMismatchError {}, _) -> False

      (ImmutableDB.ParseError fp1 (pt1 :: RealPoint blk1) df1,
       ImmutableDB.ParseError fp2 (pt2 :: RealPoint blk2) df2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> fp1 == fp2 && pt1 == pt2 && df1 == df2
      (ImmutableDB.ParseError {}, _) -> False

      (ImmutableDB.TrailingDataError fp1 (pt1 :: RealPoint blk1) td1,
       ImmutableDB.TrailingDataError fp2 (pt2 :: RealPoint blk2) td2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> fp1 == fp2 && pt1 == pt2 && td1 == td2
      (ImmutableDB.TrailingDataError {}, _) -> False

      (ImmutableDB.MissingBlockError (mb1 :: ImmutableDB.MissingBlock blk1),
       ImmutableDB.MissingBlockError (mb2 :: ImmutableDB.MissingBlock blk2)) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> mb1 == mb2
      (ImmutableDB.MissingBlockError {}, _) -> False

{-------------------------------------------------------------------------------
  ChainDB
-------------------------------------------------------------------------------}

instance Eq ChainDbFailure where
  f1 == f2 = case (f1, f2) of
      (ChainDB.VolatileDbCorruptBlock (br1 :: ChainDB.BlockRef blk1),
       ChainDB.VolatileDbCorruptBlock (br2 :: ChainDB.BlockRef blk2)) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> br1 == br2
      (ChainDB.VolatileDbCorruptBlock {}, _) -> False

      (ChainDB.LgrDbFailure fse1, ChainDB.LgrDbFailure fse2) -> fse1 == fse2
      (ChainDB.LgrDbFailure {}, _ ) -> False

      (ChainDB.ChainDbMissingBlock (pt1 :: RealPoint blk1),
       ChainDB.ChainDbMissingBlock (pt2 :: RealPoint blk2)) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> pt1 == pt2
      (ChainDB.ChainDbMissingBlock {}, _) -> False

instance Eq ChainDbError where
  e1 == e2 = case (e1, e2) of
      (ChainDB.ClosedDBError _, ChainDB.ClosedDBError _) -> True
      (ChainDB.ClosedDBError {}, _) -> False

      (ChainDB.ClosedReaderError, ChainDB.ClosedReaderError) -> True
      (ChainDB.ClosedReaderError, _) -> False

      (ChainDB.InvalidIteratorRange (f1 :: StreamFrom blk1) t1,
       ChainDB.InvalidIteratorRange (f2 :: StreamFrom blk2) t2) ->
        case eqT @blk1 @blk2 of
          Nothing   -> False
          Just Refl -> f1 == f2 && t1 == t2
      (ChainDB.InvalidIteratorRange {}, _) -> False

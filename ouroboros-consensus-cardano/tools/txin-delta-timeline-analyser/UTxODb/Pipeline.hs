{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- |

module UTxODb.Pipeline where

import UTxODb.Snapshots hiding (Seq)
import Control.Concurrent.Async
import Control.Concurrent.STM (TVar)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Concurrent.STM.TVar (writeTVar)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Concurrent.STM (stateTVar, newTBQueueIO, readTBQueue, writeTBQueue)
import Data.Maybe
import Control.Monad
import Data.Foldable
import Control.Monad.Catch
import Data.Either

data Pipeline handle state = Pipeline
  { db :: !handle
  , changelog :: !(TVar (DbChangelog state))
  , flush_seq :: !(TVar (Seq (SeqNo state)))
  , rollback_window :: !Int
  , flush_window :: !Int
  }

newtype ReadHandle state = ReadHandle (Async (AnnTableReadSets state (KeySetSanityInfo state, SeqNo state)))

prepare :: (DiskDb handle state, HasOnDiskTables state) => Pipeline handle state -> TableKeySets state -> IO (ReadHandle state)
prepare Pipeline{db, changelog} keyset = do
  a <- async $ do
    ann_tks <- atomically $ do
      cl <- readTVar changelog
      pure $ rewindTableKeySets cl keyset
    r <- readDb db ann_tks
    pure r

  pure $ ReadHandle a


submit :: forall handle state a. (DiskDb handle state, HasOnDiskTables state, HasSeqNo state)
  => Pipeline handle state
  -> ReadHandle state
  -> (state TrackingTable -> (state TableDiff, a))
  -> IO a
submit Pipeline{changelog, flush_seq, rollback_window, flush_window, db} (ReadHandle a) op = do
  readsets <- wait a
  (changes_to_flush, old_ondisk_anchor, new_ondisk_anchor, rollback_anchor, new_sn, l, r) <- atomically $ do
    cl0 <- readTVar changelog
    let
      Just tbl_rs = forwardTableReadSets cl0 readsets
      s_empty_table = currentStateDbChangeLog cl0
      s_tracking_table = readsetToTrackingTables (injectTables tbl_rs s_empty_table)
      (s_table_diff, r) = op s_tracking_table
      new_sn = stateSeqNo s_table_diff
      cl1 :: DbChangelog state
      cl1 = extendDbChangelog new_sn s_table_diff Nothing cl0
    (mb_new_anchor :: Maybe (SeqNo state), l, should_flush) <- stateTVar flush_seq $ \s0 -> let
      s1 = new_sn Seq.<| s0
      (s2, to_flush) = Seq.splitAt (rollback_window + flush_window) s1
      mb_new_anchor = Seq.lookup rollback_window s2
      in ((mb_new_anchor, length s2, not $ null to_flush), s2)
    let
      cl2 = fromMaybe cl1 $ join $ advanceDbChangelog <$> mb_new_anchor <*> pure cl1
      (changes_to_flush, cl3)
        | should_flush = flushDbChangelog cl2
        | otherwise = ([], cl2)
      new_ondisk_anchor = diskAnchorDbChangelog cl3
    writeTVar changelog cl3
    pure (changes_to_flush, diskAnchorDbChangelog cl0, new_ondisk_anchor,  stateAnchorDbChangelog cl3,  endOfDbChangelog cl3, l, r)
  putStrLn $ unwords ["submit:"
                     , "new_sn:" , show new_sn
                     , "changes_to_flush:", show . length $ changes_to_flush
                     , "old ondisk_anchor:", show old_ondisk_anchor
                     , "new ondisk_anchor:", show new_ondisk_anchor
                     , "rollback_anchor:", show rollback_anchor
                     , "in memory buffer size:", show l
                     ]
  unless (null changes_to_flush) $ writeDb db changes_to_flush old_ondisk_anchor new_ondisk_anchor
  pure r

runPipeline :: (Show a, HasOnDiskTables state, DiskDb handle state, HasSeqNo state)
  => handle
  -> state EmptyTable
  -> Int
  -> [a]
  -> (a -> TableKeySets state)
  -> (a -> state TrackingTable -> state TableDiff)
  -> IO ()
runPipeline h init_state q inputs get_keys do_op = do
  pipeline <- initPipeline h init_state
  chan <- newTBQueueIO $ fromIntegral q
  let drain_chan = do
        x <- atomically $ readTBQueue chan
        case x of
          Nothing -> pure ()
          Just (rh, op) -> do
            submit pipeline rh $ (,()) . op
            putStrLn "submit finished"
            drain_chan

  withAsync (drain_chan `catchAll` (putStrLn . show)) $ \drain_handle -> do
    for_ inputs $ \i -> do
      putStrLn $ "row: " <> show i
      rh <- prepare pipeline (get_keys i)
      atomically $ writeTBQueue chan $ Just (rh, do_op i)
    atomically $ writeTBQueue chan Nothing
    wait drain_handle


initPipeline :: (HasOnDiskTables state, HasSeqNo state) => handle -> state EmptyTable -> IO (Pipeline handle state)
initPipeline db  init_state = do
  changelog <- newTVarIO $ initialDbChangelog (stateSeqNo init_state) init_state
  flush_seq <- newTVarIO mempty
  let
    rollback_window = 1000 -- these are both arbitrary
    flush_window = 1000
  pure Pipeline{..}

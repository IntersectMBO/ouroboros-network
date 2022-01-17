{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
-- |

module UTxODb.InMemory where
import UTxODb.Snapshots
import qualified Data.Map.Strict as Map
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TVar)
import Control.Monad.STM (STM)
import Control.Monad.Catch
import Data.Foldable
import Control.Monad

-- In-mem mock of disk operations
---------------------------------
data TVarDb state = TVarDb !(Tables state TVarDbTable)
                           !(TVar (SeqNo state))

newtype TVarDbTable (t :: TableType) k v = TVarDbTable (TVar (Map.Map k v))

-- data ProductTable table1 table2 (t :: TableType) k v = ProductTable (table1 t k v) (table2 t k v)

newtype ComposeTable f table (t :: TableType) k v = ComposeTable { runComposeTable :: f (table t k v) }

newtype WrappedMap (t :: TableType) k v = WrappedMap (Map.Map k v)

instance (HasOnDiskTables state, HasSeqNo state) => DiskDb (TVarDb state) state where
  readDb (TVarDb tables seqnovar) keysets = STM.atomically $ do
    seqno <- STM.readTVar seqnovar
    traverse2Tables (readTVarDbTable seqno) tables keysets
    where
      readTVarDbTable :: Ord k
                      => SeqNo state
                      -> TableTag t v
                      -> TVarDbTable t k v
                      -> AnnTable TableKeySet a t k v
                      -> STM (AnnTable TableReadSet (a, SeqNo state) t k v)
      readTVarDbTable seqno t (TVarDbTable tv) (AnnTable ks a) = do
          tbl <- STM.readTVar tv
          let !pmap = mkTableReadSet t ks tbl
          return $ AnnTable pmap (a, seqno)

  writeDb (TVarDb tables seqnovar) diffs_and_snapshots oldseqno newseqno =
      STM.atomically $ do
        oldseqno' <- STM.readTVar seqnovar
        unless (oldseqno' == oldseqno) $
          STM.throwSTM (DbSeqNoIncorrect (fromEnum oldseqno') (fromEnum oldseqno))
        unless (newseqno > oldseqno) $
          STM.throwSTM (DbSeqNoNotMonotonic (fromEnum oldseqno) (fromEnum newseqno))
        STM.writeTVar seqnovar newseqno
        for_ diffs_and_snapshots $ \case
          Left diffs -> traverse2Tables_ writeTVarDbTable tables diffs
          Right ss -> do
            swizzled_mb_tables <- interpretTableSnapshots take_snapshot (pure $ ComposeTable Nothing) ss tables
            traverse2Tables_ combine_swizzle swizzled_mb_tables tables
    where
      writeTVarDbTable :: Ord k
                       => TableTag t v
                       -> TVarDbTable t k v
                       -> TableDiff   t k v
                       -> STM ()
      writeTVarDbTable TableTagRO (TVarDbTable _tv) TableDiffRO =
        return ()

      writeTVarDbTable TableTagRW (TVarDbTable tv) (TableDiffRW d) = do
        m <- STM.readTVar tv
        let (MapRW m') = applyDiff (MapRW m) d
        STM.writeTVar tv m'

      writeTVarDbTable TableTagRWU (TVarDbTable tv) (TableDiffRWU d) = do
        m <- STM.readTVar tv
        let (MapRWU m') = applyDiff (MapRWU m) d
        STM.writeTVar tv m'

      take_snapshot (TVarDbTable tv) = ComposeTable . Just . WrappedMap <$> STM.readTVar tv

      combine_swizzle _ (ComposeTable mb_m) (TVarDbTable tv) = case mb_m of
        Nothing -> pure ()
        Just (WrappedMap m) -> STM.writeTVar tv m

data DbSeqNoException =
       DbSeqNoIncorrect    Int Int -- ^ Expected and given sequence number
     | DbSeqNoNotMonotonic Int Int -- ^ Current and given sequence number
  deriving Show

instance Exception DbSeqNoException

initTVarDb :: HasOnDiskTables state => SeqNo state -> IO (TVarDb state)
initTVarDb sq = do
  seq_tv <- STM.newTVarIO sq
  tables <- traverse0Tables $ \_ -> TVarDbTable <$> STM.newTVarIO Map.empty
  pure $ TVarDb tables seq_tv

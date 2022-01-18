{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
module TxIn.UTxODb where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Text.Short as TextShort
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Short as Short
import Control.Monad.Catch

import qualified Control.Monad.State.Strict as Strict
import Data.Kind
import Data.Coerce
import Data.ByteString.Short.Base64 (encodeBase64)
import Data.ByteString.Short.Base16 (encodeBase16')
import Data.Functor
import Control.Monad (foldM, unless)
import Data.Int

import TxIn.Types(Row(..), TxIn(..), outputTxIns)
import qualified UTxODb.Snapshots as Db
import qualified UTxODb.Haskey.Tree as HaskeyDb

  -- writeDb :: dbhandle
  --         -> [Either (TableDiffs state) (TableSnapshots state)]
  --         -> SeqNo state -- ^ The old sequence number, as a sanity check
  --         -> SeqNo state -- ^ The new sequence number, must be strictly greater
  --         -> IO ()

-- data Row = Row {
--     rBlockNumber :: {-# UNPACK #-} !Word64
--   , rSlotNumber  :: {-# UNPACK #-} !Word64
--   , rNumTx       :: {-# UNPACK #-} !Int
--   , rConsumed    :: {-# UNPACK #-} !(V.Vector TxIn)
--   , rCreated     :: {-# UNPACK #-} !(V.Vector TxOutputIds)
--   }
data LedgerState table = LedgerState
  { utxo :: !(table Db.TableTypeRW TxIn Bool)
  , seq_no :: !Int64
  }

instance Db.HasSeqNo LedgerState where
  stateSeqNo LedgerState{seq_no} = coerce seq_no

initLedgerState :: Db.SeqNo LedgerState -> LedgerState Db.EmptyTable
initLedgerState sn = LedgerState { seq_no = coerce sn, utxo = Db.EmptyTable }

instance Db.HasTables LedgerState where
  type StateTableKeyConstraint LedgerState = All
  type StateTableValueConstraint LedgerState= All
  traverseTables f ls@LedgerState { utxo } =
    (\x -> ls {utxo = x}) <$> f Db.TableTagRW utxo

class All k
instance All k

instance Db.HasTables (Db.Tables LedgerState) where
  type StateTableKeyConstraint (Db.Tables LedgerState) = All
  type StateTableValueConstraint (Db.Tables  LedgerState) = All
  traverseTables f OnDiskLedgerState {od_utxo} =  OnDiskLedgerState <$> f Db.TableTagRW od_utxo

instance Db.HasOnlyTables (Db.Tables LedgerState) where
  traverse0Tables f = OnDiskLedgerState  <$> f Db.TableTagRW
  traverse2Tables f x y = OnDiskLedgerState <$> f Db.TableTagRW (od_utxo x) (od_utxo y)

instance Db.HasOnDiskTables LedgerState where
  newtype Tables LedgerState table = OnDiskLedgerState { od_utxo :: table Db.TableTypeRW TxIn Bool }
  projectTables LedgerState {utxo} = OnDiskLedgerState {od_utxo = utxo}
  injectTables OnDiskLedgerState{od_utxo} ls@LedgerState {} = ls { utxo = od_utxo }

instance HaskeyDb.HasHaskeyOnDiskTables LedgerState where
  haskeyTraverseTables f ls@LedgerState {utxo} = f Db.TableTagRW utxo <&> \x -> ls { utxo = x }
  haskeyTraverse0Tables f = OnDiskLedgerState  <$> f Db.TableTagRW
  haskeyTraverse1Tables f OnDiskLedgerState {od_utxo}= OnDiskLedgerState  <$> f Db.TableTagRW od_utxo
  haskeyTraverse2Tables f od1 od2 = OnDiskLedgerState  <$> f Db.TableTagRW (od_utxo od1) (od_utxo od2)

newtype LedgerRulesException = LedgerRulesException String
  deriving stock(Show)

instance Exception LedgerRulesException

data RowTxIns = RowTxIns
  { created :: Set TxIn
  , consumed :: Set TxIn
  }
  deriving stock (Show)

keysForRow :: Row -> RowTxIns
keysForRow row = let
  consumed0 = Set.fromList $                         V.toList $ rConsumed row
  created0  = Set.fromList $ concatMap outputTxIns $ V.toList $ rCreated  row
  created  = Set.difference created0 consumed0
  consumed = Set.difference consumed0 created0
  in RowTxIns{..}

ledgerRules :: (MonadThrow m, Db.MappingW (table Db.TableTypeRW), Db.MappingR (table Db.TableTypeRW)) => Row -> LedgerState table -> m (LedgerState table)
ledgerRules r ls@LedgerState{utxo = utxo0, seq_no = old_seq_no} = do
  let RowTxIns{created, consumed} = keysForRow r
      sho1 (TxIn h i) = TextShort.toString (encodeBase64 h) <> "@" <> show i
      sho2 (TxIn h i) = Char8.unpack (Char8.fromStrict (Short.fromShort (encodeBase16' h))) <> "@" <> show i

  (utxo1, missing) <- let
    go utxo txin = case Db.lookup txin utxo of
      Nothing -> Strict.modify' (<> Set.singleton txin) $> utxo
      Just _ -> pure $ Db.delete txin utxo -- don't even look at value
    in flip Strict.runStateT mempty $ foldM go utxo0 consumed

  let utxo2 = foldr (\k utxo -> Db.insert k True utxo) utxo1 created
  unless (Set.null missing) $ let
    message = unlines
        [ unwords ["ERROR: missing TxIn", show (rBlockNumber r), show (Set.size consumed), show (Set.size missing)]
        , unwords $ map sho1 $ Set.toList missing
        , unwords $ map sho2 $ Set.toList missing
        ]
    in throwM $ LedgerRulesException message
  let
    new_seq_no = fromIntegral $ rSlotNumber r
    new_ls = ls { utxo = utxo2, seq_no = new_seq_no}
  unless (old_seq_no < new_seq_no) $ throwM $ LedgerRulesException $ unwords ["nonmonotonic slot no:", show old_seq_no, ">", show new_seq_no]
  pure new_ls


addTxIns :: Db.DiskDb dbhandle LedgerState => dbhandle -> Set TxIn -> Db.SeqNo state -> LedgerState Db.EmptyTable -> IO (LedgerState Db.EmptyTable)
addTxIns handle txins new_seq_no ls0 = do
  let keyset = OnDiskLedgerState { od_utxo = Db.AnnTable (Db.TableKeySet txins) ()}
  tracking_tables <-
    Db.annotatedReadsetToTrackingTables <$> Db.readDb handle keyset

  ls1 <- let
    init_ls = Db.injectTables tracking_tables ls0
    go !ls txin = case Db.lookup txin (utxo ls ) of
      Nothing -> pure $ ls { utxo = Db.insert txin True (utxo ls) }
      Just _ -> throwM $ LedgerRulesException $ "addTxIns: duplicate txin:" <> show txin
    in (\x -> x { seq_no = coerce new_seq_no }) <$> foldM go init_ls txins
  let table_diffs = Db.projectTables . Db.trackingTablesToTableDiffs $ ls1
  Db.writeDb handle [Left table_diffs] (Db.stateSeqNo ls0) (Db.stateSeqNo ls1)
  putStrLn $ "addTxIns: " <> show (length txins)
  pure $ Db.injectTables Db.emptyTables ls1

-- addRow :: forall dbhandle. Db.DiskDb dbhandle LedgerState => dbhandle -> Row -> IO ()
addRow :: Db.DiskDb dbhandle LedgerState => dbhandle -> Row -> LedgerState Db.EmptyTable -> IO (LedgerState Db.EmptyTable)
addRow handle r ls0 = do
  let RowTxIns {created, consumed} = keysForRow r
      keyset :: Db.AnnTableKeySets LedgerState ()
      keyset = OnDiskLedgerState { od_utxo = Db.AnnTable (Db.TableKeySet consumed) ()}

  tracking_tables <-
    Db.annotatedReadsetToTrackingTables <$> Db.readDb handle keyset
  let ls = Db.injectTables tracking_tables ls0
  ls1 <-  ledgerRules r ls
  let table_diffs = Db.projectTables . Db.trackingTablesToTableDiffs $ ls1
  Db.writeDb handle [Left table_diffs] (Db.stateSeqNo ls0) (Db.stateSeqNo ls1)
  putStrLn $   show (rBlockNumber r)
    <> "\t" <> show (rSlotNumber  r)
    <> "\t" <> show (Set.size consumed)
    <> "\t" <> show (Set.size created)

  pure $ Db.injectTables Db.emptyTables ls1


rowOp :: Row -> LedgerState Db.TrackingTable -> LedgerState Db.TableDiff
rowOp r ls0 = let
  ls1 = case ledgerRules r ls0 of
    Nothing -> error "ledgerRules"
    Just x -> x
  table_diffs = Db.trackingTablesToTableDiffs ls1
  in table_diffs

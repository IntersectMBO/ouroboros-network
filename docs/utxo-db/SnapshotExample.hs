{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module SnapshotExample where


import           Prelude hiding (lookup)

import           Data.Kind
import           Data.Functor.Identity
import           Data.Foldable

import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.FingerTree as FT
import qualified Data.STRef as ST
import           Data.STRef (STRef)
import           Data.Word (Word64)

import           Control.Monad
import qualified Control.Monad.ST.Strict as ST
import           Control.Monad.ST.Strict (ST)
import           Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM
import           Control.Exception



import Snapshots

-- Example
------------

-- | An example of a top level state consisting of a few parts. This
-- demonstrates a top level state type with multiple tables, and other
-- in-memory state that are not tables, and is not kept on disk.
--
data LedgerState map =
     LedgerState {

       -- | An example of some nested state
       utxos   :: UTxOState map,

       -- | More nested state for snapshots
       snapshots :: UTxOSnapshots map,

       -- | Something standing in for other state, like protocol parameters.
       pparams :: PParams,

       -- | The current slot number, used to identify states within evolving
       -- sequences of states.
       --
       curslot :: SlotNo
     }

-- | The content of this doesn't actually matter. It's just a place-holder.
--
data PParams = PParams
  deriving Show

-- | An example sub-part of the state, to demonstrate \"vertical\" composition:
-- that the state and operations can work over components of the overall state.
--
-- In particular this one demonstrates two tables. The second of the two
-- tables can benefit from monoidal updates, since it incrementally maintains
-- an aggregation of the first table.
--
data UTxOState map =
     UTxOState {
       -- | A simple UTxO structure.
       utxo    :: map TableTypeRW TxIn TxOut,

       -- | An aggregation of the UTxO's coins by address.
       utxoagg :: map TableTypeRWU Addr Coin
     }


-- | A demonstration of snapshoting functionality: we keep three recent copies
-- of the UTxO aggregation. These are
--
data UTxOSnapshots map =
     UTxOSnapshots {
       utxoagg1 :: map TableTypeRO Addr Coin,
       utxoagg2 :: map TableTypeRO Addr Coin,
       utxoagg3 :: map TableTypeRO Addr Coin
     }

data    Tx     = Tx [TxIn] [TxOut]  deriving (Eq, Show)
data    TxIn   = TxIn !TxId !TxIx   deriving (Eq, Ord, Show)
newtype TxId   = TxId Int           deriving (Eq, Ord, Show)
newtype TxIx   = TxIx Int           deriving (Eq, Ord, Show)
data    TxOut  = TxOut !Addr !Coin  deriving (Eq, Show)
newtype Addr   = Addr Int           deriving (Eq, Ord, Show)
newtype Coin   = Coin Int           deriving (Eq, Num, Show)

instance Semigroup Coin  where a <> b = a + b

data    Block  = Block !SlotNo [Tx] deriving (Eq, Show)
newtype SlotNo = SlotNo Int         deriving (Eq, Ord, Bounded, Enum, Show)

txId :: Tx -> TxId
txId = TxId . H.hash

---

applyBlock :: Block -> LedgerState TrackingTable -> Maybe (LedgerState TrackingTable)
applyBlock (Block slotno txs) st = do
  guard $ slotno > curslot st
  st' <- foldM (flip applyTx) st txs
  pure st' {curslot = slotno}

applyTx :: Tx -> LedgerState TrackingTable -> Maybe (LedgerState TrackingTable)
applyTx tx LedgerState{utxos, pparams} = do
    utxos' <- applyTx' tx utxos
    Just LedgerState {
           utxos = utxos',
           pparams
         }

-- Operations on a sub-component of the state

applyTx' :: Tx -> UTxOState map -> Maybe (UTxOState TrackingTable)
applyTx' tx@(Tx txins txouts) = fmap (insertOutputs txid txouts)
                              . deleteInputs txins
  where
    txid = txId tx

deleteInputs :: [TxIn] -> UTxOState TrackingTable -> Maybe (UTxOState TrackingTable)
deleteInputs []           st = Just st
deleteInputs (txin:txins) st = deleteInput txin st >>= deleteInputs txins

deleteInput :: TxIn -> UTxOState TrackingTable -> Maybe (UTxOState TrackingTable)
deleteInput txin UTxOState {utxo, utxoagg} = do
    -- Verify that the input exists, and also update the utxo aggregate
    TxOut addr coin <- lookup txin utxo
    return UTxOState {
      utxo    = delete txin utxo,
      utxoagg = update addr (negate coin) utxoagg
    }

insertOutputs :: TxId -> [TxOut] -> UTxOState TrackingTable -> UTxOState TrackingTable
insertOutputs txid txouts st =
    foldr (\(txix, txout) -> insertOutput (TxIn txid (TxIx txix)) txout)
          st (zip [0 ..] txouts)

insertOutput :: TxIn -> TxOut -> UTxOState TrackingTable -> UTxOState TrackingTable
insertOutput txin txout@(TxOut addr coin) UTxOState {utxo, utxoagg} =
    -- also update the utxo aggregate
    UTxOState {
      utxo    = insert txin txout utxo,
      utxoagg = update addr coin utxoagg
    }

-- | This is a place-holder for serialisation constraints
class Example a
instance Example TxIn
instance Example TxOut
instance Example Addr
instance Example Coin


instance HasTables LedgerState where
  type StateTableKeyConstraint   LedgerState = Example
  type StateTableValueConstraint LedgerState = Example

  traverseTables f LedgerState { utxos, snapshots, pparams, curslot } =
    LedgerState <$> traverseTables f utxos
                <*> traverseTables f snapshots
                <*> pure pparams
                <*> pure curslot

  traverseTables_ f LedgerState { utxos, snapshots } =
    () <$ traverseTables_ f utxos
       <* traverseTables_ f snapshots

instance HasTables UTxOState where
  type StateTableKeyConstraint   UTxOState = Example
  type StateTableValueConstraint UTxOState = Example

  traverseTables f UTxOState { utxo, utxoagg } =
    UTxOState <$> f TableTagRW  utxo
              <*> f TableTagRWU utxoagg

  traverseTables_ f UTxOState { utxo, utxoagg } =
    () <$ f TableTagRW  utxo
       <* f TableTagRWU utxoagg

instance HasTables UTxOSnapshots where
  type StateTableKeyConstraint   UTxOSnapshots = Example
  type StateTableValueConstraint UTxOSnapshots = Example

  traverseTables f UTxOSnapshots { utxoagg1, utxoagg2, utxoagg3 } =
    UTxOSnapshots <$> f TableTagRO utxoagg1
                  <*> f TableTagRO utxoagg2
                  <*> f TableTagRO utxoagg3

  traverseTables_ f UTxOSnapshots { utxoagg1, utxoagg2, utxoagg3 } =
    () <$ f TableTagRO utxoagg1
       <* f TableTagRO utxoagg2
       <* f TableTagRO utxoagg3


instance HasTables (Tables LedgerState) where
  type StateTableKeyConstraint   (Tables LedgerState) = Example
  type StateTableValueConstraint (Tables LedgerState) = Example

  traverseTables f LedgerStateTables {
                       utxoTable,
                       utxoaggTable,
                       utxoagg1Table,
                       utxoagg2Table,
                       utxoagg3Table
                     } =
    LedgerStateTables <$> f TableTagRW  utxoTable
                        <*> f TableTagRWU utxoaggTable
                        <*> f TableTagRO  utxoagg1Table
                        <*> f TableTagRO  utxoagg2Table
                        <*> f TableTagRO  utxoagg3Table

  traverseTables_ f LedgerStateTables {
                       utxoTable,
                       utxoaggTable,
                       utxoagg1Table,
                       utxoagg2Table,
                       utxoagg3Table
                     } =
    () <$ f TableTagRW  utxoTable
       <* f TableTagRWU utxoaggTable
       <* f TableTagRO  utxoagg1Table
       <* f TableTagRO  utxoagg2Table
       <* f TableTagRO  utxoagg3Table


instance HasOnlyTables (Tables LedgerState) where
  traverse0Tables f =
    LedgerStateTables <$> f TableTagRW
                        <*> f TableTagRWU
                        <*> f TableTagRO
                        <*> f TableTagRO
                        <*> f TableTagRO

  traverse2Tables f st1 st2 =
    LedgerStateTables
      <$> f TableTagRW  (utxoTable st1) (utxoTable st2)
      <*> f TableTagRWU (utxoaggTable st1) (utxoaggTable st2)
      <*> f TableTagRO  (utxoagg1Table st1) (utxoagg1Table st2)
      <*> f TableTagRO  (utxoagg2Table st1) (utxoagg2Table st2)
      <*> f TableTagRO  (utxoagg3Table st1) (utxoagg3Table st2)

  traverse2Tables_ f st1 st2 =
     () <$ f TableTagRW  (utxoTable    st1) (utxoTable    st2)
        <* f TableTagRWU (utxoaggTable st1) (utxoaggTable st2)
        <* f TableTagRO  (utxoagg1Table st1) (utxoagg1Table st2)
        <* f TableTagRO  (utxoagg2Table st1) (utxoagg2Table st2)
        <* f TableTagRO  (utxoagg3Table st1) (utxoagg3Table st2)


instance HasOnDiskTables LedgerState where

  data Tables LedgerState table =
         LedgerStateTables {
           utxoTable     :: table TableTypeRW  TxIn TxOut,
           utxoaggTable  :: table TableTypeRWU Addr Coin,
           utxoagg1Table :: table TableTypeRO  Addr Coin,
           utxoagg2Table :: table TableTypeRO  Addr Coin,
           utxoagg3Table :: table TableTypeRO  Addr Coin
         }

  projectTables LedgerState {
                  utxos     = UTxOState {
                                utxo,
                                utxoagg
                              },
                  snapshots = UTxOSnapshots {
                                utxoagg1,
                                utxoagg2,
                                utxoagg3
                              }
                } =
    LedgerStateTables {
      utxoTable     = utxo,
      utxoaggTable  = utxoagg,
      utxoagg1Table = utxoagg1,
      utxoagg2Table = utxoagg2,
      utxoagg3Table = utxoagg3
    }

  injectTables
    LedgerStateTables {
      utxoTable,
      utxoaggTable,
      utxoagg1Table,
      utxoagg2Table,
      utxoagg3Table
    }
    LedgerState { pparams, curslot } =

    LedgerState {
      utxos     = UTxOState {
                    utxo    = utxoTable,
                    utxoagg = utxoaggTable
                  },
      snapshots = UTxOSnapshots {
                    utxoagg1 = utxoagg1Table,
                    utxoagg2 = utxoagg2Table,
                    utxoagg3 = utxoagg3Table
                  },
      pparams,
      curslot
    }

instance HasSeqNo LedgerState where
  stateSeqNo LedgerState {curslot = SlotNo s} = SeqNo (fromIntegral s)

class ShowTable (table :: TableType -> * -> * -> *) where
    showsTable :: (Show k, Show v) => table t k v -> ShowS

instance ShowTable EmptyTable where
    showsTable = shows

instance ShowTable TableKeySet where
    showsTable = shows

--instance ShowTable TableId where
--    showsTable = shows

instance ShowTable table => Show (Tables LedgerState table) where
  showsPrec _ LedgerStateTables{..} =
      showString "LedgerStateTables {\n"
    . showString "  utxoTable     = " . showsTable utxoTable     . showString ",\n"
    . showString "  utxoaggTable  = " . showsTable utxoaggTable  . showString ",\n"
    . showString "  utxoagg1Table = " . showsTable utxoagg1Table . showString ",\n"
    . showString "  utxoagg2Table = " . showsTable utxoagg2Table . showString ",\n"
    . showString "  utxoagg3Table = " . showsTable utxoagg3Table . showString ",\n"
    . showString "}"

-- | Simulate taking snapshots at epoch boundaries
--
epochSnapshotSwizzle :: Tables LedgerState table
                     -> Tables LedgerState (SnapshotOfTable table)
epochSnapshotSwizzle LedgerStateTables {
                       utxoaggTable,
                       utxoagg1Table,
                       utxoagg2Table
                     } =
    -- Take a snapshot of the current utxo aggregate.
    -- Shuffle all the older snapshots down by one.
    LedgerStateTables {
      utxoTable     = KeepTable,
      utxoaggTable  = KeepTable,
      utxoagg1Table = SnapshotOfTable utxoaggTable,
      utxoagg2Table = SnapshotOfTable utxoagg1Table,
      utxoagg3Table = SnapshotOfTable utxoagg2Table
    }

exampleKeySets :: TableKeySets LedgerState
exampleKeySets =
    LedgerStateTables {
      utxoTable     = TableKeySet (Set.singleton (TxIn (TxId 0) (TxIx 0))),
      utxoaggTable  = TableKeySet (Set.singleton (Addr 1)),
      utxoagg1Table = TableKeySet (Set.singleton (Addr 2)),
      utxoagg2Table = TableKeySet (Set.singleton (Addr 3)),
      utxoagg3Table = TableKeySet (Set.singleton (Addr 4))
    }


type ChainFragment = [Block]

findIntersection :: ChainFragment -> ChainFragment -> SeqNo LedgerState
findIntersection = undefined

applyBlockKeys :: Block -> TableKeySets LedgerState
applyBlockKeys = undefined

-- ledgerStateForBlock :: DbChangelog LedgerState
--                     -> Tables LedgerState PTMap
--                     -> LedgerState PTMap
ledgerStateForBlock lseq rs = undefined
  --   injectOnDiskMappings (zipMappings applyPTDiffMap rs d) s
  -- where
  --   s :: LedgerState EmptyMap
  --   _ FT.:> (DbExtension s _) = FT.viewr lseq

  --   d :: OnDiskMappings LedgerState DiffMap
  --   DbExtensionsMeasure _ d = FT.measure lseq

-- TODO Not adapdted yet for snapshot API

-- | A highly simplfied chain selection algorithm where we just look at a
-- single candidate chain (which we assume is longer) and just evaluate
-- if the blocks are valid. If they are, we adopt the chain.
--
-- considerChain :: Tables state TVarDbTable -- ^ The ledger state disk db
--               -> TVar ChainFragment  -- ^ Our current chain
--               -> TVar (DbChangelog state) -- ^ The ledger state in-mem extension
--                                      --   matching the current chain
--               -> ChainFragment       -- ^ The candidate chain
--               -> IO ()
-- considerChain db chainvar lseqvar candidate = do
--     -- start by reading state and finding the intersection
--     (lseq, chain) <- STM.atomically $ (,) <$> STM.readTVar lseqvar
--                                           <*> STM.readTVar chainvar
--     let intersectSlot = findIntersection chain candidate

--     result <- evaluateCandidate (rewindTableKeySets intersectSlot lseq) candidate
--     case result of
--       Nothing -> return ()
--       Just lseq' ->
--         -- Adopt the new chain and corresponding ledger state
--         STM.atomically $ do
--           STM.writeTVar chainvar candidate
--           STM.writeTVar lseqvar lseq'
--   where
--     evaluateCandidate !lseq [] =
--       -- If we got to the end without failing, we assume we've got the longer
--       -- chain now, so return the corresponding ledger state (seq)
--       return (Just lseq)

--     evaluateCandidate !lseq (b:bs) = do
--       rs <- readDb db (applyBlockKeys b)
--       case applyBlock b (ledgerStateForBlock lseq rs) of
--         Nothing -> return Nothing
--         Just ledgerstate' ->
--             evaluateCandidate lseq' bs
--           where
--             lseq' = extendDbChangelog lseq ledgerstate'

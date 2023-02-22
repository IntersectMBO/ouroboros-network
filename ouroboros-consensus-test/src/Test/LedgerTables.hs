{-# LANGUAGE FlexibleContexts #-}

module Test.LedgerTables (
    prop_stowable_laws
  , prop_tablestuff_laws
  ) where

import           Data.Function (on)

import           Ouroboros.Consensus.Ledger.Basics

import           Test.QuickCheck

-- | We compare the Ledger Tables of the result because the comparison with the
-- rest of the LedgerState takes considerably more time to run.
(==?) ::
  ( IsMapKind mk
  , HasLedgerTables (LedgerState blk)
  , Eq (LedgerTables (LedgerState blk) mk)
  , Show (LedgerTables (LedgerState blk) mk)
  )
  => LedgerState blk mk
  -> LedgerState blk mk
  -> Property
(==?) = (===) `on` projectLedgerTables

infix 4 ==?

-- | The StowableLedgerTables instances should follow these two laws:
--
-- > stow . unstow == id
--
-- > unstow . stow == id
prop_stowable_laws ::
     ( HasLedgerTables (LedgerState blk)
     , CanStowLedgerTables (LedgerState blk)
     , Eq (LedgerTables (LedgerState blk) EmptyMK)
     , Show (LedgerTables (LedgerState blk) EmptyMK)
     , Show (LedgerTables (LedgerState blk) ValuesMK)
     )
  => LedgerState blk EmptyMK
  -> LedgerState blk ValuesMK
  -> Property
prop_stowable_laws = \ls ls' ->
  stowLedgerTables (unstowLedgerTables ls) ==? ls .&&.
  unstowLedgerTables (stowLedgerTables ls') ==? ls'

-- | The TableStuff instances should follow these two laws:
--
-- > with . project == id
--
-- > project . with == id
prop_tablestuff_laws ::
     ( HasLedgerTables (LedgerState blk)
     , Eq (LedgerTables (LedgerState blk) EmptyMK)
     , Show (LedgerTables (LedgerState blk) EmptyMK)
     , Show (LedgerTables (LedgerState blk) ValuesMK)
     )
  => LedgerState blk EmptyMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> Property
prop_tablestuff_laws = \ls tbs ->
  (ls `withLedgerTables` (projectLedgerTables ls)) ==? ls .&&.
  projectLedgerTables (ls `withLedgerTables` tbs) === tbs

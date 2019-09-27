{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
module Test.Consensus.Mempool.TestBlock
  ( -- * Test block
    TestBlock (..)
  , Header(..)
    -- * Test infrastructure: ledger state
  , LedgerState (..)
  , LedgerConfig (..)
  , testInitLedger
    -- * Test infrastructure: mempool support
  , TestTxError (..)
  , GenTx (..)
  , GenTxId (..)
    -- * Re-exported
  , TestTx (..)
  , TestTxId (..)
  , testTxId
  , testTxValidate
  , singleNodeTestConfig
  ) where

import           Control.Monad.Except (runExcept)
import           Data.FingerTree (Measured (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (pattern GenesisPoint, HasHeader (..),
                     HeaderHash, Point, StandardHash)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool (ApplyTx (..))
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Consensus.Mempool.TestTx
import           Test.Util.TestBlock (singleNodeTestConfig)

{-------------------------------------------------------------------------------
  Test block
-------------------------------------------------------------------------------}

-- | Only 'ApplyTx' and parts of 'UpdateLedger' are actually required for the
-- Mempool (tests). However, these have superclasses requiring us to implement
-- many more irrelevant type class. We use this /bottom/ to implement unused
-- type class methods.
notNeeded :: HasCallStack => a
notNeeded = error "not needed for the Mempool tests"

-- | We're only using it for the 'ApplyTx' and 'UpdateLedger' instances.
data TestBlock = TestBlock deriving (StandardHash)

instance GetHeader TestBlock where
  data Header TestBlock = TestHeader
    deriving (Eq, Show, Generic, NoUnexpectedThunks)
  getHeader = notNeeded

type instance HeaderHash TestBlock = Word64

instance HasHeader TestBlock where
  blockHash      = notNeeded
  blockPrevHash  = notNeeded
  blockSlot      = notNeeded
  blockNo        = notNeeded
  blockInvariant = notNeeded

instance HasHeader (Header TestBlock) where
  blockHash      = notNeeded
  blockPrevHash  = notNeeded
  blockSlot      = notNeeded
  blockNo        = notNeeded
  blockInvariant = notNeeded

instance Measured Block.BlockMeasure TestBlock where
  measure = notNeeded

{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol TestBlock = Bft BftMockCrypto

instance SignedHeader (Header TestBlock) where
  type Signed (Header TestBlock) = ()
  headerSigned _ = notNeeded

instance HeaderSupportsBft BftMockCrypto (Header TestBlock) where
  headerBftFields = notNeeded

instance SupportedBlock TestBlock

instance UpdateLedger TestBlock where
  data LedgerState TestBlock =
      TestLedger {
          tlLastApplied :: Point TestBlock
        , tlTxIds       :: [TestTxId]
          -- ^ From new-to-old.
        }
    deriving (Show, Eq, Generic, NoUnexpectedThunks)

  data LedgerConfig TestBlock = LedgerConfig
  type LedgerError  TestBlock = ()

  ledgerConfigView _ = LedgerConfig

  applyChainTick _ _ = notNeeded

  applyLedgerBlock = notNeeded

  reapplyLedgerBlock = notNeeded

  ledgerTipPoint = tlLastApplied

testInitLedger :: LedgerState TestBlock
testInitLedger = TestLedger
  { tlLastApplied = GenesisPoint
  , tlTxIds       = []
  }

{-------------------------------------------------------------------------------
  Test infrastructure: mempool support
-------------------------------------------------------------------------------}

instance ApplyTx TestBlock where
  newtype GenTx TestBlock = TestGenTx
    { unTestGenTx :: TestTx
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (Condense)

  newtype GenTxId TestBlock = TestGenTxId
    { unTestGenTxId :: TestTxId
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (Condense)

  txId (TestGenTx tx) = TestGenTxId (testTxId tx)

  txSize _ = 2000 -- TODO #745

  type ApplyTxErr TestBlock = TestTxError

  applyTx _ (TestGenTx tx) ledger@TestLedger { tlTxIds } = do
    testTxValidate tx tlTxIds
    return ledger { tlTxIds = testTxId tx : tlTxIds }

  reapplyTx = applyTx

  reapplyTxSameState cfg tx ledger = mustBeRight $ applyTx cfg tx ledger
    where
      mustBeRight = either (error "cannot fail") id . runExcept

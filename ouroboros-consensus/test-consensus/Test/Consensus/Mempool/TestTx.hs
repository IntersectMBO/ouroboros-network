{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Test.Consensus.Mempool.TestTx (
    -- * Transactions
    TestTxId (..)
  , TestTx (..)
  , testTxId
  , TestTxError (..)
  , testTxValidate
  ) where

import           Control.Monad.Except (Except, throwError)
import           Data.Word (Word64)

import           Test.QuickCheck (Arbitrary (..), oneof)

import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Test infrastructure: test transaction
-------------------------------------------------------------------------------}

-- | A simple transaction identifier.
newtype TestTxId = TestTxId { unTestTxId :: Word64 }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Condense)

instance Arbitrary TestTxId where
  arbitrary           = TestTxId <$> arbitrary
  shrink (TestTxId w) = [TestTxId w' | w' <- shrink w]

-- | A simple transaction used to test the @Mempool@.
data TestTx
  = ValidTestTx   !TestTxId
    -- ^ Note that a 'ValidTestTx' may still be invalid: if the same
    -- transaction is already in the ledger.
  | InvalidTestTx !TestTxId
  deriving (Show, Eq, Ord)

instance Condense TestTx where
  condense = \case
    ValidTestTx       txid     -> "(Valid "   <> condense txid <> ")"
    InvalidTestTx     txid     -> "(Invalid " <> condense txid <> ")"

testTxId :: TestTx -> TestTxId
testTxId = \case
    ValidTestTx   txid -> txid
    InvalidTestTx txid -> txid

data TestTxError
  = InvalidTx
    -- ^ The transaction was an 'InvalidTestTx'.
  | TxAlreadyInChain TestTxId
    -- ^ The transaction was a 'ValidTestTx' but the same transaction is
    -- already in the ledger.
  deriving (Eq, Show)

testTxValidate :: TestTx
              -> [TestTxId]
                 -- ^ The ids of the transactions in the ledger.
              -> Except TestTxError ()
testTxValidate tx txidsInLedger = case tx of
    ValidTestTx txid
      | txid `elem` txidsInLedger
      -> throwError $ TxAlreadyInChain txid
      | otherwise
      -> return ()
    InvalidTestTx _
      -> throwError InvalidTx

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

instance Arbitrary TestTx where
  arbitrary = oneof
    [ ValidTestTx       <$> arbitrary
    , InvalidTestTx     <$> arbitrary
    ]

  shrink (ValidTestTx txid)   = [ValidTestTx txid'   | txid' <- shrink txid]
  shrink (InvalidTestTx txid) = [InvalidTestTx txid' | txid' <- shrink txid]

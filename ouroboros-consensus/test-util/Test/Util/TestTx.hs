module Test.Util.TestTx (
    -- * Transactions
    TestTxId
  , TestTx (..)
  ) where

import           Data.Word (Word64)
import           Test.QuickCheck (Arbitrary (..), oneof)

{-------------------------------------------------------------------------------
  Test infrastructure: test transaction
-------------------------------------------------------------------------------}

-- | A simple transaction identifier used in testing 'Mempool' support.
newtype TestTxId = TestTxId Word64
  deriving (Show, Eq, Ord)

instance Arbitrary TestTxId where
  arbitrary           = TestTxId <$> arbitrary
  shrink (TestTxId w) = [TestTxId w' | w' <- shrink w]

-- | A simple valid or invalid transaction used in testing 'Mempool' support.
--
-- Currently, in the tests for the basic 'Mempool' operations and properties,
-- we don't really need to know much more about a transaction other than
-- whether it is valid or invalid. At the moment, we're only testing properties
-- surrounding a basic subset of 'Mempool' functionality: When attempting to
-- add transactions to the 'Mempool', those which are valid are stored and
-- those which are invalid are dropped.
--
-- TODO:
-- This will most definitely need to be extended in order to test other
-- situations. For example, situations in which known valid transactions
-- (which are in the 'Mempool') can be dropped due to later becoming invalid
-- with respect to the current ledger state. This can happen when either that
-- transaction is already included on the chain or one of its inputs has
-- already been spent with respect to the current ledger state.
data TestTx = ValidTestTx !TestTxId
            | InvalidTestTx !TestTxId
  deriving (Show, Eq, Ord)

instance Arbitrary TestTx where
  arbitrary = oneof
    [ ValidTestTx   <$> arbitrary
    , InvalidTestTx <$> arbitrary
    ]

  shrink (ValidTestTx txid)   = [ValidTestTx txid'   | txid' <- shrink txid]
  shrink (InvalidTestTx txid) = [InvalidTestTx txid' | txid' <- shrink txid]

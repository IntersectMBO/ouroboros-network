-- | Immutable DB tests.
--
-- The immutable DB bundles a (configurable) number of blocks into "chunk files".
-- By design, chunk files are literally just the raw blocks, one after the other,
-- so that we can efficiently support binary streaming of blocks.
--
-- Every chunk file is accompanied by two indices: a _primary_ index that for
-- each slot in the chunk file provides an offset into a _secondary_ index, which
-- stores some derived information about the blocks in the chunk file for
-- improved performance. Both the primary and the secondary index can be
-- reconstructed from the chunk file itself.
--
-- The tests for the immutable DB consist of a handful of unit tests, a set of
-- property tests of the primary index, and then the main event, model based
-- checking.
--
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Test.Ouroboros.Storage.ImmutableDB.Primary as Primary
import qualified Test.Ouroboros.Storage.ImmutableDB.StateMachine as StateMachine
import           Test.Tasty (TestTree, testGroup)

{------------------------------------------------------------------------------
  The list of all tests
------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "ImmutableDB" [
      Primary.tests
    , StateMachine.tests
    ]

-- | Chain DB tests.
--
-- The chain DB is the most complicated part of the storage layer, and this is
-- visible in the tests also. We have separate tests for:
--
-- * The GC schedule: the chain database occassionally garbage collects blocks
--   from the volatile DB that it is sure it won't need anymore (because adopting
--   them would result in switching to a fork that is too distant from our own
--   chain).
-- * Iterators. Iterators are cursors that allow to read a sequence of blocks
--   from the DB. The iterators provided by the chain DB can span both the
--   immutable DB and the volatile DB, some of those blocks might be /moved/
--   from the volatile DB /to/ the immutable DB during the iterator's life-time,
--   and some of those blocks might be garbage collected from the volatile DB.
--   This makes the iterator code quite complex, having to deal with a lot of
--   edge cases. Their main tests are part of the model based test, but we also
--   test some specific properties of the iterators themselves.
-- * The model of the chain DB /itself/ is reasonably complex also, and so we
--   have some properties that verify that that the model behaves the way we
--   think it should.
-- * The main DB itself.
--
module Test.Ouroboros.Storage.ChainDB (tests) where

import           System.Info (os)

import           Test.Tasty

import qualified Test.Ouroboros.Storage.ChainDB.FollowerPromptness as FollowerPromptness
import qualified Test.Ouroboros.Storage.ChainDB.GcSchedule as GcSchedule
import qualified Test.Ouroboros.Storage.ChainDB.Iterator as Iterator
import qualified Test.Ouroboros.Storage.ChainDB.Model.Test as Model
import qualified Test.Ouroboros.Storage.ChainDB.Paths as Paths
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as StateMachine
import qualified Test.Ouroboros.Storage.ChainDB.Unit as Unit

tests :: TestTree
tests = testGroup "ChainDB" $ [
      Iterator.tests
    , FollowerPromptness.tests
    , GcSchedule.tests
    , Model.tests
    , Paths.tests
    , Unit.tests
    ] <>
    -- The ChainDB q-s-m test is flaky on Windows, see
    -- https://github.com/input-output-hk/ouroboros-network/issues/3874
    [ StateMachine.tests | os /= "mingw32" ]

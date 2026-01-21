{-# LANGUAGE NamedFieldPuns #-}

module Test.Ouroboros.Network.TxSubmission.Mempool.Simple
  (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime (MonadTime)
import Control.Monad.IOSim

import Data.List (nub, nubBy)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.Set qualified as Set
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..), MempoolSeq (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission.Mempool.Simple"
        [ testProperty "mempool writer" prop_mempool_writer
        ]


-- | `Tx` is a tuple of `txid` and a bool representing validity of the tx
--
data Tx   = Tx { getTxId    :: TxId,
                 getTxValid :: Bool
               }
  deriving (Show, Eq)
type TxId = Int

data ValidtionError = DuplicateTxId
                    | InvalidTx
  deriving Show


instance Arbitrary Tx where
  arbitrary = Tx <$> arbitrary
                 <*> arbitrary
  shrink tx@Tx { getTxId, getTxValid } =
    [ tx { getTxId = getTxId' }
    | getTxId' <- shrink getTxId
    ]
    ++
    [ tx { getTxValid = getTxValid' }
    | getTxValid' <- shrink getTxValid
    ]

-- | A list of valid tx's
newtype MempoolTxs = MempoolTxs [Tx]
  deriving Show

instance Arbitrary MempoolTxs where
  arbitrary = do
    txids <- arbitrary
    return $ MempoolTxs [ Tx txid True | txid <- nub txids ]
  shrink (MempoolTxs txs) = MempoolTxs <$> shrinkList (const []) txs


prop_mempool_writer
  :: MempoolTxs
  -> [Tx]
  -> Property
prop_mempool_writer (MempoolTxs mempoolTxs) candidateTxs =
    runSimOrThrow sim
  where
    candidateTxIdsSet = Set.fromList [ getTxId tx | tx <- candidateTxs ]
    -- candidateTxsMap = Map.fromList [ (getTxId tx, tx) | tx <- candidateTxs ]

    sim :: (MonadSTM m, MonadTime m)
        => m Property
    sim = do
      mempool@(Mempool mempoolVar) <- Mempool.new getTxId mempoolTxs
      let writer = Mempool.getWriter
                     DuplicateTxId
                     getTxId
                     (\_ txs -> pure [ case tx of
                                         Tx { getTxValid = False } -> Left (getTxId tx, InvalidTx)
                                         Tx { getTxValid = True }  -> Right tx
                                     | tx <- txs
                                     ])
                     (\_ -> pure ())
                     mempool
      (accepted, rejected) <- Mempool.mempoolAddTxs writer candidateTxs
      let acceptedSet = Set.fromList accepted
          rejectedSet = Set.fromList (fst <$> rejected)
      mempoolTxs' <- Mempool.read mempool
      MempoolSeq { mempoolSeq, nextIdx } <- readTVarIO mempoolVar
      let indices = Mempool.getIdx <$> Foldable.toList mempoolSeq
      return . counterexample ("accepted " ++ show accepted)
             . counterexample ("rejected " ++ show rejected)
             . counterexample ("mempoolTxs' " ++ show mempoolTxs')
             $ counterexample "acceptedSet not a subset of candidateTxIdsSet"
               (acceptedSet `Set.isSubsetOf` candidateTxIdsSet)
          .&&. counterexample "rejectedSet not a subset of candidateTxIdsSet"
               (rejectedSet `Set.isSubsetOf` candidateTxIdsSet)
          .&&. counterexample "acceptedSet and rejectedSet does not sum up to the candidateTxIdsSet"
                              ((acceptedSet `Set.union` rejectedSet) === candidateTxIdsSet)
          .&&. counterexample "all txs in the mempool are valid"
                              (all getTxValid mempoolTxs')
          .&&. counterexample "no duplicate txids in the mempool"
                              (nubBy (on (==) getTxId) mempoolTxs' === mempoolTxs')
          .&&. counterexample "number of accepted and rejected txs is not equal to the number of candidate txs"
                              (length accepted + length rejected === length candidateTxs)
          .&&. counterexample "indices are distinct"
                              (indices === nub indices)
          .&&. counterexample "nextIdx is correct"
                              (if null mempoolSeq
                                 then nextIdx === 0
                                 else nextIdx === maximum indices + 1
                              )

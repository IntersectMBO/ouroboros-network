module Test.Cardano.Network.Diffusion.Testnet.ChainedTxs
  ( ChainedPeerTxs (..)
  , tests
  ) where

import Data.Function (on)
import Data.List as List (foldl', nub, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Test.Ouroboros.Network.TxSubmission.Types (Tx (..), TxId)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


-- | A randomly generated transaction forest distributed across two peers,
-- used by chain-integrity tests at the diffusion layer.
--
-- Each tx may carry a parent pointer ('getTxParent') to any earlier tx in
-- the forest, with some probability of having no parent at all. Invalidity
-- propagates down the chain at generation time: a descendant of an invalid
-- tx is itself generated as invalid, matching mainnet semantics where a tx
-- that consumes an invalid parent's output is itself invalid by construction.
--
-- Peer assignment reflects realistic mainnet conditions:
--
--   * Each peer carries a random subset of the forest (peers may lack txs
--     that, for example, were already included in an adopted block).
--   * Well-behaved peers (the common case) advertise their subset in
--     chain-topological order: parents before children.
--   * Adversarial or buggy peers (the occasional case) advertise in a
--     shuffled order to stress V2's handling of out-of-order streams.
--   * Every tx is carried by at least one /well-behaved/ peer so the full
--     forest is reachable via the reliable path, even when adversarial
--     peers misorder or mishandle their share.
data ChainedPeerTxs = ChainedPeerTxs {
    chainedTxsA :: [Tx TxId]
  , chainedTxsB :: [Tx TxId]
  }
  deriving Show

instance Arbitrary ChainedPeerTxs where
  arbitrary = do
    chainLen <- choose (3, 15)
    chain    <- genChainedTxs chainLen
    perPeer  <- distributeAcrossPeers 2 chain
    case perPeer of
      [txsA, txsB] -> return (ChainedPeerTxs txsA txsB)
      _            -> error "ChainedPeerTxs: distributeAcrossPeers invariant broken"

  shrink (ChainedPeerTxs txsA txsB) =
    [ ChainedPeerTxs (dropDoomed txsA) (dropDoomed txsB)
    | tid <- nub (map getTxId (txsA ++ txsB))
    , let doomed    = transitiveDescendants tid (txsA ++ txsB)
          dropDoomed = filter (\t -> getTxId t `Set.notMember` doomed)
    ]
    where
      -- | All txids reachable as descendants of @root@, including @root@
      -- itself. Used so that dropping a parent also drops every
      -- transitive child, avoiding dangling parent pointers in the
      -- shrunken value.
      transitiveDescendants :: TxId -> [Tx TxId] -> Set TxId
      transitiveDescendants root txs =
        let children p = [ getTxId t | t <- txs, getTxParent t == Just p ]
            go acc p
              | p `Set.member` acc = acc
              | otherwise          = List.foldl' go (Set.insert p acc) (children p) in
        go Set.empty root


-- | Generate a forest of @n@ transactions.
--
-- Tx 0 has no parent. Each subsequent tx picks its parent uniformly from
-- @{Nothing}@ unioned with the ids of earlier txs, weighted so a parent
-- link is more common than no parent. Invalidity is propagated: if the
-- parent is invalid, the tx is also invalid regardless of its own drawn
-- validity.
genChainedTxs :: Int -> Gen [Tx TxId]
genChainedTxs n = go 0 Map.empty []
  where
    baseId :: TxId
    baseId = 1

    go :: Int -> Map TxId Bool -> [Tx TxId] -> Gen [Tx TxId]
    go i validMap acc
      | i >= n = return (reverse acc)
      | otherwise = do
          let txid = baseId + i
          parent <- if i == 0
                      then return Nothing
                      else frequency
                             [ (1, return Nothing)
                             , (3, Just . (baseId +) <$> choose (0, i - 1))
                             ]
          ownValid <- frequency [ (3, return True), (1, return False) ]
          size     <- chooseEnum (0, 1024)
          let parentValid    = maybe True (validMap Map.!) parent
              effectiveValid = ownValid && parentValid
              tx = Tx
                { getTxId      = txid
                , getTxSize    = size
                , getTxAdvSize = size
                , getTxValid   = effectiveValid
                , getTxParent  = parent
                }
          go (i + 1) (Map.insert txid effectiveValid validMap) (tx : acc)


-- | Per-peer behaviour selected up front so chain-coverage constraints
-- can be satisfied against the well-behaved subset before any lists are
-- emitted.
data PeerBehaviour = WellBehaved | Adversarial
  deriving (Eq, Show)

-- | Distribute a forest across @peerCount@ peers.
--
-- Each peer is classified as well-behaved (advertises in chain-topological
-- order, the common case) or adversarial (advertises in a shuffled order,
-- the occasional case).  To keep the property invariant clean, every tx
-- is guaranteed to be carried by at least one /well-behaved/ peer:
-- adversarial peers add noise and races but cannot singly strand a tx.  At
-- least one peer is always well-behaved.
--
-- Adversarial peers may duplicate txs already carried by well-behaved
-- peers; this exercises V2's cross-peer retry path when an adversarial
-- peer's out-of-order delivery causes a rejection.
distributeAcrossPeers :: Int -> [Tx TxId] -> Gen [[Tx TxId]]
distributeAcrossPeers peerCount chain = do
  behaviours <- ensureSomeWellBehaved <$>
                  vectorOf peerCount
                    (frequency [ (4, pure WellBehaved)
                               , (1, pure Adversarial) ])

  subsets <- vectorOf peerCount (sublistOf chain)

  let wellBehavedIxs =
        [ i | (i, WellBehaved) <- zip [0..] behaviours ]
      wellBehavedCov =
        Set.unions
          [ Set.fromList (map getTxId (subsets !! i))
          | i <- wellBehavedIxs ]
      uncovered =
        [ t | t <- chain, getTxId t `Set.notMember` wellBehavedCov ]
  additions <- traverse
                 (\t -> do i <- elements wellBehavedIxs; pure (i, t))
                 uncovered

  let subsetsWithCoverage :: [[Tx TxId]]
      subsetsWithCoverage =
        zipWith
          (\i base -> base ++ [ t | (p, t) <- additions, p == i ])
          [0 .. peerCount - 1]
          subsets

      inChainOrder :: Set TxId -> [Tx TxId]
      inChainOrder peerSet =
        filter (\t -> getTxId t `Set.member` peerSet) chain

  traverse
    (\(b, s) -> case b of
        WellBehaved -> pure (inChainOrder (Set.fromList (map getTxId s)))
        Adversarial -> shuffle s)
    (zip behaviours subsetsWithCoverage)
  where
    ensureSomeWellBehaved bs
      | WellBehaved `elem` bs = bs
      | otherwise             = WellBehaved : drop 1 bs


--
-- Meta-tests: verify generator and shrinker invariants.
--

-- | Every parent pointer in the combined peer lists resolves to a tx that
-- also appears somewhere in the union. No dangling parents.
prop_parentsResolvable :: ChainedPeerTxs -> Bool
prop_parentsResolvable (ChainedPeerTxs txsA txsB) =
  let ids     = Set.fromList (map getTxId (txsA ++ txsB))
      parents = [ p | t <- txsA ++ txsB, Just p <- [getTxParent t] ] in
  all (`Set.member` ids) parents

-- | Invalidity propagates along the dependency chain: if a tx's parent is
-- present in the union and invalid, the tx itself must be invalid too.
prop_invalidityPropagates :: ChainedPeerTxs -> Bool
prop_invalidityPropagates (ChainedPeerTxs txsA txsB) =
  let allTxs = nubBy ((==) `on` getTxId) (txsA ++ txsB)
      txMap  = Map.fromList [(getTxId t, t) | t <- allTxs] in
  all (\t -> case getTxParent t of
               Nothing -> True
               Just p  -> case Map.lookup p txMap of
                 Nothing -> True
                 Just pt -> getTxValid pt || not (getTxValid t))
                         -- not (getTxValid pt) => not (getTxValid t)
      allTxs

-- | At least one peer's tx list is in chain-topological order (parents
-- before children), and the union of all such peers' txids covers every
-- tx in the value. This is the external face of the "every tx is carried
-- by at least one well-behaved peer" guarantee from
-- 'distributeAcrossPeers': adversarial peers may exist and misorder, but
-- the full forest is always reachable via the chain-ordered subset.
prop_wellBehavedCoverage :: ChainedPeerTxs -> Property
prop_wellBehavedCoverage (ChainedPeerTxs txsA txsB) =
  let allIds          = Set.fromList (map getTxId (txsA ++ txsB))
      chainOrdered    = filter isChainOrdered [txsA, txsB]
      coverage        = Set.unions
                          [ Set.fromList (map getTxId txs) | txs <- chainOrdered ] in
  counterexample ("peer A: " ++ show txsA)
   $ counterexample ("peer B: " ++ show txsB)
   $ counterexample ("chain-ordered peers: " ++ show (length chainOrdered))
   $ counterexample ("coverage: " ++ show (Set.toList coverage))
   $ counterexample ("all ids: "  ++ show (Set.toList allIds))
   $ property (not (null chainOrdered) && coverage == allIds)
  where
    isChainOrdered :: [Tx TxId] -> Bool
    isChainOrdered txs =
      let positions = Map.fromList (zip (map getTxId txs) [(0 :: Int) ..]) in
      all (\t -> case getTxParent t of
                   Nothing -> True
                   Just p  -> case Map.lookup p positions of
                     Nothing   -> True
                     Just ppos -> ppos < positions Map.! getTxId t)
          txs


-- | Every shrink of a value satisfies the same structural invariants as
-- a freshly generated one.
prop_shrinkPreservesInvariants :: ChainedPeerTxs -> Property
prop_shrinkPreservesInvariants cpt =
  conjoin
    [ counterexample ("shrunk: " ++ show s) $
           prop_parentsResolvable s
      .&&. prop_invalidityPropagates s
      .&&. prop_wellBehavedCoverage s
    | s <- shrink cpt
    ]

-- | Each shrink strictly reduces the total tx count so shrinking converges.
prop_shrinkMakesProgress :: ChainedPeerTxs -> Property
prop_shrinkMakesProgress cpt =
  let size (ChainedPeerTxs a b) = length a + length b
      origSize                  = size cpt in
  conjoin
    [ counterexample ("shrunk: " ++ show s) (size s < origSize)
    | s <- shrink cpt
    ]

tests :: TestTree
tests = testGroup "ChainedTxs"
  [ testProperty "parents resolvable"          prop_parentsResolvable
  , testProperty "invalidity propagates"       prop_invalidityPropagates
  , testProperty "well-behaved coverage"       prop_wellBehavedCoverage
  , testProperty "shrink preserves invariants" prop_shrinkPreservesInvariants
  , testProperty "shrink makes progress"       prop_shrinkMakesProgress
  ]

{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.InjectTxs (
    InjectTx (..)
  , cannotInjectTx
  , matchTx
  , matchTxNS
  , matchTxsNS
  ) where

import           Data.Bifunctor
import           Data.Functor.Product
import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Util (pairFst)

{-------------------------------------------------------------------------------
  Match a transaction with a ledger
-------------------------------------------------------------------------------}

data InjectTx blk blk' = InjectTx {
      injectTxWith :: GenTx blk  -> Maybe (GenTx blk')
    }

cannotInjectTx :: InjectTx blk blk'
cannotInjectTx = InjectTx $ const Nothing

-- | Match transaction with a ledger, attempting to inject where possible
matchTx' ::
     InPairs InjectTx xs
  -> NS GenTx xs
  -> Telescope g f xs
  -> Either (Mismatch GenTx f xs)
            (Telescope g (Product GenTx f) xs)
matchTx' = go
  where
    go :: InPairs InjectTx xs
       -> NS GenTx xs
       -> Telescope g f xs
       -> Either (Mismatch GenTx f xs)
                 (Telescope g (Product GenTx f) xs)
    go _            (Z x) (TZ f)   = Right $ TZ (Pair x f)
    go (PCons _ is) (S x) (TS g f) = bimap MS (TS g) $ go is x f
    go _            (S x) (TZ f)   = Left $ MR x f
    go (PCons i is) (Z x) (TS g f) =
        case injectTxWith i x of
          Nothing -> Left $ ML x (Telescope.tip f)
          Just x' -> bimap MS (TS g) $ go is (Z x') f

matchTx ::
     SListI xs
  => InPairs InjectTx xs
  -> NS GenTx xs
  -> HardForkState f xs
  -> Either (Mismatch GenTx (Current f) xs)
            (HardForkState (Product GenTx f) xs)
matchTx is tx =
      fmap (HardForkState . hmap distrib)
    . matchTx' is tx
    . getHardForkState
  where
    distrib :: Product GenTx (Current f) blk -> Current (Product GenTx f) blk
    distrib (Pair tx' Current{..}) = Current {
          currentStart = currentStart
        , currentState = Pair tx' currentState
        }

-- | Match transaction with an 'NS', attempting to inject where possible
matchTxNS ::
     InPairs InjectTx xs
  -> NS GenTx xs
  -> NS f xs
  -> Either (Mismatch GenTx f xs)
            (NS (Product GenTx f) xs)
matchTxNS = go
  where
    go :: InPairs InjectTx xs
       -> NS GenTx xs
       -> NS f xs
       -> Either (Mismatch GenTx f xs)
                 (NS (Product GenTx f) xs)
    go _            (Z x) (Z f) = Right $ Z (Pair x f)
    go (PCons _ is) (S x) (S f) = bimap MS S $ go is x f
    go _            (S x) (Z f) = Left $ MR x f
    go (PCons i is) (Z x) (S f) =
        case injectTxWith i x of
          Nothing -> Left $ ML x f
          Just x' -> bimap MS S $ go is (Z x') f

-- | Match a list of transactions with an 'NS, attempting to inject where
-- possible
matchTxsNS ::
     forall f xs. SListI xs
  => InPairs InjectTx xs
  -> NS f xs
  -> [NS GenTx xs]
  -> ([Mismatch GenTx f xs], NS (Product f ([] :.: GenTx)) xs)
matchTxsNS is ns = go
  where
    go :: [NS GenTx xs]
       -> ([Mismatch GenTx f xs], NS (Product f ([] :.: GenTx)) xs)
    go []       = ([], hmap (`Pair` Comp []) ns)
    go (tx:txs) =
      let (mismatched, matched) = go txs
      in case matchTxNS is tx matched of
           Left  err      -> (hmap pairFst err : mismatched, matched)
           Right matched' -> (mismatched, insert matched')

    insert :: NS (Product GenTx (Product f ([] :.: GenTx))) xs
           -> NS (Product f ([] :.: GenTx)) xs
    insert = hmap $ \(Pair tx (Pair f (Comp txs))) -> Pair f (Comp (tx:txs))

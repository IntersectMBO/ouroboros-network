{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}

module Ouroboros.Consensus.HardFork.Combinator.InjectTxs (
    InjectTx(..)
  , cannotInjectTx
  , matchTx
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

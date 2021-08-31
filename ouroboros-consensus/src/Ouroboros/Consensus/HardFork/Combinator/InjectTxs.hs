{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Injecting a transaction from one block type to another
module Ouroboros.Consensus.HardFork.Combinator.InjectTxs (
    -- * Polymorphic
    InjectPolyTx (..)
  , cannotInjectPolyTx
  , matchPolyTx
  , matchPolyTxsNS
    -- * Unvalidated transactions
  , InjectTx
  , cannotInjectTx
  , matchTx
  , pattern InjectTx
    -- * Validated transactions
  , InjectValidatedTx
  , cannotInjectValidatedTx
  , matchValidatedTx
  , matchValidatedTxsNS
  , pattern InjectValidatedTx
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
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (pairFst)

{-------------------------------------------------------------------------------
  Polymorphic definitions
-------------------------------------------------------------------------------}

-- | @tx@ is either 'GenTx' or 'WrapValidatedGenTx'
--
-- See 'InjectTx' and 'InjectValidatedTx', respectively.
data InjectPolyTx tx blk blk' = InjectPolyTx {
      injectTxWith :: tx blk  -> Maybe (tx blk')
    }

-- | The injection that always fails
cannotInjectPolyTx :: InjectPolyTx tx blk blk'
cannotInjectPolyTx = InjectPolyTx $ const Nothing

-- | Match transaction with a telescope, attempting to inject where possible
matchPolyTx' ::
     InPairs (InjectPolyTx tx) xs
  -> NS tx xs
  -> Telescope g f xs
  -> Either (Mismatch tx f xs)
            (Telescope g (Product tx f) xs)
matchPolyTx' = go
  where
    go :: InPairs (InjectPolyTx tx) xs
       -> NS tx xs
       -> Telescope g f xs
       -> Either (Mismatch tx f xs)
                 (Telescope g (Product tx f) xs)
    go _            (Z x) (TZ f)   = Right $ TZ (Pair x f)
    go (PCons _ is) (S x) (TS g f) = bimap MS (TS g) $ go is x f
    go _            (S x) (TZ f)   = Left $ MR x f
    go (PCons i is) (Z x) (TS g f) =
        case injectTxWith i x of
          Nothing -> Left $ ML x (Telescope.tip f)
          Just x' -> bimap MS (TS g) $ go is (Z x') f

matchPolyTx ::
     SListI xs
  => InPairs (InjectPolyTx tx) xs
  -> NS tx xs
  -> HardForkState f xs
  -> Either (Mismatch tx (Current f) xs)
            (HardForkState (Product tx f) xs)
matchPolyTx is tx =
      fmap (HardForkState . hmap distrib)
    . matchPolyTx' is tx
    . getHardForkState
  where
    distrib :: Product tx (Current f) blk -> Current (Product tx f) blk
    distrib (Pair tx' Current{..}) = Current {
          currentStart = currentStart
        , currentState = Pair tx' currentState
        }

-- | Match transaction with an 'NS', attempting to inject where possible
matchPolyTxNS ::
     InPairs (InjectPolyTx tx) xs
  -> NS tx xs
  -> NS f xs
  -> Either (Mismatch tx f xs)
            (NS (Product tx f) xs)
matchPolyTxNS = go
  where
    go :: InPairs (InjectPolyTx tx) xs
       -> NS tx xs
       -> NS f xs
       -> Either (Mismatch tx f xs)
                 (NS (Product tx f) xs)
    go _            (Z x) (Z f) = Right $ Z (Pair x f)
    go (PCons _ is) (S x) (S f) = bimap MS S $ go is x f
    go _            (S x) (Z f) = Left $ MR x f
    go (PCons i is) (Z x) (S f) =
        case injectTxWith i x of
          Nothing -> Left $ ML x f
          Just x' -> bimap MS S $ go is (Z x') f

-- | Match a list of transactions with an 'NS', attempting to inject where
-- possible
matchPolyTxsNS ::
     forall tx f xs. SListI xs
  => InPairs (InjectPolyTx tx) xs
  -> NS f xs
  -> [NS tx xs]
  -> ([Mismatch tx f xs], NS (Product f ([] :.: tx)) xs)
matchPolyTxsNS is ns = go
  where
    go :: [NS tx xs]
       -> ([Mismatch tx f xs], NS (Product f ([] :.: tx)) xs)
    go []       = ([], hmap (`Pair` Comp []) ns)
    go (tx:txs) =
      let (mismatched, matched) = go txs
      in case matchPolyTxNS is tx matched of
           Left  err      -> (hmap pairFst err : mismatched, matched)
           Right matched' -> (mismatched, insert matched')

    insert :: NS (Product tx (Product f ([] :.: tx))) xs
           -> NS (Product f ([] :.: tx)) xs
    insert = hmap $ \(Pair tx (Pair f (Comp txs))) -> Pair f (Comp (tx:txs))

{-------------------------------------------------------------------------------
  Monomorphic aliases
-------------------------------------------------------------------------------}

type InjectTx = InjectPolyTx GenTx

-- | 'InjectPolyTx' at type 'InjectTx'
pattern InjectTx :: (GenTx blk -> Maybe (GenTx blk')) -> InjectTx blk blk'
pattern InjectTx f = InjectPolyTx f

-- | 'cannotInjectPolyTx' at type 'InjectTx'
cannotInjectTx :: InjectTx blk blk'
cannotInjectTx = cannotInjectPolyTx

-- | 'matchPolyTx' at type 'InjectTx'
matchTx ::
     SListI xs
  => InPairs InjectTx xs
  -> NS GenTx xs
  -> HardForkState f xs
  -> Either (Mismatch GenTx (Current f) xs)
            (HardForkState (Product GenTx f) xs)
matchTx = matchPolyTx

-----

type InjectValidatedTx = InjectPolyTx WrapValidatedGenTx

-- | 'InjectPolyTx' at type 'InjectValidatedTx'
pattern InjectValidatedTx ::
     (WrapValidatedGenTx blk -> Maybe (WrapValidatedGenTx blk'))
  -> InjectValidatedTx blk blk'
pattern InjectValidatedTx f = InjectPolyTx f

-- | 'cannotInjectPolyTx' at type 'InjectValidatedTx'
cannotInjectValidatedTx :: InjectValidatedTx blk blk'
cannotInjectValidatedTx = cannotInjectPolyTx

-- | 'matchPolyTx' at type 'InjectValidatedTx'
matchValidatedTx ::
     SListI xs
  => InPairs InjectValidatedTx xs
  -> NS WrapValidatedGenTx xs
  -> HardForkState f xs
  -> Either (Mismatch WrapValidatedGenTx (Current f) xs)
            (HardForkState (Product WrapValidatedGenTx f) xs)
matchValidatedTx = matchPolyTx

-- | 'matchPolyTxsNS' at type 'InjectValidatedTx'
matchValidatedTxsNS ::
     forall f xs. SListI xs
  => InPairs InjectValidatedTx xs
  -> NS f xs
  -> [NS WrapValidatedGenTx xs]
  -> ([Mismatch WrapValidatedGenTx f xs], NS (Product f ([] :.: WrapValidatedGenTx)) xs)
matchValidatedTxsNS = matchPolyTxsNS

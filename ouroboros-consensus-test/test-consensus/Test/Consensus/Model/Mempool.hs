{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Consensus.Model.Mempool where

import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Realized, RunModel (..), StateModel (..))

data Mempool = Mempool {transactions :: [SimpleTx]}
    deriving (Show)

type SimpleTx = Int

instance StateModel Mempool where
    data Action Mempool a where
        AddTxs :: [SimpleTx] -> Action Mempool ()
        -- | An 'observation' that checks the given list of transactions is part of
        -- the mempool
        HasValidatedTxs :: [SimpleTx] -> Action Mempool ()
        -- | An action to explicitly wait some amount of time
        Wait :: Int -> Action Mempool ()

    arbitraryAction = error "not implemented"

    initialState = Mempool []

deriving instance Show (Action Mempool a)
deriving instance Eq (Action Mempool a)

instance DynLogicModel Mempool

newtype RunMonad m a = RunMonad {runMonad :: m a}
    deriving (Functor, Applicative, Monad)

type instance Realized (RunMonad m) a = a

instance Monad m => RunModel Mempool (RunMonad m) where
    perform _st _act _env = error "not implemented"

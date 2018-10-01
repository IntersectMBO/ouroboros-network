{-# LANGUAGE ScopedTypeVariables #-}
module Node where

import Data.List
import Data.Graph
import Data.Maybe (listToMaybe, catMaybes)
import Control.Applicative
import Control.Monad

import MonadClass
import Block
import qualified Chain
import           Chain (Chain (..), Point)
import qualified ConsumerProtocol as CP

import qualified SimSTM as Sim

import Test.QuickCheck


longestChainSelection :: forall block m stm. MonadSTM m stm
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (Chain block)
                      -> m ()
longestChainSelection candidateChainVars currentChainVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: stm ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      currentChain    <- readTVar currentChainVar
      case longerChain (Chain.length currentChain) (catMaybes candidateChains) of
        Nothing -> retry
        Just c  -> writeTVar currentChainVar c

    longerChain :: Int -> [(Chain block)] -> Maybe (Chain block)
    longerChain curlen = listToMaybe . filter (\c -> Chain.length c > curlen)


chainValidation :: forall block m stm. (HasHeader block, MonadSTM m stm)
                => TVar m (Chain block)
                -> TVar m (Maybe (Chain block))
                -> m ()
chainValidation peerChainVar candidateChainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (atomically (update st))
  where
    update :: TVar m Point -> stm ()
    update stateVar = do
      peerChain      <- readTVar peerChainVar
      candidatePoint <- readTVar stateVar
      check (Chain.headPoint peerChain /= candidatePoint)
      writeTVar stateVar (Chain.headPoint peerChain)
      let candidateChain | Chain.valid peerChain = Just peerChain
                         | otherwise             = Nothing
      writeTVar candidateChainVar (candidateChain)


chainTransferProtocol :: forall block m stm.
                         ( HasHeader block
                         , MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> TVar m (Chain block)
                      -> TVar m (Chain block)
                      -> m ()
chainTransferProtocol delay inputVar outputVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      input <- atomically $ do
                input    <- readTVar inputVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint input /= curPoint)
                writeTVar stateVar (Chain.headPoint input)
                return input
      timer delay $ atomically $ writeTVar outputVar input


chainGenerator :: forall block m stm.
                  ( MonadSTM m stm
                  , MonadTimer m
                  )
               => Duration (Time m)
               -> Chain block
               -> m (TVar m (Chain block))
chainGenerator offset chain = do
    outputVar <- atomically (newTVar Genesis)
    sequence_ [ timer (offset + fromIntegral n) (atomically (writeTVar outputVar v))
              | (v, n) <- zip (inits chain) [0,2..] ]
    return outputVar
  where
    inits = reverse
          . unfoldr (\c -> case c of
                              Genesis -> Nothing
                              _       -> Just (c, Chain.drop 1 c))


observeChain :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                , MonadSay m
                )
             => String
             -> TVar m (Chain block)
             -> m ()
observeChain labelPrefix chainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      chain <- atomically $ do
                chain    <- readTVar chainVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint chain /= curPoint)
                writeTVar stateVar (Chain.headPoint chain)
                return chain
      say (labelPrefix ++ ": " ++ show (Chain.length chain, Chain.headPoint chain))


nodeExample1 :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                , MonadTimer m
                , MonadSay m
                )
             => (Chain block)
             -> (Chain block)
             -> m ()
nodeExample1 c1 c2 = do
    generatorVar1 <- chainGenerator 1 c1
    generatorVar2 <- chainGenerator 2 c2

    peerChainVar1 <- atomically $ newTVar Genesis
    peerChainVar2 <- atomically $ newTVar Genesis

    candidateChainVar1 <- atomically $ newTVar Nothing
    candidateChainVar2 <- atomically $ newTVar Nothing
    let candidateChainVars = [candidateChainVar1, candidateChainVar2]

    currentChainVar <- atomically $ newTVar Genesis

    fork $ chainTransferProtocol 1 generatorVar1 peerChainVar1
    fork $ chainTransferProtocol 2 generatorVar2 peerChainVar2

    fork $ chainValidation peerChainVar1 candidateChainVar1
    fork $ chainValidation peerChainVar2 candidateChainVar2

    fork $ longestChainSelection candidateChainVars currentChainVar

    fork $ observeChain "generatorVar1" generatorVar1
    fork $ observeChain "generatorVar2" generatorVar2
    fork $ observeChain "currentChain " currentChainVar

relayNode :: forall block m stm.
        ( HasHeader block
        , MonadSTM m stm
        , MonadTimer m
        , MonadSay m
        )
     => [TVar m (Chain block)] -- ^ inputs
     -> [TVar m (Chain block)] -- ^ outputs
     -> m ()
relayNode inputs outputs = undefined

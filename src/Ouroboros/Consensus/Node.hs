{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Node (
    NodeId(..)
  , RelayNode(..)
  , relayNode
  ) where

import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain hiding (selectChain)
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Protocol
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Orphans ()

-- TODO: We currently stil import /some/ stuff from the network layer Node
-- module. We should audit this and perhaps move these to different modules
-- in the networking layer to make the division clearer.

import           Protocol.Channel
import           Protocol.Transition

import           Ouroboros.Network.Node (NodeId (..))

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data RelayNode m pid cid b = RelayNode {
      -- | Register a new upstream node (chain producer)
      registerProducer :: Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                       -> pid
                       -> m ()

      -- | Register a new downstream node (chain consumer)
    , registerConsumer :: Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                       -> cid
                       -> m ()
    }

-- FIXME is this used? I have put errors in
-- intRegisterConsumer and intRegisterProducer but tests still pass.
-- Apparently tests use the relayNode from Ouroboros.Network.Node
-- Let's sort this out.
relayNode :: ( MonadSTM m
             , MonadSay m
             , ProtocolLedgerView b
             , Show b
             , Show cid
             , Show pid
             , Ord pid
             )
          => NodeId
          -> NodeConfig (BlockProtocol b)
          -> ExtLedgerState b
          -> Chain b
          -> m (RelayNode m pid cid b)
relayNode nid cfg initLedgerState initChain = do
    cpsVar   <- atomically $ newTVar (initChainProducerState initChain)
    upstream <- atomically $ newTVar Map.empty
    fork $ chainSelection cfg upstream cpsVar
    return RelayNode {
        registerProducer = intRegisterProducer nid cfg initLedgerState upstream
      , registerConsumer = intRegisterConsumer nid cpsVar
      }

{-------------------------------------------------------------------------------
  Dealing with consumers (downstream nodes)
-------------------------------------------------------------------------------}

intRegisterConsumer :: ( MonadSTM m
                       , MonadSay m
                       , HasHeader b
                       , Show b
                       , Show cid
                       )
                    => NodeId
                    -> TVar m (ChainProducerState b)
                    -> Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                    -> cid
                    -> m ()
intRegisterConsumer nid cpsVar chan cid = error "intRegisterConsumer"
{-
    fork $ producerSideProtocol1 (exampleProducer cpsVar)
             (loggingSend (ProducerId nid cid) (sendMsg chan))
             (loggingRecv (ProducerId nid cid) (recvMsg chan))
-}

{-------------------------------------------------------------------------------
  Dealing with producers (upstream nodes)
-------------------------------------------------------------------------------}

intRegisterProducer :: ( MonadSTM m
                       , MonadSay m
                       , ProtocolLedgerView b
                       , Show b
                       , Show pid
                       , Ord pid
                       )
                    => NodeId
                    -> NodeConfig (BlockProtocol b)
                    -> ExtLedgerState b
                    -> TVar m (Map pid (TVar m (Maybe (Chain b))))
                    -> Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                    -> pid
                    -> m ()
intRegisterProducer nid cfg initLedgerState upstream chan pid = error "intRegisterProducer" {- do
    chainVar     <- atomically $ newTVar Genesis
    candidateVar <- atomically $ newTVar Nothing
    fork $ consumerSideProtocol1 (exampleConsumer chainVar)
             (loggingSend (ConsumerId nid pid) (sendMsg chan))
             (loggingRecv (ConsumerId nid pid) (recvMsg chan))
    fork $ chainValidation cfg initLedgerState chainVar candidateVar
    atomically $ modifyTVar' upstream $ Map.insert pid candidateVar -}

chainValidation :: forall b m. (MonadSTM m, ProtocolLedgerView b)
                => NodeConfig (BlockProtocol b)
                -> ExtLedgerState b
                -> TVar m (Chain b)
                -> TVar m (Maybe (Chain b))
                -> m ()
chainValidation cfg initLedgerState chainVar candidateVar = do
    st <- atomically (newTVar genesisPoint)
    forever (atomically (update st))
  where
    update :: TVar m (Point b) -> Tr m ()
    update stateVar = do
      chain <- readTVar chainVar
      point <- readTVar stateVar
      check (headPoint chain /= point)
      writeTVar stateVar (headPoint chain)
      let candidateChain | verifyChain cfg chain initLedgerState = Just chain
                         | otherwise                             = Nothing
      writeTVar candidateVar candidateChain

chainSelection :: forall b m pid.
                  ( OuroborosTag (BlockProtocol b)
                  , MonadSTM m
                  , SupportedBlock (BlockProtocol b) b
                  , HasHeader b
                  )
               => NodeConfig (BlockProtocol b)
               -> TVar m (Map pid (TVar m (Maybe (Chain b))))
               -> TVar m (ChainProducerState b)
               -> m ()
chainSelection cfg candidates cpsVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: Tr m ()
    updateCurrentChain = do
        candidateVars   <- Map.elems <$> readTVar candidates
        candidateChains <- catMaybes <$> mapM readTVar candidateVars
        cps@ChainProducerState{chainState = ourChain} <- readTVar cpsVar
        let chain' = selectChain cfg ourChain candidateChains
        if headPoint chain' == headPoint ourChain
          then retry
          else writeTVar cpsVar (switchFork chain' cps)

{-------------------------------------------------------------------------------
  Logging support
-------------------------------------------------------------------------------}

-- | Message sent by or to a producer
data ProducerId pid = ProducerId {
      producerUs   :: NodeId
    , producerThem :: pid
    }
  deriving (Show)

-- | Message sent by or to a consumer
data ConsumerId cid = ConsumerId {
      consumerUs   :: NodeId
    , consumerThem :: cid
    }
  deriving (Show)

instance Condense pid => Condense (ProducerId pid) where
  condense ProducerId{..} = condense (producerUs, producerThem)

instance Condense pid => Condense (ConsumerId pid) where
  condense ConsumerId{..} = condense (consumerUs, consumerThem)

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Node (
    NodeKernel(..)
  , nodeKernel
    -- * Network layer abstraction
  , NetworkLayer(..)
  , NetworkCallbacks(..)
  , initNetworkLayer
    -- * Re-exports from the network layer
  , NodeId(..)
  , Chan(..)
  , Network.createCoupledChannels
  ) where

import           Control.Monad
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), ChainUpdate (..), Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Protocol

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Orphans ()

-- TODO: We currently stil import /some/ stuff from the network layer Node
-- module. We should audit this and perhaps move these to different modules
-- in the networking layer to make the division clearer.

import           Ouroboros.Network.Node (Chan (..), NodeId (..))
import qualified Ouroboros.Network.Node as Network

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m pid cid b = NodeKernel {
      -- | Register a new upstream node (chain producer)
      registerUpstream :: pid
                       -> Chan m (MsgConsumer b) (MsgProducer b)
                       -> m ()

      -- | Register a new downstream node (chain consumer)
    , registerDownstream :: cid
                         -> Chan m (MsgProducer b) (MsgConsumer b)
                         -> m ()

      -- | Get current chain
    , getCurrentChain :: Tr m (Chain b)

      -- | Publish block
    , publishBlock :: (Point b -> BlockNo -> Tr m b) -> m ()
    }

nodeKernel :: forall m b up down. ( MonadSTM m
             , MonadSay m
             , ProtocolLedgerView b
             , Show b
             , Show down
             , Show up
             , Ord b
             )
          => NodeId
          -> NodeConfig (BlockProtocol b)
          -> ExtLedgerState b
          -> Chain b
          -> ([ChainUpdate b] -> Tr m ())
          -> m (NodeKernel m up down b)
nodeKernel us cfg initLedgerState initChain notifyUpdates = do
    ourChainVar <- atomically $ newTVar initChain
    updatesVar  <- atomically $ newEmptyTMVar

    -- NOTE: Right now "header validation" is actually just "full validation",
    -- because we cannot validate headers without also having the ledger. Once
    -- we have the header/body split, we're going to have to be more precise
    -- about what we can and cannot validate.
    nw <- initNetworkLayer us initChain NetworkCallbacks {
              prioritizeChains     = selectChain cfg
            , validateChainHeaders = verifyChain cfg initLedgerState
            , chainDownloaded      = \newChain -> atomically $
                -- We are supposed to validate the chain bodies now
                -- This is a potentially expensive operation, which we model
                -- by putting it in a processing queue
                putTMVar updatesVar newChain
            }

    -- We should never write to the chain without calling 'notifyUpdates'
    let updateChain :: Chain b -> [ChainUpdate b] -> Tr m ()
        updateChain newChain upd = do
          writeTVar ourChainVar newChain
          notifyUpdates upd
          networkChainAdopted nw newChain

    -- Thread to do chain validation and adoption
    fork $ forever $ do
         newChain <- atomically $ takeTMVar updatesVar
         -- ... validating ... mumble mumble mumble ...
         atomically $ do
           ourChain <- readTVar ourChainVar
           when (newChain `longerThan` ourChain) $ do
             let i :: Point b
                 i = fromMaybe Chain.genesisPoint $
                       Chain.intersectChains newChain ourChain

                 upd :: [ChainUpdate b]
                 upd = (if i /= Chain.headPoint ourChain
                         then (RollBack i :)
                         else id)
                     $ map AddBlock (fromPoint i newChain)

             updateChain newChain upd

    return NodeKernel {
        registerUpstream   = networkRegisterUpstream   nw
      , registerDownstream = networkRegisterDownstream nw
      , getCurrentChain    = readTVar ourChainVar
      , publishBlock       = \mkBlock -> atomically $ do
            ourChain <- readTVar ourChainVar
            newBlock <- mkBlock (Chain.headPoint ourChain)
                                (Chain.headBlockNo ourChain)
            let newChain = ourChain :> newBlock
            updateChain newChain [AddBlock newBlock]
      }

{-------------------------------------------------------------------------------
  Attempt to mock what the network API will eventually look like
-------------------------------------------------------------------------------}

data NetworkLayer up down b m = NetworkLayer {
      -- | Notify network layer that a new chain is adopted
      --
      -- This can be used when adopting upstream chains (in response to
      -- 'chainDownloaded'), but also when producing new blocks locally.
      networkChainAdopted :: Chain b -> Tr m ()

      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , networkRegisterUpstream :: up
                              -> Chan m (MsgConsumer b) (MsgProducer b)
                              -> m ()


      -- | Notify netwok layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , networkRegisterDownstream :: down
                                -> Chan m (MsgProducer b) (MsgConsumer b)
                                -> m ()
    }

data NetworkCallbacks b m = NetworkCallbacks {
      -- | Notify consensus layer chain has been downloaded
      --
      -- It is up to the consensus layer now to validate the blocks and
      -- (potentially) adopt the chain.
      chainDownloaded      :: Chain b -> m ()

      -- | Validate chain headers
    , validateChainHeaders :: Chain b -> Bool

      -- | Prioritize chains
      --
      -- TODO: This should eventually return a list, rather than a single chain.
    , prioritizeChains     :: Chain b -> [Chain b] -> Chain b
    }

initNetworkLayer :: forall m b up down.
                    ( MonadSTM m
                    , MonadSay m
                    , HasHeader b
                    , Show b
                    , Show down
                    , Show up
                    , Ord b
                    )
                 => NodeId   -- ^ Our node ID
                 -> Chain b  -- ^ Initial chain
                 -> NetworkCallbacks b m
                 -> m (NetworkLayer up down b m)
initNetworkLayer us initChain NetworkCallbacks{..} = do
    cpsVar        <- atomically $ newTVar $ initChainProducerState initChain
    potentialsVar <- atomically $ newTVar Set.empty

    return $ NetworkLayer {
        networkChainAdopted = modifyTVar cpsVar . switchFork
      , networkRegisterDownstream = \down chan ->
          fork $ producerSideProtocol1 (exampleProducer cpsVar)
                   (loggingSend (ProducerId us down) (sendMsg chan))
                   (loggingRecv (ProducerId us down) (recvMsg chan))
      , networkRegisterUpstream = \up chan -> do
          chainVar <- atomically $ newTVar Genesis
          fork $ consumerSideProtocol1 (exampleConsumer chainVar)
                   (loggingSend (ConsumerId us up) (sendMsg chan))
                   (loggingRecv (ConsumerId us up) (recvMsg chan))
          fork $ monitor Chain.headPoint
                         Chain.genesisPoint
                         chainVar $ \newChain ->
              if validateChainHeaders newChain
                then newPotentialChain cpsVar potentialsVar newChain
                else -- We should at this point disregard this peer,
                     -- but the MonadSTM abstraction we work with doesn't
                     -- give us thread IDs. For now we just ignore the
                     -- chain and keep monitoring the peer
                     return ()
      }
  where
    newPotentialChain :: TVar m (ChainProducerState b)
                      -> TVar m (Set (Chain b))
                      -> Chain b
                      -> m ()
    newPotentialChain cpsVar potentialsVar newChain = do
      atomically $ do
        ourChain <- chainState <$> readTVar cpsVar
        when (newChain `longerThan` ourChain) $
          modifyTVar potentialsVar $ Set.insert newChain

      -- At this point we would download blocks, which will be ready
      -- some point later. For now of course we have the entire chain
      -- available. We inform the consensus layer that the longest of
      -- these chains is "now available".

      mDownloaded <- atomically $ do
        ourChain   <- chainState <$> readTVar cpsVar
        potentials <- readTVar potentialsVar
        let potentials' = Set.filter (`longerThan` ourChain) potentials
        writeTVar potentialsVar potentials'
        return $ do
            guard $ not (Set.null potentials')
            let preferred = prioritizeChains ourChain (Set.toList potentials')
            guard $ preferred /= ourChain
            return preferred

      case mDownloaded of
        Nothing -> return ()
        Just c  -> chainDownloaded c

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

longerThan :: Chain b -> Chain b -> Bool
c `longerThan` c' = Chain.length c > Chain.length c'

monitor :: (MonadSTM m, Eq b)
        => (a -> b) -> b -> TVar m a -> (a -> m ()) -> m ()
monitor f b tvar notify = do
    (a, b') <- atomically $ do
                 a <- readTVar tvar
                 let b' = f a
                 if b' == b
                   then retry
                   else return (a, b')
    notify a
    monitor f b' tvar notify

-- | The suffix of the chain starting at the specified point
fromPoint :: HasHeader b => Point b -> Chain b -> [b]
fromPoint p = dropWhile (\b -> blockSlot b < Chain.pointSlot p)
            . Chain.toOldestFirst

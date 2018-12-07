{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Node (
    -- * Blockchain time
    BlockchainTime(..)
  , onSlot
  , testBlockchainTime
    -- * Node
  , NodeKernel(..)
  , NodeCallbacks(..)
  , nodeKernel
    -- * Network layer abstraction
  , NetworkLayer(..)
  , NetworkCallbacks(..)
  , initNetworkLayer
    -- * Re-exports from the network layer
  , NodeId(..)
  , Channel
  , Network.createCoupledChannels
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Function (on)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import           Protocol.Channel
import           Protocol.Driver
import           Protocol.Transition

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), ChainUpdate (..), Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.ChainSyncExamples
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec.Id
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM

-- TODO: We currently stil import /some/ stuff from the network layer Node
-- module. We should audit this and perhaps move these to different modules
-- in the networking layer to make the division clearer.

import           Ouroboros.Network.Node (NodeId (..), loggingChannel)
import qualified Ouroboros.Network.Node as Network

{-------------------------------------------------------------------------------
  "Blockchain time"
-------------------------------------------------------------------------------}

-- | Blockchain time
--
-- When we run the blockchain, there is a single, global time. We abstract over
-- this here to allow to query this time (in terms of the current slot), and
-- execute an action each time we advance a slot.
data BlockchainTime m = BlockchainTime {
      -- | Get current slot
      getCurrentSlot :: Tr m Slot

      -- | Spawn a thread to run an action each time the slot changes
    , onSlotChange   :: (Slot -> m ()) -> m ()
    }

-- | Execute action on specific slot
onSlot :: MonadSTM m => BlockchainTime m -> Slot -> m () -> m ()
onSlot BlockchainTime{..} slot act = onSlotChange $ \slot' -> do
    when (slot == slot') act

-- | Construct new blockchain time that ticks at the specified slot duration
--
-- NOTE: This is just one way to construct time. We can of course also connect
-- this to the real time (if we are in IO), or indeed to a manual tick
-- (in a demo).
--
-- NOTE: The number of slots is only there to make sure we terminate the
-- thread (otherwise the system will keep waiting).
testBlockchainTime :: forall m. (MonadSTM m, MonadTimer m)
                   => Int                -- ^ Number of slots
                   -> Duration (Time m)  -- ^ Slot duration
                   -> m (BlockchainTime m)
testBlockchainTime numSlots slotDuration = do
    slotVar <- atomically $ newTVar firstSlot
    fork $ replicateM_ numSlots $ do
        threadDelay slotDuration
        atomically $ modifyTVar slotVar succ
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      , onSlotChange   = monitorTVar id firstSlot slotVar
      }
  where
    firstSlot :: Slot
    firstSlot = 0

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m up down b = NodeKernel {
      -- | Register a new upstream node (chain producer)
      registerUpstream :: up
                       -> Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                       -> m ()

      -- | Register a new downstream node (chain consumer)
    , registerDownstream :: down
                         -> Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                         -> m ()

      -- | Get current chain
    , getCurrentChain :: Tr m (Chain b)
    }

-- | Callbacks required when initializing the node
data NodeCallbacks b = NodeCallbacks {
      -- | Produce a block
      produceBlock :: forall m.
                      ( MonadRandom m
                      , HasNodeState (BlockProtocol b) m
                      )
                   => IsLeader (BlockProtocol b) -- Proof we are leader
                   -> ExtLedgerState b -- Current ledger state
                   -> Slot             -- Current slot
                   -> Point b          -- Previous point
                   -> BlockNo          -- Previous block number
                   -> m b
    }

nodeKernel :: forall m (rndT :: (* -> *) -> (* -> *)) b up down.
              ( MonadSTM m
              , MonadSay m
              , MonadRandom (rndT (Tr m))
              , ProtocolLedgerView b
              , Eq b
              , Show down
              , Show up
              , Ord up
              )
           => NodeId
           -> NodeConfig (BlockProtocol b)
           -> NodeState (BlockProtocol b)
           -> (forall m'. Sim m' m -> Sim (rndT m') m)
           -- ^ Provide access to a random number generator
           -> BlockchainTime m
           -> ExtLedgerState b
           -> Chain b
           -> NodeCallbacks b
           -> m (NodeKernel m up down b)
nodeKernel us cfg initState simRnd btime initLedger initChain NodeCallbacks{..} = do
    stateVar    <- atomically $ newTVar initState
    ourChainVar <- atomically $ newTVar initChain
    updatesVar  <- atomically $ newEmptyTMVar -- TODO: use bounded queue instead
    ledgerVar   <- atomically $ newTVar initLedger

    let runProtocol :: NodeStateT (BlockProtocol b) (rndT (Tr m)) a -> Tr m a
        runProtocol = simOuroborosStateT stateVar $ simRnd $ id

    -- TODO: New chain only to fake rollback
    let updateLedgerState :: Chain b -> [ChainUpdate b] -> Tr m ()
        updateLedgerState newChain (RollBack _:_) =
            modifyTVar' ledgerVar $ \_st ->
              case runExcept (chainExtLedgerState cfg newChain initLedger) of
                Left err  -> error (show err)
                Right st' -> st'
        updateLedgerState _ upd = do
            let newBlock :: ChainUpdate b -> b
                newBlock (RollBack _) = error "newBlock: unexpected rollback"
                newBlock (AddBlock b) = b
            modifyTVar' ledgerVar $ \st ->
              case runExcept (foldExtLedgerState cfg (map newBlock upd) st) of
                Left err  -> error (show err)
                Right st' -> st'

    -- NOTE: Right now "header validation" is actually just "full validation",
    -- because we cannot validate headers without also having the ledger. Once
    -- we have the header/body split, we're going to have to be more precise
    -- about what we can and cannot validate.
    nw <- initNetworkLayer us initChain btime NetworkCallbacks {
              prioritizeChains     = selectChain cfg
            , validateChainHeaders = verifyChain cfg initLedger
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
          updateLedgerState newChain upd
          networkChainAdopted nw newChain

    -- Thread to do chain validation and adoption
    fork $ forever $ do
         newChain <- atomically $ takeTMVar updatesVar
         -- ... validating ... mumble mumble mumble ...
         atomically $ do
           slot     <- getCurrentSlot btime
           ourChain <- readTVar ourChainVar
           -- Check: do we still want this chain? (things might have changed)
           when (selectChain cfg slot ourChain [newChain] `sameChainAs` newChain) $ do
             let i :: Point b
                 i = fromMaybe Chain.genesisPoint $
                       Chain.intersectChains newChain ourChain

                 upd :: [ChainUpdate b]
                 upd = (if i /= Chain.headPoint ourChain
                         then (RollBack i :)
                         else id)
                     $ map AddBlock (afterPoint i newChain)

             updateChain newChain upd

    -- Block production
    onSlotChange btime $ \slot -> atomically $ do
      l@ExtLedgerState{..} <- readTVar ledgerVar
      mIsLeader            <- runProtocol $
                                 checkIsLeader
                                 cfg
                                 slot
                                 (protocolLedgerView cfg ledgerState)
                                 ouroborosChainState

      case mIsLeader of
        Nothing    -> return ()
        Just proof -> do
          (ourChain, upd) <- overrideBlock slot <$> readTVar ourChainVar
          let prevPoint = Chain.headPoint   ourChain
              prevNo    = Chain.headBlockNo ourChain
          newBlock <- runProtocol $ produceBlock proof l slot prevPoint prevNo
          let newChain = ourChain :> newBlock
          updateChain newChain $ upd ++ [AddBlock newBlock]

    return NodeKernel {
        registerUpstream   = networkRegisterUpstream   nw
      , registerDownstream = networkRegisterDownstream nw
      , getCurrentChain    = readTVar ourChainVar
      }
  where
    -- Drop the most recent block if it occupies the current slot
    overrideBlock :: Slot -> Chain b -> (Chain b, [ChainUpdate b])
    overrideBlock slot c
      | Chain.headSlot c <  slot = (c, [])
      | Chain.headSlot c == slot = let c' = dropMostRecent c
                                   in (c', [RollBack (Chain.headPoint c')])
      | otherwise                = error "overrideBlock: block in future"

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
                              -> Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                              -> m ()

      -- | Notify netwok layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , networkRegisterDownstream :: down
                                -> Channel m (SomeTransition (ChainSyncMessage b (Point b)))
                                -> m ()
    }

data NetworkCallbacks b m = NetworkCallbacks {
      -- | Notify consensus layer chain has been downloaded
      --
      -- It is up to the consensus layer now to validate the blocks and
      -- (potentially) adopt the chain.
      chainDownloaded      :: Chain b -> m () -- Chain Block -> ..

      -- | Validate chain headers
    , validateChainHeaders :: Chain b -> Bool -- Chain Header -> ..

      -- | Prioritize chains
      --
      -- TODO: This should eventually return a list, rather than a single chain.
                           -- Chain Block -> Chain Header -> Chain Header (?)
    , prioritizeChains     :: Slot -> Chain b -> [Chain b] -> Chain b
    }

initNetworkLayer :: forall m b up down.
                    ( MonadSTM m
                    , MonadSay m
                    , HasHeader b
                    , Show down
                    , Show up
                    , Ord up
                    )
                 => NodeId   -- ^ Our node ID
                 -> Chain b  -- ^ Our initial (current) chain
                 -> BlockchainTime m
                 -> NetworkCallbacks b m
                 -> m (NetworkLayer up down b m)
initNetworkLayer us initChain btime NetworkCallbacks{..} = do
    cpsVar <- atomically $ newTVar $ initChainProducerState initChain

    -- PotentialsVar is modelling state in the network layer tracking the
    -- potential chains we might like to be downloaded and from whom.
    potentialsVar <- atomically $ newTVar Map.empty

    return $ NetworkLayer {
        networkChainAdopted = modifyTVar cpsVar . switchFork
      , networkRegisterDownstream = \down chan -> do
          let producer = chainSyncServerPeer (chainSyncServerExample () cpsVar)
          fork $ void $ useCodecWithDuplex
            (loggingChannel (ProducerId us down) (withSomeTransition show) (withSomeTransition show) chan)
            codecChainSync
            producer
      , networkRegisterUpstream = \up chan -> do
          chainVar     <- atomically $ newTVar Genesis
          let consumer = chainSyncClientPeer (chainSyncClientExample chainVar pureClient)
          fork $ void $ useCodecWithDuplex
            (loggingChannel (ConsumerId us up) (withSomeTransition show) (withSomeTransition show) chan)
            codecChainSync
            consumer
          monitorTVar Chain.headPoint Chain.genesisPoint chainVar $ \newChain ->
              if validateChainHeaders newChain
                then newPotentialChain cpsVar potentialsVar up newChain
                else -- We should at this point disregard this peer,
                     -- but the MonadSTM abstraction we work with doesn't
                     -- give us thread IDs. For now we just error out, we're
                     -- not (yet) testing with invbalid chains.
                     error "ERROR: Received invalid chain from peer"
      }
  where
    newPotentialChain :: TVar m (ChainProducerState b)
                      -> TVar m (Map up (Chain b))
                      -> up
                      -> Chain b
                      -> m ()
    newPotentialChain cpsVar potentialsVar up newChain = do
      atomically $ modifyTVar potentialsVar $ Map.insert up newChain

      -- At this point we would download blocks, which will be ready
      -- some point later. For now of course we have the entire chain
      -- available. We inform the consensus layer that the longest of
      -- these chains is "now available".

      mDownloaded <- atomically $ do
        slot       <- getCurrentSlot btime
        ourChain   <- chainState <$> readTVar cpsVar
        potentials <- readTVar potentialsVar
        return $ do
            guard $ not (Map.null potentials)
            let preferred = prioritizeChains slot ourChain (Map.elems potentials)
            guard $ not (preferred `sameChainAs` ourChain)
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

-- | The suffix of the chain starting after the specified point
afterPoint :: HasHeader b => Point b -> Chain b -> [b]
afterPoint p = dropWhile (\b -> blockSlot b <= Chain.pointSlot p)
            . Chain.toOldestFirst

sameChainAs :: HasHeader b => Chain b -> Chain b -> Bool
sameChainAs = (==) `on` Chain.headPoint

dropMostRecent :: Chain b -> Chain b
dropMostRecent Genesis  = error "dropMostRecent: empty chain"
dropMostRecent (c :> _) = c

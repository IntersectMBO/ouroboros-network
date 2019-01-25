{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.Node (
    -- * Node IDs
    NodeId(..)
  , CoreNodeId(..)
  , fromCoreNodeId
    -- * Blockchain time
  , BlockchainTime(..)
  , onSlot
  , NumSlots(..)
  , finalSlot
  , testBlockchainTime
  , realBlockchainTime
    -- * Node
  , NodeKernel(..)
  , NodeCallbacks(..)
  , nodeKernel
    -- * Network layer abstraction
  , NetworkLayer(..)
  , NetworkCallbacks(..)
  , initNetworkLayer
    -- * Channels (re-exports from the network layer)
  , Channel
  , Network.createCoupledChannels
  , Network.loggingChannel
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Function (on)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer

import           Protocol.Channel
import           Protocol.Codec
import           Protocol.Driver

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), ChainUpdate (..), Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.STM

-- TODO: We currently stil import /some/ stuff from the network layer Node
-- module. We should audit this and perhaps move these to different modules
-- in the networking layer to make the division clearer.

import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Node as Network

{-------------------------------------------------------------------------------
  Node IDs
-------------------------------------------------------------------------------}

-- | Core node ID
newtype CoreNodeId = CoreNodeId Int
  deriving (Show, Eq, Ord, Condense, Serialise)

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId (CoreNodeId n) = CoreId n

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

-- | Number of slots
newtype NumSlots = NumSlots Int

finalSlot :: NumSlots -> Slot
finalSlot (NumSlots n) = Slot (fromIntegral n)

-- | Construct new blockchain time that ticks at the specified slot duration
--
-- NOTE: This is just one way to construct time. We can of course also connect
-- this to the real time (if we are in IO), or indeed to a manual tick
-- (in a demo).
--
-- NOTE: The number of slots is only there to make sure we terminate the
-- thread (otherwise the system will keep waiting).
testBlockchainTime :: forall m. (MonadSTM m, MonadTimer m)
                   => NumSlots           -- ^ Number of slots
                   -> Duration (Time m)  -- ^ Slot duration
                   -> m (BlockchainTime m)
testBlockchainTime (NumSlots numSlots) slotDuration = do
    slotVar <- atomically $ newTVar firstSlot
    fork $ replicateM_ numSlots $ do
        threadDelay slotDuration
        atomically $ modifyTVar slotVar succ
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      , onSlotChange   = onEachChange id firstSlot (readTVar slotVar)
      }
  where
    firstSlot :: Slot
    firstSlot = 0

-- | Real blockchain time
realBlockchainTime :: UTCTime -- ^ Chain start time
                   -> Double  -- ^ Slot duration (seconds)
                   -> IO (BlockchainTime IO)
realBlockchainTime systemStart slotDuration = do
    first   <- currentSlot
    slotVar <- atomically $ newTVar first
    fork $ forever $ do
      threadDelay $ round (slotDuration * 1000000)
      atomically . writeTVar slotVar =<< currentSlot
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      , onSlotChange   = onEachChange id first (readTVar slotVar)
      }
  where
    currentSlot :: IO Slot
    currentSlot = do
      now <- getCurrentTime
      let diff :: Double -- system duration in seconds
          diff = realToFrac $ now `diffUTCTime` systemStart
      return $ Slot $ floor (diff / slotDuration) + 1

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m up b = NodeKernel {
      -- | Access to underlying network layer
      --
      -- Can be used for registering upstream and downstream nodes.
      nodeNetworkLayer  :: NetworkLayer up b m

      -- | Get current chain
    , getCurrentChain   :: Tr m (Chain b)

      -- | Get current extended ledger state
    , getExtLedgerState :: Tr m (ExtLedgerState b)
    }

-- | Callbacks required when initializing the node
data NodeCallbacks m rndT b = NodeCallbacks {
      -- | Produce a block
      --
      -- This has access to the random number generator
      produceBlock :: IsLeader (BlockProtocol b) -- Proof we are leader
                   -> ExtLedgerState b -- Current ledger state
                   -> Slot             -- Current slot
                   -> Point b          -- Previous point
                   -> BlockNo          -- Previous block number
                   -> NodeStateT (BlockProtocol b) (rndT (Tr m)) b

      -- | Callback called whenever we adopt a new chain
      --
      -- NOTE: This intentionally lives in @m@ rather than @Tr m@ so that this
      -- callback can have side effects.
    , adoptedNewChain   :: Chain b -> m ()
    }

nodeKernel :: forall m (rndT :: (* -> *) -> (* -> *)) b up.
              ( MonadSTM m
              , MonadRandom (rndT (Tr m))
              , ProtocolLedgerView b
              , Eq b
              , Ord up
              )
           => NodeConfig (BlockProtocol b)
           -> NodeState (BlockProtocol b)
           -> (forall m'. Sim m' m -> Sim (rndT m') m)
           -- ^ Provide access to a random number generator
           -> BlockchainTime m
           -> ExtLedgerState b
           -> Chain b
           -> NodeCallbacks m rndT b
           -> m (NodeKernel m up b)
nodeKernel cfg initState simRnd btime initLedger initChain NodeCallbacks{..} = do
    stateVar    <- atomically $ newTVar initState
    ourChainVar <- atomically $ newTVar initChain
    updatesVar  <- atomically $ newTBQueue 64
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
    nw <- initNetworkLayer initChain btime NetworkCallbacks {
              prioritizeChains     = selectChain cfg
            , validateChainHeaders = verifyChain cfg initLedger
            , chainDownloaded      = \newChain -> atomically $
                -- We are supposed to validate the chain bodies now
                -- This is a potentially expensive operation, which we model
                -- by putting it in a processing queue
                writeTBQueue updatesVar newChain
            }

    -- We should never write to the chain without calling 'notifyUpdates'
    let updateChain :: Chain b -> [ChainUpdate b] -> Tr m ()
        updateChain newChain upd = do
          writeTVar ourChainVar newChain
          updateLedgerState newChain upd
          chainAdopted nw newChain

    -- Thread to do chain validation and adoption
    --
    -- TODO: Once everything flips, this gets the pool of /all/ candidates,
    -- and it can decide to verify on the best.
    fork $ forever $ do
         newChain <- atomically $ readTBQueue updatesVar
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

         -- Inform layer above us that we adopted a new chain
         -- NOTE: This is used in tests only for now.
         adoptedNewChain newChain

    -- Block production
    onSlotChange btime $ \slot -> do
      -- TODO: Construct a ChaCha RNG using proper MonadRandom to use inside tr
      mNewChain <- atomically $ do
        l@ExtLedgerState{..} <- readTVar ledgerVar
        mIsLeader            <- runProtocol $
                                   checkIsLeader
                                   cfg
                                   slot
                                   (protocolLedgerView cfg ledgerState)
                                   ouroborosChainState

        case mIsLeader of
          Nothing    -> return Nothing
          Just proof -> do
            (ourChain, upd) <- overrideBlock slot <$> readTVar ourChainVar
            let prevPoint = Chain.headPoint   ourChain
                prevNo    = Chain.headBlockNo ourChain
            newBlock <- runProtocol $ produceBlock proof l slot prevPoint prevNo
            let newChain = ourChain :> newBlock
            updateChain newChain $ upd ++ [AddBlock newBlock]
            return $ Just newChain

      -- Make sure the callback is also called when /we/ make a new chain
      case mNewChain of
        Nothing       -> return ()
        Just newChain -> adoptedNewChain newChain

    return NodeKernel {
        nodeNetworkLayer  = nw
      , getCurrentChain   = readTVar ourChainVar
      , getExtLedgerState = readTVar ledgerVar
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

data NetworkLayer up b m = NetworkLayer {
      -- | Notify network layer that a new chain is adopted
      --
      -- This can be used when adopting upstream chains (in response to
      -- 'chainDownloaded'), but also when producing new blocks locally.
      --
      -- TODO: Make this a query instead  @Tr m (Chain block)@
      chainAdopted :: Chain b -> Tr m ()


      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      --
      -- Instead of passing a channel directly, we pass in a function that
      -- is given the chance to /create/ a channel for the lifetime of the
      -- connection.
    , registerUpstream :: forall concreteSend concreteRecv.
                          up
                       -> Codec m Text concreteSend concreteRecv (ChainSyncMessage b (Point b)) 'StIdle
                       -> (forall a. (Duplex m m concreteSend concreteRecv -> m a) -> m a)
                       -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      --
      -- Instead of passing a channel directly, we pass in a function that
      -- is given the chance to /create/ a channel for the lifetime of the
      -- connection.
    , registerDownstream :: forall concreteSend concreteRecv.
                            Codec m Text concreteSend concreteRecv (ChainSyncMessage b (Point b)) 'StIdle
                         -> (forall a. (Duplex m m concreteSend concreteRecv -> m a) -> m a)
                         -> m ()
    }

data NetworkCallbacks b m = NetworkCallbacks {
      -- | Notify consensus layer chain has been downloaded
      --
      -- It is up to the consensus layer now to validate the blocks and
      -- (potentially) adopt the chain.
      --
      -- TODO: Change this to a query "give us the downloaded chains"
      -- > getDownloadedChains :: Tr m [Chain b]
      chainDownloaded      :: Chain b -> m () -- Chain Block -> ..

      -- | Validate chain headers
      --
      -- NOTE: We can only validate up to a certain point in the
      -- future (as well as up to a certain point in the past),
      -- but we switch to genesis before we get to that point.
      --
      -- TODO: Do /not/ use chainSyncClientExample, instead
      -- /provide/ a ChainSyncClient /to/ the network layer, which implements
      -- the logic /and/ validation (and can deal with genesis also).
    , validateChainHeaders :: Chain b -> Bool -- Chain Header -> ..

      -- | Prioritize chains
      --
      -- TODO: This should eventually return a list, rather than a single chain.
      --                      forall peer. Slot -> Chain Block -> [(peer, Chain Header)] -> [(peer, Chain Header)] (?)
    , prioritizeChains     :: Slot -> Chain b -> [Chain b] -> Chain b
    }

initNetworkLayer :: forall m b up. (MonadSTM m, HasHeader b, Ord up)
                 => Chain b  -- ^ Our initial (current) chain
                 -> BlockchainTime m
                 -> NetworkCallbacks b m
                 -> m (NetworkLayer up b m)
initNetworkLayer initChain btime NetworkCallbacks{..} = do
    -- cpsVar records chain for server side of the node
    -- NOTE: This does /not/ need to be updated synchronously with our chain.
    cpsVar <- atomically $ newTVar $ initChainProducerState initChain

    -- PotentialsVar is modelling state in the network layer tracking the
    -- potential chains we might like to be downloaded and from whom.
    potentialsVar <- atomically $ newTVar Map.empty

    return $ NetworkLayer {
        chainAdopted = modifyTVar cpsVar . switchFork
      , registerDownstream = \codec withChan -> do
          let producer = chainSyncServerPeer (chainSyncServerExample () cpsVar)
          fork $ void $ withChan $ \chan -> useCodecWithDuplex chan codec producer
      , registerUpstream = \up codec withChan -> do
          chainVar     <- atomically $ newTVar Genesis
          let consumer = chainSyncClientPeer (chainSyncClientExample chainVar pureClient)
          fork $ void $ withChan $ \chan -> useCodecWithDuplex chan codec consumer
          onEachChange Chain.headPoint
                       Chain.genesisPoint
                       (readTVar chainVar) $ \newChain ->
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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

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
  , NodeComms(..)
  , nodeKernel
    -- * Channels (re-exports from the network layer)
  , Channel
  , Network.createCoupledChannels
  , Network.loggingChannel
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Crypto.Random (ChaChaDRG)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text)
import           Data.Time

import           Control.Monad.Class.MonadSay
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
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
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
  deriving (Show)

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
data NodeKernel m up blk = NodeKernel {
      -- | Get current chain
      getCurrentChain   :: Tr m (Chain blk)

      -- | Get current extended ledger state
    , getExtLedgerState :: Tr m (ExtLedgerState blk)

      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addUpstream       :: forall s r. up -> NodeComms m blk s r -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addDownstream     :: forall s r. NodeComms m blk s r -> m ()
    }

-- | Monad that we run protocol specific functions in
type ProtocolM blk m = NodeStateT (BlockProtocol blk) (ChaChaT (Tr m))

-- | Callbacks required when initializing the node
data NodeCallbacks m blk = NodeCallbacks {
      -- | Produce a block
      produceBlock :: IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> ExtLedgerState blk -- Current ledger state
                   -> Slot               -- Current slot
                   -> Point blk          -- Previous point
                   -> BlockNo            -- Previous block number
                   -> ProtocolM blk m blk

      -- | Produce a random seed
      --
      -- We want to be able to use real (crypto strength) random numbers, but
      -- obviously have no access to a sytem random number source inside an
      -- STM transaction. So we use the system RNG to generate a local DRG,
      -- which we then use for this transaction, and /only/ this transaction.
      -- The loss of entropy therefore is minimal.
      --
      -- In IO, can use 'Crypto.Random.drgNew'.
    , produceDRG :: m ChaChaDRG

      -- | Callback called whenever we adopt a 2new chain
      --
      -- NOTE: This intentionally lives in @m@ rather than @Tr m@ so that this
      -- callback can have side effects.
    , adoptedNewChain :: Chain blk -> m ()
    }

nodeKernel :: forall m blk up.
              ( MonadSTM m
              , MonadSay m
              , ProtocolLedgerView blk
              , Eq                 blk
              , Condense           blk
              , Ord up
              )
           => NodeConfig (BlockProtocol blk)
           -> NodeState (BlockProtocol blk)
           -> BlockchainTime m
           -> ExtLedgerState blk
           -> Chain blk
           -> NodeCallbacks m blk
           -> m (NodeKernel m up blk)
nodeKernel cfg initState btime initLedger initChain callbacks = do
    st <- initInternalState cfg btime initChain initLedger initState callbacks

    forkMonitorDownloads st
    forkBlockProduction  st

    return NodeKernel {
        addUpstream       = npAddUpstream   (networkLayer st)
      , addDownstream     = npAddDownstream (networkLayer st)
      , getCurrentChain   = readTVar (varChain  st)
      , getExtLedgerState = readTVar (varLedger st)
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

-- TODO: Turn into single TVar for chain and ledger state
data InternalState m up blk = IS {
      cfg           :: NodeConfig (BlockProtocol blk)
    , btime         :: BlockchainTime m
    , initChain     :: Chain blk
    , initLedger    :: ExtLedgerState blk
    , callbacks     :: NodeCallbacks m blk
    , networkLayer  :: NetworkProvides m up blk blk
    , varChain      :: TVar m (Chain blk)
    , varLedger     :: TVar m (ExtLedgerState blk)
    , varCandidates :: TVar m (Candidates up blk)
    , varState      :: TVar m (NodeState (BlockProtocol blk))
    }

initInternalState :: forall m up blk.
                     (MonadSTM m, ProtocolLedgerView blk, Ord up, Eq blk)
                  => NodeConfig (BlockProtocol blk) -- ^ Node configuration
                  -> BlockchainTime m               -- ^ Time
                  -> Chain blk                      -- ^ Initial chain
                  -> ExtLedgerState blk             -- ^ Initial ledger state
                  -> NodeState (BlockProtocol blk)  -- ^ Init node state
                  -> NodeCallbacks m blk            -- ^ Callbacks
                  -> m (InternalState m up blk)
initInternalState cfg btime initChain initLedger initState callbacks = do
    varChain      <- atomically $ newTVar initChain
    varLedger     <- atomically $ newTVar initLedger
    varCandidates <- atomically $ newTVar noCandidates
    varState      <- atomically $ newTVar initState

    let networkRequires :: NetworkRequires m up blk blk
        networkRequires = NetworkRequires {
            nrCurrentChain = readTVar varChain
          , nrCandidates   = readTVar varCandidates
          , nrSyncClient   = consensusSyncClient
                               cfg
                               btime
                               initLedger
                               varChain
                               varCandidates
          }

    networkLayer <- initNetworkLayer networkRequires

    return IS{..}

forkMonitorDownloads :: forall m up blk.
                        ( MonadSTM m
                        , MonadSay m
                        , ProtocolLedgerView blk
                        , Eq                 blk
                        , Condense           blk
                        )
                     => InternalState m up blk -> m ()
forkMonitorDownloads st@IS{..} =
    -- TODO: We should probably not just look at the headSlot
    onEachChange (map Chain.headSlot) [] npDownloaded $ \downloaded -> do
      -- TODO: At this point we should validate the chain /bodies/
      -- (we'd only have verified headers so far)
      -- TODO: Right now we have no way of informing the network layer when
      -- block verification failed

      mNew <- atomically $ do
        old <- readTVar varChain
        now <- getCurrentSlot btime
        case selectChain cfg now old downloaded of
          Nothing  -> return Nothing
          Just new -> do
            adoptNewChain st old new
            return (Just new)
      case mNew of
        Nothing  -> say $ "Ignoring downloaded chains " ++ condense downloaded
        Just new -> say $ "Adopted new chain " ++ condense new

      forM_ mNew adoptedNewChain
  where
    NetworkProvides{..} = networkLayer
    NodeCallbacks{..}   = callbacks

forkBlockProduction :: forall m up blk. (MonadSTM m, ProtocolLedgerView blk)
                    => InternalState m up blk -> m ()
forkBlockProduction st@IS{..} =
    onSlotChange btime $ \slot -> do
      drg  <- produceDRG
      mNew <- atomically $ do
        varDRG <- newTVar drg
        l@ExtLedgerState{..} <- readTVar varLedger
        mIsLeader            <- runProtocol varDRG $
                                   checkIsLeader
                                     cfg
                                     slot
                                     (protocolLedgerView cfg ledgerState)
                                     ouroborosChainState

        case mIsLeader of
          Nothing    -> return Nothing
          Just proof -> do
            (old, upd) <- overrideBlock slot <$> readTVar varChain
            let prevPoint = Chain.headPoint   old
                prevNo    = Chain.headBlockNo old
            newBlock <- runProtocol varDRG $
                          produceBlock proof l slot prevPoint prevNo
            let new = Chain.addBlock newBlock old
            applyUpdate st new (upd ++ [AddBlock newBlock])
            return $ Just new

      forM_ mNew adoptedNewChain
  where
    NodeCallbacks{..} = callbacks

    -- Drop the most recent block if it occupies the current slot
    overrideBlock :: Slot -> Chain blk -> (Chain blk, [ChainUpdate blk])
    overrideBlock slot c
      | Chain.headSlot c <  slot = (c, [])
      | Chain.headSlot c == slot = let c' = dropMostRecent c
                                   in (c', [RollBack (Chain.headPoint c')])
      | otherwise                = error "overrideBlock: block in future"

    runProtocol :: TVar m ChaChaDRG -> ProtocolM blk m a -> Tr m a
    runProtocol varDRG = simOuroborosStateT varState
                       $ simChaChaT varDRG
                       $ id

adoptNewChain :: forall m up blk.
                 ( MonadSTM m
                 , ProtocolLedgerView blk
                 )
              => InternalState m up blk
              -> Chain blk  -- ^ Old chain
              -> Chain blk  -- ^ New chain
              -> Tr m ()
adoptNewChain is old new =
    applyUpdate is new upd
  where
    i :: Point blk
    i = fromMaybe Chain.genesisPoint $ Chain.intersectChains old new

    upd :: [ChainUpdate blk]
    upd = (if i /= Chain.headPoint old
            then (RollBack i :)
            else id)
        $ map AddBlock (afterPoint i new)

-- | Apply chain update
applyUpdate :: ( MonadSTM m
               , ProtocolLedgerView blk
               )
            => InternalState m up blk
            -> Chain blk          -- ^ New chain
            -> [ChainUpdate blk]  -- ^ Update
            -> Tr m ()
applyUpdate st@IS{..} new upd = do
    writeTVar varChain new
    updateLedgerState st new upd

-- | Update the ledger state
--
-- TODO: Rethink this in relation to validation. Chains have already been
-- validated at this point, but now we need to compute ledger state;
-- but validation basically /is/ ledger updating. Perhaps we need to store
-- the updated ledger state with the candidates. Or something.
updateLedgerState :: ( MonadSTM m
                     , ProtocolLedgerView blk
                     )
                  => InternalState m up blk
                  -> Chain blk          -- ^ New chain (TODO: remove this arg)
                  -> [ChainUpdate blk]  -- ^ Chain update
                  -> Tr m ()
updateLedgerState IS{..} new upd =
    case upd of
      RollBack _:_ ->
        -- TODO: Properly implement support for rollback
        modifyTVar' varLedger $ \_st ->
          case runExcept (chainExtLedgerState cfg new initLedger) of
            Left err  -> error (show err)
            Right st' -> st'
      _otherwise ->
        modifyTVar' varLedger $ \st ->
          case runExcept (foldExtLedgerState cfg (map newBlock upd) st) of
            Left err  -> error (show err)
            Right st' -> st'
  where
    -- Only the first update can be a rollback
    newBlock :: ChainUpdate b -> b
    newBlock (RollBack _) = error "newBlock: unexpected rollback"
    newBlock (AddBlock b) = b

{-------------------------------------------------------------------------------
  (Consensus layer provided) Chain sync client

  TODO: Implement genesis here

  Genesis in paper:

    When we compare a candidate to our own chain, and that candidate forks off
    more than k in the past, we compute the intersection point between that
    candidate and our chain, select s slots from both chains, and compare the
    number of blocks within those s slots. If the candidate has more blocks
    in those s slots, we prefer the candidate, otherwise we stick with our own
    chain.

  Genesis as we will implement it:

    * We decide we are in genesis mode if the head of our chain is more than
      @k@ blocks behind the blockchain time. We will have to approximate this
      as @k/f@ /slots/ behind the blockchain time time.
    * In this situation, we must make sure we have a sufficient number of
      upstream nodes "and collect chains from all of them"
    * We still never consider chains that would require /us/ to rollback more
      than k blocks.
    * In order to compare two candidates, we compute the intersection point of
      X of those two candidates and compare the density at point X.




  Scribbled notes during meeting with Duncan:

   geensis mode: compare clock to our chain
   do we have enough peers?
   still only interested in chains that don't fork more than k from our own chain

     downloading headers from a /single/ node, download at least s headers
     inform /other/ peers: "here is a point on our chain"
     if all agree ("intersection imporved") -- all peers agree
     avoid downloading tons of headers
     /if/ there is a difference, get s headers from the peer who disagrees,
       pick the denser one, and ignore the other
       PROBLEM: what if the denser node has invalid block bodies??
-------------------------------------------------------------------------------}

-- | Something went wrong during the chain sync protocol
--
-- This either indicates an intentional failure (malicious client) or a buggy
-- client. The additional data we record here is for debugging only.
data ChainSyncFailure hdr =
    -- | The node we're connecting to forked more than k slots ago
    --
    -- We record their current head.
    ForkTooDeep (Point hdr)

    -- | They send us an invalid header
    --
    -- We record their (invalid) chain.
    --
    -- TODO: Of course eventually this won't be possible, and we should
    -- just record the invalid block.
  | InvalidBlock (Chain hdr)

    -- | The client sent us an intersection point not on their chain
    --
    -- We record the intersection point and their head.
  | InvalidRollback (Point hdr) (Point hdr)

type Consensus (client :: * -> * -> (* -> *) -> * -> *) hdr m =
   client hdr (Point hdr) m (ChainSyncFailure hdr)

-- | Chain sync client
--
-- This only terminates on failures.
consensusSyncClient :: forall m up blk hdr.
                       ( MonadSTM m
                       , blk ~ hdr -- for now
                       , ProtocolLedgerView hdr
                       , Ord up
                       , Eq hdr
                       )
                    => NodeConfig (BlockProtocol hdr)
                    -> BlockchainTime m
                    -> ExtLedgerState blk
                    -> TVar m (Chain blk)
                    -> TVar m (Candidates up hdr)
                    -> up -> Consensus ChainSyncClient hdr m
consensusSyncClient cfg btime initLedger varChain candidatesVar up =
    ChainSyncClient initialise
  where
    initialise :: m (Consensus ClientStIdle hdr m)
    initialise = do
      (theirChainVar, ourChain) <- atomically $ do
        -- We optimistically assume that the upstream node's chain is similar to
        -- ours, so we start with assuming it's /equal/ to our chain, and then
        -- let the chain sync protocol do its job
        chain <- readTVar varChain
        (, chain) <$> newTVar chain

      return $ SendMsgFindIntersect (Chain.selectPoints (map fromIntegral offsets) ourChain) $
        ClientStIntersect {
            recvMsgIntersectImproved = \_intersection _theirHead ->
              -- We found an intersection within the last k slots. All good
              ChainSyncClient $ return (requestNext theirChainVar)

          , recvMsgIntersectUnchanged = \theirHead -> ChainSyncClient $
              -- If the intersection point is unchanged, this means that the
              -- best intersection point was the initial assumption: genesis.
              -- If the genesis point is within k of our own head, this is
              -- fine, but if it is not, we cannot sync with this client
              if getSlot (Chain.headSlot ourChain) > fromIntegral k
                then return $ SendMsgDone (ForkTooDeep theirHead)
                else return $ requestNext theirChainVar
          }

    requestNext :: TVar m (Chain hdr) -> Consensus ClientStIdle hdr m
    requestNext theirChainVar =
        SendMsgRequestNext
          (handleNext theirChainVar)
          (return (handleNext theirChainVar)) -- case for when we have to wait

    handleNext :: TVar m (Chain hdr) -> Consensus ClientStNext hdr m
    handleNext theirChainVar = ClientStNext {
          recvMsgRollForward = \hdr _theirHead -> ChainSyncClient $
            atomically $ do
              -- Right now we validate the entire chain here. We should only
              -- validate the new block.
              theirChain <- readTVar theirChainVar
              let theirChain' = Chain.addBlock hdr theirChain
              if not (verifyChain cfg initLedger theirChain') then
                return $ SendMsgDone (InvalidBlock theirChain')
              else do
                writeTVar theirChainVar theirChain'
                updateCandidates' theirChain'
                return $ requestNext theirChainVar

        , recvMsgRollBackward = \intersection theirHead -> ChainSyncClient $
            atomically $ do
              theirChain <- readTVar theirChainVar
              case Chain.rollback intersection theirChain of
                Just theirChain' -> do
                  -- No need to validate (prefix of a valid chain must be valid)
                  writeTVar theirChainVar theirChain'
                  updateCandidates' theirChain'
                  return $ requestNext theirChainVar
                Nothing ->
                  return $ SendMsgDone (InvalidRollback intersection theirHead)
        }

    -- Update set of candidates
    updateCandidates' :: Chain hdr -> Tr m ()
    updateCandidates' theirChain = do
        now      <- getCurrentSlot btime
        ourChain <- readTVar varChain
        modifyTVar candidatesVar $
          updateCandidates cfg now ourChain (up, theirChain)

    -- Recent offsets
    --
    -- These offsets are used to find an intersection point between our chain
    -- and the upstream node's. We use the fibonacci sequence to try blocks
    -- closer to our tip, and fewer blocks further down the chain. It is
    -- important that this sequence constains at least a point @k@ back: if
    -- no intersection can be found at most @k@ back, then this is not a peer
    -- that we can sync with (since we will never roll back more than @k).
    --
    -- For @k = 2160@, this evaluates to
    --
    -- > [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2160]
    --
    -- For @k = 5@ (during testing), this evaluates to
    --
    -- > [0,1,2,3,5]
    offsets :: [Word]
    offsets = [0] ++ takeWhile (< k) [fib n | n <- [2..]] ++ [k]

    k :: Word
    k = maxRollbacks $ protocolSecurityParam cfg

{-------------------------------------------------------------------------------
  Chain candidates
-------------------------------------------------------------------------------}

newtype Candidates up hdr = Candidates {
      -- | Candidate chains
      --
      -- When we track upstream nodes, /candidate/ chains are chains that the
      -- chain selection rule tells us are preferred over our current chain.
      -- We network layer will ask the consensus layer to prioritize these
      -- candidates: elements earlier in the list are preferred over elements
      -- later in the list; we have no preference between candidates within
      -- a single map.
      --
      -- Invariants:
      --
      -- * All maps must be non-empty
      -- * All map domains must be mutually disjoint
      -- * All elements of a single map must be equally preferable
      --   (according to the protocol's chain selection rule)
      -- * Elements earlier in the list must be strictly preferred over elements
      --   later in the list (again, according to chain selection)
      --
      -- Moreover, we should have a system invariant that /all/ candidates we
      -- record must be preferred over our own chain.
      candidates :: [Map up (Chain hdr)]
    }

noCandidates :: Candidates up hdr
noCandidates = Candidates []

-- | Remove candidate from a certain node
removeCandidate :: forall up hdr. Ord up
                => up -> Candidates up hdr -> Candidates up hdr
removeCandidate up (Candidates cs) = Candidates $ mapMaybe delete cs
  where
    delete :: Map up (Chain hdr) -> Maybe (Map up (Chain hdr))
    delete m = do
        let m' = Map.delete up m
        guard $ not (Map.null m')
        return m'

-- | Insert a new candidate (one that we /know/ we prefer over our own chain)
--
-- Precondition: no chain for this upstream node already present.
insertCandidate :: forall up hdr.
                   ( OuroborosTag (BlockProtocol hdr)
                   , HasHeader hdr
                   , Ord up
                   , Eq hdr
                   )
                => NodeConfig (BlockProtocol hdr)
                -> Slot -- ^ Present slot
                -> (up, Chain hdr) -> Candidates up hdr -> Candidates up hdr
insertCandidate cfg now (up, cand) (Candidates cands) =
    Candidates $ go cands
  where
    go :: [Map up (Chain hdr)] -> [Map up (Chain hdr)]
    go []       = [Map.singleton up cand]
    go (cs:css) =
      case compareCandidates cfg now cand (head' cs) of
        GT -> Map.singleton up cand : cs : css
        EQ -> Map.insert up cand cs : css
        LT -> cs : go css

    -- Pick any candidate from this (necessarily non-empty) map
    -- (Any element will do, because all elements are equally preferable)
    head' :: Map a b -> b
    head' = snd . head . Map.toList

-- | Update candidates
updateCandidates :: ( OuroborosTag (BlockProtocol hdr)
                    , HasHeader hdr
                    , Ord up
                    , Eq hdr
                    , hdr ~ blk
                    )
                 => NodeConfig (BlockProtocol hdr)
                 -> Slot            -- ^ Present slot
                 -> Chain blk       -- ^ Our chain
                 -> (up, Chain hdr) -- ^ New potential candidate
                 -> Candidates up hdr -> Candidates up hdr
updateCandidates cfg now ourChain (up, theirChain) cs =
    case preferCandidate cfg now ourChain theirChain of
      Just cand' -> insertCandidate cfg now (up, cand') cs'
      Nothing    -> cs' -- if candidate not strictly preferred, we ignore it
  where
    -- We unconditionally remove the old chain
    --
    -- This means that /if/ we for some reason prefer the old candidate from
    -- this node but not the new, we nonetheless forget this old candidate.
    -- This is reasonable: the node might not even be able to serve this
    -- old candidate anymore.
    --
    -- TODO: Discuss this with Duncan.
    cs' = removeCandidate up cs

{-------------------------------------------------------------------------------
  New network layer
-------------------------------------------------------------------------------}

data NetworkRequires m up blk hdr = NetworkRequires {
      -- | Start tracking a new upstream node
      --
      -- Although it is (typically) the responsibility of the network layer to
      -- decide whether or not to track another peer, each time it does decide
      -- to do so, it will ask the consensus layer for a client to track this
      -- upstream node. It will be the responsibility of this client to do
      -- block validation and implement the logic required to implement the
      -- genesis rule.
      nrSyncClient   :: up -> Consensus ChainSyncClient hdr m

      -- | Return the current chain
    , nrCurrentChain :: Tr m (Chain blk)

      -- | Get current chain candidates
      --
      -- This is used by the network layer's block download logic.
      -- See 'CandidateChains' for more details.
    , nrCandidates   :: Tr m (Candidates up hdr)
    }

-- | Required by the network layer to initiate comms to a new node
data NodeComms m hdr s r = NodeComms {
      -- | Codec for concrete send type @s@ and receive type @r@
      ncCodec    :: Codec m Text s r (ChainSyncMessage hdr (Point hdr)) 'StIdle

      -- | Construct a channel to the node
      --
      -- This is in CPS style to allow for resource allocation. However, it
      -- is important to note that this resource allocation will run in a thread
      -- which itself is untracked, so if resource deallocation absolutely
      -- /must/ happen additional measures must be taken
    , ncWithChan :: forall a. (Duplex m m s r -> m a) -> m a
    }

data NetworkProvides m up blk hdr = NetworkProvides {
      -- | The chains downloaded by the network layer
      --
      -- It will be the responsibility of the consensus layer to monitor this,
      -- validate new chains when downloaded, and adopt them when appropriate.
      npDownloaded    :: Tr m [Chain blk]

      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , npAddUpstream   :: forall s r. up -> NodeComms m hdr s r -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , npAddDownstream :: forall s r. NodeComms m hdr s r -> m ()
    }

initNetworkLayer :: forall m up hdr blk.
                    ( MonadSTM m
                    , HasHeader hdr
                    , hdr ~ blk     -- TODO: for now
                    , Eq blk        -- TODO: for now
                    )
                 => NetworkRequires m up blk hdr
                 -> m (NetworkProvides m up blk hdr)
initNetworkLayer NetworkRequires{..} = do
    -- The chain producer state is entirely the responsibility of the network
    -- layer; it does not get exposed in the 'NetworkLayer' API. Moreover, it
    -- is not necessary for the chain in the chain producer state to be updated
    -- synchronously with our chain, it is fine for this to lag.
    cpsVar <- atomically $ newTVar . initChainProducerState =<< nrCurrentChain

    -- Downloaded chains
    downloadedVar <- atomically $ newTVar []

    -- We continuously monitor the chain candidates, and download one as soon
    -- as one becomes available. This is merely a mock implementation, we make
    -- all chains available as soon as they are candidates.
    fork $ forever $ atomically $ do
      downloaded <- readTVar downloadedVar
      candidates <- (concatMap Map.elems . candidates) <$> nrCandidates
      if candidates == downloaded
        then retry
        else writeTVar downloadedVar candidates

    -- We also continously monitor the our own chain, so that we can update our
    -- downstream peers when our chain changes
    fork $ forever $ atomically $ do
      chain <- nrCurrentChain
      cps   <- readTVar cpsVar
      -- TODO: We should probably not just compare the slot
      if Chain.headSlot chain == Chain.headSlot (chainState cps)
        then retry
        else modifyTVar cpsVar (switchFork chain)

    return $ NetworkProvides {
          npDownloaded = readTVar downloadedVar
        , npAddDownstream = \NodeComms{..} -> do
            let producer = chainSyncServerPeer (chainSyncServerExample () cpsVar)
            fork $ void $ ncWithChan $ \chan ->
              useCodecWithDuplex chan ncCodec producer
        , npAddUpstream = \up NodeComms{..} -> do
            let consumer = nrSyncClient up
            fork $ void $ ncWithChan $ \chan ->
              useCodecWithDuplex chan ncCodec (chainSyncClientPeer consumer)
        }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | The suffix of the chain starting after the specified point
afterPoint :: HasHeader b => Point b -> Chain b -> [b]
afterPoint p = dropWhile (\b -> blockSlot b <= Chain.pointSlot p)
            . Chain.toOldestFirst

dropMostRecent :: Chain b -> Chain b
dropMostRecent Genesis  = error "dropMostRecent: empty chain"
dropMostRecent (c :> _) = c

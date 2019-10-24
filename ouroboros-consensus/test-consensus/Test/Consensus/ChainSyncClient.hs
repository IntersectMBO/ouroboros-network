{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.ChainSyncClient ( tests ) where

import           Control.Monad (replicateM_, void)
import           Control.Monad.Except (runExcept)
import           Control.Monad.State.Strict
import           Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import           Data.Bifunctor (first)
import           Data.Foldable (foldl')
import           Data.List (intercalate, span, unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim (runSimOrThrow)

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec (CodecFailure)
import           Network.TypedProtocol.Driver

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block hiding (ChainUpdate (..))
import           Ouroboros.Network.MockChain.Chain (Chain (Genesis))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState (chainState,
                     initChainProducerState)
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.Point (WithOrigin (..), blockPointHash,
                     blockPointSlot)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import           Cardano.Crypto.DSIGN.Mock

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.Ledger.Extended hiding (ledgerState)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock
import           Test.Util.Time (ioSimSecondsToDiffTime)
import           Test.Util.Tracer (recordingTracerTVar)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "ChainSyncClient"
    [ testProperty "chainSync"                 $ prop_chainSync
    , testProperty "joinUpdates/spreadUpdates" $ prop_joinUpdates_spreadUpdates k
    , testProperty "genChainUpdates"           $ prop_genChainUpdates           k updatesToGenerate
    , testProperty "genUpdateSchedule slots do not extend in the future" $
      prop_genUpdateSchedule_notInFuture k
    ]
  where
    k = SecurityParam 3

updatesToGenerate :: Int
updatesToGenerate = 100


{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

prop_chainSync :: ChainSyncClientSetup -> Property
prop_chainSync ChainSyncClientSetup {..} =
    counterexample
    ("Client chain: "      <> ppChain clientChain        <> "\n" <>
     "Server chain: "      <> ppChain serverChain        <> "\n" <>
     "Synched fragment: "  <> ppFragment synchedFragment <> "\n" <>
     "Trace:\n"            <> unlines (map ppTraceEvent events)) $
    -- If an exception has been thrown, we check that it was right to throw
    -- it, but not the other way around: we don't check whether a situation
    -- has occured where an exception should have been thrown, but wasn't.
    case mbEx of
      Just (ForkTooDeep { _intersection = intersection })     ->
        label "ForkTooDeep" $
        counterexample ("ForkTooDeep intersection: " <> ppPoint intersection) $
        not (AF.withinFragmentBounds intersection clientFragment)
      Just (InvalidRollBack { _newPoint = intersection }) ->
        label "InvalidRollBack" $
        counterexample ("InvalidRollBack intersection: " <> ppPoint intersection) $
        not (AF.withinFragmentBounds intersection synchedFragment)
      Just (NoMoreIntersection { _ourTip   = Our   (ExampleTip ourHead   _)
                               , _theirTip = Their (ExampleTip theirHead _)
                               }) ->
        label "NoMoreIntersection" $
        counterexample ("NoMoreIntersection ourHead: " <> ppPoint ourHead <>
                        ", theirHead: " <> ppPoint theirHead) $
        not (clientFragment `forksWithinK` synchedFragment)
      Just e ->
        counterexample ("Exception: " ++ displayException e) False
      Nothing ->
        counterexample "Synced fragment not a suffix of the server chain"
        (synchedFragment `isSuffixOf` serverChain) .&&.
        counterexample "Synced fragment doesn't intersect with the client chain"
        (clientFragment `forksWithinK` synchedFragment) .&&.
        counterexample "Synced fragment doesn't have the same anchor as the client fragment"
        (AF.anchorPoint clientFragment === AF.anchorPoint synchedFragment)
  where
    k = maxRollbacks securityParam

    (clientChain, serverChain, synchedFragment, mbEx, events) = runSimOrThrow $
      runChainSync securityParam maxClockSkew clientUpdates serverUpdates
                   startSlot

    clientFragment = AF.anchorNewest k $ Chain.toAnchoredFragment clientChain

    forksWithinK
      :: AnchoredFragment TestBlock  -- ^ Our chain
      -> AnchoredFragment TestBlock  -- ^ Their chain
      -> Bool
    forksWithinK ourChain theirChain = case AF.intersect ourChain theirChain of
      Nothing -> False
      Just (_ourPrefix, _theirPrefix, ourSuffix, _theirSuffix) ->
        fromIntegral (AF.length ourSuffix) <= k

-- | Check whether the anchored fragment is a suffix of the chain.
isSuffixOf :: AnchoredFragment TestBlock -> Chain TestBlock -> Property
isSuffixOf fragment chain =
    fragmentAnchor === chainAnchor .&&.  fragmentBlocks === chainBlocks
  where
    nbBlocks       = AF.length fragment
    fragmentBlocks = AF.toOldestFirst fragment
    fragmentAnchor = AF.anchorPoint fragment
    chainBlocks    = reverse $ take nbBlocks $ Chain.toNewestFirst chain
    chainAnchor    = Chain.headPoint $ Chain.drop nbBlocks chain

{-------------------------------------------------------------------------------
  Infastructure to run a Chain Sync test
-------------------------------------------------------------------------------}

-- | Chain Sync Client
clientId :: CoreNodeId
clientId = CoreNodeId 0

-- | Chain Sync Server
serverId :: CoreNodeId
serverId = CoreNodeId 1

-- | Terser notation
type ChainSyncException = ChainSyncClientException TestBlock (ExampleTip TestBlock)

-- | Using slots as times, a schedule plans updates to a chain on certain
-- slots.
--
-- TODO Note that a schedule can't express delays between the messages sent
-- over the chain sync protocol. Generating such delays may expose more (most
-- likely concurrency-related) bugs.
type Schedule a = Map SlotNo [ChainUpdate]

-- | Return the last slot at which an update is planned, if no updates are
-- planned, return 0.
lastSlot :: Schedule a -> SlotNo
lastSlot = fromMaybe (SlotNo 0) . maxKey
  where
    maxKey :: forall k v. Map k v -> Maybe k
    maxKey = fmap (fst . fst) . Map.maxViewWithKey

data ChainUpdate
  = SwitchFork (Point TestBlock) [TestBlock]
  | AddBlock TestBlock
  deriving (Eq, Show)

toChainUpdates :: [ChainUpdate] -> [Chain.ChainUpdate TestBlock TestBlock]
toChainUpdates = concatMap $ \case
    SwitchFork pt bs -> Chain.RollBack pt : map Chain.AddBlock bs
    AddBlock b       -> Chain.AddBlock b  : []

chainUpdateHighestSlotNo :: ChainUpdate -> SlotNo
chainUpdateHighestSlotNo cu = tbSlot $ case cu of
    AddBlock b      -> b
    SwitchFork _ bs -> last bs

newtype ClientUpdates =
  ClientUpdates { getClientUpdates :: Schedule [ChainUpdate] }
  deriving (Show)

newtype ServerUpdates =
  ServerUpdates { getServerUpdates :: Schedule [ChainUpdate] }
  deriving (Show)

type TraceEvent = (SlotNo, Either
  (TraceChainSyncClientEvent TestBlock (ExampleTip TestBlock))
  (TraceSendRecv (ChainSync (Header TestBlock) (ExampleTip TestBlock))
                 CoreNodeId
                 CodecFailure))

-- | We have a client and a server chain that both start at genesis. At
-- certain slots, we apply updates to both of these chains to simulate changes
-- to the chains.
--
-- At a certain slot, we start the chain sync protocol with a \"real\" chain
-- sync client and the example chain sync server. The chain sync client will
-- start to maintain a candidate fragment that is following the server chain.
-- Note that if client and/or server updates are scheduled at the same slot as
-- the start of the syncing, then those updates are applied before syncing
-- starts.
--
-- Both the client and server chain will keep on receiving updates. The chain
-- sync client will keep the candidate fragment in sync with the updating
-- server chain.
--
-- At the end, we return the final chains, the synched candidate fragment, and
-- any exception thrown by the chain sync client. The candidate fragment can
-- then be compared to the actual server chain. If an exception was thrown, no
-- more chain updates are applied so the state at the time of the exception is
-- returned.
--
-- Note that updates that are scheduled before the slot at which we start
-- syncing help generate different chains to start syncing from.
runChainSync
    :: forall m. IOLike m
    => SecurityParam
    -> ClockSkew
    -> ClientUpdates
    -> ServerUpdates
    -> SlotNo  -- ^ Start chain syncing at this slot.
    -> m (Chain TestBlock, Chain TestBlock,
          AnchoredFragment TestBlock, Maybe ChainSyncException,
          [TraceEvent])
       -- ^ (The final client chain, the final server chain, the synced
       --    candidate fragment, exception thrown by the chain sync client,
       --    the traced ChainSync and protocol events)
runChainSync securityParam maxClockSkew (ClientUpdates clientUpdates)
    (ServerUpdates serverUpdates) startSyncingAt = withRegistry $ \registry -> do

    testBtime <- newTestBlockchainTime registry numSlots
      (\_s -> threadDelay (ioSimSecondsToDiffTime 100000))
    let btime = testBlockchainTime testBtime

    -- Set up the client
    varCandidates      <- uncheckedNewTVarM Map.empty
    varClientState     <- uncheckedNewTVarM (Genesis, testInitExtLedger)
    varClientException <- uncheckedNewTVarM Nothing
    -- Candidates are removed from the candidates map when disconnecting, so
    -- we lose access to them. Therefore, store the candidate 'TVar's in a
    -- separate map too, one that isn't emptied. We can use this map to look
    -- at the final state of each candidate.
    varFinalCandidates <- uncheckedNewTVarM Map.empty

    (tracer, getTrace) <- first (addSlotNo btime) <$> recordingTracerTVar
    let chainSyncTracer = contramap Left  tracer
        protocolTracer  = contramap Right tracer

    let chainDbView :: ChainDbView m TestBlock (ExampleTip TestBlock)
        chainDbView = ChainDbView
          { getCurrentChain   =
              AF.mapAnchoredFragment TestHeader . AF.anchorNewest k .
              Chain.toAnchoredFragment . fst <$>
              readTVar varClientState
          , getCurrentLedger  = snd <$> readTVar varClientState
          , getOurTip         = do
              chain <- fst <$> readTVar varClientState
              return $ ExampleTip (Chain.headPoint chain) (Chain.headBlockNo chain)
          , getIsInvalidBlock = return $
              WithFingerprint (const False) (Fingerprint 0)
          }

        client :: StrictTVar m (AnchoredFragment (Header TestBlock))
               -> Consensus ChainSyncClientPipelined
                    TestBlock
                    (ExampleTip TestBlock)
                    m
        client = chainSyncClient
                   (pipelineDecisionLowHighMark 10 20)
                   exampleTipBlock
                   chainSyncTracer
                   (nodeCfg clientId)
                   btime
                   maxClockSkew
                   chainDbView

    -- Set up the server
    varChainProducerState <- uncheckedNewTVarM $ initChainProducerState Genesis
    let server :: ChainSyncServer (Header TestBlock) (ExampleTip TestBlock) m ()
        server = chainSyncServerExample () varChainProducerState

    -- Schedule updates of the client and server chains
    varLastUpdate <- uncheckedNewTVarM 0
    onSlotChange btime $ \slot -> do
      -- Stop updating the client and server chains when the chain sync client
      -- has thrown an exception, so that at the end, we can read the chains
      -- in the states they were in when the exception was thrown.
      stop <- fmap isJust $ atomically $ readTVar varClientException
      unless stop $ do
        -- Client
        whenJust (Map.lookup slot clientUpdates) $ \chainUpdates ->
          atomically $ do
            (chain, ledger) <- readTVar varClientState
            writeTVar varClientState $
              updateClientState (nodeCfg clientId) chain ledger chainUpdates

        -- Server
        whenJust (Map.lookup slot serverUpdates) $ \chainUpdates ->
          atomically $ do
            chainProducerState <- readTVar varChainProducerState
            case CPS.applyChainUpdates
                   (map (fmap TestHeader) (toChainUpdates chainUpdates))
                   chainProducerState of
              Just chainProducerState' ->
                writeTVar varChainProducerState chainProducerState'
              Nothing                  ->
                error $ "Invalid chainUpdates: " <> show chainUpdates <>
                        " for " <> show (chainState chainProducerState)
        atomically $ writeTVar varLastUpdate slot

    -- Connect client to server and run the chain sync protocol
    onSlot btime startSyncingAt $ do
      -- When updates are planned at the same slot that we start syncing, we
      -- wait until these updates are done before we start syncing.
      when (isJust (Map.lookup startSyncingAt clientUpdates) ||
            isJust (Map.lookup startSyncingAt serverUpdates)) $
        atomically $ do
          lastUpdate <- readTVar varLastUpdate
          check (lastUpdate == startSyncingAt)

      (clientChannel, serverChannel) <- createConnectedChannels
      -- Don't link the thread (which will cause the exception to be rethrown
      -- in the main thread), just catch the exception and store it, because
      -- we want a "regular ending".
      void $ forkThread registry $
        bracketChainSyncClient
           chainSyncTracer
           chainDbView
           varCandidates
           serverId $ \varCandidate -> do
             atomically $ modifyTVar varFinalCandidates $
               Map.insert serverId varCandidate
             runPipelinedPeer protocolTracer codecChainSyncId serverId clientChannel $
               chainSyncClientPeerPipelined $ client varCandidate
        `catch` \(e :: ChainSyncException) -> do
          -- TODO: Is this necessary? Wouldn't the Async's internal MVar do?
          atomically $ writeTVar varClientException (Just e)
          -- Rethrow, but it will be ignored anyway.
          throwM e
      void $ forkLinkedThread registry $
        runPeer nullTracer codecChainSyncId clientId serverChannel
                (chainSyncServerPeer server)

    testBlockchainTimeDone testBtime
    -- Wait a random amount of time after the final slot for the chain sync
    -- to finish
    threadDelay 2000

    trace <- getTrace
    -- Collect the return values
    atomically $ do
      clientChain       <- fst <$> readTVar varClientState
      serverChain       <- chainState <$> readTVar varChainProducerState
      candidateFragment <- readTVar varFinalCandidates >>= readTVar . (Map.! serverId)
      clientException   <- readTVar varClientException
      return (
          clientChain
        , fmap testHeader serverChain
        , AF.mapAnchoredFragment testHeader candidateFragment
        , clientException
        , trace
        )
  where
    k = maxRollbacks securityParam

    nodeCfg :: CoreNodeId -> NodeConfig (Bft BftMockCrypto)
    nodeCfg coreNodeId = BftNodeConfig
      { bftParams   = BftParams
        { bftSecurityParam = securityParam
        , bftNumNodes      = 2
        }
      , bftNodeId   = fromCoreNodeId coreNodeId
      , bftSignKey  = SignKeyMockDSIGN 0
      , bftVerKeys  = Map.fromList
                      [ (CoreId 0, VerKeyMockDSIGN 0)
                      , (CoreId 1, VerKeyMockDSIGN 1)
                      ]
      }

    -- | Take the last slot at which a client or server update is planned, or
    -- the slot at which syncing starts, and add one to it
    numSlots :: NumSlots
    numSlots = NumSlots $ fromIntegral $ unSlotNo $ succ $ maximum
      [ lastSlot clientUpdates
      , lastSlot serverUpdates
      , startSyncingAt
      ]

    addSlotNo :: forall ev. BlockchainTime m
              -> Tracer m (SlotNo, ev)
              -> Tracer m ev
    addSlotNo btime tr = Tracer $ \ev -> do
      slot <- atomically $ getCurrentSlot btime
      traceWith tr (slot, ev)

getAddBlock :: ChainUpdate -> Maybe TestBlock
getAddBlock (AddBlock b)    = Just b
getAddBlock (SwitchFork {}) = Nothing

updateClientState :: NodeConfig (Bft BftMockCrypto)
                  -> Chain TestBlock
                  -> ExtLedgerState TestBlock
                  -> [ChainUpdate]
                  -> (Chain TestBlock, ExtLedgerState TestBlock)
updateClientState cfg chain ledgerState chainUpdates =
    case forwardOnlyOrNot chainUpdates of
      -- If the updates don't contain a roll back, we can incrementally
      -- validate the chain
      Just bs -> (chain', ledgerState')
        where
          chain'       = foldl' (flip Chain.addBlock) chain bs
          ledgerState' = runValidate $
            foldExtLedgerState BlockNotPreviouslyApplied cfg bs ledgerState
      Nothing
      -- There was a roll back in the updates, so validate the chain from
      -- scratch
        | Just chain' <- Chain.applyChainUpdates (toChainUpdates chainUpdates) chain
        -> let ledgerState' = runValidate $
                 foldExtLedgerState BlockNotPreviouslyApplied cfg (Chain.toOldestFirst chain') testInitExtLedger
           in (chain', ledgerState')
        | otherwise
        -> error "Client chain update failed"
  where
    forwardOnlyOrNot :: [ChainUpdate] -> Maybe [TestBlock]
    forwardOnlyOrNot = traverse getAddBlock

    runValidate m = case runExcept m of
      Left  _ -> error "Client ledger validation error"
      Right x -> x

{-------------------------------------------------------------------------------
  ChainSyncClientSetup
-------------------------------------------------------------------------------}

-- | Bundle dependent arguments for test generation
data ChainSyncClientSetup = ChainSyncClientSetup
  { securityParam :: SecurityParam
  , maxClockSkew  :: ClockSkew
  , clientUpdates :: ClientUpdates
    -- ^ Depends on 'securityParam' and 'clientUpdates'
  , serverUpdates :: ServerUpdates
    -- ^ Depends on 'securityParam' and 'clientUpdates'
  , startSlot     :: SlotNo
    -- ^ Depends on 'clientUpdates' and 'serverUpdates'
  }

instance Arbitrary ChainSyncClientSetup where
  arbitrary = do
    securityParam  <- SecurityParam <$> choose (2, 5)
    maxClockSkew   <- arbitrary
    clientUpdates0 <- evalStateT
      (ClientUpdates <$> genUpdateSchedule securityParam maxClockSkew)
      emptyUpdateState
    serverUpdates  <- evalStateT
      (ServerUpdates <$> genUpdateSchedule securityParam maxClockSkew)
      emptyUpdateState
    let clientUpdates = removeLateClientUpdates serverUpdates clientUpdates0
        maxStartSlot  = unSlotNo $ maximum
          [ 1
          , lastSlot (getClientUpdates clientUpdates) - 1
          , lastSlot (getServerUpdates serverUpdates) - 1 ]
    startSlot <- SlotNo <$> choose (1, maxStartSlot)
    return ChainSyncClientSetup {..}
  shrink cscs@ChainSyncClientSetup {..} =
    -- We don't shrink 'securityParam' and 'maxClockSkew', because the updates
    -- depend on them.
    [ cscs
      { serverUpdates = ServerUpdates serverUpdates'
      , clientUpdates = removeLateClientUpdates
                          (ServerUpdates serverUpdates')
                          clientUpdates
      , startSlot     = startSlot'
      }
    | serverUpdates' <- shrinkUpdateSchedule (getServerUpdates serverUpdates)
    , let maxStartSlot = maximum
            [ 1
            , lastSlot (getClientUpdates clientUpdates) - 1
            , lastSlot serverUpdates' - 1 ]
    , startSlot' <- [1..min startSlot maxStartSlot]
    ] <>
    [ cscs
      { clientUpdates = clientUpdates'
      , startSlot     = startSlot'
      }
    | clientUpdates' <-
        removeLateClientUpdates serverUpdates . ClientUpdates <$>
        shrinkUpdateSchedule (getClientUpdates clientUpdates)
    , let maxStartSlot = maximum
            [ 1
            , lastSlot (getClientUpdates clientUpdates') - 1
            , lastSlot (getServerUpdates serverUpdates)  - 1 ]
    , startSlot' <- [1..min startSlot maxStartSlot]
    ]

instance Show ChainSyncClientSetup where
  show ChainSyncClientSetup {..} = unlines
      [ "ChainSyncClientSetup:"
      , "securityParam: " <> show (maxRollbacks securityParam)
      , "maxClockSkew: "  <> show (unClockSkew maxClockSkew)
      , "clientUpdates:"
      , ppUpdates (getClientUpdates clientUpdates) <> "--"
      , "serverUpdates:"
      , ppUpdates (getServerUpdates serverUpdates) <> "--"
      , "startSlot: " <> show (unSlotNo startSlot)
      ]

-- | Remove client updates that happen at a slot after the slot in which the
-- last server updates happened.
--
-- If we don't do this, the client's chain might no longer intersect with the
-- synced candidate. This is because the ChainSync protocol won't have had a
-- chance to update the candidate fragment, as the code to handle this case
-- (our chain has changed such that it no longer intersects with the synched
-- candidate -> initiate the \"find a new intersection\" part of the protocol)
-- is run when we receive new messages (roll forward/backward) from the
-- server.
removeLateClientUpdates :: ServerUpdates -> ClientUpdates -> ClientUpdates
removeLateClientUpdates (ServerUpdates sus)
    | Just ((lastServerUpdateSlotNo, _), _) <- Map.maxViewWithKey sus
    = \(ClientUpdates cus) ->
       let (cus', _) = Map.split (succ lastServerUpdateSlotNo) cus
           -- @cus'@ contains the entries with a key < @succ
           -- lastServerUpdateSlotNo@
       in ClientUpdates cus'
    | otherwise
    = id

{-------------------------------------------------------------------------------
  Generating a schedule of updates
-------------------------------------------------------------------------------}

genUpdateSchedule
  :: SecurityParam -> ClockSkew
  -> StateT ChainUpdateState Gen (Schedule [ChainUpdate])
genUpdateSchedule securityParam maxClockSkew = do
    cus  <- get
    cus' <- lift $ genChainUpdates securityParam 10 cus
    put cus'
    let chainUpdates = getChainUpdates cus'
    lift $ spreadUpdates maxClockSkew chainUpdates

-- | Repeatedly remove the last entry (highest SlotNo)
shrinkUpdateSchedule :: Schedule [ChainUpdate]
                     -> [Schedule [ChainUpdate]]
shrinkUpdateSchedule = unfoldr (fmap (\(_, m) -> (m, m)) . Map.maxView)

-- | Spread out updates over a schedule, i.e. schedule a number of updates to
-- be executed on each slot. We use slot as a proxy for time. Most slots will
-- have no planned updates.
--
-- Note that the current slot (from the BlockchainTime) is used to reject
-- chains that extend too much in the future (see 'ClockSkew'), so be careful
-- not to schedule updates that add blocks with slot numbers from the future.
--
-- Don't shedule updates at slot 0, because 'onEachChange' isn't called for
-- 0.
--
-- Each roll back of @x@ blocks will be immediately followed (in the same
-- slot) by adding @y@ blocks, where @y >= x@.
spreadUpdates :: ClockSkew
              -> [ChainUpdate]
              -> Gen (Schedule [ChainUpdate])
spreadUpdates (ClockSkew maxClockSkew) = go Map.empty 1
  where
    go !schedule slot updates
      | null updates = return schedule
      | otherwise    = do
        nbUpdates <- frequency [ (2, return 0), (1, choose (1, 5)) ]
        let maxSlot = SlotNo (unSlotNo slot + maxClockSkew)
            (updates', tooFarInTheFuture) =
              span ((<= maxSlot) . chainUpdateHighestSlotNo) updates
            (this, rest) = splitAt nbUpdates updates'
        go (Map.insert slot this schedule) (succ slot) (rest <> tooFarInTheFuture)

-- | Inverse of 'spreadUpdates'
joinUpdates :: Schedule [ChainUpdate] -> [ChainUpdate]
joinUpdates = concatMap snd . Map.toAscList

prop_joinUpdates_spreadUpdates :: SecurityParam -> ClockSkew -> Property
prop_joinUpdates_spreadUpdates securityParam maxClockSkew =
    forAll genUpdatesAndSpread $ \(updates, spread) ->
      joinUpdates spread === updates
  where
    genUpdatesAndSpread = do
      updates <- getChainUpdates <$>
                 genChainUpdates securityParam updatesToGenerate emptyUpdateState
      spread  <- spreadUpdates maxClockSkew updates
      return (updates, spread)

-- | Test that we don't generate updates that add blocks with slots that are
-- too far in the future.
prop_genUpdateSchedule_notInFuture :: SecurityParam -> ClockSkew -> Property
prop_genUpdateSchedule_notInFuture securityParam maxClockSkew =
    forAll genUpdateSchedule' $ \updates -> conjoin
      [ unSlotNo (chainUpdateHighestSlotNo chainUpdate) <= unSlotNo slot + maxSkew
      | (slot, chainUpdates) <- Map.toAscList updates
      , chainUpdate          <- chainUpdates
      ]
  where
    genUpdateSchedule' = evalStateT
      (genUpdateSchedule securityParam maxClockSkew)
      emptyUpdateState
    maxSkew = unClockSkew maxClockSkew

{-------------------------------------------------------------------------------
  Generating ChainUpdates
-------------------------------------------------------------------------------}

-- | We need some state to generate @ChainUpdate@s
data ChainUpdateState = ChainUpdateState
  { _currentChain :: !(Chain TestBlock)
    -- ^ The current chain, obtained by applying all the '_updates' in reverse
    -- order.
  , _updates      :: ![ChainUpdate]
    -- ^ The updates that have been generated so far, in reverse order: the
    -- first update in the list is the last update to apply.
  } deriving (Show)

emptyUpdateState :: ChainUpdateState
emptyUpdateState = ChainUpdateState
  { _currentChain = Genesis
  , _updates      = []
  }

getChainUpdates :: ChainUpdateState -> [ChainUpdate]
getChainUpdates = reverse . _updates

-- | Test that applying the generated updates gives us the same chain as
-- '_currentChain'.
prop_genChainUpdates :: SecurityParam -> Int -> Property
prop_genChainUpdates securityParam n =
    forAll (genChainUpdates securityParam n emptyUpdateState) $ \cus ->
      Chain.applyChainUpdates (toChainUpdates (getChainUpdates cus)) Genesis ===
      Just (_currentChain cus)

genChainUpdates :: SecurityParam
                -> Int  -- ^ The number of updates to generate
                -> ChainUpdateState
                -> Gen ChainUpdateState
genChainUpdates securityParam n =
    execStateT (replicateM_ n genChainUpdate)
  where
    -- Modify the state
    addUpdate u cus = cus { _updates = u : _updates cus }
    setChain  c cus = cus { _currentChain = c }

    k = fromIntegral $ maxRollbacks securityParam

    genChainUpdate = do
      ChainUpdateState { _currentChain = chain } <- get
      frequency'
        [ (3, genAddBlock)
        , (if Chain.null chain then 0 else 1, genSwitchFork)
        ]

    genForkNo = frequency
      [ (1, return 0)
      , (1, choose (1, 2))
      ]

    genBlockToAdd = do
      ChainUpdateState { _currentChain = chain } <- get
      block <- lift $ case Chain.head chain of
        Nothing      -> firstBlock <$> genForkNo
        Just curHead -> do
          forkNo <- genForkNo
          return $ modifyFork (const forkNo) (successorBlock curHead)
      modify $ setChain (Chain.addBlock block chain)
      return block

    genAddBlock = do
      block <- genBlockToAdd
      modify $ addUpdate (AddBlock block)

    genSwitchFork  = do
      ChainUpdateState { _currentChain = chain } <- get
      rollBackBlocks <- lift $ choose (1, k)
      let chain' = Chain.drop rollBackBlocks chain
      modify $ setChain chain'
      blocks <- replicateM rollBackBlocks genBlockToAdd
      modify $ addUpdate (SwitchFork (Chain.headPoint chain') blocks)

-- | Variant of 'frequency' that allows for transformers of 'Gen'
frequency' :: (MonadTrans t, Monad (t Gen)) => [(Int, t Gen a)] -> t Gen a
frequency' [] = error "frequency' used with empty list"
frequency' xs0 = lift (choose (1, tot)) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "pick used with empty list"


{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

ppBlock :: TestBlock -> String
ppBlock = condense

ppPoint :: Point TestBlock -> String
ppPoint (Point Origin)   = "Origin"
ppPoint (Point (At blk)) =
    "(S:" <> show s <> "; H:" <> show h <> ")"
  where
    SlotNo s = blockPointSlot blk
    h        = blockPointHash blk

ppChain :: Chain TestBlock -> String
ppChain = ppBlocks genesisPoint . Chain.toOldestFirst

ppFragment :: AnchoredFragment TestBlock -> String
ppFragment f = ppBlocks (AF.anchorPoint f) (AF.toOldestFirst f)

ppBlocks :: Point TestBlock -> [TestBlock] -> String
ppBlocks a bs = ppPoint a <> " ] " <> intercalate " :> " (map ppBlock bs)

ppUpdates :: Schedule [ChainUpdate] -> String
ppUpdates = unlines
          . map (uncurry showEntry)
          . filter (not . null . snd)
          . Map.toAscList
  where
    showEntry :: SlotNo -> [ChainUpdate] -> String
    showEntry (SlotNo slot) updates = show slot <> ": " <>
      intercalate ", " (map showChainUpdate updates)
    showChainUpdate :: ChainUpdate -> String
    showChainUpdate u = case u of
      AddBlock b -> "AddBlock " <> ppBlock b
      SwitchFork p bs -> "SwitchFork <- " <> ppPoint p <> " -> " <>
        unwords (map ppBlock bs)

ppTraceEvent :: TraceEvent -> String
ppTraceEvent (SlotNo n, ev) = show n <> " | " <> case ev of
    Left  cl -> "Client: "   <> show cl
    Right pt -> "Protocol: " <> show pt

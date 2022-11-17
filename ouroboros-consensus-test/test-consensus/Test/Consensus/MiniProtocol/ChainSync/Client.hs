{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.MiniProtocol.ChainSync.Client (tests) where

import           Control.Monad.Class.MonadThrow (Handler (..), catches)
import           Control.Monad.State.Strict
import           Control.Tracer (contramap, nullTracer)
import           Data.Bifunctor (first)
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import qualified Data.Set as Set
import           Data.Typeable

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Crypto.DSIGN.Mock

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (getTipPoint)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Mock.Chain (Chain (Genesis))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ProducerState (chainState,
                     initChainProducerState)
import qualified Ouroboros.Network.Mock.ProducerState as CPS
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..))
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended hiding (ledgerState)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Storage.ChainDB.API
                     (InvalidBlockReason (ValidationError))
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..), forkLinkedWatcher)

import           Test.Util.ChainUpdates (ChainUpdate (..), UpdateBehavior (..),
                     genChainUpdates, toChainUpdates)
import qualified Test.Util.LogicalClock as LogicalClock
import           Test.Util.LogicalClock (NumTicks (..), Tick (..))
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Schedule (Schedule (..), genSchedule, joinSchedule,
                     lastTick, shrinkSchedule)
import qualified Test.Util.TestBlock as TestBlock
import           Test.Util.TestBlock
import           Test.Util.Tracer (recordingTracerTVar)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "ChainSyncClient"
    [ testProperty "chainSync" prop_chainSync
    ]

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

prop_chainSync :: ChainSyncClientSetup -> Property
prop_chainSync ChainSyncClientSetup {..} =
    counterexample
    ("Client chain: "     <> ppChain finalClientChain  <> "\n" <>
     "Server chain: "     <> ppChain finalServerChain  <> "\n" <>
     "Synced fragment: "  <> ppFragment syncedFragment <> "\n" <>
     "Trace:\n"           <> unlines (map ppTraceEvent traceEvents)) $
    -- If an exception has been thrown, we check that it was right to throw
    -- it, but not the other way around: we don't check whether a situation
    -- has occured where an exception should have been thrown, but wasn't.
    case mbResult of
      Just (Right (ForkTooDeep intersection _ _))     ->
        label "ForkTooDeep" $
        counterexample ("ForkTooDeep intersection: " <> ppPoint intersection) $
        not (withinFragmentBounds intersection clientFragment)
      Just (Right (NoMoreIntersection (Our ourTip) (Their theirTip))) ->
        label "NoMoreIntersection" $
        counterexample ("NoMoreIntersection ourHead: " <> ppPoint (getTipPoint ourTip) <>
                        ", theirHead: " <> ppPoint (getTipPoint theirTip)) $
        not (clientFragment `forksWithinK` syncedFragment)
      Just (Right (RolledBackPastIntersection intersection _ _)) ->
        label "RolledBackPastIntersection" $
        counterexample ("RolledBackPastIntersection intersection: " <> ppPoint intersection) $
        not (withinFragmentBounds intersection syncedFragment)
      Just (Right result) ->
        counterexample ("Terminated with result: " ++ show result) False
      Just (Left ex) ->
        counterexample ("Exception: " ++ displayException ex) False
      Nothing ->
        counterexample "Synced fragment not a suffix of the server chain"
        (syncedFragment `isSuffixOf` finalServerChain) .&&.
        counterexample "Synced fragment doesn't intersect with the client chain"
        (clientFragment `forksWithinK` syncedFragment) .&&.
        counterexample "Synced fragment doesn't have the same anchor as the client fragment"
        (AF.anchorPoint clientFragment === AF.anchorPoint syncedFragment)
  where
    k = maxRollbacks securityParam

    ChainSyncOutcome {..} = runSimOrThrow $
      runChainSync securityParam clientUpdates serverUpdates invalidBlocks startTick

    clientFragment = AF.anchorNewest k $ Chain.toAnchoredFragment finalClientChain

    forksWithinK
      :: AnchoredFragment TestBlock  -- ^ Our chain
      -> AnchoredFragment TestBlock  -- ^ Their chain
      -> Bool
    forksWithinK ourChain theirChain = case AF.intersect ourChain theirChain of
      Nothing -> False
      Just (_ourPrefix, _theirPrefix, ourSuffix, _theirSuffix) ->
        fromIntegral (AF.length ourSuffix) <= k

-- | Generalization of 'AF.withinFragmentBounds' that returns false if the
-- types don't line up
--
-- This kind of "dynamic type checking" is a bit ugly but is only necessary
-- for the tests.
withinFragmentBounds :: forall blk blk'. (HasHeader blk, Typeable blk')
                     => Point blk -> AnchoredFragment blk' -> Bool
withinFragmentBounds p af =
    case eqT @blk @blk' of
      Just Refl -> AF.withinFragmentBounds p af
      Nothing   -> False

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

-- | Chain Sync Server
serverId :: CoreNodeId
serverId = CoreNodeId 1

newtype ClientUpdates =
  ClientUpdates { getClientUpdates :: Schedule ChainUpdate }
  deriving (Show)

newtype ServerUpdates =
  ServerUpdates { getServerUpdates :: Schedule ChainUpdate }
  deriving (Show)

-- | A 'Schedule' of events when we learn that a specific block is invalid. Note
-- that it is possible that learning that a block is invalid can precede us
-- receiving it from the ChainSync server (which models the possibility that
-- other peers already sent us that block earlier).
newtype InvalidBlocks =
  InvalidBlocks { getInvalidBlocks :: Schedule TestHash }
  deriving (Show)

type TraceEvent = (Tick, Either
  (TraceChainSyncClientEvent TestBlock)
  (TraceSendRecv (ChainSync (Header TestBlock) (Point TestBlock) (Tip TestBlock))))

data ChainSyncOutcome = ChainSyncOutcome {
      finalClientChain :: Chain TestBlock
    , finalServerChain :: Chain TestBlock
    , syncedFragment   :: AnchoredFragment TestBlock
    , mbResult         :: Maybe (Either ChainSyncClientException ChainSyncClientResult)
    , traceEvents      :: [TraceEvent]
    }

-- | We have a client and a server chain that both start at genesis. At
-- certain times, we apply updates to both of these chains to simulate changes
-- to the chains.
--
-- At a certain time, we start the chain sync protocol with a \"real\" chain
-- sync client and the example chain sync server. The chain sync client will
-- start to maintain a candidate fragment that is following the server chain.
-- Note that if client and/or server updates are scheduled at the same time as
-- the start of the syncing, then those updates are applied before syncing
-- starts.
--
-- Both the client and server chain will keep on receiving updates. The chain
-- sync client will keep the candidate fragment in sync with the updating
-- server chain.
--
-- At the end, we return the final chains, the synced candidate fragment, and
-- any exception thrown by the chain sync client. The candidate fragment can
-- then be compared to the actual server chain. If an exception was thrown, no
-- more chain updates are applied so the state at the time of the exception is
-- returned.
--
-- Note that updates that are scheduled before the time at which we start
-- syncing help generate different chains to start syncing from.
runChainSync
    :: forall m. IOLike m
    => SecurityParam
    -> ClientUpdates
    -> ServerUpdates
    -> InvalidBlocks
    -> Tick  -- ^ Start chain syncing at this time
    -> m ChainSyncOutcome
runChainSync securityParam (ClientUpdates clientUpdates)
    (ServerUpdates serverUpdates) (InvalidBlocks invalidBlocks)
    startSyncingAt = withRegistry $ \registry -> do

    clock <- LogicalClock.new registry numTicks

    -- Set up the client
    varCandidates   <- uncheckedNewTVarM Map.empty
    varClientState  <- uncheckedNewTVarM Genesis
    varClientResult <- uncheckedNewTVarM Nothing
    varKnownInvalid <- uncheckedNewTVarM mempty
    -- Candidates are removed from the candidates map when disconnecting, so
    -- we lose access to them. Therefore, store the candidate 'TVar's in a
    -- separate map too, one that isn't emptied. We can use this map to look
    -- at the final state of each candidate.
    varFinalCandidates <- uncheckedNewTVarM Map.empty

    (tracer, getTrace) <- first (LogicalClock.tickTracer clock) <$> recordingTracerTVar
    let chainSyncTracer = contramap Left  tracer
        protocolTracer  = contramap Right tracer

    let chainDbView :: ChainDbView m TestBlock
        chainDbView = ChainDbView
          { getCurrentChain =
              AF.mapAnchoredFragment TestHeader . AF.anchorNewest k .
                Chain.toAnchoredFragment <$>
                readTVar varClientState
          , getHeaderStateHistory =
              computeHeaderStateHistory nodeCfg <$>
                readTVar varClientState
          , getPastLedger     = \pt ->
              computePastLedger nodeCfg pt <$>
                readTVar varClientState
          , getIsInvalidBlock = do
              knownInvalid <- readTVar varKnownInvalid
              let isInvalidBlock hash =
                    if hash `Set.member` knownInvalid
                    then Just
                       . ValidationError
                       . ExtValidationErrorLedger
                       $ TestBlock.InvalidBlock
                    else Nothing
                  -- The set of known-invalid blocks grows monotonically (as a
                  -- function in the tick number), so its size can serve as a
                  -- fingerprint.
                  fp = Fingerprint $ fromIntegral $ Set.size knownInvalid
              pure $ WithFingerprint isInvalidBlock fp
          }

        client :: StrictTVar m (AnchoredFragment (Header TestBlock))
               -> Consensus ChainSyncClientPipelined
                    TestBlock
                    m
        client = chainSyncClient
                   (pipelineDecisionLowHighMark 10 20)
                   chainSyncTracer
                   nodeCfg
                   chainDbView
                   maxBound
                   (return Continue)
                   nullTracer

    -- Set up the server
    varChainProducerState <- uncheckedNewTVarM $ initChainProducerState Genesis
    let server :: ChainSyncServer (Header TestBlock) (Point TestBlock)
                                  (Tip TestBlock) m ()
        server = chainSyncServerExample () varChainProducerState

    -- Schedule updates of the client and server chains
    varLastUpdate <- uncheckedNewTVarM 0
    let forkLinkedTickWatcher :: (Tick -> m ()) -> m ()
        forkLinkedTickWatcher =
              void
            . forkLinkedWatcher registry "scheduled updates"
            . LogicalClock.tickWatcher clock
    forkLinkedTickWatcher $ \tick -> do
      -- Stop updating the client and server chains when the chain sync client
      -- has thrown an exception or has gracefully terminated, so that at the
      -- end, we can read the chains in the states they were in when the
      -- exception was thrown.
      stop <- fmap isJust $ atomically $ readTVar varClientResult
      unless stop $ do
        -- Newly discovered invalid blocks
        whenJust (Map.lookup tick (getSchedule invalidBlocks)) $
          atomically . modifyTVar varKnownInvalid . Set.union . Set.fromList

        -- Client
        whenJust (Map.lookup tick (getSchedule clientUpdates)) $ \chainUpdates ->
          atomically $ modifyTVar varClientState $ updateClientState chainUpdates

        -- Server
        whenJust (Map.lookup tick (getSchedule serverUpdates)) $ \chainUpdates ->
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
        atomically $ writeTVar varLastUpdate tick

    -- Connect client to server and run the chain sync protocol
    LogicalClock.onTick registry clock "startSyncing" startSyncingAt $ do
      -- When updates are planned at the same time that we start syncing, we
      -- wait until these updates are done before we start syncing.
      when (Map.member startSyncingAt (getSchedule clientUpdates) ||
            Map.member startSyncingAt (getSchedule serverUpdates)) $
        atomically $ do
          lastUpdate <- readTVar varLastUpdate
          check (lastUpdate == startSyncingAt)

      (clientChannel, serverChannel) <- createConnectedChannels
      -- Don't link the thread (which will cause the exception to be rethrown
      -- in the main thread), just catch the exception and store it, because
      -- we want a "regular ending".
      void $ forkThread registry "ChainSyncClient" $
        bracketChainSyncClient
           chainSyncTracer
           chainDbView
           varCandidates
           serverId
           maxBound $ \varCandidate -> do
             atomically $ modifyTVar varFinalCandidates $
               Map.insert serverId varCandidate
             (result, _) <-
               runPipelinedPeer protocolTracer codecChainSyncId clientChannel $
                 chainSyncClientPeerPipelined $ client varCandidate
             atomically $ writeTVar varClientResult (Just (Right result))
             return ()
        `catchAlsoLinked` \ex -> do
          atomically $ writeTVar varClientResult (Just (Left ex))
          -- Rethrow, but it will be ignored anyway.
          throwIO ex
      void $ forkLinkedThread registry "ChainSyncServer" $
        runPeer nullTracer codecChainSyncId serverChannel
                (chainSyncServerPeer server)

    LogicalClock.waitUntilDone clock
    -- Wait a random amount of time after the final tick for the chain sync
    -- to finish
    threadDelay 2000

    traceEvents <- getTrace
    -- Collect the return values
    atomically $ do
      finalClientChain       <- readTVar varClientState
      finalServerChain       <- chainState <$> readTVar varChainProducerState
      candidateFragment <- readTVar varFinalCandidates >>= readTVar . (Map.! serverId)
      mbResult      <- readTVar varClientResult
      return ChainSyncOutcome {
          finalServerChain = testHeader <$> finalServerChain
        , syncedFragment   = AF.mapAnchoredFragment testHeader candidateFragment
        , ..
        }
  where
    k = maxRollbacks securityParam

    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    nodeCfg :: TopLevelConfig TestBlock
    nodeCfg = TopLevelConfig {
        topLevelConfigProtocol = BftConfig {
            bftParams  = BftParams {
                             bftSecurityParam = securityParam
                           , bftNumNodes      = numCoreNodes
                           }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.fromList [
                             (CoreId (CoreNodeId 0), VerKeyMockDSIGN 0)
                           , (CoreId (CoreNodeId 1), VerKeyMockDSIGN 1)
                           ]
          }
      , topLevelConfigLedger  = eraParams
      , topLevelConfigBlock   = TestBlockConfig numCoreNodes
      , topLevelConfigCodec   = TestBlockCodecConfig
      , topLevelConfigStorage = TestBlockStorageConfig
      }

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams securityParam slotLength

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 2

    numTicks :: NumTicks
    numTicks = LogicalClock.sufficientTimeFor
      [ lastTick clientUpdates
      , lastTick serverUpdates
      , startSyncingAt
      ]

    catchAlsoLinked :: Exception e => m a -> (e -> m a) -> m a
    catchAlsoLinked ma handler = ma `catches`
      [ Handler handler
      , Handler $ \(ExceptionInLinkedThread _ ex) -> throwIO ex `catch` handler
      ]

updateClientState :: [ChainUpdate] -> Chain TestBlock -> Chain TestBlock
updateClientState chainUpdates chain =
    case Chain.applyChainUpdates (toChainUpdates chainUpdates) chain of
      Just chain' -> chain'
      Nothing     -> error "Client chain update failed"

-- | Simulates 'ChainDB.getPastLedger'.
computePastLedger ::
     TopLevelConfig TestBlock
  -> Point TestBlock
  -> Chain TestBlock
  -> Maybe (ExtLedgerState TestBlock)
computePastLedger cfg pt chain
    | pt `elem` validPoints
    = Just $ go testInitExtLedger (Chain.toOldestFirst chain)
    | otherwise
    = Nothing
  where
    SecurityParam k = configSecurityParam cfg

    curFrag :: AnchoredFragment TestBlock
    curFrag =
          AF.anchorNewest k
        . Chain.toAnchoredFragment
        $ chain

    validPoints :: [Point TestBlock]
    validPoints =
        AF.anchorPoint curFrag : map blockPoint (AF.toOldestFirst curFrag)

    -- | Apply blocks to the ledger state until we have applied the block
    -- matching @pt@, after which we return the resulting ledger.
    --
    -- PRECONDITION: @pt@ is in the list of blocks or genesis.
    go :: ExtLedgerState TestBlock -> [TestBlock] -> ExtLedgerState TestBlock
    go !st blks
        | castPoint (getTip st) == pt
        = st
        | blk:blks' <- blks
        = go (tickThenReapply (ExtLedgerCfg cfg) blk st) blks'
        | otherwise
        = error "point not in the list of blocks"

-- | Simulates 'ChainDB.getHeaderStateHistory'.
computeHeaderStateHistory ::
     TopLevelConfig TestBlock
  -> Chain TestBlock
  -> HeaderStateHistory TestBlock
computeHeaderStateHistory cfg =
      HeaderStateHistory.trim (fromIntegral k)
    . HeaderStateHistory.fromChain cfg testInitExtLedger
  where
    SecurityParam k = configSecurityParam cfg

{-------------------------------------------------------------------------------
  ChainSyncClientSetup
-------------------------------------------------------------------------------}

-- | Bundle dependent arguments for test generation
data ChainSyncClientSetup = ChainSyncClientSetup
  { securityParam :: SecurityParam
  , clientUpdates :: ClientUpdates
    -- ^ Depends on 'securityParam' and 'clientUpdates'
  , serverUpdates :: ServerUpdates
    -- ^ Depends on 'securityParam' and 'clientUpdates'
  , startTick     :: Tick
    -- ^ Depends on 'clientUpdates' and 'serverUpdates'
  , invalidBlocks :: InvalidBlocks
    -- ^ Blocks that are discovered to be invalid.
  }

instance Arbitrary ChainSyncClientSetup where
  arbitrary = do
    securityParam  <- SecurityParam <$> choose (2, 5)
    clientUpdates0 <- ClientUpdates <$>
      genUpdateSchedule SelectedChainBehavior securityParam
    serverUpdates  <- ServerUpdates <$>
      genUpdateSchedule TentativeChainBehavior securityParam
    let clientUpdates = removeLateClientUpdates serverUpdates clientUpdates0
        maxStartTick  = maximum
          [ Tick 1
          , lastTick (getClientUpdates clientUpdates) - 1
          , lastTick (getServerUpdates serverUpdates) - 1
          ]
    startTick <- choose (1, maxStartTick)
    let trapBlocks =
          [ blockHash b
          | AddBlock b <- joinSchedule (getServerUpdates serverUpdates)
          , tbValid b == Invalid
          ]
    invalidBlocks <- InvalidBlocks <$> (genSchedule =<< shuffle trapBlocks)
    return ChainSyncClientSetup {..}
  shrink cscs@ChainSyncClientSetup {..} =
    -- We don't shrink 'securityParam' because the updates depend on it

    -- We also don't shrink 'invalidBlocks' right now (as it does not impact
    -- correctness), but it might be confusing to see blocks in it that are not
    -- part of the update schedules.
    [ cscs
      { serverUpdates = ServerUpdates serverUpdates'
      , clientUpdates = removeLateClientUpdates
                          (ServerUpdates serverUpdates')
                          clientUpdates
      , startTick     = startTick'
      }
    | serverUpdates' <- shrinkSchedule (getServerUpdates serverUpdates)
    , let maxStartTick = maximum
            [ 1
            , lastTick (getClientUpdates clientUpdates) - 1
            , lastTick serverUpdates' - 1
            ]
    , startTick' <- [1..min startTick maxStartTick]
    ] <>
    [ cscs
      { clientUpdates = clientUpdates'
      , startTick     = startTick'
      }
    | clientUpdates' <-
        removeLateClientUpdates serverUpdates . ClientUpdates <$>
        shrinkSchedule (getClientUpdates clientUpdates)
    , let maxStartTick = maximum
            [ 1
            , lastTick (getClientUpdates clientUpdates') - 1
            , lastTick (getServerUpdates serverUpdates)  - 1
            ]
    , startTick' <- [1..min startTick maxStartTick]
    ]

instance Show ChainSyncClientSetup where
  show ChainSyncClientSetup {..} = unlines
      [ "ChainSyncClientSetup:"
      , "securityParam: " <> show (maxRollbacks securityParam)
      , "clientUpdates:"
      , condense (getClientUpdates clientUpdates) <> "--"
      , "serverUpdates:"
      , condense (getServerUpdates serverUpdates) <> "--"
      , "startTick: " <> show startTick
      , "invalidBlocks: "
      , condense (getInvalidBlocks invalidBlocks)
      ]

-- | Remove client updates that happen at a tick after the tick in which the
-- last server updates happened.
--
-- If we don't do this, the client's chain might no longer intersect with the
-- synced candidate. This is because the ChainSync protocol won't have had a
-- chance to update the candidate fragment, as the code to handle this case
-- (our chain has changed such that it no longer intersects with the synced
-- candidate -> initiate the \"find a new intersection\" part of the protocol)
-- is run when we receive new messages (roll forward/backward) from the
-- server.
removeLateClientUpdates :: ServerUpdates -> ClientUpdates -> ClientUpdates
removeLateClientUpdates (ServerUpdates (Schedule sus))
    | Just ((lastServerUpdateTickNo, _), _) <- Map.maxViewWithKey sus
    = \(ClientUpdates (Schedule cus)) ->
       let (cus', _) = Map.split (succ lastServerUpdateTickNo) cus
           -- @cus'@ contains the entries with a key < @succ
           -- lastServerUpdateTickNo@
       in ClientUpdates (Schedule cus')
    | otherwise
    = id

{-------------------------------------------------------------------------------
  Generating a schedule of updates
-------------------------------------------------------------------------------}

genUpdateSchedule
  :: UpdateBehavior
  -> SecurityParam
  -> Gen (Schedule ChainUpdate)
genUpdateSchedule updateBehavior securityParam =
    genChainUpdates updateBehavior securityParam 10 >>= genSchedule

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

ppBlock :: TestBlock -> String
ppBlock = condense

ppPoint :: StandardHash blk => Point blk -> String
ppPoint GenesisPoint              = "Origin"
ppPoint (BlockPoint (SlotNo s) h) = "(S:" <> show s <> "; H:" <> show h <> ")"

ppChain :: Chain TestBlock -> String
ppChain = ppBlocks GenesisPoint . Chain.toOldestFirst

ppFragment :: AnchoredFragment TestBlock -> String
ppFragment f = ppBlocks (AF.anchorPoint f) (AF.toOldestFirst f)

ppBlocks :: Point TestBlock -> [TestBlock] -> String
ppBlocks a bs = ppPoint a <> " ] " <> intercalate " :> " (map ppBlock bs)

ppTraceEvent :: TraceEvent -> String
ppTraceEvent (Tick n, ev) = show n <> " | " <> case ev of
    Left  cl -> "Client: "   <> show cl
    Right pt -> "Protocol: " <> show pt

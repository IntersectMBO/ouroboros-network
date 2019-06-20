{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.ChainSyncClient ( tests ) where

import           Control.Monad (replicateM_, void)
import           Control.Monad.Except (runExcept)
import           Control.Monad.State.Strict
import           Control.Tracer (nullTracer)
import           Data.Coerce (coerce)
import           Data.Foldable (foldl')
import           Data.List (intercalate, span, unfoldr)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust, maybe)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim (runSimOrThrow)

import           Network.TypedProtocol.Driver

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (Genesis))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (chainState,
                     initChainProducerState)
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Ledger.Extended hiding (ledgerState)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.TestBlock

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
    ("Client chain: "     <> ppChain clientChain <> "\n" <>
     "Server chain: "     <> ppChain serverChain <> "\n" <>
     "Synched fragment: " <> ppFragment synchedChain) $
    -- If an exception has been thrown, we check that it was right to throw
    -- it, but not the other way around: we don't check whether a situation
    -- has occured where an exception should have been thrown, but wasn't.
    case mbEx of
      Just (ForkTooDeep intersection _theirHead)     ->
        label "ForkTooDeep" $
        counterexample ("ForkTooDeep intersection: " <> ppPoint intersection) $
        not (AF.withinFragmentBounds intersection clientFragment)
      Just (InvalidRollBack intersection _theirHead) ->
        label "InvalidRollBack" $
        counterexample ("InvalidRollBack intersection: " <> ppPoint intersection) $
        not (AF.withinFragmentBounds intersection synchedChain)
      Just e ->
        counterexample ("Exception: " ++ displayException e) False
      Nothing ->
        synchedChain `isSuffix` serverChain .&&.
        -- TODO in the future we might strengthen this to: must fork at most k
        -- blocks back from the current tip
        synchedChain `intersects` clientChain
  where
    k = maxRollbacks securityParam

    (clientChain, serverChain, synchedChain, mbEx) = runSimOrThrow $
      runChainSync securityParam maxClockSkew clientUpdates serverUpdates
                   startSlot

    clientFragment = AF.anchorNewest k clientChain

-- | Check whether the anchored fragment is a suffix of the chain.
isSuffix :: AnchoredFragment TestBlock -> Chain TestBlock -> Property
isSuffix fragment chain =
    fragmentAnchor === chainAnchor .&&. fragmentBlocks === chainBlocks
  where
    nbBlocks       = AF.length fragment
    fragmentBlocks = AF.toOldestFirst fragment
    fragmentAnchor = AF.anchorPoint fragment
    chainBlocks    = reverse $ take nbBlocks $ Chain.toNewestFirst chain
    chainAnchor    = Chain.headPoint $ Chain.drop nbBlocks chain

-- | Check whether the anchored fragment intersects with the chain.
intersects :: AnchoredFragment TestBlock -> Chain TestBlock -> Bool
intersects fragment chain =
    isJust (AF.intersectionPoint fragment (AF.fromChain chain))

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
type ChainSyncException = ChainSyncClientException TestBlock

-- | Using slots as times, a schedule plans updates to a chain on certain
-- slots.
--
-- TODO Note that a schedule can't express delays between the messages sent
-- over the chain sync protocol. Generating such delays may expose more (most
-- likely concurrency-related) bugs.
type Schedule a = Map SlotNo [ChainUpdate TestBlock]

-- | Return the last slot at which an update is planned, if no updates are
-- planned, return 0.
lastSlot :: Schedule a -> SlotNo
lastSlot = fromMaybe (SlotNo 0) . maxKey
  where
    maxKey :: forall k v. Map k v -> Maybe k
    maxKey = fmap (fst . fst) . Map.maxViewWithKey


newtype ClientUpdates =
  ClientUpdates { getClientUpdates :: Schedule [ChainUpdate TestBlock] }
  deriving (Show)

newtype ServerUpdates =
  ServerUpdates { getServerUpdates :: Schedule [ChainUpdate TestBlock] }
  deriving (Show)

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
runChainSync :: forall m.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadTimer m
       , MonadThrow (STM m)
       )
    => SecurityParam
    -> ClockSkew
    -> ClientUpdates
    -> ServerUpdates
    -> SlotNo  -- ^ Start chain syncing at this slot.
    -> m (Chain TestBlock, Chain TestBlock,
          AnchoredFragment TestBlock, Maybe ChainSyncException)
       -- ^ (The final client chain, the final server chain, the synced
       --    candidate fragment, exception thrown by the chain sync client)
runChainSync securityParam maxClockSkew (ClientUpdates clientUpdates)
    (ServerUpdates serverUpdates) startSyncingAt = withThreadRegistry $ \registry -> do

    btime <- testBlockchainTime registry numSlots slotDuration

    -- Set up the client
    varCandidates      <- newTVarM Map.empty
    varClientState     <- newTVarM (Genesis, testInitExtLedger)
    varClientException <- newTVarM Nothing

    let getCurrentChain :: STM m (AnchoredFragment TestBlock)
        getCurrentChain =
          AF.anchorNewest k . fst <$>
          readTVar varClientState
        getLedgerState :: STM m (ExtLedgerState TestBlock)
        getLedgerState  = snd <$> readTVar varClientState
        client = chainSyncClient
                   nullTracer
                   (nodeCfg clientId)
                   btime
                   maxClockSkew
                   getLedgerState

    -- Set up the server
    varChainProducerState <- newTVarM $ initChainProducerState Genesis
    let server :: ChainSyncServer (Header TestBlock) (Point TestBlock) m ()
        server = chainSyncServerExample () varChainProducerState

    -- Schedule updates of the client and server chains
    varLastUpdate <- newTVarM 0
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
            case CPS.applyChainUpdates (map (mapChainUpdate TestHeader) chainUpdates) chainProducerState of
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
      void $ fork registry $
        (bracketChainSyncClient
           (AF.mapAnchoredFragment coerce <$> getCurrentChain)
           getLedgerState
           varCandidates
           serverId $ \varCandidate curChain -> do
             runPeer nullTracer codecChainSyncId clientChannel
                    (chainSyncClientPeer (client varCandidate curChain)))
        `catch` \(e :: ChainSyncException) -> do
          atomically $ writeTVar varClientException (Just e)
          -- Rethrow, but it will be ignored anyway.
          throwM e
      void $ forkLinked registry $
        runPeer nullTracer codecChainSyncId serverChannel
                (chainSyncServerPeer server)

    -- STM variable to record the final synched candidate chain
    varCandidateChain <- atomically $ newTVar Nothing

    onSlot btime (finalSlot numSlots) $ do
      -- Wait a random amount of time after the final slot for the chain sync
      -- to finish
      threadDelay 2000
      atomically $ do
        candidate <- readTVar varCandidates >>= readTVar . (Map.! serverId)
        writeTVar varCandidateChain $ Just (candidateChain candidate)

    -- Collect the return values
    atomically $ do
      clientChain       <- fst <$> readTVar varClientState
      serverChain       <- chainState <$> readTVar varChainProducerState
      candidateFragment <- blockUntilJust $ readTVar varCandidateChain
      clientException   <- readTVar varClientException
      return (
          clientChain
        , fmap testHeader serverChain
        , AF.mapAnchoredFragment testHeader candidateFragment
        , clientException
        )
  where
    k = maxRollbacks securityParam

    slotDuration :: DiffTime
    slotDuration = 100000

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

getAddBlock :: ChainUpdate b -> Maybe b
getAddBlock (AddBlock b) = Just b
getAddBlock (RollBack _) = Nothing

updateClientState :: NodeConfig (Bft BftMockCrypto)
                  -> Chain TestBlock
                  -> ExtLedgerState TestBlock
                  -> [ChainUpdate TestBlock]
                  -> (Chain TestBlock, ExtLedgerState TestBlock)
updateClientState cfg chain ledgerState chainUpdates =
    case forwardOnlyOrNot chainUpdates of
      -- If the updates don't contain a roll back, we can incrementally
      -- validate the chain
      Just bs -> (chain', ledgerState')
        where
          chain'       = foldl' (flip Chain.addBlock) chain bs
          ledgerState' = runValidate $
            foldExtLedgerState cfg bs ledgerState
      Nothing
      -- There was a roll back in the updates, so validate the chain from
      -- scratch
        | Just chain' <- Chain.applyChainUpdates chainUpdates chain
        -> let ledgerState' = runValidate $
                 chainExtLedgerState cfg chain' testInitExtLedger
           in (chain', ledgerState')
        | otherwise
        -> error "Client chain update failed"
  where
    forwardOnlyOrNot :: [ChainUpdate TestBlock] -> Maybe [TestBlock]
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
    securityParam <- SecurityParam <$> choose (2, 5)
    maxClockSkew  <- arbitrary
    (clientUpdates,  cus)  <- runStateT
      (ClientUpdates <$> genUpdateSchedule securityParam maxClockSkew)
      emptyUpdateState
    (serverUpdates, _cus) <- runStateT
      (ServerUpdates <$> genUpdateSchedule securityParam maxClockSkew)
      (newChain cus)
    let maxStartSlot = unSlotNo $ maximum
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
      { clientUpdates = ClientUpdates clientUpdates'
      , startSlot     = startSlot'
      }
    | clientUpdates' <- shrinkUpdateSchedule (getClientUpdates clientUpdates)
    , let maxStartSlot = maximum
            [ 1
            , lastSlot clientUpdates' - 1
            , lastSlot (getServerUpdates serverUpdates) - 1 ]
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

{-------------------------------------------------------------------------------
  Generating a schedule of updates
-------------------------------------------------------------------------------}

genUpdateSchedule
  :: SecurityParam -> ClockSkew
  -> StateT ChainUpdateState Gen (Schedule [ChainUpdate TestBlock])
genUpdateSchedule securityParam maxClockSkew = do
    cus  <- get
    cus' <- lift $ genChainUpdates securityParam 10 cus
    put cus'
    let chainUpdates = getChainUpdates cus'
    lift $ spreadUpdates maxClockSkew chainUpdates

-- | Repeatedly remove the last entry (highest SlotNo)
shrinkUpdateSchedule :: Schedule [ChainUpdate TestBlock]
                     -> [Schedule [ChainUpdate TestBlock]]
shrinkUpdateSchedule = unfoldr (fmap (\(_, m) -> (m, m)) . Map.maxView)

-- | Spread out updates over a schedule, i.e. schedule a number of updates to
-- be executed on each slot. We use slot as a proxy for time. Most slots will
-- have no planned updates.
--
-- Note that the current slot (from the BlockchainTime) is used to reject
-- chains that extend too much in the future (see 'ClockSkew'), so be careful
-- not to schedule updates that add blocks with slot numbers from the future.
--
-- Don't schedule updates at slot 0, because 'onEachChange' isn't called for
-- 0.
spreadUpdates :: ClockSkew
              -> [ChainUpdate TestBlock]
              -> Gen (Schedule [ChainUpdate TestBlock])
spreadUpdates (ClockSkew maxClockSkew) = go Map.empty 1
  where
    go !schedule slot updates
      | null updates = return schedule
      | otherwise    = do
        nbUpdates <- frequency [ (2, return 0), (1, choose (1, 5)) ]
        let maxSlot = SlotNo (unSlotNo slot + maxClockSkew)
            (updates', tooFarInTheFuture) =
              span (maybe True (<= maxSlot) . slotOfUpdate) updates
            (this, rest) = splitAt nbUpdates updates'
        go (Map.insert slot this schedule) (succ slot) (rest <> tooFarInTheFuture)

    slotOfUpdate :: ChainUpdate TestBlock -> Maybe SlotNo
    slotOfUpdate = fmap tbSlot . getAddBlock

-- | Inverse of 'spreadUpdates'
joinUpdates :: Schedule [ChainUpdate TestBlock] -> [ChainUpdate TestBlock]
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
      [ case chainUpdate of
          RollBack _ -> True
          AddBlock b -> unSlotNo (tbSlot b) <= unSlotNo slot + maxSkew
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

-- | We need some state to generate [ChainUpdates]
data ChainUpdateState = ChainUpdateState
  { _currentChain :: !(Chain TestBlock)
    -- ^ The current chain, obtained by applying all the '_updates' in reverse
    -- order.
  , _successors   :: !(Map (ChainHash TestBlock) (NonEmpty TestBlock))
    -- ^ When generating an update that adds a block to the chain, we can look
    -- here for already generated blocks that are a successor of the block
    -- with a given hash (or Genesis).
  , _blocks       :: !(Map SlotNo (NonEmpty TestBlock))
    -- ^ The blocks that have already been generated. When generating another
    -- block for some slot, this map is used to make sure we give the new
    -- block a hash different from the hashes of the existing blocks generated
    -- for the same slot.
  , _updates      :: ![ChainUpdate TestBlock]
    -- ^ The updates that have been generated so far, in reverse order: the
    -- first update in the list is the last update to apply.
  } deriving (Show)

emptyUpdateState :: ChainUpdateState
emptyUpdateState = ChainUpdateState
  { _currentChain = Genesis
  , _successors   = Map.empty
  , _blocks       = Map.empty
  , _updates      = []
  }

getChainUpdates :: ChainUpdateState -> [ChainUpdate TestBlock]
getChainUpdates = reverse . _updates

-- | Use the same blocks to generate a new chain and chain updates. Note that
-- it is important that after generating the client updates, the same blocks
-- are used to generate the server updates (or vice versa).
newChain :: ChainUpdateState -> ChainUpdateState
newChain cus = cus
  { _currentChain = Genesis
  , _updates      = []
  }

-- | Test that applying the generated updates gives us the same chain as
-- '_currentChain'.
prop_genChainUpdates :: SecurityParam -> Int -> Property
prop_genChainUpdates securityParam n =
    forAll (genChainUpdates securityParam n emptyUpdateState) $ \cus ->
      Chain.applyChainUpdates (getChainUpdates cus) Genesis ===
      Just (_currentChain cus)

genChainUpdates :: SecurityParam
                -> Int  -- ^ The number of updates to generate
                -> ChainUpdateState
                -> Gen ChainUpdateState
genChainUpdates securityParam n =
    execStateT (replicateM_ n genChainUpdate)
  where
    -- Get something from the state
    getSuccessors   h = Map.lookup h . _successors
    getBlocksInSlot s = Map.lookup s . _blocks

    -- Modify the state
    addUpdate    u cus = cus { _updates = u : _updates cus }
    setChain     c cus = cus { _currentChain = c }
    addBlock     b cus = cus
      { _blocks     = Map.unionWith (<>) (_blocks cus)
          (Map.singleton (tbSlot b)     (b NE.:| []))
      , _successors = Map.unionWith (<>) (_successors cus)
          (Map.singleton (tbPrevHash b) (b NE.:| []))
      }

    k = fromIntegral $ maxRollbacks securityParam

    genChainUpdate = do
      ChainUpdateState { _currentChain = chain } <- get
      frequency'
        [ (3, genAddBlock)
        , (if Chain.null chain then 0 else 1, genSwitchFork)
        ]

    genAddBlock = do
      cus@ChainUpdateState { _currentChain = chain } <- get
      block <- case getSuccessors (Chain.headHash chain) cus of
        Nothing    -> makeBlock chain
        Just succs -> frequency'
          [ (1, lift $ elements (NE.toList succs))
          , (1, makeBlock chain)
          ]
      modify $ addUpdate (AddBlock block) . setChain (Chain.addBlock block chain)

    genSwitchFork  = do
      ChainUpdateState { _currentChain = chain } <- get
      rollBackBlocks <- lift $ choose (1, k)
      let chain' = Chain.drop rollBackBlocks chain
      modify $ addUpdate (RollBack (Chain.headPoint chain')) . setChain chain'
      rollForwardExtraBlocks <- lift $ choose (0, 3)
      -- Rolling back x blocks must always be followed by adding y blocks
      -- where y >= x
      replicateM_ (rollBackBlocks + rollForwardExtraBlocks) genAddBlock

    makeBlock chain = do
        existingBlocksInSlot <- gets $
          maybe 0 (fromIntegral . length) . getBlocksInSlot slot
        let b = TestBlock
              { tbHash     = TestHash (100000 * existingBlocksInSlot + unSlotNo slot)
              , tbPrevHash = prevHash
              , tbNo       = succ (Chain.headBlockNo chain)
              , tbSlot     = slot
              }
        modify (addBlock b)
        return b
      where
        prevHash = Chain.headHash chain
        slot     = succ (Chain.headSlot chain)

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
ppBlock TestBlock { tbSlot = SlotNo s, tbHash = h, tbPrevHash = p } =
    "(S:" <> show s <> "; H:" <> show h <> "; P:" <> p' <> ")"
  where
    p' = case p of
      GenesisHash    -> "Gen"
      BlockHash hash -> show hash

ppPoint :: Point TestBlock -> String
ppPoint Point { pointSlot = SlotNo s, pointHash = h } =
    "(S:" <> show s <> "; H:" <> show h <> ")"


ppChain :: Chain TestBlock -> String
ppChain = ppBlocks Chain.genesisPoint . Chain.toOldestFirst

ppFragment :: AnchoredFragment TestBlock -> String
ppFragment f = ppBlocks (AF.anchorPoint f) (AF.toOldestFirst f)

ppBlocks :: Point TestBlock -> [TestBlock] -> String
ppBlocks a bs = ppPoint a <> " ] " <> intercalate " :> " (map ppBlock bs)

ppUpdates :: Schedule [ChainUpdate TestBlock] -> String
ppUpdates = unlines
          . map (uncurry showEntry)
          . filter (not . null . snd)
          . Map.toAscList
  where
    showEntry :: SlotNo -> [ChainUpdate TestBlock] -> String
    showEntry (SlotNo slot) updates = show slot <> ": " <>
      intercalate ", " (map showChainUpdate updates)
    showChainUpdate :: ChainUpdate TestBlock -> String
    showChainUpdate u = case u of
      RollBack p -> "RollBack " <> ppPoint p
      AddBlock b -> "AddBlock " <> ppBlock b

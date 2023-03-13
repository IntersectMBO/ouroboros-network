{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.NodeKernel (
    -- * Node kernel
    MempoolCapacityBytesOverride (..)
  , NodeKernel (..)
  , NodeKernelArgs (..)
  , TraceForgeEvent (..)
  , getMempoolReader
  , getMempoolWriter
  , getPeersFromCurrentLedger
  , getPeersFromCurrentLedgerAfterSlot
  , initNodeKernel
  ) where



import           Control.DeepSeq (force)
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor (second)
import           Data.Hashable (Hashable)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import           Data.Maybe (isJust, mapMaybe)
import           Data.Proxy
import qualified Data.Text as Text
import           System.Random (StdGen)

import           Control.Tracer

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TxSubmissionMempoolWriter)
import qualified Ouroboros.Network.TxSubmission.Inbound as Inbound
import           Ouroboros.Network.TxSubmission.Mempool.Reader
                     (TxSubmissionMempoolReader)
import qualified Ouroboros.Network.TxSubmission.Mempool.Reader as MempoolReader

import           Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.EarlyExit
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m remotePeer localPeer blk = NodeKernel {
      -- | The 'ChainDB' of the node
      getChainDB             :: ChainDB m blk

      -- | The node's mempool
    , getMempool             :: Mempool m blk

      -- | The node's top-level static configuration
    , getTopLevelConfig      :: TopLevelConfig blk

      -- | The fetch client registry, used for the block fetch clients.
    , getFetchClientRegistry :: FetchClientRegistry remotePeer (Header blk) blk m

      -- | The fetch mode, used by diffusion.
      --
    , getFetchMode           :: STM m FetchMode

      -- | Read the current candidates
    , getNodeCandidates      :: StrictTVar m (Map remotePeer (StrictTVar m (AnchoredFragment (Header blk))))

      -- | The node's tracers
    , getTracers             :: Tracers m remotePeer localPeer blk
    }

-- | Arguments required when initializing a node
data NodeKernelArgs m remotePeer localPeer blk = NodeKernelArgs {
      tracers                 :: Tracers m remotePeer localPeer blk
    , registry                :: ResourceRegistry m
    , cfg                     :: TopLevelConfig blk
    , btime                   :: BlockchainTime m
    , chainDB                 :: ChainDB m blk
    , initChainDB             :: StorageConfig blk -> InitChainDB m blk -> m ()
    , blockFetchSize          :: Header blk -> SizeInBytes
    , blockForging            :: [BlockForging m blk]
    , mempoolCapacityOverride :: MempoolCapacityBytesOverride
    , miniProtocolParameters  :: MiniProtocolParameters
    , blockFetchConfiguration :: BlockFetchConfiguration
    , keepAliveRng            :: StdGen
    }

initNodeKernel
    :: forall m remotePeer localPeer blk.
       ( IOLike m
       , RunNode blk
       , NoThunks remotePeer
       , Ord remotePeer
       , Hashable remotePeer
       )
    => NodeKernelArgs m remotePeer localPeer blk
    -> m (NodeKernel m remotePeer localPeer blk)
initNodeKernel args@NodeKernelArgs { registry, cfg, tracers
                                   , blockForging, chainDB, initChainDB
                                   , blockFetchConfiguration
                                   } = do

    initChainDB (configStorage cfg) (InitChainDB.fromFull chainDB)

    st <- initInternalState args

    mapM_ (forkBlockForging st) blockForging

    let IS { blockFetchInterface, fetchClientRegistry, varCandidates,
             mempool } = st

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $ forkLinkedThread registry "NodeKernel.blockFetchLogic" $
      blockFetchLogic
        (blockFetchDecisionTracer tracers)
        (blockFetchClientTracer   tracers)
        blockFetchInterface
        fetchClientRegistry
        blockFetchConfiguration

    return NodeKernel
      { getChainDB             = chainDB
      , getMempool             = mempool
      , getTopLevelConfig      = cfg
      , getFetchClientRegistry = fetchClientRegistry
      , getFetchMode           = readFetchMode blockFetchInterface
      , getNodeCandidates      = varCandidates
      , getTracers             = tracers
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

data InternalState m remotePeer localPeer blk = IS {
      tracers             :: Tracers m remotePeer localPeer blk
    , cfg                 :: TopLevelConfig blk
    , registry            :: ResourceRegistry m
    , btime               :: BlockchainTime m
    , chainDB             :: ChainDB m blk
    , blockFetchInterface :: BlockFetchConsensusInterface remotePeer (Header blk) blk m
    , fetchClientRegistry :: FetchClientRegistry remotePeer (Header blk) blk m
    , varCandidates       :: StrictTVar m (Map remotePeer (StrictTVar m (AnchoredFragment (Header blk))))
    , mempool             :: Mempool m blk
    }

initInternalState
    :: forall m remotePeer localPeer blk.
       ( IOLike m
       , Ord remotePeer
       , NoThunks remotePeer
       , RunNode blk
       )
    => NodeKernelArgs m remotePeer localPeer blk
    -> m (InternalState m remotePeer localPeer blk)
initInternalState NodeKernelArgs { tracers, chainDB, registry, cfg
                                 , blockFetchSize, btime
                                 , mempoolCapacityOverride
                                 } = do
    varCandidates <- newTVarIO mempty
    mempool       <- openMempool registry
                                 (chainDBLedgerInterface chainDB)
                                 (configLedger cfg)
                                 mempoolCapacityOverride
                                 (mempoolTracer tracers)
                                 txInBlockSize

    fetchClientRegistry <- newFetchClientRegistry

    let getCandidates :: STM m (Map remotePeer (AnchoredFragment (Header blk)))
        getCandidates = readTVar varCandidates >>= traverse readTVar

    slotForgeTimeOracle <- BlockFetchClientInterface.initSlotForgeTimeOracle cfg chainDB
    let readFetchMode = BlockFetchClientInterface.readFetchModeDefault
          btime
          (ChainDB.getCurrentChain chainDB)
        blockFetchInterface :: BlockFetchConsensusInterface remotePeer (Header blk) blk m
        blockFetchInterface = BlockFetchClientInterface.mkBlockFetchConsensusInterface
          (configBlock cfg)
          (BlockFetchClientInterface.defaultChainDbView chainDB)
          getCandidates
          blockFetchSize
          slotForgeTimeOracle
          readFetchMode

    return IS {..}

forkBlockForging
    :: forall m remotePeer localPeer blk.
       (IOLike m, RunNode blk)
    => InternalState m remotePeer localPeer blk
    -> BlockForging m blk
    -> m ()
forkBlockForging IS{..} blockForging =
      void
    $ forkLinkedWatcher registry threadLabel
    $ knownSlotWatcher btime go
  where
    threadLabel :: String
    threadLabel =
        "NodeKernel.blockForging." <> Text.unpack (forgeLabel blockForging)

    go :: SlotNo -> m ()
    go currentSlot = do
      -- Once the forging logic is invoked, a block context is selected and
      -- the logic then *MUST SUCCESFULLY PRODUCE* a block on top of the
      -- selected context, i.e. it cannot fail even if the LedgerDB switches
      -- to a different fork while the forging logic is executed. For this
      -- reason, we need a complete view on the ledger state we are going to
      -- forge on top of.
      --
      -- A complete view on the ledger state (like the one we need to forge on
      -- top of it), includes the UTxO set. In order to arbitrarily query for
      -- UTxO entries on that ledger state, we need to be able to
      -- rewind-read-forward on a DbChangelog.
      --
      -- We need to hold the read lock so that the anchor of the DbChangelog
      -- that we get when starting the forging logic doesn't get out of sync
      -- with the Backing store by the time we ask the mempool for a set of
      -- transactions. This is because we might need to revalidate said
      -- transactions and therefore we cannot lose the consistency on the
      -- DbChangelog that we have.
      --
      -- It is important to note that this cannot be held indefinitely as
      -- adding the block to the ledger db might trigger storing a snapshot
      -- which acquires the write lock which would result in a deadlock. For
      -- this reason, we release the read lock just as we finish asking the
      -- mempool for a snapshot.
      res <- ChainDB.withLgrReadLock chainDB $ withEarlyExit $ do
        trace $ TraceStartLeadershipCheck currentSlot

        -- Figure out which block to connect to
        --
        -- Normally this will be the current block at the tip, but it may
        -- be the /previous/ block, if there were multiple slot leaders
        BlockContext{bcBlockNo, bcPrevPoint} <- do
          eBlkCtx <- lift $ atomically $
            mkCurrentBlockContext currentSlot
                <$> ChainDB.getCurrentChain chainDB
          case eBlkCtx of
            Right blkCtx -> return blkCtx
            Left failure -> do
              trace failure
              exitEarly

        trace $ TraceBlockContext currentSlot bcBlockNo bcPrevPoint

        -- Get ledger state corresponding to bcPrevPoint
        --
        -- This might fail if, in between choosing 'bcPrevPoint' and this call to
        -- 'getPastLedger', we switched to a fork where 'bcPrevPoint' is no longer
        -- on our chain. When that happens, we simply give up on the chance to
        -- produce a block.
        unticked <- do
          mExtLedger <- lift $ atomically $ ChainDB.getPastLedger chainDB bcPrevPoint
          case mExtLedger of
            Just l  -> return l
            Nothing -> do
              trace $ TraceNoLedgerState currentSlot bcPrevPoint
              exitEarly

        trace $ TraceLedgerState currentSlot bcPrevPoint

        -- We require the ticked ledger view in order to construct the ticked
        -- 'ChainDepState'.
        ledgerView <-
          case runExcept $ forecastFor
                           (ledgerViewForecastAt
                              (configLedger cfg)
                              (ledgerState unticked))
                           currentSlot of
            Left err -> do
              -- There are so many empty slots between the tip of our chain and
              -- the current slot that we cannot get an ledger view anymore
              -- In principle, this is no problem; we can still produce a block
              -- (we use the ticked ledger state). However, we probably don't
              -- /want/ to produce a block in this case; we are most likely
              -- missing a blocks on our chain.
              trace $ TraceNoLedgerView currentSlot err
              exitEarly
            Right lv ->
              return lv

        trace $ TraceLedgerView currentSlot

        -- Tick the 'ChainDepState' for the 'SlotNo' we're producing a block
        -- for. We only need the ticked 'ChainDepState' to check the whether
        -- we're a leader. This is much cheaper than ticking the entire
        -- 'ExtLedgerState'.
        let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
            tickedChainDepState =
                tickChainDepState
                  (configConsensus cfg)
                  ledgerView
                  currentSlot
                  (headerStateChainDep (headerState unticked))

        -- Check if we are the leader
        proof <- do
          shouldForge <- lift $
            checkShouldForge blockForging
              (contramap (TraceLabelCreds (forgeLabel blockForging))
                (forgeStateInfoTracer tracers))
              cfg
              currentSlot
              tickedChainDepState
          case shouldForge of
            ForgeStateUpdateError err -> do
              trace $ TraceForgeStateUpdateError currentSlot err
              exitEarly
            CannotForge cannotForge -> do
              trace $ TraceNodeCannotForge currentSlot cannotForge
              exitEarly
            NotLeader -> do
              trace $ TraceNodeNotLeader currentSlot
              exitEarly
            ShouldForge p -> return p

        -- At this point we have established that we are indeed slot leader
        trace $ TraceNodeIsLeader currentSlot

        -- Tick the ledger state for the 'SlotNo' we're producing a block for
        let tickedLedgerState :: Ticked1 (LedgerState blk) DiffMK
            tickedLedgerState =
              applyChainTick
                (configLedger cfg)
                currentSlot
                (ledgerState unticked)

        _ <- evaluate tickedLedgerState
        trace $ TraceForgeTickedLedgerState currentSlot bcPrevPoint

        -- Get a snapshot of the mempool that is consistent with the ledger
        --
        -- NOTE: It is possible that due to adoption of new blocks the
        -- /current/ ledger will have changed. This doesn't matter: we will
        -- produce a block that fits onto the ledger we got above; if the
        -- ledger in the meantime changes, the block we produce here may or
        -- may not be adopted, but it won't be invalid.
        (mempoolHash, mempoolSlotNo) <- lift $ atomically $ do
              snap <- getSnapshot mempool   -- only used for its tip-like information
              let h :: ChainHash blk
                  h = castHash $ snapshotTipHash snap
              pure (h, snapshotSlotNo snap)

        mempoolSnapshot <- lift $ getSnapshotFor
                             mempool
                             (castPoint $ getTip unticked)
                             currentSlot
                             tickedLedgerState

        let e = error "Mempool snapshot failed in forging loop. RAWLock violation"

        pure ( maybe e id mempoolSnapshot -- OK because we are holding the read lock
             , bcPrevPoint
             , mempoolHash
             , mempoolSlotNo
             , bcBlockNo
             , tickedLedgerState
             , proof
             , unticked)

      case res of
          Nothing -> pure ()
          Just ( mempoolSnapshot
               , bcPrevPoint
               , mempoolHash
               , mempoolSlotNo
               , bcBlockNo
               , tickedLedgerState
               , proof
               , unticked
               ) -> withEarlyExit_ $ do
            let txs = map fst $ snapshotTxs mempoolSnapshot

            -- force the mempool's computation before the tracer event
            _ <- evaluate (length txs)
            _ <- evaluate (snapshotTipHash mempoolSnapshot)
            trace $ TraceForgingMempoolSnapshot currentSlot bcPrevPoint mempoolHash mempoolSlotNo

            -- Actually produce the block
            newBlock <- lift $
              Block.forgeBlock blockForging
                cfg
                bcBlockNo
                currentSlot
                tickedLedgerState
                txs
                proof

            trace $ TraceForgedBlock
                      currentSlot
                      (ledgerTipPoint (ledgerState unticked))
                      newBlock
                      (snapshotMempoolSize mempoolSnapshot)

            -- Add the block to the chain DB
            let noPunish = InvalidBlockPunishment.noPunishment   -- no way to punish yourself
            result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
            -- Block until we have processed the block
            curTip <- lift $ atomically $ ChainDB.blockProcessed result

            -- Check whether we adopted our block
            when (curTip /= blockPoint newBlock) $ do
              isInvalid <- lift $ atomically $
                ($ blockHash newBlock) . forgetFingerprint <$>
                ChainDB.getIsInvalidBlock chainDB
              case isInvalid of
                Nothing ->
                  trace $ TraceDidntAdoptBlock currentSlot newBlock
                Just reason -> do
                  trace $ TraceForgedInvalidBlock currentSlot newBlock reason
                  -- We just produced a block that is invalid according to the
                  -- ledger in the ChainDB, while the mempool said it is valid.
                  -- There is an inconsistency between the two!
                  --
                  -- Remove all the transactions in that block, otherwise we'll
                  -- run the risk of forging the same invalid block again. This
                  -- means that we'll throw away some good transactions in the
                  -- process.
                  lift $ removeTxs mempool $ NE.fromList (map (txId . txForgetValidated) txs)
                  exitEarly

            -- We successfully produced /and/ adopted a block
            --
            -- NOTE: we are tracing the transactions we retrieved from the Mempool,
            -- not the transactions actually /in the block/. They should always
            -- match, if they don't, that would be a bug. Unfortunately, we can't
            -- assert this here because the ability to extract transactions from a
            -- block, i.e., the @HasTxs@ class, is not implementable by all blocks,
            -- e.g., @DualBlock@.
            trace $ TraceAdoptedBlock currentSlot newBlock txs

    trace :: TraceForgeEvent blk -> WithEarlyExit m ()
    trace =
          lift
        . traceWith (forgeTracer tracers)
        . TraceLabelCreds (forgeLabel blockForging)

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo   :: !BlockNo
    -- ^ the block number of the block to be forged
  , bcPrevPoint :: !(Point blk)
    -- ^ the point of /the predecessor of/ the block
    --
    -- Note that a block/header stores the hash of its predecessor but not the
    -- slot.
  }

-- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
     HasHeader (Header blk)
  => Header blk -> BlockContext blk
blockContextFromPrevHeader hdr =
    -- Recall that an EBB has the same block number as its predecessor, so this
    -- @succ@ is even correct when @hdr@ is an EBB.
    BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
--
-- The 'bcPrevPoint' will either refer to the header at the tip of the current
-- chain or, in case there is already a block in this slot (e.g. another node
-- was also elected leader and managed to produce a block before us), the tip's
-- predecessor. If the chain is empty, then it will refer to the chain's anchor
-- point, which may be genesis.
mkCurrentBlockContext
  :: forall blk. RunNode blk
  => SlotNo
     -- ^ the current slot, i.e. the slot of the block about to be forged
  -> AnchoredFragment (Header blk)
     -- ^ the current chain fragment
     --
     -- Recall that the anchor point is the tip of the ImmutableDB.
  -> Either (TraceForgeEvent blk) (BlockContext blk)
     -- ^ the event records the cause of the failure
mkCurrentBlockContext currentSlot c = case c of
    Empty AF.AnchorGenesis ->
      -- The chain is entirely empty.
      Right $ BlockContext (expectedFirstBlockNo (Proxy @blk)) GenesisPoint

    Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
      let p :: Point blk = BlockPoint anchorSlot anchorHash
      in if anchorSlot < currentSlot
           then Right $ BlockContext (succ anchorBlockNo) p
           else Left  $ TraceSlotIsImmutable currentSlot p anchorBlockNo

    c' :> hdr -> case blockSlot hdr `compare` currentSlot of

      -- The block at the tip of our chain has a slot number /before/ the
      -- current slot number. This is the common case, and we just want to
      -- connect our new block to the block at the tip.
      LT -> Right $ blockContextFromPrevHeader hdr

      -- The block at the tip of our chain has a slot that lies in the
      -- future. Although the chain DB does not adopt future blocks, if the
      -- system is under heavy load, it is possible (though unlikely) that
      -- one or more slots have passed after @currentSlot@ that we got from
      -- @onSlotChange@ and and before we queried the chain DB for the block
      -- at its tip. At the moment, we simply don't produce a block if this
      -- happens.

      -- TODO: We may wish to produce a block here anyway, treating this
      -- as similar to the @EQ@ case below, but we should be careful:
      --
      -- 1. We should think about what slot number to use.
      -- 2. We should be careful to distinguish between the case where we
      --    need to drop a block from the chain and where we don't.
      -- 3. We should be careful about slot numbers and EBBs.
      -- 4. We should probably not produce a block if the system is under
      --    very heavy load (e.g., if a lot of blocks have been produced
      --    after @currentTime@).
      --
      -- See <https://github.com/input-output-hk/ouroboros-network/issues/1462>
      GT -> Left $ TraceBlockFromFuture currentSlot (blockSlot hdr)

      -- The block at the tip has the same slot as the block we're going to
      -- produce (@currentSlot@).
      EQ -> Right $ if isJust (headerIsEBB hdr)
        -- We allow forging a block that is the successor of an EBB in the
        -- same slot.
        then blockContextFromPrevHeader hdr
        -- If @hdr@ is not an EBB, then forge an alternative to @hdr@: same
        -- block no and same predecessor.
        else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'

{-------------------------------------------------------------------------------
  TxSubmission integration
-------------------------------------------------------------------------------}

getMempoolReader
  :: forall m blk.
     ( LedgerSupportsMempool blk
     , IOLike m
     , HasTxId (GenTx blk)
     )
  => Mempool m blk
  -> TxSubmissionMempoolReader (GenTxId blk) (Validated (GenTx blk)) TicketNo m
getMempoolReader mempool = MempoolReader.TxSubmissionMempoolReader
    { mempoolZeroIdx     = zeroTicketNo
    , mempoolGetSnapshot = convertSnapshot <$> getSnapshot mempool
    }
  where
    convertSnapshot
      :: MempoolSnapshot               blk
      -> MempoolReader.MempoolSnapshot (GenTxId blk) (Validated (GenTx blk)) TicketNo
    convertSnapshot MempoolSnapshot { snapshotTxsAfter, snapshotLookupTx,
                                      snapshotHasTx } =
      MempoolReader.MempoolSnapshot
        { mempoolTxIdsAfter = \idx ->
            [ (txId (txForgetValidated tx), idx', getTxSize mempool (txForgetValidated tx))
            | (tx, idx') <- snapshotTxsAfter idx
            ]
        , mempoolLookupTx   = snapshotLookupTx
        , mempoolHasTx      = snapshotHasTx
        }

getMempoolWriter
  :: ( LedgerSupportsMempool blk
     , IOLike m
     , HasTxId (GenTx blk)
     )
  => Mempool m blk
  -> TxSubmissionMempoolWriter (GenTxId blk) (GenTx blk) TicketNo m
getMempoolWriter mempool = Inbound.TxSubmissionMempoolWriter
    { Inbound.txId          = txId
    , mempoolAddTxs = \txs ->
        map (txId . txForgetValidated) . mapMaybe mempoolTxAddedToMaybe <$>
        addTxs mempool txs
    }

{-------------------------------------------------------------------------------
  PeerSelection integration
-------------------------------------------------------------------------------}

-- | Retrieve the peers registered in the current chain/ledger state by
-- descending stake.
--
-- For example, for Shelley, this will return the stake pool relays ordered by
-- descending stake.
--
-- Only returns a 'Just' when the given predicate returns 'True'. This predicate
-- can for example check whether the slot of the ledger state is older or newer
-- than some slot number.
--
-- We don't use the ledger state at the tip of the chain, but the ledger state
-- @k@ blocks back, i.e., at the tip of the immutable chain, because any stake
-- pools registered in that ledger state are guaranteed to be stable. This
-- justifies merging the future and current stake pools.
getPeersFromCurrentLedger ::
     (IOLike m, LedgerSupportsPeerSelection blk)
  => NodeKernel m remotePeer localPeer blk
  -> (LedgerState blk EmptyMK -> Bool)
  -> STM m (Maybe [(PoolStake, NonEmpty RelayAccessPoint)])
getPeersFromCurrentLedger kernel p = do
    immutableLedger <-
      ledgerState <$> ChainDB.getImmutableLedger (getChainDB kernel)
    return $ do
      guard (p immutableLedger)
      return
        $ map (second (fmap stakePoolRelayAccessPoint))
        $ force
        $ getPeers immutableLedger

-- | Like 'getPeersFromCurrentLedger' but with a \"after slot number X\"
-- condition.
getPeersFromCurrentLedgerAfterSlot ::
     forall m blk localPeer remotePeer.
     ( IOLike m
     , LedgerSupportsPeerSelection blk
     , UpdateLedger blk
     )
  => NodeKernel m remotePeer localPeer blk
  -> SlotNo
  -> STM m (Maybe [(PoolStake, NonEmpty RelayAccessPoint)])
getPeersFromCurrentLedgerAfterSlot kernel slotNo =
    getPeersFromCurrentLedger kernel afterSlotNo
  where
    afterSlotNo :: LedgerState blk mk -> Bool
    afterSlotNo st =
      case ledgerTipSlot st of
        Origin        -> False
        NotOrigin tip -> tip > slotNo

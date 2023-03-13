{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Initialization of the 'BlockFetchConsensusInterface'
module Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface (
    ChainDbView (..)
  , SlotForgeTimeOracle
  , defaultChainDbView
  , initSlotForgeTimeOracle
  , mkBlockFetchConsensusInterface
  , readFetchModeDefault
  ) where

import           Control.Monad
import           Data.Map.Strict (Map)
import           Data.Proxy
import           Data.Time.Clock (UTCTime)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Config.SupportsNode as SupportsNode
import qualified Ouroboros.Consensus.HardFork.Abstract as History
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
                     (InvalidBlockPunishment)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (MaxSlotNo)
import           Ouroboros.Network.BlockFetch.ConsensusInterface
                     (BlockFetchConsensusInterface (..), FetchMode (..),
                     FromConsensus (..), WhetherReceivingTentativeBlocks (..))
import           Ouroboros.Network.SizeInBytes

-- | Abstract over the ChainDB
data ChainDbView m blk = ChainDbView {
     getCurrentChain           :: STM m (AnchoredFragment (Header blk))
   , getIsFetched              :: STM m (Point blk -> Bool)
   , getMaxSlotNo              :: STM m MaxSlotNo
   , addBlockWaitWrittenToDisk :: InvalidBlockPunishment m -> blk -> m Bool
   }

defaultChainDbView :: IOLike m => ChainDB m blk -> ChainDbView m blk
defaultChainDbView chainDB = ChainDbView {
    getCurrentChain           = ChainDB.getCurrentChain chainDB
  , getIsFetched              = ChainDB.getIsFetched chainDB
  , getMaxSlotNo              = ChainDB.getMaxSlotNo chainDB
  , addBlockWaitWrittenToDisk = ChainDB.addBlockWaitWrittenToDisk chainDB
  }

-- | How to get the wall-clock time of a slot. Note that this is a very
-- non-trivial operation in the context of the HFC, cf. 'headerForgeUTCTime'.
type SlotForgeTimeOracle m blk = RealPoint blk -> STM m UTCTime

-- | Create a HFC-enabled 'SlotForgeTimeOracle'. Note that its semantics are
-- rather tricky, cf. 'headerForgeUTCTime'.
initSlotForgeTimeOracle ::
     forall m blk.
     ( IOLike m
     , BlockSupportsProtocol blk
     , History.HasHardForkHistory blk
     , SupportsNode.ConfigSupportsNode blk
     , IsLedger (LedgerState blk)
     )
  => TopLevelConfig blk
  -> ChainDB m blk
  -> m (SlotForgeTimeOracle m blk)
initSlotForgeTimeOracle cfg chainDB = do
    cache <-
      History.runWithCachedSummary
        (toSummary <$> ChainDB.getCurrentLedger chainDB)
    let slotForgeTime rp =
              fmap
                (either errMsg toAbsolute)
            $ History.cachedRunQuery
                cache
                (fst <$> History.slotToWallclock (realPointSlot rp))
          where
            -- This @cachedRunQuery@ fail for the following reasons.
            --
            -- By the PRECONDITIONs documented in the 'headerForgeUTCTime', we
            -- can assume that the given header was validated by the ChainSync
            -- client. This means its slot was, at some point, within the ledger
            -- view forecast range of the ledger state of our contemporary
            -- intersection with the header itself (and that intersection
            -- extended our contemporary immutable tip). A few additional facts
            -- ensure that we will always be able to thereafter correctly
            -- convert that header's slot using our current chain's ledger
            -- state.
            --
            --   o For under-developed reasons, the ledger view forecast range
            --     is equivalent to the time forecast range, ie " Definition
            --     17.2 (Forecast range) " from The Consensus Report.
            --
            --   o Because rollback is bounded, our currently selected chain
            --     will always be an evolution (ie " switch(n, bs) ") of that
            --     intersection point. (This one is somewhat obvious in
            --     retrospect, but we're being explicit here in order to
            --     emphasize the relation to the " chain evolution " jargon.)
            --
            --   o Because " stability itself is stable ", the HFC satisfies "
            --     Property 17.3 (Time conversions stable under chain evolution)
            --     " from The Consensus Report.
            errMsg err =
              error $
                 "Consensus could not determine forge UTCTime!"
              <> " " <> show rp
              <> " " <> show err
    pure slotForgeTime
  where
    toSummary ::
         ExtLedgerState blk EmptyMK
      -> History.Summary (History.HardForkIndices blk)
    toSummary = History.hardForkSummary (configLedger cfg) . ledgerState

    toAbsolute :: RelativeTime -> UTCTime
    toAbsolute =
        fromRelativeTime (SupportsNode.getSystemStart (configBlock cfg))

readFetchModeDefault ::
     (MonadSTM m, HasHeader blk)
  => BlockchainTime m
  -> STM m (AnchoredFragment blk)
  -> STM m FetchMode
readFetchModeDefault btime getCurrentChain = do
    mCurSlot <- getCurrentSlot btime
    case mCurSlot of
      -- The current chain's tip far away from "now", so use bulk sync mode.
      CurrentSlotUnknown  -> return FetchModeBulkSync
      CurrentSlot curSlot -> do
        curChainSlot <- AF.headSlot <$> getCurrentChain
        let slotsBehind = case curChainSlot of
              -- There's nothing in the chain. If the current slot is 0, then
              -- we're 1 slot behind.
              Origin         -> unSlotNo curSlot + 1
              NotOrigin slot -> unSlotNo curSlot - unSlotNo slot
            maxSlotsBehind = 1000
        return $ if slotsBehind < maxSlotsBehind
          -- When the current chain is near to "now", use deadline mode,
          -- when it is far away, use bulk sync mode.
          then FetchModeDeadline
          else FetchModeBulkSync

mkBlockFetchConsensusInterface ::
     forall m peer blk.
     ( IOLike m
     , BlockSupportsProtocol blk
     )
  => BlockConfig blk
  -> ChainDbView m blk
  -> STM m (Map peer (AnchoredFragment (Header blk)))
  -> (Header blk -> SizeInBytes)
  -> SlotForgeTimeOracle m blk
     -- ^ Slot forge time, see 'headerForgeUTCTime' and 'blockForgeUTCTime'.
  -> STM m FetchMode
     -- ^ See 'readFetchMode'.
  -> BlockFetchConsensusInterface peer (Header blk) blk m
mkBlockFetchConsensusInterface
  bcfg chainDB getCandidates blockFetchSize slotForgeTime readFetchMode =
    BlockFetchConsensusInterface {..}
  where
    blockMatchesHeader :: Header blk -> blk -> Bool
    blockMatchesHeader = Block.blockMatchesHeader

    readCandidateChains :: STM m (Map peer (AnchoredFragment (Header blk)))
    readCandidateChains = getCandidates

    readCurrentChain :: STM m (AnchoredFragment (Header blk))
    readCurrentChain = getCurrentChain chainDB

    readFetchedBlocks :: STM m (Point blk -> Bool)
    readFetchedBlocks = getIsFetched chainDB

    -- See 'mkAddFetchedBlock_'
    mkAddFetchedBlock ::
         WhetherReceivingTentativeBlocks
      -> STM m (Point blk -> blk -> m ())
    mkAddFetchedBlock enabledPipelining = do
      unlessImproved <- InvalidBlockPunishment.mkUnlessImproved (Proxy @blk)
      pure $ mkAddFetchedBlock_ unlessImproved enabledPipelining

    -- Waits until the block has been written to disk, but not until chain
    -- selection has processed the block.
    mkAddFetchedBlock_ ::
         (   SelectView (BlockProtocol blk)
          -> InvalidBlockPunishment m
          -> InvalidBlockPunishment m
         )
      -> WhetherReceivingTentativeBlocks
      -> Point blk
      -> blk
      -> m ()
    mkAddFetchedBlock_ unlessImproved enabledPipelining _pt blk = void $ do
       disconnect <- InvalidBlockPunishment.mkPunishThisThread
       -- A BlockFetch peer can either send an entire range or none of the
       -- range; anything else will incur a disconnect. And in 'FetchDeadline'
       -- mode, which is the relevant case for this kind of DoS attack (because
       -- in bulk sync, our honest peers will be streaming a very dense chain
       -- very quickly, meaning the adversary only has very small windows during
       -- which we're interested in its chains), the node only requests whole
       -- suffixes from peers: the BlockFetch decision logic does not avoid
       -- requesting a block that is already in-flight from other peers. Thus
       -- the adversary cannot send us blocks out-of-order (during
       -- 'FetchDeadline'), even if they control more than one of our peers.
       --
       -- Therefore, the following punishment logic only needs to cover the
       -- "whole chain received in-order from a single-peer" case. Which it
       -- currently does.
       --
       -- TODO maintain the context of which ChainSync candidate incurring this
       -- fetch request, and disconnect immediately if the invalid block is not
       -- the tip of that candidate. As-is, in 'FetchDeadline' they must also
       -- send the next block, but they might be able to wait long enough that
       -- it is not desirable when it arrives, and therefore not be disconnected
       -- from. So their choices are: cause a disconnect or else do nothing for
       -- long enough. Both are fine by us, from a DoS mitigation perspective.
       let punishment = InvalidBlockPunishment.branch $ \case
             -- invalid parents always cause a disconnect
             InvalidBlockPunishment.BlockPrefix -> disconnect
             -- when pipelining, we forgive an invalid block itself if it's
             -- better than the previous invalid block this peer delivered
             InvalidBlockPunishment.BlockItself -> case enabledPipelining of
               NotReceivingTentativeBlocks -> disconnect
               ReceivingTentativeBlocks    ->
                 unlessImproved (selectView bcfg (getHeader blk)) disconnect
       addBlockWaitWrittenToDisk
         chainDB
         punishment
         blk

    readFetchedMaxSlotNo :: STM m MaxSlotNo
    readFetchedMaxSlotNo = getMaxSlotNo chainDB

    -- Note that @ours@ comes from the ChainDB and @cand@ from the ChainSync
    -- client.
    --
    -- Fragments are proxies for their corresponding chains; it is possible, in
    -- principle, that an empty fragment corresponds to the chain we want to
    -- adopt, and should therefore be preferred over other fragments (whose
    -- blocks we therefore do not want to download). The precondition to
    -- 'preferAnchoredCandidates' is designed precisely to rule out this
    -- possibility (for details, see the Consensus Report), but unfortunately we
    -- cannot always satisfy this precondition: although the chain sync client
    -- preserves an invariant that relates our current chain to the candidate
    -- fragment, by the time the block fetch download logic considers the
    -- fragment, our current chain might have changed.
    plausibleCandidateChain :: HasCallStack
                            => AnchoredFragment (Header blk)
                            -> AnchoredFragment (Header blk)
                            -> Bool
    plausibleCandidateChain ours cand
      -- 1. The ChainDB maintains the invariant that the anchor of our fragment
      --    corresponds to the immutable tip.
      --
      -- 2. The ChainSync client locally maintains the invariant that our
      --    fragment and the candidate fragment have the same anchor point. This
      --    establishes the precondition required by @preferAnchoredCandidate@.
      --
      -- 3. However, by the time that the BlockFetch logic processes a fragment
      --    presented to it by the ChainSync client, our current fragment might
      --    have changed, and they might no longer be anchored at the same
      --    point. This means that we are no longer guaranteed that the
      --    precondition holds.
      --
      -- 4. Our chain's anchor can only move forward. We can detect this by
      --    looking at the block numbers of the anchors.
      --
      | AF.anchorBlockNo cand < AF.anchorBlockNo ours  -- (4)
      = case (AF.null ours, AF.null cand) of
          -- Both are non-empty, the precondition trivially holds.
          (False, False) -> preferAnchoredCandidate bcfg ours cand
          -- The candidate is shorter than our chain and, worse, we'd have to
          -- roll back past our immutable tip (the anchor of @cand@).
          (_,     True)  -> False
          -- As argued above we can only reach this case when our chain's anchor
          -- has changed (4).
          --
          -- It is impossible for our chain to change /and/ still be empty: the
          -- anchor of our chain only changes when a new block becomes
          -- immutable. For a new block to become immutable, we must have
          -- extended our chain with at least @k + 1@ blocks. Which means our
          -- fragment can't be empty.
          (True,  _)     -> error "impossible"

      | otherwise
      = preferAnchoredCandidate bcfg ours cand

    compareCandidateChains :: AnchoredFragment (Header blk)
                           -> AnchoredFragment (Header blk)
                           -> Ordering
    compareCandidateChains = compareAnchoredFragments bcfg

    headerForgeUTCTime = slotForgeTime . headerRealPoint . unFromConsensus
    blockForgeUTCTime  = slotForgeTime . blockRealPoint  . unFromConsensus

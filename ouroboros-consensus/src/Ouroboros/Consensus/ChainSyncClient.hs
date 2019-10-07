{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncClient (
    Consensus
  , chainSyncClient
  , bracketChainSyncClient
  , ChainSyncClientException (..)
  , ChainSyncClientExceptionType (..)
  , ChainDbView (..)
  , ClockSkew (..)
  , Our (..)
  , Their (..)
    -- * Trace events
  , TraceChainSyncClientEvent (..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import           Data.List (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Typeable
import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Pipelined
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm (checkInvariant,
                     unsafeNoThunks)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.SlotBounded as SB
import           Ouroboros.Consensus.Util.STM (WithFingerprint (..),
                     onEachChange)

-- | Clock skew: the number of slots the chain of an upstream node may be
-- ahead of the current slot (according to 'BlockchainTime').
--
-- E.g. a 'ClockSkew' value of @1@ means that a block produced by an upstream
-- it may have a slot number that is 1 greater than the current slot.
newtype ClockSkew = ClockSkew { unClockSkew :: Word64 }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Enum, Bounded, Num)

type Consensus (client :: * -> * -> (* -> *) -> * -> *) blk tip m =
   client (Header blk) tip m Void

-- | Abstract over the ChainDB
data ChainDbView m blk tip = ChainDbView
  { getCurrentChain   :: STM m (AnchoredFragment (Header blk))
  , getCurrentLedger  :: STM m (ExtLedgerState blk)
  , getOurTip         :: STM m tip
  , getIsInvalidBlock :: STM m (WithFingerprint (HeaderHash blk -> Bool))
  }

-- newtype wrappers to avoid confusing our tip with their tip.
newtype Their a = Their { unTheir :: a }
  deriving stock   (Eq)
  deriving newtype (Show, NoUnexpectedThunks)

newtype Our   a = Our   { unOur   :: a }
  deriving stock   (Eq)
  deriving newtype (Show, NoUnexpectedThunks)

bracketChainSyncClient
    :: ( IOLike m
       , Ord peer
       , SupportedBlock blk
       , Typeable tip
       , Show tip
       )
    => Tracer m (TraceChainSyncClientEvent blk tip)
    -> ChainDbView m blk tip
    -> StrictTVar m (Map peer (StrictTVar m (AnchoredFragment (Header blk))))
       -- ^ The candidate chains, we need the whole map because we
       -- (de)register nodes (@peer@).
    -> peer
    -> (    StrictTVar m (AnchoredFragment (Header blk))
         -> m a
       )
    -> m a
bracketChainSyncClient tracer ChainDbView { getIsInvalidBlock } varCandidates
                       peer body =
    withRegistry $ \registry ->
      bracket register unregister $ \varCandidate -> do
        rejectInvalidBlocks
          tracer
          registry
          getIsInvalidBlock
          (readTVar varCandidate)
        body varCandidate
  where
    register = do
      varCandidate <- newTVarM $ AF.Empty GenesisPoint
      atomically $ modifyTVar varCandidates $ Map.insert peer varCandidate
      return varCandidate

    unregister _ = do
      atomically $ modifyTVar varCandidates $ Map.delete peer

-- Our task: after connecting to an upstream node, try to maintain an
-- up-to-date header-only fragment representing their chain. We maintain
-- such candidate chains in a map with upstream nodes as keys.
--
-- The block fetch logic will use these candidate chains to download
-- blocks from, prioritising certain candidate chains over others using
-- the consensus protocol. Whenever such a block has been downloaded and
-- added to the local 'ChainDB', the 'ChainDB' will perform chain
-- selection.
--
-- We also validate the headers of a candidate chain by advancing the
-- 'ChainState' with the headers, which returns an error when validation
-- failed. Thus, in addition to the chain fragment of each candidate, we
-- also store a 'ChainState' corresponding to the head of the candidate
-- chain.
--
-- We must keep the candidate chain synchronised with the corresponding
-- upstream chain. The upstream node's chain might roll forward or
-- backwards, and they will inform us about this. When we get these
-- messages, we will replicate these actions on our candidate chain.
--
-- INVARIANT:
--
-- >           our tip
-- >             v
-- >   /--* .... *
-- >   |
-- > --*
-- >   |
-- >   \--* .... *
-- >        fragment tip
--
-- The distance from our tip to the intersection between our chain and the
-- fragment maintained for the upstream node cannot exceed @k@ blocks. When
-- this invariant cannot be maintained, the upstream node is on a fork that
-- is too distant and we should disconnect.
--
-- TODO #423 rate-limit switching chains, otherwise we can't place blame (we
-- don't know which candidate's chain included the point that was
-- poisoned). E.g. two rollbacks per time slot -> make it configurable ->
-- just a simple argument for now.
--
-- TODO #467 if the 'theirTip' that they sent us is on our chain, just
-- switch to it.

-- | State used when the intersection between the candidate and the current
-- chain is unknown.
data UnknownIntersectionState blk tip = UnknownIntersectionState
  { ourFrag       :: !(AnchoredFragment (Header blk))
    -- ^ A view of the current chain fragment. Note that this might be
    -- temporarily out of date w.r.t. the actual current chain until we update
    -- it again.
    --
    -- This fragment is used to select points from to find an intersection
    -- with the candidate.
    --
    -- INVARIANT: 'ourFrag' contains @k@ headers, unless close to genesis.
  , ourChainState :: !(ChainState (BlockProtocol blk))
    -- ^ 'ChainState' corresponding to the tip (most recent block) of
    -- 'ourFrag'.
  , ourTip        :: !(Our tip)
    -- ^ INVARIANT: must correspond to the tip of 'ourFrag'.
  }
  deriving (Generic)

instance ( ProtocolLedgerView blk
         , NoUnexpectedThunks tip
         ) => NoUnexpectedThunks (UnknownIntersectionState blk tip) where
  showTypeOf _ = show $ typeRep (Proxy @(UnknownIntersectionState blk))

-- | State used when the intersection between the candidate and the current
-- chain is known.
data KnownIntersectionState blk tip = KnownIntersectionState
  { theirFrag       :: !(AnchoredFragment (Header blk))
    -- ^ The candidate, the synched fragment of their chain.
  , theirChainState :: !(ChainState (BlockProtocol blk))
    -- ^ 'ChainState' corresponding to the tip (most recent block) of
    -- 'theirFrag'.
  , ourFrag         :: !(AnchoredFragment (Header blk))
    -- ^ A view of the current chain fragment used to maintain the invariants
    -- with. Note that this might be temporarily out of date w.r.t. the actual
    -- current chain until we update it again.
    --
    -- INVARIANT: 'ourFrag' contains @k@ headers, unless close to genesis.
    --
    -- INVARIANT: 'theirFrag' and 'ourFrag' have the same anchor point. From
    -- this follows that both fragments intersect. This also means that
    -- 'theirFrag' forks off within the last @k@ headers/blocks of the
    -- 'ourFrag'.
  , ourTip          :: !(Our tip)
    -- ^ INVARIANT: must correspond to the tip of 'ourFrag'.
  }
  deriving (Generic)

instance ( ProtocolLedgerView blk
         , NoUnexpectedThunks tip
         ) => NoUnexpectedThunks (KnownIntersectionState blk tip) where
  showTypeOf _ = show $ typeRep (Proxy @(KnownIntersectionState blk))

-- | Chain sync client
--
-- This never terminates. In case of a failure, a 'ChainSyncClientException'
-- is thrown. The network layer classifies exception such that the
-- corresponding peer will never be chosen again.
chainSyncClient
    :: forall m blk tip.
       ( IOLike m
       , ProtocolLedgerView blk
       , NoUnexpectedThunks tip
       , Exception (ChainSyncClientException blk tip)
       )
    => MkPipelineDecision
    -> (tip -> BlockNo)
    -> Tracer m (TraceChainSyncClientEvent blk tip)
    -> NodeConfig (BlockProtocol blk)
    -> BlockchainTime m
    -> ClockSkew   -- ^ Maximum clock skew
    -> ChainDbView m blk tip
    -> StrictTVar m (AnchoredFragment (Header blk))
    -> Consensus ChainSyncClientPipelined blk tip m
chainSyncClient mkPipelineDecision0 getTipBlockNo tracer cfg btime
                (ClockSkew maxSkew)
                ChainDbView
                { getCurrentChain
                , getCurrentLedger
                , getOurTip
                , getIsInvalidBlock
                }
                varCandidate = ChainSyncClientPipelined $
    continueWithState () $ initialise
  where
    -- | Start ChainSync by looking for an intersection between our current
    -- chain fragment and their chain.
    initialise :: Stateful m blk tip () (ClientPipelinedStIdle Z)
    initialise = findIntersection (ForkTooDeep GenesisPoint)

    -- | Try to find an intersection by sending points of our current chain to
    -- the server, if any of them intersect with their chain, roll back our
    -- chain to that point and start synching using that fragment. If none
    -- intersect, disconnect by throwing the given exception.
    findIntersection
      :: ChainSyncClientExceptionType blk
         -- ^ Exception to throw when no intersection is found.
      -> Stateful m blk tip () (ClientPipelinedStIdle Z)
    findIntersection ex = Stateful $ \() -> do
      (ourFrag, ourChainState, ourTip) <- atomically $ (,,)
        <$> getCurrentChain
        <*> (ouroborosChainState <$> getCurrentLedger)
        <*> (Our <$> getOurTip)
      -- We select points from the last @k@ headers of our current chain. This
      -- means that if an intersection is found for one of these points, it
      -- was an intersection within the last @k@ blocks of our current chain.
      -- If not, we could never switch to this candidate chain anyway.
      let maxOffset = fromIntegral (AF.length ourFrag)
          points    = AF.selectPoints
                        (map fromIntegral (offsets maxOffset))
                        ourFrag
          uis = UnknownIntersectionState
            { ourFrag       = ourFrag
            , ourChainState = ourChainState
            , ourTip        = ourTip
            }
      return $ SendMsgFindIntersect points $ ClientPipelinedStIntersect
        { recvMsgIntersectFound = \i theirTip' ->
            continueWithState uis $
              intersectFound (castPoint i) (Their theirTip')
        , recvMsgIntersectNotFound = \theirTip' -> traceException $
            disconnect ourTip (Their theirTip') ex
        }

    -- | One of the points we sent intersected our chain. This intersection
    -- point will become the new tip of the candidate chain.
    intersectFound :: Point blk  -- ^ Intersection
                   -> Their tip
                   -> Stateful m blk tip
                        (UnknownIntersectionState blk tip)
                        (ClientPipelinedStIdle Z)
    intersectFound intersection theirTip
                 = Stateful $ \UnknownIntersectionState
                     { ourFrag
                     , ourChainState
                     , ourTip = ourTip
                     } -> do
      traceWith tracer $ TraceFoundIntersection intersection ourTip theirTip
      traceException $ do
        -- Roll back the current chain fragment to the @intersection@.
        --
        -- While the primitives in the ChainSync protocol are "roll back",
        -- "roll forward (apply block)", etc. The /real/ primitive is "switch
        -- to fork", which means that a roll back is always followed by
        -- applying at least as many blocks that we rolled back.
        --
        -- This is important for 'rewindChainState', which can only roll back
        -- up to @k@ blocks, /once/, i.e., we cannot keep rolling back the
        -- same chain state multiple times, because that would mean that we
        -- store the chain state for the /whole chain/, all the way to
        -- genesis.
        --
        -- So the rewind below is fine when we are switching to a fork (i.e.
        -- it is followed by rolling forward again), but we need some
        -- guarantees that the ChainSync protocol /does/ in fact give us a
        -- switch-to-fork instead of a true rollback.
        (theirFrag, theirChainState) <-
          case (,) <$> AF.rollback (castPoint intersection) ourFrag
                   <*> rewindChainState cfg ourChainState (pointSlot intersection)
          of
            Just (c, d) -> return (c, d)
            -- The @intersection@ is not on the candidate chain, even though
            -- we sent only points from the candidate chain to find an
            -- intersection with. The node must have sent us an invalid
            -- intersection point.
            Nothing     -> disconnect ourTip theirTip $
              InvalidIntersection intersection

        atomically $ writeTVar varCandidate theirFrag
        let kis = KnownIntersectionState
              { theirFrag       = theirFrag
              , theirChainState = theirChainState
              , ourFrag         = ourFrag
              , ourTip          = ourTip
              }
        continueWithState kis $ nextStep mkPipelineDecision0 Zero theirTip

    -- | Look at the current chain fragment that may have been updated in the
    -- background. Check whether the candidate fragment still intersects with
    -- it. If so, update the 'KnownIntersectionState' and trim the candidate
    -- fragment to the new current chain fragment's anchor point. If not,
    -- return 'Nothing'.
    intersectsWithCurrentChain
      :: KnownIntersectionState blk tip
      -> STM m (Maybe (KnownIntersectionState blk tip))
    intersectsWithCurrentChain kis@KnownIntersectionState
                               { theirFrag
                               , ourFrag
                               } = do
      ourFrag' <- getCurrentChain
      ourTip'  <- Our <$> getOurTip
      if
        | AF.headPoint ourFrag == AF.headPoint ourFrag' ->
          -- Our current chain didn't change, and changes to their chain that
          -- might affect the intersection point are handled elsewhere
          -- ('rollBackward'), so we have nothing to do.
          return $ Just kis

        | isJust (AF.intersectionPoint ourFrag' theirFrag) ->
          -- Our current chain changed, but it still intersects with candidate
          -- fragment, so update the 'ourFrag' field and trim to the
          -- candidate fragment to the same anchor point.
          --
          -- Note that this is the only place we need to trim. Headers on
          -- their chain can only become unnecessary (eligible for trimming)
          -- in two ways: 1. we adopted them, i.e., our chain changed (handled
          -- in this function); 2. we will /never/ adopt them, which is
          -- handled in the "no more intersection case".
          case AF.splitAfterPoint theirFrag (AF.anchorPoint ourFrag') of
           -- + Before the update to our fragment, both fragments were
           --   anchored at the same anchor.
           -- + We still have an intersection.
           -- + The number of blocks after the intersection cannot have
           --   shrunk, but could have increased.
           -- + If it did increase, the anchor point will have shifted up.
           -- + It can't have moved up past the intersection point (because
           --   then there would be no intersection anymore).
           -- + This means the new anchor point must be between the old anchor
           --   point and the new intersection point.
           -- + Since we know both the old anchor point and the new
           --   intersection point exist on their fragment, the new anchor
           --   point must also.
           Nothing -> error
             "anchor point must be on candidate fragment if they intersect"
           Just (_, trimmedCandidateFrag) -> return $ Just kis
             { ourFrag   = ourFrag'
             , theirFrag = trimmedCandidateFrag
             , ourTip    = ourTip'
             }

        | otherwise ->
          -- No more intersection with the current chain
          return Nothing

    -- | Request the next message (roll forward or backward), unless our chain
    -- has changed such that it no longer intersects with the candidate, in
    -- which case we initiate the intersection finding part of the protocol.
    --
    -- Note that this is the only place we check whether our current chain has
    -- changed.
    nextStep :: MkPipelineDecision
             -> Nat n
             -> Their tip
             -> Stateful m blk tip
                  (KnownIntersectionState blk tip)
                  (ClientPipelinedStIdle n)
    nextStep mkPipelineDecision n theirTip = Stateful $ \kis -> do
      mKis' <- atomically $ intersectsWithCurrentChain kis
      case mKis' of
        Just kis'@KnownIntersectionState { theirFrag, ourFrag } -> do
          -- Our chain (tip) didn't change or if it did, it still intersects
          -- with the candidate fragment, so we can continue requesting the
          -- next block.
          atomically $ writeTVar varCandidate theirFrag
          let candTipBlockNo = case AF.headBlockNo theirFrag of
                Just b  -> b
                -- If their fragment is somehow empty, base ourselves on our
                -- fragment instead. We know they must have the same anchor
                -- point. Look at the first block after the anchor point, use
                -- its block number - 1, this should correspond to the synced
                -- tip, i.e. their anchor point. If our fragment is empty too,
                -- then we and they are at Genesis.
                Nothing -> either
                  (const genesisBlockNo)
                  (blockNoBefore . blockNo)
                  (AF.last ourFrag)
              blockNoBefore 0 = 0 -- the genesis EBB
              blockNoBefore b = pred b

          return $ requestNext kis' mkPipelineDecision n theirTip candTipBlockNo
        Nothing ->
          -- Our chain (tip) has changed and it no longer intersects with the
          -- candidate fragment, so we have to find a new intersection, but
          -- first drain the pipe.
          continueWithState ()
            $ drainThePipe n
            $ findIntersection NoMoreIntersection

    -- | "Drain the pipe": collect and discard all in-flight responses and
    -- finally execute the given action.
    drainThePipe :: forall s n. NoUnexpectedThunks s
                 => Nat n
                 -> Stateful m blk tip s (ClientPipelinedStIdle Z)
                 -> Stateful m blk tip s (ClientPipelinedStIdle n)
    drainThePipe n0 m = Stateful $ go n0
      where
        go :: forall n'. Nat n'
           -> s
           -> m (Consensus (ClientPipelinedStIdle n') blk tip m)
        go n s = case n of
          Zero    -> continueWithState s m
          Succ n' -> return $ CollectResponse Nothing $ ClientStNext
            { recvMsgRollForward  = \_hdr _tip -> go n' s
            , recvMsgRollBackward = \_pt  _tip -> go n' s
            }

    requestNext :: KnownIntersectionState blk tip
                -> MkPipelineDecision
                -> Nat n
                -> Their tip
                -> BlockNo
                -> Consensus (ClientPipelinedStIdle n) blk tip m
    requestNext kis mkPipelineDecision n theirTip candTipBlockNo =
        case (n, decision) of
          (Zero, (Request, mkPipelineDecision')) ->
            SendMsgRequestNext
              (handleNext kis mkPipelineDecision' Zero)
              (return $ handleNext kis mkPipelineDecision' Zero) -- when we have to wait
          (_, (Pipeline, mkPipelineDecision')) ->
            SendMsgRequestNextPipelined
              (requestNext kis mkPipelineDecision' (Succ n) theirTip candTipBlockNo)
          (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
            CollectResponse
              (Just $ SendMsgRequestNextPipelined $
                requestNext kis mkPipelineDecision' (Succ n) theirTip candTipBlockNo)
              (handleNext kis mkPipelineDecision' n')
          (Succ n', (Collect, mkPipelineDecision')) ->
            CollectResponse
              Nothing
              (handleNext kis mkPipelineDecision' n')
      where
        theirTipBlockNo = getTipBlockNo (unTheir theirTip)
        decision = runPipelineDecision
          mkPipelineDecision
          n
          candTipBlockNo
          theirTipBlockNo

    handleNext :: KnownIntersectionState blk tip
               -> MkPipelineDecision
               -> Nat n
               -> Consensus (ClientStNext n) blk tip m
    handleNext kis mkPipelineDecision n = ClientStNext
      { recvMsgRollForward  = \hdr theirTip -> do
          traceWith tracer $ TraceDownloadedHeader hdr
          continueWithState kis $
            rollForward mkPipelineDecision n hdr (Their theirTip)
      , recvMsgRollBackward = \intersection theirTip -> do
          let intersection' :: Point blk
              intersection' = castPoint intersection
          traceWith tracer $ TraceRolledBack intersection'
          continueWithState kis $
            rollBackward mkPipelineDecision n intersection' (Their theirTip)
      }

    rollForward :: MkPipelineDecision
                -> Nat n
                -> Header blk
                -> Their tip
                -> Stateful m blk tip
                     (KnownIntersectionState blk tip)
                     (ClientPipelinedStIdle n)
    rollForward mkPipelineDecision n hdr theirTip
              = Stateful $ \kis@KnownIntersectionState
                  { theirChainState
                  , theirFrag
                  , ourTip
                  } -> traceException $ do
      -- Reject the block if invalid
      let hdrHash  = headerHash hdr
          hdrPoint = headerPoint hdr
      isInvalidBlock <- atomically $ forgetFingerprint <$> getIsInvalidBlock
      when (isInvalidBlock hdrHash) $
        disconnect ourTip theirTip $ InvalidBlock hdrPoint

      -- Get the ledger view required to validate the header
      -- NOTE: This will block if we are too far behind.
      ledgerView <- atomically $ getLedgerView hdr ourTip theirTip

      -- Check for clock skew
      wallclock <- atomically $ getCurrentSlot btime
      when (fmap unSlotNo (pointSlot hdrPoint) > At (unSlotNo wallclock + maxSkew)) $
        disconnect ourTip theirTip $ HeaderExceedsClockSkew hdrPoint wallclock

      -- Validate header
      let expectPrevHash = castHash (AF.headHash theirFrag)
          actualPrevHash = headerPrevHash hdr
      when (actualPrevHash /= expectPrevHash) $
        disconnect ourTip theirTip $ DoesntFit actualPrevHash expectPrevHash

      theirChainState' <-
        case runExcept $ applyChainState cfg ledgerView hdr theirChainState of
          Right theirChainState' -> return theirChainState'
          Left vErr              -> disconnect ourTip theirTip $
            ChainError hdrPoint vErr

      let theirFrag' = theirFrag :> hdr
          kis' = kis
            { theirFrag       = theirFrag'
            , theirChainState = theirChainState'
            }
      atomically $ writeTVar varCandidate theirFrag'

      continueWithState kis' $ nextStep mkPipelineDecision n theirTip

    -- Get the ledger view required to validate the header
    --
    -- To validate the block, we need the consensus chain state (updated using
    -- headers only, and kept as part of the candidate state) and the
    -- (anachronistic) ledger view. We read the latter as the first thing in
    -- the transaction, because we might have to retry the transaction if the
    -- ledger state is too far behind the upstream peer (see below).
    --
    -- NOTE: this doesn't need to be consistent with our current (possibly
    -- outdated) view of our chain, i.e. 'ourFrag', we /only/ use
    -- @curLedger@ to validate /their/ header, even in the special case
    -- discussed below.
    --
    -- NOTE: Low density chains
    --
    -- The ledger gives us an "anachronistic ledger view", which allows us
    -- to validate headers within a certain range of slots, provided that we
    -- maintain the invariant that the intersection between our tip and the
    -- tip of the peer fragment is within @k@ blocks from our tip (see
    -- detailed description at 'anachronisticProtocolLedgerView'). This
    -- range is in terms of /slots/, not blocks: this is important, because
    -- certain transitions on the ledger happen at slot boundaries (for
    -- instance, update proposals).
    --
    -- Under normal circumstances this is fine, but it can be problematic in
    -- the case of low density chains. For example, we might get the header
    -- for a block which is only two /blocks/ away from our current tip, but
    -- many slots (because for whatever reason simply no blocks were produced
    -- at all in that period).
    --
    -- We can mitigate this to /some/ degree by introducing one special case:
    -- if the header that we receive fits /directly/ onto our current chain,
    -- we can validate it even if it is outside the anachronistic ledger view
    -- window (based on its slot number). This is a useful special case
    -- because it means that we can catch up with a node that has an extension
    -- of our chain, even if there are many empty slots in between.
    --
    -- It is important to realize however that this special case does not help
    -- with forks. Suppose we have
    --
    -- >    our tip
    -- >      v
    -- > --*--*
    -- >   |
    -- >   \--*--*--*--*-- (chain we might be able to switch to)
    -- >      A
    --
    -- If the slot number for the block marked @A@ is way in the future,
    -- we will not be able to verify it and so we will not be able to switch
    -- to this fork.
    getLedgerView :: Header blk
                  -> Our tip
                  -> Their tip
                  -> STM m (LedgerView (BlockProtocol blk))
    getLedgerView hdr ourTip theirTip = do
        curLedger <- ledgerState <$> getCurrentLedger
        if headerPrevHash hdr == pointHash (ledgerTipPoint curLedger) then
          -- Special case mentioned above
          return $ protocolLedgerView cfg curLedger
        else
          -- The invariant guarantees us that the intersection of their tip
          -- and our tip is within k blocks from our tip. This means that the
          -- anachronistic ledger view must be available, unless they are
          -- too far /ahead/ of us. In this case we must simply wait

          -- TODO: Chain sync Client: Reuse anachronistic ledger view? #581
          case anachronisticProtocolLedgerView cfg curLedger (pointSlot hdrPoint) of
            -- unexpected alternative; see comment before this case expression
            Left TooFarBehind ->
                disconnect ourTip theirTip $ InvalidRollForward hdrPoint
            Left TooFarAhead  -> retry
            Right view -> case view `SB.at` hdrSlot of
                Nothing -> error "anachronisticProtocolLedgerView invariant violated"
                Just lv -> return lv
              where
                hdrSlot = case pointSlot hdrPoint of
                  Origin      -> SlotNo 0
                  At thisSlot -> thisSlot
      where
        hdrPoint = headerPoint hdr

    rollBackward :: MkPipelineDecision
                 -> Nat n
                 -> Point blk
                 -> Their tip
                 -> Stateful m blk tip
                      (KnownIntersectionState blk tip)
                      (ClientPipelinedStIdle n)
    rollBackward mkPipelineDecision n intersection
                 theirTip
               = Stateful $ \kis@KnownIntersectionState
                   { theirFrag
                   , theirChainState
                   , ourTip
                   } -> traceException $ do
      (theirFrag', theirChainState') <-
        case (,) <$> AF.rollback (castPoint intersection) theirFrag
                 <*> rewindChainState cfg theirChainState (pointSlot intersection)
        of
          Just (c, d) -> return (c,d)
          -- Remember that we use our current chain fragment as the starting
          -- point for the candidate's chain. Our fragment contained @k@
          -- headers. At this point, the candidate fragment might have grown to
          -- more than @k@ or rolled back to less than @k@ headers.
          --
          -- But now, it rolled back to some point that is not on the fragment,
          -- which means that it tried to roll back to some point before one of
          -- the last @k@ headers we initially started from. We could never
          -- switch to this fork anyway, so just disconnect. Furthermore, our
          -- current chain might have advanced in the meantime, so the point we
          -- would have to roll back to might have been much further back than
          -- @k@ blocks (> @k@ + the number of blocks we have advanced since
          -- starting syncing).
          --
          -- INVARIANT: a candidate fragment contains @>=k@ headers (unless
          -- near genesis, in which case we mean the total number of blocks in
          -- the fragment) minus @r@ headers where @r <= k@. This ghost
          -- variable @r@ indicates the number of headers we temporarily
          -- rolled back. Such a rollback must always be followed by rolling
          -- forward @s@ new headers where @s >= r@.
          --
          -- Thus, @k - r + s >= k@.
          Nothing     -> disconnect ourTip theirTip $
            InvalidRollBack intersection

      let kis' = kis
            { theirFrag       = theirFrag'
            , theirChainState = theirChainState'
            }
      atomically $ writeTVar varCandidate theirFrag'

      continueWithState kis' $ nextStep mkPipelineDecision n theirTip

    -- | Disconnect from the upstream node by throwing the given exception.
    -- The cleanup is handled in 'bracketChainSyncClient'.
    disconnect :: forall m' x'. MonadThrow m'
               => Our tip
               -> Their tip
               -> ChainSyncClientExceptionType blk -> m' x'
    disconnect ourTip theirTip typ = throwM $
      ChainSyncClientException { typ, ourTip, theirTip }

    -- | Trace any 'ChainSyncClientException' if thrown.
    traceException :: m a -> m a
    traceException m = m `catch` \(e :: ChainSyncClientException blk tip) -> do
      traceWith tracer $ TraceException e
      throwM e

    -- Recent offsets
    --
    -- These offsets are used to find an intersection point between our chain
    -- and the upstream node's. We use the fibonacci sequence to try blocks
    -- closer to our tip, and fewer blocks further down the chain. It is
    -- important that this sequence constains at least a point @k@ back: if no
    -- intersection can be found at most @k@ back, then this is not a peer
    -- that we can sync with (since we will never roll back more than @k).
    --
    -- For @k = 2160@, this evaluates to
    --
    -- > [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2160]
    --
    -- For @k = 5@ (during testing), this evaluates to
    --
    -- > [0,1,2,3,5]
    --
    -- In case the fragment contains less than @k@ blocks, we use the length
    -- of the fragment as @k@. This ensures that the oldest rollback point is
    -- selected.
    offsets :: Word64 -> [Word64]
    offsets maxOffset = [0] ++ takeWhile (< l) [fib n | n <- [2..]] ++ [l]
      where
        l = k `min` maxOffset

    k :: Word64
    k = maxRollbacks $ protocolSecurityParam cfg

-- | Watch the invalid block checker function for changes (using its
-- fingerprint). Whenever it changes, i.e., a new invalid block is detected,
-- check whether the current candidate fragment contains any header that is
-- invalid, if so, disconnect by throwing an 'InvalidBlock' exception.
--
-- Note that it is possible, yet unlikely, that the candidate fragment
-- contains a header that corresponds to an invalid block, but before we have
-- discovered this (after downloading and validating the block), the upstream
-- node could have rolled back such that its candidate chain no longer
-- contains the invalid block, in which case we do not disconnect from it.
--
-- This function spawns a background thread using the given 'ResourceRegistry'.
--
-- The cost of this check is \( O(cand * check) \) where /cand/ is the size of
-- the candidate fragment and /check/ is the cost of checking whether a block
-- is invalid (typically \( O(\log(invalid)) \) where /invalid/ is the number
-- of invalid blocks).
rejectInvalidBlocks
    :: forall m blk tip.
       ( IOLike m
       , SupportedBlock blk
       , Typeable tip
       , Show tip
       )
    => Tracer m (TraceChainSyncClientEvent blk tip)
    -> ResourceRegistry m
    -> STM m (WithFingerprint (HeaderHash blk -> Bool))
       -- ^ Get the invalid block checker
    -> STM m (AnchoredFragment (Header blk))
    -> m ()
rejectInvalidBlocks tracer registry getIsInvalidBlock getCandidate =
    onEachChange
      registry
      getFingerprint
      Nothing
      getIsInvalidBlock
      (checkInvalid . forgetFingerprint)
  where
    checkInvalid :: (HeaderHash blk -> Bool) -> m ()
    checkInvalid isInvalidBlock = do
      theirFrag <- atomically getCandidate
      -- The invalid block is likely to be a more recent block, so check from
      -- newest to oldest.
      mapM_ disconnect $
        find (isInvalidBlock . headerHash) (AF.toNewestFirst theirFrag)

    disconnect :: Header blk -> m ()
    disconnect invalidHeader = do
      let ex = ChainSyncClientException
            { typ      = InvalidBlock (headerPoint invalidHeader)
            , ourTip   = undefined
            , theirTip = undefined
            }
      traceWith tracer $ TraceException ex
      throwM ex

{-------------------------------------------------------------------------------
  Explicit state
-------------------------------------------------------------------------------}

-- | Make the state maintained by the chain sync client explicit
--
-- The chain sync client contains of a bunch of functions that basically look
-- like "do some network stuff, compute some stuff, and then continue with
-- such-and-such a new state". We want to make sure to keep that state in NF
-- at all times, but since we don't use a TVar to store it, we cannot reuse
-- the existing infrastructure for checking TVars for NF. Instead, we make
-- the state explicit in the types and do the check in 'continueWithState'.
newtype Stateful m blk tip s st = Stateful (s -> m (Consensus st blk tip m))

continueWithState :: forall m blk tip s st. NoUnexpectedThunks s
                  => s -> Stateful m blk tip s st -> m (Consensus st blk tip m)
continueWithState !s (Stateful f) = checkInvariant (unsafeNoThunks s) $ f s

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

data ChainSyncClientExceptionType blk =
    -- | The header we received was for a slot too far in the future.
    --
    -- I.e., the slot of the received header was > current slot (according to
    -- the wall time) + the max clock skew.
    --
    -- We store the point corresponding to the received header and the current
    -- slot number according to the wall clock.
    HeaderExceedsClockSkew (Point blk) SlotNo

    -- | The server we're connecting to forked more than @k@ blocks ago.
    --
    -- We store the found intersection.
  | ForkTooDeep (Point blk)

    -- | The chain validation threw an error for the header at the given
    -- point.
  | ChainError (Point blk) (ValidationErr (BlockProtocol blk))

    -- | The upstream node rolled forward to a point too far in our past. This
    -- may happen if, during catch-up, our local node has moved too far ahead
    -- of the upstream node.
  | InvalidRollForward (Point blk)

    -- | The upstream node rolled back more than @k@ blocks to the given
    -- point.
  | InvalidRollBack (Point blk)

    -- | We send the upstream node a bunch of points from a chain fragment and
    -- the upstream node responded with an intersection point that is not on
    -- our chain fragment, and thus not among the points we sent.
    --
    -- We store the intersection point the upstream node sent us.
  | InvalidIntersection (Point blk)

    -- | Our chain changed such that it no longer intersects with the
    -- candidate's fragment, and asking for a new intersection did not yield
    -- one.
  | NoMoreIntersection

    -- | The received header to roll forward doesn't fit onto the previous
    -- one.
    --
    -- The first 'ChainHash' is the previous hash of the received header and
    -- the second 'ChainHash' is the hash of the previous header, i.e., the
    -- expected value.
  | DoesntFit (ChainHash blk) (ChainHash blk)

    -- | The upstream node's chain contained a block that we know is invalid.
  | InvalidBlock (Point blk)

deriving instance SupportedBlock blk => Show (ChainSyncClientExceptionType blk)
deriving instance (SupportedBlock blk, Eq (ValidationErr (BlockProtocol blk)))
               => Eq (ChainSyncClientExceptionType blk)

data ChainSyncClientException blk tip = ChainSyncClientException
  { typ      :: ChainSyncClientExceptionType blk
  , ourTip   :: Our   tip
  , theirTip :: Their tip
  } deriving (Show, Exception)

deriving instance ( SupportedBlock blk
                  , Eq (ValidationErr (BlockProtocol blk))
                  , Eq tip
                  ) => Eq (ChainSyncClientException blk tip)


{-------------------------------------------------------------------------------
  TODO #221: Implement genesis

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

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Client.
data TraceChainSyncClientEvent blk tip
  = TraceDownloadedHeader (Header blk)
    -- ^ While following a candidate chain, we rolled forward by downloading a
    -- header.
  | TraceRolledBack (Point blk)
    -- ^ While following a candidate chain, we rolled back to the given point.
  | TraceFoundIntersection (Point blk) (Our tip) (Their tip)
    -- ^ We found an intersection between our chain fragment and the
    -- candidate's chain.
  | TraceException (ChainSyncClientException blk tip)
    -- ^ An exception was thrown by the Chain Sync Client.

deriving instance ( SupportedBlock blk
                  , Eq (ValidationErr (BlockProtocol blk))
                  , Eq (Header blk)
                  , Eq tip
                  )
               => Eq   (TraceChainSyncClientEvent blk tip)
deriving instance ( SupportedBlock blk
                  , Show (Header blk)
                  , Show tip
                  )
               => Show (TraceChainSyncClientEvent blk tip)

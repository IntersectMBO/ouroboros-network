{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncClient (
    Consensus
  , chainSyncClient
  , bracketChainSyncClient
  , ChainSyncClientException (..)
  , ClockSkew (..)
  , CandidateState (..)
    -- * Trace events
  , TraceChainSyncClientEvent (..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import           Data.List (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Control.Monad.Class.MonadThrow

import           Cardano.Prelude (NoUnexpectedThunks)

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
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.SlotBounded as SB
import           Ouroboros.Consensus.Util.STM (Fingerprint, onEachChange)


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

data ChainSyncClientException blk tip =
      -- | The header we received was for a slot too far in the future.
      --
      -- I.e., the slot of the received header was > current slot (according
      -- to the wall time) + the max clock skew.
      --
      -- The first 'Point' argument is the point of the received header, the
      -- second 'SlotId' argument is the current slot (by wall clock).
      HeaderExceedsClockSkew (Point blk) SlotNo

      -- | The server we're connecting to forked more than @k@ blocks ago.
      --
      -- The first 'Point' is the intersection point with the server that was
      -- too far in the past, the second 'Point' is the head of the server.
    | ForkTooDeep (Point blk) tip

      -- | The chain validation threw an error.
    | ChainError (ValidationErr (BlockProtocol blk))

      -- | The upstream node rolled forward to a point too far in our past.
      -- This may happen if, during catch-up, our local node has moved too far ahead of the upstream node.
      --
      -- We store the requested point and head point from the upstream node as
      -- well as the tip of our current ledger.
    | InvalidRollForward (Point blk) tip (Point blk)

      -- | The upstream node rolled back more than @k@ blocks.
      --
      -- We store the requested intersection point and head point from the
      -- upstream node.
    | InvalidRollBack (Point blk) tip

      -- | We send the upstream node a bunch of points from a chain fragment and
      -- the upstream node responded with an intersection point that is not on
      -- our chain fragment, and thus not among the points we sent.
      --
      -- We store the intersection point the upstream node sent us.
    | InvalidIntersection (Point blk)

      -- | The received header to roll forward doesn't fit onto the previous
      -- one.
      --
      -- The first 'ChainHash' is the previous hash of the received header and
      -- the second 'ChainHash' is that of the previous one.
    | DoesntFit (ChainHash blk) (ChainHash blk)

      -- | The upstream node's chain contained a block that we know is invalid.
    | InvalidBlock (Point blk)

deriving instance ( SupportedBlock blk
                  , Show tip
                  ) => Show (ChainSyncClientException blk tip)
deriving instance ( SupportedBlock blk
                  , Eq (ValidationErr (BlockProtocol blk))
                  , Eq tip
                  )
               => Eq (ChainSyncClientException blk tip)
instance ( SupportedBlock blk
         , Typeable tip
         , Show tip
         ) => Exception (ChainSyncClientException blk tip)

-- | The state of the candidate chain synched with an upstream node.
data CandidateState blk = CandidateState
    { candidateChain      :: !(AnchoredFragment (Header blk))
    , candidateChainState :: !(ChainState (BlockProtocol blk))
      -- ^ 'ChainState' corresponding to the tip (most recent block) of the
      -- 'candidateChain'.
    }
  deriving (Generic)

deriving instance SupportedBlock blk => NoUnexpectedThunks (CandidateState blk)

bracketChainSyncClient
    :: ( IOLike m
       , Ord peer
       , SupportedBlock blk
       , Typeable tip
       , Show tip
       )
    => Tracer m (TraceChainSyncClientEvent blk tip)
    -> STM m (AnchoredFragment (Header blk))
       -- ^ Get the current chain
    -> STM m (ExtLedgerState blk)
       -- ^ Get the current ledger state
    -> STM m (HeaderHash blk -> Bool, Fingerprint)
       -- ^ Get the invalid block checker
    -> StrictTVar m (Map peer (StrictTVar m (CandidateState blk)))
       -- ^ The candidate chains, we need the whole map because we
       -- (de)register nodes (@peer@).
    -> peer
    -> (StrictTVar m (CandidateState blk) -> AnchoredFragment (Header blk) -> m a)
    -> m a
bracketChainSyncClient tracer getCurrentChain getCurrentLedger
                       getIsInvalidBlock varCandidates peer body =
    withRegistry $ \registry ->
      bracket register unregister $ \(varCandidate, curChain) -> do
        rejectInvalidBlocks
          tracer
          registry
          getIsInvalidBlock
          (readTVar varCandidate)
        body varCandidate curChain
  where
    register = do
      (curChain, curChainState) <- atomically $ (,)
        <$> getCurrentChain
        <*> (ouroborosChainState <$> getCurrentLedger)
      varCandidate <- uncheckedNewTVarM CandidateState
        { candidateChain       = curChain
        , candidateChainState  = curChainState
        }
      atomically $ modifyTVar varCandidates $ Map.insert peer varCandidate
      -- We use our current chain, which contains the last @k@ headers, as
      -- the initial chain for the candidate.
      return (varCandidate, curChain)

    unregister _ = do
      atomically $ modifyTVar varCandidates $ Map.delete peer

-- | Chain sync client
--
-- This never terminates. In case of a failure, a 'ChainSyncClientException'
-- is thrown. The network layer classifies exception such that the
-- corresponding peer will never be chosen again.
chainSyncClient
    :: forall m blk tip.
       ( IOLike m
       , ProtocolLedgerView blk
       , Exception (ChainSyncClientException blk tip)
       )
    => MkPipelineDecision
    -> (tip -> BlockNo)
    -> Tracer m (TraceChainSyncClientEvent blk tip)
    -> NodeConfig (BlockProtocol blk)
    -> BlockchainTime m
    -> ClockSkew                                    -- ^ Maximum clock skew
    -> STM m (ExtLedgerState blk)                   -- ^ Get the current ledger state
    -> STM m (HeaderHash blk -> Bool, Fingerprint)  -- ^ Get the invalid block checker
    -> STM m BlockNo                                -- ^ Get the BlockNo of our current tip
    -> StrictTVar m (CandidateState blk)            -- ^ Our peer's state var
    -> AnchoredFragment (Header blk)                -- ^ The current chain
    -> Consensus ChainSyncClientPipelined blk tip m
chainSyncClient mkPipelineDecision0
                 tipBlockNo
                 tracer cfg btime (ClockSkew maxSkew)
                 getCurrentLedger getIsInvalidBlock getTipBlockNo varCandidate =
    \curChain -> ChainSyncClientPipelined (initialise curChain)
  where
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
    -- TODO #579 Simplification for now: we don't maintain the above invariant
    -- yet. Additionally, we don't monitor our current chain in order to find
    -- a better intersection point either.
    --
    -- TODO #465 Simplification for now: we don't trim candidate chains, so
    -- they might grow indefinitely.
    --
    -- TODO #423 rate-limit switching chains, otherwise we can't place blame (we
    -- don't know which candidate's chain included the point that was
    -- poisoned). E.g. two rollbacks per time slot -> make it configurable ->
    -- just a simple argument for now.
    --
    -- TODO #467 if the 'theirHead' that they sent us is on our chain, just
    -- switch to it.
    --
    -- TODO split in two parts: one requiring only the @TVar@ for the
    -- candidate instead of the whole map.
    initialise :: AnchoredFragment (Header blk)
               -> m (Consensus (ClientPipelinedStIdle Z) blk tip m)
    initialise curChain = do
      -- We select points from the last @k@ headers of our current chain. This
      -- means that if an intersection is found for one of these points, it
      -- was an intersection within the last @k@ blocks of our current chain.
      -- If not, we could never switch to this candidate chain anyway.
      --
      -- TODO #465 However, by the time we get a response about an
      -- intersection, our chain might have changed already and it could be
      -- that the intersection is now more than @k@ blocks in the past, which
      -- means the candidate is no longer eligible after all.
      let points = AF.selectPoints (map fromIntegral offsets) curChain
      return $ SendMsgFindIntersect points $ ClientPipelinedStIntersect
        { recvMsgIntersectFound    = intersectFound . castPoint
        , recvMsgIntersectNotFound = intersectNotFound
        }

    -- One of the points we sent intersected our chain. This intersection
    -- point will become the new tip of the candidate chain.
    intersectFound :: Point blk  -- ^ Intersection
                   -> tip        -- ^ Their head
                   -> m (Consensus (ClientPipelinedStIdle Z) blk tip m)
    intersectFound intersection theirHead = traceException $ atomically $ do

      CandidateState { candidateChain, candidateChainState } <- readTVar varCandidate
      -- Roll back the candidate to the @intersection@.
      --
      -- While the primitives in the ChainSync protocol are "roll back", "roll
      -- forward (apply block)", etc. The /real/ primitive is "switch to
      -- fork", which means that a roll back is always followed by applying at
      -- least as many blocks that we rolled back.
      --
      -- This is important for 'rewindChainState', which can only roll back up
      -- to @k@ blocks, /once/, i.e., we cannot keep rolling back the same
      -- chain state multiple times, because that would mean that we store the
      -- chain state for the /whole chain/, all the way to genesis.
      --
      -- So the rewind below is fine when we are switching to a fork (i.e. it
      -- is followed by rolling forward again), but we need some guarantees
      -- that the ChainSync protocol /does/ in fact give us a switch-to-fork
      -- instead of a true rollback.
      (candidateChain', candidateChainState') <-
        case (,) <$> AF.rollback (castPoint intersection) candidateChain
                 <*> rewindChainState cfg candidateChainState (pointSlot intersection)
        of
          Just (c,d) -> return (c,d)
          -- The @intersection@ is not on the candidate chain, even though we
          -- sent only points from the candidate chain to find an intersection
          -- with. The node must have sent us an invalid intersection point.
          Nothing    -> disconnect $ InvalidIntersection intersection

      -- TODO make sure the header state is fully evaluated, otherwise we'd
      -- hang on to the entire ledger state. This applies to everywhere we
      -- update the header state.
      writeTVar varCandidate CandidateState
        { candidateChain      = candidateChain'
        , candidateChainState = candidateChainState'
        }

      ourHeadBlockNo <- getTipBlockNo

      return $ requestNext mkPipelineDecision0 Zero ourHeadBlockNo theirHead

    -- If the intersection point is unchanged, this means that the best
    -- intersection point was the initial assumption: genesis.
    --
    -- Note: currently, we only try to find an intersection at start-up. When
    -- we later optimise this client to also find intersections after
    -- start-up, this code will have to be adapted, as it assumes it is only
    -- called at start-up.
    intersectNotFound :: tip               -- Their head
                      -> m (Consensus (ClientPipelinedStIdle Z) blk tip m)
    intersectNotFound theirHead = traceException $ atomically $ do
      curChainState <- ouroborosChainState <$> getCurrentLedger

      CandidateState { candidateChain } <- readTVar varCandidate
      -- If the genesis point is within the bounds of the candidate fragment
      -- (initially equal to our fragment), it means we (the client) are <=
      -- @k@ blocks from genesis and we want to sync with this
      -- server/candidate. If not, the server/candidate is on a fork that
      -- forked off too far in the past so that we do not intersect with them
      -- within the last @k@ blocks, so we don't want to sync with them.
      unless (AF.withinFragmentBounds genesisPoint candidateChain) $
        disconnect $ ForkTooDeep genesisPoint theirHead

      -- Get the 'ChainState' at genesis.
      let candidateChain' = Empty genesisPoint
      candidateChainState' <- case rewindChainState cfg curChainState Origin of
        Nothing -> disconnect $ ForkTooDeep genesisPoint theirHead
        Just c  -> pure c

      writeTVar varCandidate CandidateState
        { candidateChain       = candidateChain'
        , candidateChainState  = candidateChainState'
        }

      ourHeadBlockNo <- getTipBlockNo

      return $ requestNext mkPipelineDecision0 Zero ourHeadBlockNo theirHead

    requestNext :: MkPipelineDecision
                -> Nat n
                -> BlockNo  -- ^ Our head
                -> tip      -- ^ Their head
                -> Consensus (ClientPipelinedStIdle n) blk tip m
    requestNext mkPipelineDecision n ourHeadBlockNo theirHead =
        case (n, decision) of
          (_zero, (Request, mkPipelineDecision')) ->
            SendMsgRequestNext
              (handleNext mkPipelineDecision' Zero)
              (return $ handleNext mkPipelineDecision' Zero) -- when we have to wait
          (_, (Pipeline, mkPipelineDecision')) ->
            SendMsgRequestNextPipelined
              (requestNext mkPipelineDecision' (Succ n) ourHeadBlockNo theirHead)
          (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
            CollectResponse
              (Just $ SendMsgRequestNextPipelined $
                requestNext mkPipelineDecision' (Succ n) ourHeadBlockNo theirHead)
              (handleNext mkPipelineDecision' n')
          (Succ n', (Collect, mkPipelineDecision')) ->
            CollectResponse
              Nothing
              (handleNext mkPipelineDecision' n')
      where
        theirHeadBlockNo = tipBlockNo theirHead
        decision = runPipelineDecision
          mkPipelineDecision
          n
          ourHeadBlockNo
          theirHeadBlockNo

    handleNext :: MkPipelineDecision
               -> Nat n
               -> Consensus (ClientStNext n) blk tip m
    handleNext mkPipelineDecision n = ClientStNext
      { recvMsgRollForward  = \hdr theirHead -> do
          traceWith tracer $ TraceDownloadedHeader hdr
          rollForward mkPipelineDecision n hdr theirHead
      , recvMsgRollBackward = \intersection theirHead -> do
          let intersection' :: Point blk
              intersection' = castPoint intersection
          traceWith tracer $ TraceRolledBack intersection'
          rollBackward mkPipelineDecision n intersection' theirHead
      }

    rollForward :: MkPipelineDecision
                -> Nat n
                -> Header blk
                -> tip
                -> m (Consensus (ClientPipelinedStIdle n) blk tip m)
    rollForward mkPipelineDecision n hdr theirHead = traceException $ atomically $ do
      -- Reject the block if invalid
      let hdrHash  = headerHash hdr
          hdrPoint = headerPoint hdr
      (isInvalidBlock, _fingerprint) <- getIsInvalidBlock
      when (isInvalidBlock hdrHash) $
        disconnect $ InvalidBlock hdrPoint

      -- To validate the block, we need the consensus chain state (updated using
      -- headers only, and kept as part of the candidate state) and the
      -- (anachronistic) ledger view. We read the latter as the first thing in
      -- the transaction, because we might have to retry the transaction if the
      -- ledger state is too far behind the upstream peer (see below).
      curLedger <- ledgerState <$> getCurrentLedger
      let ourTip :: Point blk
          ourTip = ledgerTipPoint curLedger

      -- NOTE: Low density chains
      --
      -- The ledger gives us an "anachronistic ledger view", which allows us to
      -- validate headers within a certain range of slots, provided that we
      -- maintain the invariant that the intersecton between our tip and the tip
      -- of the peer fragment is within @k@ blocks from our tip (see detailed
      -- description at 'anachronisticProtocolLedgerView'). This range is in
      -- terms of /slots/, not blocks: this is important, because certain
      -- transitions on the ledger happen at slot boundaries (for instance,
      -- update proposals).
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
      ledgerView <-
        if headerPrevHash hdr == pointHash ourTip then
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
                disconnect $ InvalidRollForward hdrPoint theirHead ourTip
            Left TooFarAhead  -> retry
            Right view -> case view `SB.at` hdrSlot of
                Nothing -> error "anachronisticProtocolLedgerView invariant violated"
                Just lv -> return lv
              where
                hdrSlot = case pointSlot hdrPoint of
                  Origin      -> SlotNo 0
                  At thisSlot -> thisSlot

      -- Check for clock skew
      wallclock <- getCurrentSlot btime
      when (fmap unSlotNo (pointSlot hdrPoint) > At (unSlotNo wallclock + maxSkew)) $
        disconnect $ HeaderExceedsClockSkew hdrPoint wallclock

      -- Validate header
      CandidateState {..} <- readTVar varCandidate

      let expectPrevHash = castHash (AF.headHash candidateChain)
          actualPrevHash = headerPrevHash hdr
      when (actualPrevHash /= expectPrevHash) $
        disconnect $ DoesntFit actualPrevHash expectPrevHash

      candidateChainState' <-
        case runExcept $ applyChainState cfg ledgerView hdr candidateChainState of
          Left vErr                  -> disconnect $ ChainError vErr
          Right candidateChainState' -> return candidateChainState'

      writeTVar varCandidate CandidateState
        { candidateChain      = candidateChain :> hdr
        , candidateChainState = candidateChainState'
        }

      ourHeadBlockNo <- getTipBlockNo

      return $ requestNext mkPipelineDecision n ourHeadBlockNo theirHead

    rollBackward :: MkPipelineDecision
                 -> Nat n
                 -> Point blk
                 -> tip
                 -> m (Consensus (ClientPipelinedStIdle n) blk tip m)
    rollBackward mkPipelineDecision n intersection theirHead = traceException $ atomically $ do
      CandidateState {..} <- readTVar varCandidate

      (candidateChain', candidateChainState') <-
        case (,) <$> AF.rollback (castPoint intersection) candidateChain
                 <*> rewindChainState cfg candidateChainState (pointSlot intersection)
        of
          Just (c,d)  -> return (c,d)
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
          Nothing              -> disconnect $
            InvalidRollBack intersection theirHead

      writeTVar varCandidate CandidateState
        { candidateChain      = candidateChain'
        , candidateChainState = candidateChainState'
        }

      ourHeadBlockNo <- getTipBlockNo

      return $ requestNext mkPipelineDecision n ourHeadBlockNo theirHead

    -- | Disconnect from the upstream node by throwing the given exception.
    -- The cleanup is handled in 'bracketChainSyncClient'.
    disconnect :: ChainSyncClientException blk tip -> STM m a
    disconnect = throwM

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
    offsets :: [Word64]
    offsets = [0] ++ takeWhile (< k) [fib n | n <- [2..]] ++ [k]

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
    -> STM m (HeaderHash blk -> Bool, Fingerprint)
       -- ^ Get the invalid block checker
    -> STM m (CandidateState blk)
    -> m ()
rejectInvalidBlocks tracer registry getIsInvalidBlock getCandidateState =
    onEachChange registry snd Nothing getIsInvalidBlock checkInvalid
  where
    checkInvalid :: (HeaderHash blk -> Bool, Fingerprint) -> m ()
    checkInvalid (isInvalidBlock, _) = do
      candChain <- candidateChain <$> atomically getCandidateState
      -- The invalid block is likely to be a more recent block, so check from
      -- newest to oldest.
      mapM_ disconnect $
        find (isInvalidBlock . headerHash) (AF.toNewestFirst candChain)

    disconnect :: Header blk -> m ()
    disconnect invalidHeader = do
      let ex = InvalidBlock (headerPoint invalidHeader)
      traceWith tracer $ TraceException ex
      throwM ex

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

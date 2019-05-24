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
  , ChainSyncClientException (..)
  , ClockSkew (..)
  , CandidateState (..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (genesisPoint, genesisSlotNo)
import           Ouroboros.Network.Protocol.ChainSync.Client

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.SlotBounded as SB


-- | Clock skew: the number of slots the chain of an upstream node may be
-- ahead of the current slot (according to 'BlockchainTime').
--
-- E.g. a 'ClockSkew' value of @1@ means that a block produced by an upstream
-- it may have a slot number that is 1 greater than the current slot.
newtype ClockSkew = ClockSkew { unClockSkew :: Word64 }
  deriving (Eq, Ord, Enum, Bounded, Show, Num)

type Consensus (client :: * -> (* -> *) -> * -> *) hdr m =
   client hdr m Void

data ChainSyncClientException hdr =
      -- | The header we received was for a slot too far in the future.
      --
      -- I.e., the slot of the received header was > current slot (according
      -- to the wall time) + the max clock skew.
      --
      -- The first 'Point' argument is the point of the received header, the
      -- second 'SlotId' argument is the current slot (by wall clock).
      HeaderExceedsClockSkew (Point hdr) SlotNo

      -- | The server we're connecting to forked more than @k@ blocks ago.
      --
      -- The first 'Point' is the intersection point with the server that was
      -- too far in the past, the second 'Point' is the head of the server.
    | ForkTooDeep (Point hdr) (Point hdr)

      -- | The chain validation threw an error.
    | ChainError (ValidationErr (BlockProtocol hdr))

      -- | The upstream node rolled back more than @k@ blocks.
      --
      -- We store the requested intersection point and head point from the
      -- upstream node.
    | InvalidRollBack (Point hdr) (Point hdr)

    -- | We send the upstream node a bunch of points from a chain fragment and
    -- the upstream node responded with an intersection point that is not on
    -- our chain fragment, and thus not among the points we sent.
    --
    -- We store the intersection piont the upstream node sent us.
    | InvalidIntersection (Point hdr)


deriving instance ( StandardHash hdr
                  , OuroborosTag (BlockProtocol hdr)
                  )
    => Show (ChainSyncClientException hdr)

instance ( Typeable hdr, StandardHash hdr
         , OuroborosTag (BlockProtocol hdr)
         )
    => Exception (ChainSyncClientException hdr)

-- | The state of the candidate chain synched with an upstream node.
data CandidateState hdr = CandidateState
    { candidateChain      :: !(AnchoredFragment hdr)
    , candidateChainState :: !(ChainState (BlockProtocol hdr))
      -- ^ 'ChainState' corresponding to the tip (most recent block) of the
      -- 'candidateChain'.
    }


-- | Chain sync client
--
-- This never terminates. In case of a failure, a 'ChainSyncClientException'
-- is thrown. The network layer classifies exception such that the
-- corresponding peer will never be chosen again.
chainSyncClient
    :: forall m up blk hdr.
       ( MonadSTM m
       , MonadThrow (STM m)
       , ProtocolLedgerView blk
       , SupportedBlock (BlockProtocol hdr) hdr
       , HasHeader hdr
       , Ord up
       , Condense hdr, Condense (ChainHash hdr)
       , BlockProtocol blk ~ BlockProtocol hdr
       , HeaderHash blk ~ HeaderHash hdr
       )
    => Tracer m String
    -> NodeConfig (BlockProtocol hdr)
    -> BlockchainTime m
    -> ClockSkew                     -- ^ Maximum clock skew
    -> STM m (AnchoredFragment hdr)  -- ^ Get the current chain
    -> STM m (ExtLedgerState blk)    -- ^ Get the current ledger state
    -> TVar m (Map up (TVar m (CandidateState hdr)))
       -- ^ The candidate chains, we need the whole map because we
       -- (de)register nodes (@up@).
    -> up -> Consensus ChainSyncClient hdr m
chainSyncClient tracer cfg btime (ClockSkew maxSkew) getCurrentChain
                getCurrentLedger varCandidates up =
    ChainSyncClient initialise
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
    -- TODO #466 the 'ChainDB' exposes through 'knownInvalidBlocks :: STM m
    -- (Set (Point blk))' the invalid points. Whenever an upstream node has
    -- such a block in its chain, we must disconnect from it with an
    -- appropriate exception.
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
    initialise :: m (Consensus ClientStIdle hdr m)
    initialise = do
      (curChain, varCandidate) <- atomically $ do
        curChain  <- getCurrentChain
        curChainState <- ouroborosChainState <$> getCurrentLedger
        -- We use our current chain, which contains the last @k@ headers, as
        -- the initial chain for the candidate.
        varCandidate <- newTVar CandidateState
          { candidateChain       = curChain
          , candidateChainState  = curChainState
          }
        modifyTVar' varCandidates $ Map.insert up varCandidate
        return (curChain, varCandidate)

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

      return $ SendMsgFindIntersect points $ ClientStIntersect
        { recvMsgIntersectImproved  =
            ChainSyncClient .: intersectImproved  varCandidate
        , recvMsgIntersectUnchanged =
            ChainSyncClient .  intersectUnchanged varCandidate
        }

    -- One of the points we sent intersected our chain. This intersection
    -- point will become the new tip of the candidate chain.
    intersectImproved :: TVar m (CandidateState hdr)
                      -> Point hdr -> Point hdr
                      -> m (Consensus ClientStIdle hdr m)
    intersectImproved varCandidate intersection _theirHead = atomically $ do

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
        case (,) <$> AF.rollback intersection candidateChain
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
      return $ requestNext varCandidate


    -- If the intersection point is unchanged, this means that the best
    -- intersection point was the initial assumption: genesis.
    --
    -- Note: currently, we only try to find an intersection at start-up. When
    -- we later optimise this client to also find intersections after
    -- start-up, this code will have to be adapted, as it assumes it is only
    -- called at start-up.
    intersectUnchanged :: TVar m (CandidateState hdr)
                       -> Point hdr
                       -> m (Consensus ClientStIdle hdr m)
    intersectUnchanged varCandidate theirHead = atomically $ do
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
      candidateChainState' <- case rewindChainState cfg curChainState genesisSlotNo of
        Nothing -> disconnect $ ForkTooDeep genesisPoint theirHead
        Just c  -> pure c

      writeTVar varCandidate CandidateState
        { candidateChain       = candidateChain'
        , candidateChainState  = candidateChainState'
        }
      return $ requestNext varCandidate

    requestNext :: TVar m (CandidateState hdr)
                -> Consensus ClientStIdle hdr m
    requestNext varCandidate = SendMsgRequestNext
      (handleNext varCandidate)
      (return (handleNext varCandidate)) -- when we have to wait

    handleNext :: TVar m (CandidateState hdr)
               -> Consensus ClientStNext hdr m
    handleNext varCandidate = ClientStNext
      { recvMsgRollForward  = \hdr theirHead -> ChainSyncClient $ do
          traceWith tracer $ "Downloaded header: " <> condense hdr
          rollForward varCandidate hdr theirHead
      , recvMsgRollBackward = \intersection theirHead -> ChainSyncClient $ do
          traceWith tracer $ "Rolling back to: " <> condense intersection
          rollBackward varCandidate intersection theirHead
      }

    rollForward :: TVar m (CandidateState hdr)
                -> hdr -> Point hdr
                -> m (Consensus ClientStIdle hdr m)
    rollForward varCandidate hdr _theirHead = atomically $ do
      -- To validate the block, we need the consensus chain state (updated using
      -- headers only, and kept as part of the candidate state) and the
      -- (anachronistic) ledger view. We read the latter as the first thing in
      -- the transaction, because we might have to retry the transaction if the
      -- ledger state is too far behind the upstream peer (see below).
      curLedger <- ledgerState <$> getCurrentLedger

      let hdrPoint, ourTip :: Point hdr
          hdrPoint = blockPoint hdr
          ourTip   = castPoint $ ledgerTipPoint curLedger

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
        if blockPrevHash hdr == pointHash ourTip then
          -- Special case mentioned above
          return $ protocolLedgerView cfg curLedger
        else
          -- The invariant guarantees us that the intersection of their tip
          -- and our tip is within k blocks from our tip. This means that the
          -- anachronistic ledger view must be available, unless they are
          -- too far /ahead/ of us. In this case we must simply wait

          -- TODO: Chain sync Client: Reuse anachronistic ledger view? #581
          case anachronisticProtocolLedgerView cfg curLedger (pointSlot hdrPoint) of
            Nothing   -> retry
            Just view -> case view `SB.at` pointSlot hdrPoint of
              Nothing -> error "anachronisticProtocolLedgerView invariant violated"
              Just lv -> return lv

      -- Check for clock skew
      wallclock <- getCurrentSlot btime
      when (unSlotNo (pointSlot hdrPoint) > unSlotNo wallclock + maxSkew) $
        disconnect $ HeaderExceedsClockSkew hdrPoint wallclock

      -- Validate header
      CandidateState {..} <- readTVar varCandidate
      candidateChainState' <-
        case runExcept $ applyChainState cfg ledgerView hdr candidateChainState of
          Left vErr                  -> disconnect $ ChainError vErr
          Right candidateChainState' -> return candidateChainState'

      writeTVar varCandidate CandidateState
        { candidateChain      = candidateChain :> hdr
        , candidateChainState = candidateChainState'
        }
      return $ requestNext varCandidate

    rollBackward :: TVar m (CandidateState hdr)
                 -> Point hdr -> Point hdr
                 -> m (Consensus ClientStIdle hdr m)
    rollBackward varCandidate intersection theirHead = atomically $ do
      CandidateState {..} <- readTVar varCandidate

      (candidateChain', candidateChainState') <-
        case (,) <$> AF.rollback intersection candidateChain
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
      return $ requestNext varCandidate

    -- | Disconnect from the upstream node by throwing the given exception and
    -- removing its candidate from the map of candidates.
    disconnect :: ChainSyncClientException hdr -> STM m a
    disconnect ex = do
      modifyTVar' varCandidates $ Map.delete up
      throwM ex

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

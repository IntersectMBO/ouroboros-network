{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

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
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (genesisBlockNo, genesisPoint)
import           Ouroboros.Network.Protocol.ChainSync.Client

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()


-- | Clock skew: the number of slots the chain of an upstream node may be
-- ahead of the current slot (according to 'BlockchainTime').
--
-- E.g. a 'ClockSkew' value of @1@ means that a block produced by an upstream
-- it may have a slot number that is 1 greater than the current slot.
newtype ClockSkew = ClockSkew { unClockSkew :: Word64 }
  deriving (Eq, Ord, Enum, Bounded, Show, Num)

type Consensus (client :: * -> (* -> *) -> * -> *) hdr m =
   client hdr m Void

data ChainSyncClientException blk hdr =
      -- | The header we received was for a slot too far in the future.
      --
      -- I.e., the slot of the received header was > current slot (according
      -- to the wall time) + the max clock skew.
      --
      -- The first 'SlotNo' argument is the slot of the received header, the
      -- second 'SlotNo' argument is the current slot.
      TooFarInTheFuture SlotNo SlotNo

      -- | The server we're connecting to forked more than @k@ blocks ago.
      --
      -- The first 'Point' is the intersection point with the server that was
      -- too far in the past, the second 'Point' is the head of the server.
    | ForkTooDeep (Point hdr) (Point hdr)

      -- | The ledger threw an error.
    | LedgerError (LedgerError blk)

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


deriving instance (StandardHash hdr, Show (LedgerError blk))
    => Show (ChainSyncClientException blk hdr)

instance (Typeable hdr, Typeable blk, StandardHash hdr, Show (LedgerError blk))
    => Exception (ChainSyncClientException blk hdr)

-- | The state of the candidate chain synched with an upstream node.
data CandidateState blk hdr = CandidateState
    { candidateChain       :: !(AnchoredFragment hdr)
    , candidateHeaderState :: !(HeaderState blk)
      -- ^ 'HeaderState' corresponding to the tip (most recent block) of the
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
       , HasHeader hdr
       , BlockProtocol hdr ~ BlockProtocol blk
       , Ord up
       , Condense hdr, Condense (ChainHash hdr)
       )
    => Tracer m String
    -> NodeConfig (BlockProtocol hdr)
    -> BlockchainTime m
    -> ClockSkew                     -- ^ Maximum clock skew
    -> STM m (AnchoredFragment hdr)  -- ^ Get the current chain
    -> STM m (ExtLedgerState blk)    -- ^ Get the current ledger state
    -> TVar m (Map up (TVar m (CandidateState blk hdr)))
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
    -- 'HeaderState' with the headers, which returns an error when validation
    -- failed. Thus, in addition to the chain fragment of each candidate, we
    -- also store a 'HeaderState' corresponding to the head of the candidate
    -- chain.
    --
    -- We must keep the candidate chain synchronised with the corresponding
    -- upstream chain. The upstream node's chain might roll forward or
    -- backwards, and they will inform us about this. When we get these
    -- messages, we will replicate these actions on our candidate chain.
    --
    -- TODO #465 Simplification for now: we don't monitor our current chain in
    -- order to reject candidates that are no longer eligible (fork off more
    -- than @k@ blocks in the past) or to find a better intersection point.
    -- TODO #472 is this alright for the 'HeaderState', won't it get stale?
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
        curLedger <- ledgerState <$> getCurrentLedger
        -- We use our current chain, which contains the last @k@ headers, as
        -- the initial chain for the candidate.
        varCandidate <- newTVar CandidateState
          { candidateChain       = curChain
          , candidateHeaderState = getHeaderState curLedger 0
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

    -- One of the points we sent intersected our chain
    intersectImproved :: TVar m (CandidateState blk hdr)
                      -> Point hdr -> Point hdr
                      -> m (Consensus ClientStIdle hdr m)
    intersectImproved varCandidate intersection _theirHead = atomically $ do
      -- TODO #472
      curLedger <- ledgerState <$> getCurrentLedger

      CandidateState { candidateChain } <- readTVar varCandidate
      -- Roll back the candidate to the @intersection@.
      candidateChain' <- case AF.rollback intersection candidateChain of
        Just c  -> return c
        -- The @intersection@ is not on the candidate chain, even though we
        -- sent only points from the candidate chain to find an intersection
        -- with. The node must have sent us an invalid intersection point.
        Nothing -> disconnect $ InvalidIntersection intersection

      -- Get the HeaderState corresponding to the point/block/header we rolled
      -- back to.
      let candidateHeaderState' =
            getHeaderStateFor curLedger candidateChain candidateChain'

      -- TODO make sure the header state is fully evaluated, otherwise we'd
      -- hang on to the entire ledger state. This applies to everywhere we
      -- update the header state.
      writeTVar varCandidate CandidateState
        { candidateChain       = candidateChain'
        , candidateHeaderState = candidateHeaderState'
        }
      return $ requestNext varCandidate


    -- If the intersection point is unchanged, this means that the best
    -- intersection point was the initial assumption: genesis.
    --
    -- Note: currently, we only try to find an intersection at start-up. When
    -- we later optimise this client to also find intersections after
    -- start-up, this code will have to be adapted, as it assumes it is only
    -- called at start-up.
    intersectUnchanged :: TVar m (CandidateState blk hdr)
                       -> Point hdr
                       -> m (Consensus ClientStIdle hdr m)
    intersectUnchanged varCandidate theirHead = atomically $ do
      -- TODO #472
      curLedger <- ledgerState <$> getCurrentLedger

      CandidateState { candidateChain } <- readTVar varCandidate
      -- If the genesis point is within the bounds of the candidate fragment
      -- (initially equal to our fragment), it means we (the client) are <=
      -- @k@ blocks from genesis and we want to sync with this
      -- server/candidate. If not, the server/candidate is on a fork that
      -- forked off too far in the past so that we do not intersect with them
      -- within the last @k@ blocks, so we don't want to sync with them.
      unless (AF.withinFragmentBounds genesisPoint candidateChain) $
        disconnect $ ForkTooDeep genesisPoint theirHead

      -- Get the 'HeaderState' at genesis (0).
      let candidateChain' = Empty genesisPoint
          candidateHeaderState' =
            getHeaderStateFor curLedger candidateChain candidateChain'

      writeTVar varCandidate CandidateState
        { candidateChain       = candidateChain'
        , candidateHeaderState = candidateHeaderState'
        }
      return $ requestNext varCandidate

    requestNext :: TVar m (CandidateState blk hdr)
                -> Consensus ClientStIdle hdr m
    requestNext varCandidate = SendMsgRequestNext
      (handleNext varCandidate)
      (return (handleNext varCandidate)) -- when we have to wait

    handleNext :: TVar m (CandidateState blk hdr)
               -> Consensus ClientStNext hdr m
    handleNext varCandidate = ClientStNext
      { recvMsgRollForward  = \hdr theirHead -> ChainSyncClient $ do
          res <- rollForward varCandidate hdr theirHead
          traceWith tracer $ "Downloaded header: " <> condense hdr
          return res
      , recvMsgRollBackward = \intersection theirHead -> ChainSyncClient $ do
          res <- rollBackward varCandidate intersection theirHead
          traceWith tracer $ "Rolled back to: " <> condense intersection
          return res
      }

    rollForward :: TVar m (CandidateState blk hdr)
                -> hdr -> Point hdr
                -> m (Consensus ClientStIdle hdr m)
    rollForward varCandidate hdr theirHead = atomically $ do
      currentSlot <- getCurrentSlot btime
      let theirSlot = AF.pointSlot theirHead

      when (unSlotNo theirSlot > unSlotNo currentSlot + maxSkew) $
        disconnect $ TooFarInTheFuture theirSlot currentSlot

      -- TODO #472
      curLedger <- ledgerState <$> getCurrentLedger

      CandidateState {..} <- readTVar varCandidate

      candidateHeaderState' <-
        case runExcept $ advanceHeader curLedger hdr candidateHeaderState of
          Left ledgerError            -> disconnect $ LedgerError ledgerError
          Right candidateHeaderState' -> return candidateHeaderState'
      writeTVar varCandidate CandidateState
        { candidateChain       = candidateChain :> hdr
        , candidateHeaderState = candidateHeaderState'
        }
      return $ requestNext varCandidate

    rollBackward :: TVar m (CandidateState blk hdr)
                 -> Point hdr -> Point hdr
                 -> m (Consensus ClientStIdle hdr m)
    rollBackward varCandidate intersection theirHead = atomically $ do
      CandidateState {..} <- readTVar varCandidate
      candidateChain' <- case AF.rollback intersection candidateChain of
        Just candidateChain' -> return candidateChain'
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
        Nothing              -> disconnect $
          InvalidRollBack intersection theirHead

      -- TODO #472
      curLedger <- ledgerState <$> getCurrentLedger

      let candidateHeaderState' =
            getHeaderStateFor curLedger candidateChain candidateChain'
      writeTVar varCandidate CandidateState
        { candidateChain       = candidateChain'
        , candidateHeaderState = candidateHeaderState'
        }
      return $ requestNext varCandidate

    -- | Disconnect from the upstream node by throwing the given exception and
    -- removing its candidate from the map of candidates.
    disconnect :: ChainSyncClientException blk hdr -> STM m a
    disconnect ex = do
      modifyTVar' varCandidates $ Map.delete up
      throwM ex

    -- | Get the 'HeaderState' for the head of the given chain.
    getHeaderStateFor
      :: LedgerState blk
      -> AnchoredFragment hdr
         -- ^ The ledger state corresponds to the head of this chain
      -> AnchoredFragment hdr
         -- ^ We want the ledger state for the head of this chain
      -> HeaderState blk
    getHeaderStateFor ledgerState ledgerChain wantedChain =
        getHeaderState ledgerState rollBack
      where
        ledgerHeadBlockNo = mostRecentBlockNo ledgerChain
        wantedHeadBlockNo = mostRecentBlockNo wantedChain
        rollBack = unBlockNo ledgerHeadBlockNo - unBlockNo wantedHeadBlockNo

    -- | Return the 'BlockNo' of the most recent header of the given chain,
    -- the one at the tip. If the fragment is empty, it must be that we're
    -- near genesis, so return 'genesisBlockNo' in that case.
    mostRecentBlockNo :: AnchoredFragment hdr -> BlockNo
    mostRecentBlockNo = fromMaybe genesisBlockNo . AF.headBlockNo

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

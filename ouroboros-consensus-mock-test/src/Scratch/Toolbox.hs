{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A repository of useful combinators for debugging ThreadNet tests.
module Scratch.Toolbox (
  AnchoredFragmentStats (..),
  SelectionEvent (..),
  StmEvent (..),
  UsefulEventSubset (..),
  ) where

import           Data.Functor.Contravariant (contramap)
import           Data.Map (Map)
import           GHC.Stack

import           Control.Tracer (Tracer (..), traceWith)

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.BlockFetch.ClientState as Real
import qualified Ouroboros.Network.BlockFetch.Decision as Real
import qualified Ouroboros.Network.BlockFetch.State as Real

import           Ouroboros.Consensus.Block (BlockNo, Header, Point (..), blockNo, pointSlot)
import           Ouroboros.Consensus.Block.RealPoint (RealPoint (..), blockRealPoint, headerRealPoint)
import           Ouroboros.Consensus.Block.SupportsProtocol (BlockSupportsProtocol)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as Real
import           Ouroboros.Consensus.Mock.Ledger ()
import           Ouroboros.Consensus.Mock.Node ()
import qualified Ouroboros.Consensus.Node as Real
import qualified Ouroboros.Consensus.Node.Tracers as Real
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Slots (NumSlots (..))

import           Scratch.ThreadNet.MockNode
import           Scratch.ThreadNet.Types (PeerId)

data AnchoredFragmentStats = AnchoredFragmentStats
  { afLength :: NumBlocks
  , afSpan   :: NumSlots
  }
  deriving (Show)

anchoredFragmentStats ::
     AF.HasHeader blk
  => AF.AnchoredFragment blk -> AnchoredFragmentStats
anchoredFragmentStats af =
    AnchoredFragmentStats
    { afLength = NumBlocks $ fromIntegral $ AF.length af
    , afSpan   =
      NumSlots $
      case (pointSlot (AF.anchorPoint af), AF.headSlot af) of
        (Origin, Origin) -> 0
        (At x, At y)     -> unSlotNo y - unSlotNo x   -- TODO assert x < y?
        (Origin, At y)   -> unSlotNo y + 1   -- TODO
        (At{}, Origin)   -> error "impossible"
    }

class UsefulEventSubset e eSTM m blk where
    usefulEventSubsetTracer :: Tracer m e -> Tracer (IOLike.STM m) eSTM -> VertexTracers m blk

cantBeGenesis :: HasCallStack => Point blk -> RealPoint blk
cantBeGenesis GenesisPoint     = error "cantBeGenesis: what did I tell you!?"
cantBeGenesis (BlockPoint s h) = RealPoint s h

-- | Events for whenever a node selects a chain or forges a block.
data SelectionEvent blk =
    AmLeader          SlotNo Bool
  | DecidedToFetch (Map (Real.ConnectionId PeerId) (Real.CandidateFragment (Header blk))) [Real.TraceLabelPeer (Real.ConnectionId PeerId) (Real.FetchDecision [Point (Header blk)])]
  | DownloadedBlock          (RealPoint blk)
  | DownloadedBlockCS        (RealPoint blk)
  | DownloadedHeader         (RealPoint blk)
  | ExtendedSelection        (RealPoint blk) (WithOrigin BlockNo) AnchoredFragmentStats (Point blk)
  | FinalSelection               (Point blk) (Point blk)
  | ForgedBlock       SlotNo (RealPoint blk) BlockNo (Point blk)
  | HarnessEvent      (ThreadNetEvent blk)
  | Humoring                 (RealPoint blk)
  | SwitchedSelection        (RealPoint blk) (WithOrigin BlockNo) AnchoredFragmentStats (Point blk) (AnchoredFragmentStats, AnchoredFragmentStats)

deriving instance (AF.HasHeader blk, Show (Header blk)) => Show (SelectionEvent blk)

data StmEvent blk =
    StmBF (Real.FetchStateFingerprint (Real.ConnectionId PeerId) (Header blk) blk) (Real.FetchStateFingerprint (Real.ConnectionId PeerId) (Header blk) blk)
  | StmCS (Real.TraceLabelPeer (Real.ConnectionId PeerId) (Real.TraceStmChainSyncClientEvent blk))

deriving instance (BlockSupportsProtocol blk, Show (Header blk)) => Show (StmEvent blk)

instance
     ( blk ~ blk2
     , Monad m
     , Monad (IOLike.STM m)
     , Real.RunNode blk
     )
  => UsefulEventSubset (SelectionEvent blk) (StmEvent blk) m blk2 where
  usefulEventSubsetTracer tracer tracerSTM = nullVertexTracers {
      tracerChainDB = Tracer $ \case
        Real.TraceAddBlockEvent
          (ChainDB.AddedToCurrentChain _events tip old new)
          -> traceWith tracer $
             ExtendedSelection
               (ChainDB.newTipPoint tip)
               (AF.headBlockNo new)
               (anchoredFragmentStats new)
               (AF.castPoint $ AF.headPoint old)
        Real.TraceAddBlockEvent
          (ChainDB.SwitchedToAFork _events tip old new)
          -> traceWith tracer $
             SwitchedSelection
               (ChainDB.newTipPoint tip)
               (AF.headBlockNo new)
               (anchoredFragmentStats new)
               (AF.castPoint $ AF.headPoint old)
               ( case AF.intersect old new of
                   Nothing                          -> error "impossible!"
                   Just (_pre1, _pre2, suf1, suf2) ->
                       ( anchoredFragmentStats suf1
                       , anchoredFragmentStats suf2
                       )
               )
        Real.TraceOpenEvent (ChainDB.ClosedDB imm vol)
          -> traceWith tracer (FinalSelection imm vol)
        _ -> pure ()
    , tracerHarness = contramap HarnessEvent tracer
    , tracersConsensus   = Real.nullTracers {
          Real.blockFetchClientTracer = contramap withoutCreds $ Tracer $ \case
            Real.CompletedBlockFetch p _st1 _st2 _st3
              -> traceWith tracer $ DownloadedBlock (cantBeGenesis (AF.castPoint p))
            _ -> pure ()
        , Real.blockFetchFingerprintTracer = uncurry StmBF `contramap` tracerSTM
        , Real.blockFetchDecisionTracer = uncurry DecidedToFetch `contramap` tracer
        , Real.chainSyncClientTracer    = contramap withoutCreds $ Tracer $ \case
            Real.TraceDownloadedHeader h
              -> traceWith tracer $ DownloadedHeader (headerRealPoint h)
--            Real.TraceDownloadedBlock rp
--              -> traceWith tracer $ DownloadedBlockCS rp
--            Real.TraceRequestedBlock rp
--              -> traceWith tracer $ Humoring rp
            _ -> pure ()
        , Real.chainSyncClientTracerSTM = contramap StmCS tracerSTM
        , Real.forgeTracer = Tracer $ \case
            Real.TraceLabelCreds _
              (Real.TraceForgedBlock currentSlot point blk _mempoolSize)
              -> traceWith tracer $ ForgedBlock
                   currentSlot
                   (blockRealPoint blk)
                   (blockNo blk)
                   point
            Real.TraceLabelCreds _
              (Real.TraceNodeIsLeader currentSlot)
              -> traceWith tracer $ AmLeader currentSlot True
            Real.TraceLabelCreds _
              (Real.TraceNodeNotLeader currentSlot)
              -> traceWith tracer $ AmLeader currentSlot False
            _ -> pure ()
        }
    }
    where
      withoutCreds (Real.TraceLabelPeer _ ev) = ev

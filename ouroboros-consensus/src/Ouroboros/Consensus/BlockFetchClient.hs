{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Ouroboros.Consensus.BlockFetchClient
  ( BlockFetchClient
  , blockFetchClient
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Network.TypedProtocol.Pipelined (PeerPipelined)
import           Ouroboros.Network.Codec

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
                     (BlockFetchConsensusInterface (..))
import           Ouroboros.Network.BlockFetch.Client (FetchClientPolicy (..))
import qualified Ouroboros.Network.BlockFetch.Client as BlockFetchClient
import           Ouroboros.Network.BlockFetch.Types (FetchClientStateVars)
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Protocol.BlockFetch.Type
                     (BlockFetch (BFIdle))


-- | The block fetch layer doesn't provide a readable type for the client yet,
-- so define it ourselves for now.
type BlockFetchClient hdr blk m a =
  FetchClientStateVars hdr m ->
  PeerPipelined (BlockFetch hdr blk) AsClient BFIdle m a

-- | Block fetch client based on
-- 'Ouroboros.Network.BlockFetch.Examples.mockedBlockFetchClient1', but using
-- the 'ChainDB' (through the 'BlockFetchConsensusInterface').
blockFetchClient
    :: forall m up hdr blk.
       (MonadSTM m, MonadTime m, MonadThrow m,
        HasHeader blk, HasHeader hdr, HeaderHash hdr ~ HeaderHash blk)
    => Tracer m String
    -> BlockFetchConsensusInterface up hdr blk m
    -> up -> BlockFetchClient hdr blk m ()
blockFetchClient tracer blockFetchInterface _up clientStateVars =
   -- TODO use @up@ in the tracer.
    BlockFetchClient.blockFetchClient
        (showTracing tracer)
        policy
        clientStateVars
        -- TODO #469 replace with real GSV
        (return exampleFixedPeerGSVs)
  where
    BlockFetchConsensusInterface
      { blockFetchSize, blockMatchesHeader, addFetchedBlock } =
        blockFetchInterface
    -- TODO #468 the block fetch layer will eventually use the fields from the
    -- 'BlockFetchConsensusInterface' to make a 'FetchClientPolicy'. So long
    -- as this isn't done yet, let's do it ourselves.
    policy = FetchClientPolicy
      { blockFetchSize, blockMatchesHeader, addFetchedBlock }

    -- | Roughly 10ms ping time and 1MBit\/s bandwidth, leads to ~2200 bytes
    -- in flight minimum.
    --
    exampleFixedPeerGSVs :: PeerGSV
    exampleFixedPeerGSVs = PeerGSV { outboundGSV, inboundGSV }
      where
        inboundGSV  = ballisticGSV 10e-3 10e-6 (degenerateDistribution 0)
        outboundGSV = inboundGSV

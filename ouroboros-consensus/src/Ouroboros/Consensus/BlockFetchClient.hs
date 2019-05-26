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
import qualified Ouroboros.Network.BlockFetch.Client as BlockFetchClient
import           Ouroboros.Network.BlockFetch.ClientState
                   ( FetchClientContext(..), FetchClientPolicy (..) )
import           Ouroboros.Network.Protocol.BlockFetch.Type
                     (BlockFetch (BFIdle))

import           Ouroboros.Consensus.Util.Condense

-- | The block fetch layer doesn't provide a readable type for the client yet,
-- so define it ourselves for now.
type BlockFetchClient hdr blk m a =
  FetchClientContext hdr blk m ->
  PeerPipelined (BlockFetch hdr blk) AsClient BFIdle m a

-- | Block fetch client based on
-- 'Ouroboros.Network.BlockFetch.Examples.mockedBlockFetchClient1', but using
-- the 'ChainDB' (through the 'BlockFetchConsensusInterface').
blockFetchClient
    :: forall m up hdr blk.
       (MonadSTM m, MonadTime m, MonadThrow m,
        HasHeader blk, HasHeader hdr, HeaderHash hdr ~ HeaderHash blk,
        Condense blk)
    => Tracer m String
    -> up -> BlockFetchClient hdr blk m ()
blockFetchClient tracer _up clientCtx =
   -- TODO trace and use @up@ in the output.
    BlockFetchClient.blockFetchClient
      clientCtx { fetchClientCtxPolicy = policy' }
  where
    -- TODO: The existing behaviour has been preserved here, but it doesn't
    -- make much sense any more. What it is doing is adding a trace point onto
    -- the call to the ChainDB.addBlock within the client policy, but is doing
    -- it here rather than where the top level policy is set (where the actual
    -- call to ChainDB.addBlock is).
    --
    -- Alternatively, the existing TraceFetchClientState tracer could be used
    -- instead, since that already covers the download trace event.
    --
    policy     = fetchClientCtxPolicy clientCtx
    policy'    = policy
      { addFetchedBlock = \pt blk -> do
          addFetchedBlock policy pt blk
          traceWith tracer $ "Downloaded block: " <> condense blk
      }

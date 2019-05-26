{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Ouroboros.Consensus.BlockFetchClient
  ( BlockFetchClient
  , blockFetchClient
  ) where

import           Network.TypedProtocol.Pipelined (PeerPipelined)
import           Ouroboros.Network.Codec

import           Ouroboros.Network.BlockFetch.Client
                   (blockFetchClient, FetchClientContext)
import           Ouroboros.Network.Protocol.BlockFetch.Type
                   (BlockFetch (BFIdle))

-- | The block fetch layer doesn't provide a readable type for the client yet,
-- so define it ourselves for now.
type BlockFetchClient hdr blk m a =
  FetchClientContext hdr blk m ->
  PeerPipelined (BlockFetch hdr blk) AsClient BFIdle m a


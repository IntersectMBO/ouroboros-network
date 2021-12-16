{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Protocol.BlockFetch.Type where

import           Data.Proxy (Proxy (..))
import           Data.Void (Void)

import           Network.TypedProtocol.Core (Protocol (..))

import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))


-- | Range of blocks, defined by a lower and upper point, inclusive.
--
data ChainRange point = ChainRange !point !point
  deriving (Show, Eq, Ord)

data BlockFetch block point where
  BFIdle      :: BlockFetch block point
  BFBusy      :: BlockFetch block point
  BFStreaming :: BlockFetch block point
  BFDone      :: BlockFetch block point

instance ShowProxy block => ShowProxy (BlockFetch block point) where
    showProxy _ = "BlockFetch" ++ showProxy (Proxy :: Proxy block)

instance Protocol (BlockFetch block point) where

  data Message (BlockFetch block point) from to where
    -- | request range of blocks
    MsgRequestRange
      :: ChainRange point
      -> Message (BlockFetch block point) BFIdle BFBusy
    -- | start block streaming
    MsgStartBatch
      :: Message (BlockFetch block point) BFBusy BFStreaming
    -- | respond that there are no blocks
    MsgNoBlocks
      :: Message (BlockFetch block point) BFBusy BFIdle
    -- | stream a single block
    MsgBlock
      :: block
      -> Message (BlockFetch block point) BFStreaming BFStreaming
    -- | end of block streaming
    MsgBatchDone
      :: Message (BlockFetch block point) BFStreaming BFIdle

    -- | client termination message
    MsgClientDone
      :: Message (BlockFetch block point) BFIdle BFDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency BFIdle

  data ServerHasAgency st where
    TokBusy      :: ServerHasAgency BFBusy
    TokStreaming :: ServerHasAgency BFStreaming

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency BFDone

  exclusionLemma_ClientAndServerHaveAgency
    :: forall (st :: BlockFetch block point).
       ClientHasAgency st
    -> ServerHasAgency st
    -> Void
  exclusionLemma_ClientAndServerHaveAgency = \TokIdle x -> case x of {}

  exclusionLemma_NobodyAndClientHaveAgency
    :: forall (st :: BlockFetch block point).
       NobodyHasAgency st
    -> ClientHasAgency st
    -> Void
  exclusionLemma_NobodyAndClientHaveAgency = \TokDone x -> case x of {}

  exclusionLemma_NobodyAndServerHaveAgency
    :: forall (st :: BlockFetch block point).
       NobodyHasAgency st
    -> ServerHasAgency st
    -> Void
  exclusionLemma_NobodyAndServerHaveAgency = \TokDone x -> case x of {}

instance (Show block, Show point)
      => Show (Message (BlockFetch block point) from to) where
  show (MsgRequestRange range) = "MsgRequestRange " ++ show range
  show MsgStartBatch           = "MsgStartBatch"
  show (MsgBlock block)        = "MsgBlock " ++ show block
  show MsgNoBlocks             = "MsgNoBlocks"
  show MsgBatchDone            = "MsgBatchDone"
  show MsgClientDone           = "MsgClientDone"

instance Show (ClientHasAgency (st :: BlockFetch block point)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: BlockFetch block point)) where
  show TokBusy      = "TokBusy"
  show TokStreaming = "TokStreaming"

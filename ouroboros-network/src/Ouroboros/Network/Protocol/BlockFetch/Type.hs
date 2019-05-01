{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExplicitForAll        #-}
module Ouroboros.Network.Protocol.BlockFetch.Type where

import           Data.Void (Void)

import           Ouroboros.Network.Block (StandardHash, Point)
import           Network.TypedProtocol.Codec (AnyMessage (..))
import           Network.TypedProtocol.Core (Protocol (..))

-- | Range of headers, defined by a lower and upper point, inclusive.
--
data ChainRange header = ChainRange !(Point header) !(Point header)
  deriving (Show, Eq, Ord)

data BlockFetch header body where
  BFIdle      :: BlockFetch header body
  BFBusy      :: BlockFetch header body
  BFStreaming :: BlockFetch header body
  BFDone      :: BlockFetch header body


instance Protocol (BlockFetch header body) where

  data Message (BlockFetch header body) from to where
    -- | request range of blocks
    MsgRequestRange
      :: ChainRange header
      -> Message (BlockFetch header body) BFIdle BFBusy
    -- | start block streaming
    MsgStartBatch
      :: Message (BlockFetch header body) BFBusy BFStreaming
    -- | respond that there are no blocks
    MsgNoBlocks
      :: Message (BlockFetch header body) BFBusy BFIdle
    -- | stream a single block
    MsgBlock
      :: body
      -> Message (BlockFetch header body) BFStreaming BFStreaming
    -- | end of block streaming
    MsgBatchDone
      :: Message (BlockFetch header body) BFStreaming BFIdle

    -- | client termination message
    MsgClientDone
      :: Message (BlockFetch header body) BFIdle BFDone

  data ClientHasAgency ps where
    TokIdle :: ClientHasAgency BFIdle

  data ServerHasAgency ps where
    TokBusy      :: ServerHasAgency BFBusy
    TokStreaming :: ServerHasAgency BFStreaming

  data NobodyHasAgency ps where
    TokDone :: NobodyHasAgency BFDone

  exclusionLemma_ClientAndServerHaveAgency
    :: forall (st :: BlockFetch header body).
       ClientHasAgency st
    -> ServerHasAgency st
    -> Void
  exclusionLemma_ClientAndServerHaveAgency = \TokIdle x -> case x of {}

  exclusionLemma_NobodyAndClientHaveAgency
    :: forall (st :: BlockFetch header body).
       NobodyHasAgency st
    -> ClientHasAgency st
    -> Void
  exclusionLemma_NobodyAndClientHaveAgency = \TokDone x -> case x of {}

  exclusionLemma_NobodyAndServerHaveAgency
    :: forall (st :: BlockFetch header body).
       NobodyHasAgency st
    -> ServerHasAgency st
    -> Void
  exclusionLemma_NobodyAndServerHaveAgency = \TokDone x -> case x of {}

instance (StandardHash header, Show body) => Show (Message (BlockFetch header body) from to) where
  show (MsgRequestRange range) = "MsgRequestRange" ++ show range
  show MsgStartBatch           = "MsgStartBatch"
  show (MsgBlock block)        = "MsgBlock " ++ show block
  show MsgNoBlocks             = "MsgNoBlocks"
  show MsgBatchDone            = "MsgBatchDone"
  show MsgClientDone           = "MsgClientDone"

instance (StandardHash header, Show body) => Show (AnyMessage (BlockFetch header body)) where
  show (AnyMessage msg) = show msg

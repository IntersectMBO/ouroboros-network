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
import           Network.TypedProtocol.Core (Protocol (..))

-- | Range of blocks, defined by a lower and upper point, inclusive.
--
data ChainRange block = ChainRange !(Point block) !(Point block)
  deriving (Show, Eq, Ord)

data BlockFetch block where
  BFIdle      :: BlockFetch block
  BFBusy      :: BlockFetch block
  BFStreaming :: BlockFetch block
  BFDone      :: BlockFetch block


instance Protocol (BlockFetch block) where

  data Message (BlockFetch block) from to where
    -- | request range of blocks
    MsgRequestRange
      :: ChainRange block
      -> Message (BlockFetch block) BFIdle BFBusy
    -- | start block streaming
    MsgStartBatch
      :: Message (BlockFetch block) BFBusy BFStreaming
    -- | respond that there are no blocks
    MsgNoBlocks
      :: Message (BlockFetch block) BFBusy BFIdle
    -- | stream a single block
    MsgBlock
      :: block
      -> Message (BlockFetch block) BFStreaming BFStreaming
    -- | end of block streaming
    MsgBatchDone
      :: Message (BlockFetch block) BFStreaming BFIdle

    -- | client termination message
    MsgClientDone
      :: Message (BlockFetch block) BFIdle BFDone

  data ClientHasAgency ps where
    TokIdle :: ClientHasAgency BFIdle

  data ServerHasAgency ps where
    TokBusy      :: ServerHasAgency BFBusy
    TokStreaming :: ServerHasAgency BFStreaming

  data NobodyHasAgency ps where
    TokDone :: NobodyHasAgency BFDone

  exclusionLemma_ClientAndServerHaveAgency
    :: forall (st :: BlockFetch block).
       ClientHasAgency st
    -> ServerHasAgency st
    -> Void
  exclusionLemma_ClientAndServerHaveAgency = \TokIdle x -> case x of {}

  exclusionLemma_NobodyAndClientHaveAgency
    :: forall (st :: BlockFetch block).
       NobodyHasAgency st
    -> ClientHasAgency st
    -> Void
  exclusionLemma_NobodyAndClientHaveAgency = \TokDone x -> case x of {}

  exclusionLemma_NobodyAndServerHaveAgency
    :: forall (st :: BlockFetch block).
       NobodyHasAgency st
    -> ServerHasAgency st
    -> Void
  exclusionLemma_NobodyAndServerHaveAgency = \TokDone x -> case x of {}

instance (Show block, StandardHash block)
      => Show (Message (BlockFetch block) from to) where
  show (MsgRequestRange range) = "MsgRequestRange " ++ show range
  show MsgStartBatch           = "MsgStartBatch"
  show (MsgBlock block)        = "MsgBlock " ++ show block
  show MsgNoBlocks             = "MsgNoBlocks"
  show MsgBatchDone            = "MsgBatchDone"
  show MsgClientDone           = "MsgClientDone"

instance Show (ClientHasAgency (st :: BlockFetch block)) where
  show TokIdle = "TokIdle" 

instance Show (ServerHasAgency (st :: BlockFetch block)) where
  show TokBusy      = "TokBusy"
  show TokStreaming = "TokStreaming"

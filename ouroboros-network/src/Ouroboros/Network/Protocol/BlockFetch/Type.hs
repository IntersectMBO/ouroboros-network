{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ExplicitForAll           #-}

module Ouroboros.Network.Protocol.BlockFetch.Type where

import           Data.Kind (Type)
import           Data.Singletons

import           Network.TypedProtocol.Core (Protocol (..), Agency (..))

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

type SingBlockFetch :: BlockFetch block point
                    -> Type
data SingBlockFetch k where
    SingBFIdle      :: SingBlockFetch BFIdle
    SingBFBusy      :: SingBlockFetch BFBusy
    SingBFStreaming :: SingBlockFetch BFStreaming
    SingBFDone      :: SingBlockFetch BFDone

deriving instance Show (SingBlockFetch k)

type instance Sing = SingBlockFetch
instance SingI BFIdle      where sing = SingBFIdle
instance SingI BFBusy      where sing = SingBFBusy
instance SingI BFStreaming where sing = SingBFStreaming
instance SingI BFDone      where sing = SingBFDone

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

  type StateAgency BFIdle      = ClientAgency
  type StateAgency BFBusy      = ServerAgency
  type StateAgency BFStreaming = ServerAgency
  type StateAgency BFDone      = NobodyAgency


instance (Show block, Show point)
      => Show (Message (BlockFetch block point) from to) where
  show (MsgRequestRange range) = "MsgRequestRange " ++ show range
  show MsgStartBatch           = "MsgStartBatch"
  show (MsgBlock block)        = "MsgBlock " ++ show block
  show MsgNoBlocks             = "MsgNoBlocks"
  show MsgBatchDone            = "MsgBatchDone"
  show MsgClientDone           = "MsgClientDone"

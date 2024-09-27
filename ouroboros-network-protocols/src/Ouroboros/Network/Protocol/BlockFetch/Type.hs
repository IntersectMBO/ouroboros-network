{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

module Ouroboros.Network.Protocol.BlockFetch.Type where

import Data.Kind (Type)
import Data.Singletons

import Network.TypedProtocol.Core

import Control.DeepSeq
import GHC.Generics
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))


-- | Range of blocks, defined by a lower and upper point, inclusive.
--
data ChainRange point = ChainRange !point !point
  deriving (Show, Eq, Ord, Generic, NFData)

data BlockFetch block point where
  BFIdle      :: BlockFetch block point
  BFBusy      :: BlockFetch block point
  BFStreaming :: BlockFetch block point
  BFDone      :: BlockFetch block point

instance ShowProxy block => ShowProxy (BlockFetch block point) where
    showProxy _ = "BlockFetch" ++ showProxy (Proxy :: Proxy block)

type SingBlockFetch :: BlockFetch block point
                    -> Type
data SingBlockFetch k where
    SingBFIdle      :: SingBlockFetch BFIdle
    SingBFBusy      :: SingBlockFetch BFBusy
    SingBFStreaming :: SingBlockFetch BFStreaming
    SingBFDone      :: SingBlockFetch BFDone

deriving instance Show (SingBlockFetch k)

instance StateTokenI BFIdle      where stateToken = SingBFIdle
instance StateTokenI BFBusy      where stateToken = SingBFBusy
instance StateTokenI BFStreaming where stateToken = SingBFStreaming
instance StateTokenI BFDone      where stateToken = SingBFDone

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

  type StateToken = SingBlockFetch


instance ( NFData block
         , NFData point
         ) => NFData (Message (BlockFetch block point) from to) where
  rnf (MsgRequestRange crp) = rnf crp
  rnf MsgStartBatch         = ()
  rnf MsgNoBlocks           = ()
  rnf (MsgBlock b)          = rnf b
  rnf MsgBatchDone          = ()
  rnf MsgClientDone         = ()

instance (Show block, Show point)
      => Show (Message (BlockFetch block point) from to) where
  show (MsgRequestRange range) = "MsgRequestRange " ++ show range
  show MsgStartBatch           = "MsgStartBatch"
  show (MsgBlock block)        = "MsgBlock " ++ show block
  show MsgNoBlocks             = "MsgNoBlocks"
  show MsgBatchDone            = "MsgBatchDone"
  show MsgClientDone           = "MsgClientDone"

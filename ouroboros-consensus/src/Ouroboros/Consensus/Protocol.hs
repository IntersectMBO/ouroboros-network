
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Ouroboros.Consensus.Protocol (
    Protocol(..)
  , module X
    -- * Client support for nodes running a protocol
  , ProtocolClient(..)
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Util.IOLike

class (p ~ BlockProtocol blk, RunNode blk, IOLike m) => Protocol m blk p where
  data RunProtocol m blk p
  protocolInfo :: RunProtocol m blk p -> ProtocolInfo m blk

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
class (p ~ BlockProtocol blk, RunNode blk) => ProtocolClient blk p where
  data RunProtocolClient blk p
  protocolClientInfo :: RunProtocolClient blk p -> ProtocolClientInfo blk

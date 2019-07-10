module Ouroboros.Consensus.Node.ProtocolInfo.Abstract (
    NumCoreNodes(..)
  , enumCoreNodes
  , ProtocolInfo(..)
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Number of core nodes
-------------------------------------------------------------------------------}

newtype NumCoreNodes = NumCoreNodes Int
  deriving (Show)

enumCoreNodes :: NumCoreNodes -> [CoreNodeId]
enumCoreNodes (NumCoreNodes numNodes) =
    [ CoreNodeId n | n <- [0 .. numNodes - 1] ]

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the specified protocol.
data ProtocolInfo b = ProtocolInfo {
        pInfoConfig     :: NodeConfig (BlockProtocol b)
      , pInfoInitState  :: NodeState  (BlockProtocol b)
        -- | The ledger state at genesis
      , pInfoInitLedger :: ExtLedgerState b
      }

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Node.ProtocolInfo (
    NumCoreNodes(..)
  , enumCoreNodes
  , ProtocolInfo(..)
  ) where

import           Data.Word

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.NodeId

{-------------------------------------------------------------------------------
  Number of core nodes
-------------------------------------------------------------------------------}

newtype NumCoreNodes = NumCoreNodes Word64
  deriving (Show, NoUnexpectedThunks)

enumCoreNodes :: NumCoreNodes -> [CoreNodeId]
enumCoreNodes (NumCoreNodes 0)        = []
enumCoreNodes (NumCoreNodes numNodes) =
    [ CoreNodeId n | n <- [0 .. numNodes - 1] ]

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the specified protocol.
data ProtocolInfo b = ProtocolInfo {
        pInfoConfig     :: TopLevelConfig b
      , pInfoInitState  :: NodeState      b
      , pInfoInitLedger :: ExtLedgerState b -- ^ Genesis ledger state
      }

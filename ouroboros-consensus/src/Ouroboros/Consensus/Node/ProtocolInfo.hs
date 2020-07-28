{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Node.ProtocolInfo (
    NumCoreNodes(..)
  , enumCoreNodes
  , ProtocolInfo(..)
  , ProtocolClientInfo(..)
  ) where

import           Data.Word

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended
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
data ProtocolInfo m b = ProtocolInfo {
        pInfoConfig       :: TopLevelConfig b
      , pInfoInitLedger   :: ExtLedgerState b -- ^ At genesis
      , pInfoBlockForging :: Maybe (m (BlockForging m b))
      }

-- | Data required by clients of a node running the specified protocol.
data ProtocolClientInfo b = ProtocolClientInfo {
       pClientInfoCodecConfig :: CodecConfig b
     }

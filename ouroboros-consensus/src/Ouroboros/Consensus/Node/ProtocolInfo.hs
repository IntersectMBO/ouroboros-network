{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Node.ProtocolInfo (
    NumCoreNodes(..)
  , enumCoreNodes
  , ProtocolInfo(..)
  , ProtocolClientInfo(..)
  , castProtocolInfo
  , castProtocolClientInfo
  ) where

import           Data.Coerce
import           Data.Word

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract

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
        pInfoConfig         :: TopLevelConfig b
      , pInfoInitForgeState :: ForgeState     b
      , pInfoInitLedger     :: ExtLedgerState b -- ^ Genesis ledger state
      }

-- | Data required by clients of a node running the specified protocol.
data ProtocolClientInfo b = ProtocolClientInfo {
       pClientInfoCodecConfig :: CodecConfig b
     }

{-------------------------------------------------------------------------------
  Casting
-------------------------------------------------------------------------------}

castProtocolInfo
  :: ( Coercible (ConsensusConfig (BlockProtocol blk))
                 (ConsensusConfig (BlockProtocol blk'))
     , Coercible (BlockConfig blk) (BlockConfig blk')
     , Coercible (LedgerState blk) (LedgerState blk')
     , LedgerConfig blk ~ LedgerConfig blk'
     , ConsensusState (BlockProtocol blk) ~ ConsensusState (BlockProtocol blk')
     , TipInfo blk ~ TipInfo blk'
     , ForgeState blk ~ ForgeState blk'
     )
  => ProtocolInfo blk
  -> ProtocolInfo blk'
castProtocolInfo ProtocolInfo {..} = ProtocolInfo {
      pInfoConfig         = castTopLevelConfig pInfoConfig
    , pInfoInitForgeState = pInfoInitForgeState
    , pInfoInitLedger     = castExtLedgerState pInfoInitLedger
    }

castProtocolClientInfo :: Coercible (CodecConfig blk) (CodecConfig blk')
                       => ProtocolClientInfo blk -> ProtocolClientInfo blk'
castProtocolClientInfo ProtocolClientInfo{..} = ProtocolClientInfo {
      pClientInfoCodecConfig = coerce pClientInfoCodecConfig
    }

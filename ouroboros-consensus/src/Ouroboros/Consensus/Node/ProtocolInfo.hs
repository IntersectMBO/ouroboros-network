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

import           Data.Bifunctor
import           Data.Coerce
import           Data.Word

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
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
data ProtocolInfo m b = ProtocolInfo {
        pInfoConfig      :: TopLevelConfig b
      , pInfoInitLedger  :: ExtLedgerState b -- ^ At genesis
      , pInfoLeaderCreds :: Maybe ( CanBeLeader (BlockProtocol b)
                                  , MaintainForgeState m b
                                  )
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
     , Coercible (ChainDepState (BlockProtocol blk))
                 (ChainDepState (BlockProtocol blk'))
     , LedgerConfig blk ~ LedgerConfig blk'
     , TipInfo      blk ~ TipInfo      blk'
     , ForgeState   blk ~ ForgeState   blk'
     , Functor m
     )
  => ProtocolInfo m blk
  -> ProtocolInfo m blk'
castProtocolInfo ProtocolInfo {..} = ProtocolInfo {
      pInfoConfig      = castTopLevelConfig     pInfoConfig
    , pInfoInitLedger  = castExtLedgerState     pInfoInitLedger
    , pInfoLeaderCreds = second castMaintainForgeState <$> pInfoLeaderCreds
    }

castProtocolClientInfo :: Coercible (CodecConfig blk) (CodecConfig blk')
                       => ProtocolClientInfo blk -> ProtocolClientInfo blk'
castProtocolClientInfo ProtocolClientInfo{..} = ProtocolClientInfo {
      pClientInfoCodecConfig = coerce pClientInfoCodecConfig
    }

{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module Ouroboros.Consensus.Ledger.PermBFT where

import           Ouroboros.Consensus.Ledger.Abstract
                 (
                   BlockProtocol
                 , ProtocolLedgerView(..)
                 , UpdateLedger(..)
                 )
import           Ouroboros.Consensus.Protocol.Abstract
                 (
                   OuroborosTag
                 , SupportedBlock
                 )
import           Ouroboros.Network.Block (HasHeader (..))

import           Types (Block)

instance ( Show (LedgerState Block)
         , Show (LedgerError Block)
         ) => UpdateLedger Block where
  data LedgerState Block =
    MkLedgerState deriving Show

  data LedgerError Block =
    UndefinedLedgerError deriving Show

  -- Apply a block to the ledger state
  applyLedgerState b MkLedgerState = undefined


instance ( OuroborosTag (BlockProtocol Block)
         , UpdateLedger Block
         , HasHeader Block
         , SupportedBlock (BlockProtocol Block) Block
         ) => ProtocolLedgerView Block where
  protocolLedgerView = undefined

module Ouroboros.Consensus.Ledger.PermBFT where

import           Ouroboros.Consensus.Ledger.Abstract
                 ( UpdateLedger(..)
                 , ProtocolLedgerView(..)
                 )

type BlType = (Block (PermBft PermBftMockCrypto))


instance ( Show (LedgerState BlType)
         , Show (LedgerError BlType)
         ) => UpdateLedger BlType where
  data LedgerState BlType =
    MkLedgerState deriving Show

  data LedgerError BlType =
    UndefinedLedgerError deriving Show

  -- Apply a block to the ledger state
  applyLedgerState b MkLedgerState = undefined


instance ( OuroborosTag (BlockProtocol BlType)
         , UpdateLedger BlType
         , HasHeader BlType
         , SupportedBlock (BlockProtocol BlType) BlType
         ) => ProtocolLedgerView BlType where
  protocolLedgerView = undefined



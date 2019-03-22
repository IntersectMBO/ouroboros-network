{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module Ouroboros.Consensus.Ledger.PermBFT where

import           Ouroboros.Consensus.Ledger.Abstract
                 (
                   BlockProtocol
                 , ProtocolLedgerView(..)
                 , UpdateLedger(..)
                 )
import           Ouroboros.Consensus.Ledger.Mock
                 (
                   SimpleBlock
                 , SimpleBlockCrypto
                 )
import           Ouroboros.Consensus.Protocol.Abstract
                 (
                   OuroborosTag
                 , SupportedBlock
                 )
import           Ouroboros.Consensus.Protocol.Tags
                 (
                   PermBft
                 )
import           Ouroboros.Network.Block (HasHeader (..))

-- type instance BlockProtocol (SimpleBlock p c) = p


-- instance {-# OVERLAPPING #-} OuroborosTag PermBft => UpdateLedger (SimpleBlock PermBft c) where
--   data LedgerState (SimpleBlock PermBft c) =
--     MkLedgerState deriving Show

--   data LedgerError (SimpleBlock PermBft c) =
--     UndefinedLedgerError deriving Show

--   -- Apply a block to the ledger state
--   applyLedgerState b MkLedgerState = undefined


-- instance SimpleBlockCrypto c
--       => ProtocolLedgerView (SimpleBlock PermBft c) where
--   protocolLedgerView = undefined

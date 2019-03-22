-- | A module for protocol tags
module Ouroboros.Consensus.Protocol.Tags where

-- # LANGUAGE DataKinds    #
-- # LANGUAGE TypeFamilies #



-- | A tag for the permissive BFT protocol
data PermBft


{-------------------------------------------------------------------------------
  Choosing between a specific and a (generic) mock ledger instance
-------------------------------------------------------------------------------}

-- type family (LedgerSel p) :: Bool where
--   LedgerSel PermBft = 'False
--   LedgerSel p       = 'True


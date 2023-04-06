{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Storage.LedgerDB.LedgerDB (
    -- * LedgerDB
    LedgerDB
  , LedgerDB'
  , LedgerDbCfg (..)
  , configLedgerDb
  , mkWithAnchor
    -- * Deprecations
  , ledgerDbWithAnchor
  ) where

import           Data.SOP (K)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..),
                     ExtLedgerState)
import           Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol)
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog (DbChangelog,
                     empty)

{-------------------------------------------------------------------------------
  LedgerDB
-------------------------------------------------------------------------------}

type LedgerDB l = DbChangelog l
type LedgerDB' blk = LedgerDB (ExtLedgerState blk)

type instance HeaderHash (K @MapKind (LedgerDB' blk)) =
              HeaderHash (ExtLedgerState blk)

-- | Ledger DB starting at the specified ledger state
mkWithAnchor ::
     ( HasLedgerTables l
     , GetTip l
     )
  => l EmptyMK -> LedgerDB l
mkWithAnchor = empty

{-------------------------------------------------------------------------------
  LedgerDB Config
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

configLedgerDb ::
     ConsensusProtocol (BlockProtocol blk)
  => TopLevelConfig blk
  -> LedgerDbCfg (ExtLedgerState blk)
configLedgerDb cfg = LedgerDbCfg {
      ledgerDbCfgSecParam = configSecurityParam cfg
    , ledgerDbCfg         = ExtLedgerCfg cfg
    }

{-------------------------------------------------------------------------------
  Deprecations
-------------------------------------------------------------------------------}

{-# DEPRECATED ledgerDbWithAnchor "Use Ouroboros.Consensus.Storage.LedgerDB (mkWithAnchor)" #-}
ledgerDbWithAnchor ::
     ( HasLedgerTables l
     , GetTip l
     )
  => l EmptyMK -> LedgerDB l
ledgerDbWithAnchor = mkWithAnchor

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Ouroboros.Consensus.Storage.LedgerDB.LedgerDB (
    -- * LedgerDB
    LedgerDB (..)
  , LedgerDB'
  , LedgerDbCfg (..)
  , configLedgerDb
  , new
    -- * Deprecations
  , ledgerDbWithAnchor
  ) where

import           Data.SOP (K, unK)
import           GHC.Generics (Generic)

import           NoThunks.Class (NoThunks)

import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..),
                     ExtLedgerState)
import           Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol)

import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
                     (DbChangelog (changelogVolatileStates),
                     DbChangelogState (unDbChangelogState), empty)

{-------------------------------------------------------------------------------
  LedgerDB
-------------------------------------------------------------------------------}

-- | Newtype wrapper over a 'DbChangelog'. See the documentation there for more
-- information
newtype LedgerDB l = LedgerDB {
      ledgerDbChangelog :: DbChangelog l
    }
  deriving (Generic)

type LedgerDB' blk = LedgerDB (ExtLedgerState blk)

deriving instance Show     (DbChangelog l) => Show     (LedgerDB l)
deriving instance Eq       (DbChangelog l) => Eq       (LedgerDB l)
deriving instance NoThunks (DbChangelog l) => NoThunks (LedgerDB l)

type instance HeaderHash (K @MapKind (LedgerDB' blk)) =
              HeaderHash (ExtLedgerState blk)

instance IsLedger (ExtLedgerState blk) => GetTip (K (LedgerDB' blk)) where
  getTip = castPoint
         . getTip
         . either unDbChangelogState unDbChangelogState
         . AS.head
         . changelogVolatileStates
         . ledgerDbChangelog
         . unK

-- | Ledger DB starting at the specified ledger state
new ::
     ( HasLedgerTables l
     , GetTip l
     )
  => l EmptyMK -> LedgerDB l
new = LedgerDB . empty

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

{-# DEPRECATED ledgerDbWithAnchor "Use Ouroboros.Consensus.Storage.LedgerDB (new)" #-}
ledgerDbWithAnchor ::
     ( HasLedgerTables l
     , GetTip l
     )
  => l EmptyMK -> LedgerDB l
ledgerDbWithAnchor = new

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
                     DbChangelogState (unDbChangelogState))

{-------------------------------------------------------------------------------
  LedgerDB
-------------------------------------------------------------------------------}

-- | Internal state of the ledger DB
--
-- The ledger DB looks like
--
-- > anchor |> snapshots <| current
--
-- where @anchor@ records the oldest known snapshot and @current@ the most
-- recent. The anchor is the oldest point we can roll back to.
--
-- We take a snapshot after each block is applied and keep in memory a window
-- of the last @k@ snapshots. We have verified empirically (#1936) that the
-- overhead of keeping @k@ snapshots in memory is small, i.e., about 5%
-- compared to keeping a snapshot every 100 blocks. This is thanks to sharing
-- between consecutive snapshots.
--
-- As an example, suppose we have @k = 6@. The ledger DB grows as illustrated
-- below, where we indicate the anchor number of blocks, the stored snapshots,
-- and the current ledger.
--
-- > anchor |> #   [ snapshots ]                   <| tip
-- > ---------------------------------------------------------------------------
-- > G      |> (0) [ ]                             <| G
-- > G      |> (1) [ L1]                           <| L1
-- > G      |> (2) [ L1,  L2]                      <| L2
-- > G      |> (3) [ L1,  L2,  L3]                 <| L3
-- > G      |> (4) [ L1,  L2,  L3,  L4]            <| L4
-- > G      |> (5) [ L1,  L2,  L3,  L4,  L5]       <| L5
-- > G      |> (6) [ L1,  L2,  L3,  L4,  L5,  L6]  <| L6
-- > L1     |> (6) [ L2,  L3,  L4,  L5,  L6,  L7]  <| L7
-- > L2     |> (6) [ L3,  L4,  L5,  L6,  L7,  L8]  <| L8
-- > L3     |> (6) [ L4,  L5,  L6,  L7,  L8,  L9]  <| L9   (*)
-- > L4     |> (6) [ L5,  L6,  L7,  L8,  L9,  L10] <| L10
-- > L5     |> (6) [*L6,  L7,  L8,  L9,  L10, L11] <| L11
-- > L6     |> (6) [ L7,  L8,  L9,  L10, L11, L12] <| L12
-- > L7     |> (6) [ L8,  L9,  L10, L12, L12, L13] <| L13
-- > L8     |> (6) [ L9,  L10, L12, L12, L13, L14] <| L14
--
-- The ledger DB must guarantee that at all times we are able to roll back @k@
-- blocks. For example, if we are on line (*), and roll back 6 blocks, we get
--
-- > L3 |> []
newtype LedgerDB l = LedgerDB {
      ledgerDbChangelog :: DbChangelog l
    }
  deriving (Generic)

type LedgerDB' blk = LedgerDB (ExtLedgerState blk)

deriving instance Show     (DbChangelog l) => Show     (LedgerDB l)
deriving instance Eq       (DbChangelog l) => Eq       (LedgerDB l)
deriving instance NoThunks (DbChangelog l) => NoThunks (LedgerDB l)

type instance HeaderHash (K @MapKind (LedgerDB' blk)) = HeaderHash (ExtLedgerState blk)

instance IsLedger (ExtLedgerState blk) => GetTip (K (LedgerDB' blk)) where
  getTip = castPoint
         . getTip
         . either unDbChangelogState unDbChangelogState
         . AS.head
         . changelogVolatileStates
         . ledgerDbChangelog
         . unK

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

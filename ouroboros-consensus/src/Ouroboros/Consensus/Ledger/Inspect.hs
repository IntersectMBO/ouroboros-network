{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Ledger.Inspect (
    InspectLedger (..)
  , LedgerEvent (..)
  , castLedgerEvent
  , partitionLedgerEvents
  ) where

import           Data.Either
import           Data.Kind (Type)
import           Data.Void

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.RedundantConstraints

data LedgerEvent blk =
    LedgerWarning (LedgerWarning blk)
  | LedgerUpdate (LedgerUpdate blk)

deriving instance InspectLedger blk => Show (LedgerEvent blk)
deriving instance InspectLedger blk => Eq   (LedgerEvent blk)

castLedgerEvent ::
     ( LedgerWarning blk ~ LedgerWarning blk'
     , LedgerUpdate  blk ~ LedgerUpdate  blk'
     )
  => LedgerEvent blk -> LedgerEvent blk'
castLedgerEvent (LedgerWarning warning) = LedgerWarning warning
castLedgerEvent (LedgerUpdate  update)  = LedgerUpdate  update

ledgerEventToEither ::
     LedgerEvent blk
  -> Either (LedgerWarning blk) (LedgerUpdate blk)
ledgerEventToEither (LedgerWarning warning) = Left  warning
ledgerEventToEither (LedgerUpdate  update)  = Right update

partitionLedgerEvents ::
     [LedgerEvent blk]
  -> ([LedgerWarning blk], [LedgerUpdate blk])
partitionLedgerEvents = partitionEithers . map ledgerEventToEither

class ( Show     (LedgerWarning blk)
      , Show     (LedgerUpdate  blk)
      , Eq       (LedgerWarning blk)
      , Eq       (LedgerUpdate  blk)
      , Condense (LedgerUpdate  blk)
      ) => InspectLedger blk where
  type LedgerWarning blk :: Type
  type LedgerUpdate  blk :: Type

  -- | Inspect the ledger
  --
  -- The point of the inspection is to see if the state of the ledger might
  -- indicate a potential misconfiguration of the node.
  --
  -- TODO: We might at some point need to generalize this to 'ExtLedgerState'
  -- instead. That doesn't fit quite so neatly with the HFC at present, so
  -- leaving it at this for now.
  inspectLedger ::
       TopLevelConfig blk
    -> LedgerState    blk -- ^ Before
    -> LedgerState    blk -- ^ After
    -> [LedgerEvent   blk]

  -- Defaults
  -- The defaults just use no events at all

  type LedgerWarning blk = Void
  type LedgerUpdate  blk = Void

  default inspectLedger ::
       ( LedgerWarning blk ~ Void
       , LedgerUpdate  blk ~ Void
       )
    => TopLevelConfig blk
    -> LedgerState    blk -- ^ Before
    -> LedgerState    blk -- ^ After
    -> [LedgerEvent   blk]
  inspectLedger _ _ _ = []
    where
      _ = keepRedundantConstraint (Proxy @(LedgerWarning blk ~ Void))
      _ = keepRedundantConstraint (Proxy @(LedgerUpdate  blk ~ Void))

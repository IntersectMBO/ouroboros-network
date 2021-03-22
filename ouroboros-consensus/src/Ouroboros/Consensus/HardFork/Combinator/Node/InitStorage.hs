{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Node.InitStorage (

  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB (..))

instance CanHardFork xs => NodeInitStorage (HardForkBlock xs) where
  -- We use the chunk info from the first era
  nodeImmutableDbChunkInfo cfg =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          nodeImmutableDbChunkInfo
            (hd cfgs)
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

  -- Dispatch based on the era
  nodeCheckIntegrity cfg (HardForkBlock (OneEraBlock blk)) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          hcollapse $
            hczipWith (Proxy @SingleEraBlock) aux cfgs blk
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

      aux :: NodeInitStorage blk => StorageConfig blk -> I blk -> K Bool blk
      aux cfg' (I blk') = K $ nodeCheckIntegrity cfg' blk'

  -- Call the 'nodeInitChainDB' of the era in which the current ledger is.
  --
  -- In most cases, this will be the first era, except when one or more hard
  -- forks are statically scheduled at the first slot.
  nodeInitChainDB cfg (initChainDB :: InitChainDB m (HardForkBlock xs)) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} -> do
          currentLedger <- getCurrentLedger initChainDB
          hcollapse $
            hcizipWith
              proxySingle
              aux
              cfgs
              (State.tip (hardForkLedgerStatePerEra currentLedger))
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

      aux ::
           SingleEraBlock blk
        => Index xs blk
        -> StorageConfig blk
        -> LedgerState blk
        -> K (m ()) blk
      aux index cfg' currentLedger = K $
          nodeInitChainDB cfg' InitChainDB {
              addBlock         = addBlock initChainDB
                               . injectNS' (Proxy @I) index
            , getCurrentLedger = return currentLedger
            }

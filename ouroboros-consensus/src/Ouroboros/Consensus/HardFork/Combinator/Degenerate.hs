{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Degenerate (
  ) where

import           Control.Tracer
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.NoHardForks
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Forge ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query ()
import           Ouroboros.Consensus.HardFork.Combinator.Mempool ()
import           Ouroboros.Consensus.HardFork.Combinator.Node ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode
                     ()
import           Ouroboros.Consensus.HardFork.Combinator.Unary

instance ( SupportedNetworkProtocolVersion (HardForkBlock '[b])
         , SerialiseHFC '[b]
         , ConfigSupportsNode b
         , RunNode b
         , NoHardForks b
         ) => RunNode (HardForkBlock '[b]) where

  nodeImmDbChunkInfo cfg                 = nodeImmDbChunkInfo (project cfg)
  nodeCheckIntegrity cfg (DegenBlock  b) = nodeCheckIntegrity (project cfg) b
  nodeGetBinaryBlockInfo (DegenBlock  b) = nodeGetBinaryBlockInfo           b
  nodeBlockFetchSize     (DegenHeader h) = nodeBlockFetchSize               h

  nodeInitChainDB cfg = nodeInitChainDB (project cfg) . contramap DegenBlock

{-------------------------------------------------------------------------------
  Patterns
-------------------------------------------------------------------------------}

{-# COMPLETE DegenBlock  #-}
{-# COMPLETE DegenHeader #-}

pattern DegenBlock :: b -> HardForkBlock '[b]
pattern DegenBlock b = HardForkBlock (OneEraBlock (Z (I b)))

pattern DegenHeader :: Header b -> Header (HardForkBlock '[b])
pattern DegenHeader h = HardForkHeader (OneEraHeader (Z h))

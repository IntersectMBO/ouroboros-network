{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.BFT (
    tests
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Chain (Chain)

import           Test.Dynamic.General
import           Test.Dynamic.Util
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" $
        prop_simple_bft_convergence params
    ]
  where
    params = defaultSecurityParam

prop_simple_bft_convergence :: SecurityParam
                            -> NumCoreNodes
                            -> NumSlots
                            -> Seed
                            -> Property
prop_simple_bft_convergence params numCoreNodes =
    prop_simple_protocol_convergence
      (protocolInfo (DemoBFT params) numCoreNodes)
      isValid
      numCoreNodes
  where
    isValid :: Show time
            => [NodeId]
            -> [(time, Map NodeId (Chain (Block DemoBFT)))]
            -> Property
    isValid nodeIds trace = counterexample (show trace) $
      case trace of
        [(_, final)] -> collect (shortestLength final)
                     $    Map.keys final === nodeIds
                     .&&. allEqual (takeChainPrefix <$> Map.elems final)
        _otherwise   -> property False
      where
        takeChainPrefix :: Chain (Block DemoBFT) -> Chain (Block DemoBFT)
        takeChainPrefix = id -- in BFT, chains should indeed all be equal.

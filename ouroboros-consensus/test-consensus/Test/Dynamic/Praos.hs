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

module Test.Dynamic.Praos (
    tests
  , prop_all_common_prefix
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Node

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Chain (dropLastBlocks, lastSlot)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation"
    [ testProperty "simple Praos convergence - special case (issue #131)" $
        testPraos $ Seed ( 49644418094676
                         , 40315957626756
                         , 42668365444963
                         , 9796082466547
                         , 32684299622558
                         )
    , testProperty "simple Praos convergence - special crowded case" $
        testPraos $ Seed ( 8871923881324151440
                         , 881094692332313449
                         , 3091285302407489889
                         , 6410351877547894330
                         , 14676014321459888687
                         )
    , testProperty "simple Praos convergence" testPraos
    ]
  where
    testPraos :: Seed -> Property
    testPraos = prop_simple_praos_convergence
                  params
                  (NumCoreNodes 3)
                  (NumSlots (fromIntegral numSlots))

    params@PraosParams{..} = defaultDemoPraosParams
    numSlots  = maxRollbacks praosSecurityParam * praosSlotsPerEpoch * numEpochs
    numEpochs = 3

prop_simple_praos_convergence :: PraosParams
                              -> NumCoreNodes
                              -> NumSlots
                              -> Seed
                              -> Property
prop_simple_praos_convergence params numCoreNodes numSlots =
    prop_simple_protocol_convergence
      (protocolInfo (DemoPraos params) numCoreNodes)
      isValid
      numCoreNodes
      numSlots
  where
    PraosParams{..} = params

    isValid :: [NodeId]
            -> Map NodeId (Chain (Block DemoPraos))
            -> Property
    isValid nodeIds final = counterexample (show final) $
      let schedule = leaderScheduleFromTrace numSlots final
          longest  = longestCrowdedRun schedule
          crowded  = crowdedRunLength longest
      in    counterexample (tracesToDot final)
          $ counterexample (condense schedule)
          $ counterexample (show longest)
          $ label ("longest crowded run " <> show crowded)
          $ collect (shortestLength final)
          $ (Map.keys final === nodeIds)
            .&&. if crowded > maxRollbacks praosSecurityParam
                  then label "too crowded"     $ property True
                  else label "not too crowded" $
                          prop_all_common_prefix
                            (maxRollbacks praosSecurityParam)
                            (Map.elems final)

prop_all_common_prefix :: (HasHeader b, Condense b, Eq b)
                       => Word -> [Chain b] -> Property
prop_all_common_prefix _ []     = property True
prop_all_common_prefix l (c:cs) = conjoin [prop_common_prefix l c d | d <- cs]

prop_common_prefix :: forall b. (HasHeader b, Condense b, Eq b)
                   => Word -> Chain b -> Chain b -> Property
prop_common_prefix l x y = go x y .&&. go y x
  where
    go c d =
        let (l', c') = findPrefix c d
            e        = "after dropping "
                 <> show l'
                 <> " blocks from "
                 <> showChain c
                 <> ",\n\nthe resulting "
                 <> showChain c'
                 <> "\n\nis a prefix of "
                 <> showChain d
                 <> ",\n\nbut only "
                 <> show l
                 <> " block(s) should have been necessary"
        in  counterexample e $ l' <= l

    findPrefix c' d
        | c' `isPrefixOf` d = (0, c')
        | otherwise         = let (l', c'') = findPrefix (dropLastBlocks 1 c') d
                              in  (l' + 1, c'')

    showChain :: Chain b -> String
    showChain c = condense c
                  <> "\n(length "
                  <> show (Chain.length c)
                  <> case lastSlot c of
                        Nothing -> ")"
                        Just s  ->    ", last slot "
                                   <> show (getSlot s)
                                   <> ")"

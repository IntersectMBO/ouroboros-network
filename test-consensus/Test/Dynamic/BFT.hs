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

{-# OPTIONS -fno-warn-unused-binds #-}
{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.BFT (
    tests
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Chain (Chain)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" prop_simple_bft_convergence
    ]


type Protocol = Bft BftMockCrypto

instance HasPayload (Bft BftMockCrypto) (BlockUnderTest Protocol) where
  blockPayload _ = testPayloadP
                 . encPayloadP
                 . headerOuroboros
                 . simpleHeader

instance ProtocolLedgerView (BlockUnderTest Protocol) where
  protocolLedgerView (EncNodeConfig _ cfg) (SimpleLedgerState u) =
      ((), nodeStake cfg u)

prop_simple_bft_convergence :: Seed -> Property
prop_simple_bft_convergence =
    prop_simple_protocol_convergence mkConfig mkState initialChainState isValid
  where
    mkConfig :: Int -> NodeConfig Protocol
    mkConfig i = BftNodeConfig
        { bftNodeId   = CoreId i
        , bftSignKey  = SignKeyMockDSIGN i
        , bftNumNodes = fromIntegral numNodes
        , bftVerKeys  = verKeys
        }
      where
        verKeys :: Map NodeId (VerKeyDSIGN (BftDSIGN BftMockCrypto))
        verKeys = Map.fromList [ (CoreId j, VerKeyMockDSIGN j)
                               | j <- [0 .. numNodes - 1]
                               ]

    mkState :: Int -> NodeState Protocol
    mkState _ = ()

    initialChainState :: ChainState Protocol
    initialChainState = ()

    isValid :: [NodeId]
            -> [(VTime, Map NodeId (Chain (BlockUnderTest Protocol)))]
            -> Property
    isValid nodeIds trace = counterexample (show trace) $
      case trace of
        [(_, final)] -> collect (shortestLength final)
                     $    Map.keys final === nodeIds
                     .&&. allEqual (takeChainPrefix <$> Map.elems final)
        _otherwise   -> property False
      where
        takeChainPrefix :: Chain (BlockUnderTest Protocol) -> Chain (BlockUnderTest Protocol)
        takeChainPrefix = id -- in BFT, chains should indeed all be equal.

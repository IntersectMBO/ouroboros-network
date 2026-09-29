module Test.Cardano.Network.Diffusion.Configuration (tests) where

import Network.Mux qualified as Mx
import Network.Mux.Types (MiniProtocolDir (..))

import Cardano.Network.Diffusion.Configuration (defaultEgressScheduling)
import Cardano.Network.NodeToNode (blockFetchMiniProtocolNum,
           chainSyncMiniProtocolNum, keepAliveMiniProtocolNum,
           peerSharingMiniProtocolNum, txSubmissionMiniProtocolNum)
import Ouroboros.Network.Diffusion.Types (EgressScheduling (..))

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Diffusion.Configuration"
  [ testProperty "the default lane rule" (once prop_default_lanes)
  ]

-- | What we serve is ranked; what we ask for goes direct, tx-submission's
-- requests included, since there the responder is the side that asks; and
-- the bodies a peer pulls, the tx-submission initiator's replies, are the
-- slice's. The whole node-to-node table, so a change to it is a change
-- here.
prop_default_lanes :: Property
prop_default_lanes =
    conjoin [ counterexample (show (num, dir)) (laneOf num dir === lane)
            | (num, dir, lane) <- table ]
  where
    laneOf = esLaneOf defaultEgressScheduling
    table  = [ (chainSyncMiniProtocolNum,    ResponderDir, Mx.Scheduled)
             , (blockFetchMiniProtocolNum,   ResponderDir, Mx.Scheduled)
             , (keepAliveMiniProtocolNum,    ResponderDir, Mx.Scheduled)
             , (peerSharingMiniProtocolNum,  ResponderDir, Mx.Scheduled)
             , (txSubmissionMiniProtocolNum, ResponderDir, Mx.Direct)
             , (chainSyncMiniProtocolNum,    InitiatorDir, Mx.Direct)
             , (blockFetchMiniProtocolNum,   InitiatorDir, Mx.Direct)
             , (keepAliveMiniProtocolNum,    InitiatorDir, Mx.Direct)
             , (peerSharingMiniProtocolNum,  InitiatorDir, Mx.Direct)
             , (txSubmissionMiniProtocolNum, InitiatorDir, Mx.Slice)
             ]

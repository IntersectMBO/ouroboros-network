{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Testing form of BFT that stores additional information in the Ouroboros
-- payload, which we can then verify in unit tests.
module Ouroboros.Consensus.Protocol.Test (
    TestProtocol
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Codec.Serialise (Serialise)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Node (NodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

-- | Add additional information to the payload of an existing protocol
-- for verification purposes in the unit tests
data TestProtocol p

instance OuroborosTag p => OuroborosTag (TestProtocol p) where

  -- We need at least the node ID
  data NodeConfig (TestProtocol p) = TestNodeConfig {
      testNodeConfigP  :: NodeConfig p
    , testNodeConfigId :: NodeId
    }

  -- Payload is the standard payload plus additional fields we want to test for
  data Payload (TestProtocol p) ph = TestPayload {
        testPayloadP     :: Payload p ph
      , testPayloadStake :: Int
      }
    deriving (Generic)

  -- In this example, the test protocol records the (absolute) stake, so we
  -- should be to compute that from the ledger
  type LedgerView (TestProtocol p) = (LedgerView p, NodeId -> Int)

  -- Proof is the standard proof plus whatever additional info we need to
  -- create the additional fields in the payload
  type IsLeader (TestProtocol p) = (IsLeader p, Int)

  --
  -- The other types are unchanged
  --

  type NodeState       (TestProtocol p) = NodeState      p
  type ChainState      (TestProtocol p) = ChainState     p
  type ValidationErr   (TestProtocol p) = ValidationErr  p
  type SupportedBlock  (TestProtocol p) = SupportedBlock p

  mkPayload (TestNodeConfig cfg _) (proof, stake) ph = do
      standardPayload <- mkPayload cfg proof ph
      return TestPayload {
          testPayloadP     = standardPayload
        , testPayloadStake = stake
        }

  checkIsLeader (TestNodeConfig cfg nodeId) slot (l, stakeOf) cs = do
      mProof <- checkIsLeader cfg slot l cs
      case mProof of
        Nothing    -> return $ Nothing
        Just proof -> return $ Just (proof, stakeOf nodeId)

  preferCandidate       (TestNodeConfig cfg _) = preferCandidate       cfg
  compareCandidates     (TestNodeConfig cfg _) = compareCandidates     cfg
  applyChainState       (TestNodeConfig cfg _) = applyChainState       cfg . fst
  protocolSecurityParam (TestNodeConfig cfg _) = protocolSecurityParam cfg

deriving instance (OuroborosTag p, Show (Payload p ph)) => Show (Payload (TestProtocol p) ph)
deriving instance (OuroborosTag p, Eq   (Payload p ph)) => Eq   (Payload (TestProtocol p) ph)
deriving instance (OuroborosTag p, Ord  (Payload p ph)) => Ord  (Payload (TestProtocol p) ph)

instance Condense (Payload p ph)
      => Condense (Payload (TestProtocol p) ph) where
    condense (TestPayload pld stake) =
        condense pld <> ",stake=" <> condense stake

instance (OuroborosTag p, Serialise (Payload p ph))
      => Serialise (Payload (TestProtocol p) ph) where
  -- use generic instance

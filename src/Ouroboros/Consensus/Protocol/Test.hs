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
  , OuroborosNodeConfig(..)
  , OuroborosPayload(..)
  ) where

import           GHC.Generics (Generic)

import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Node (NodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util

-- | Add additional information to the payload of an existing protocol
-- for verification purposes in the unit tests
data TestProtocol p

instance OuroborosTag p => OuroborosTag (TestProtocol p) where

  -- We need at least the node ID
  data OuroborosNodeConfig (TestProtocol p) = TestNodeConfig {
      testNodeConfigP  :: OuroborosNodeConfig p
    , testNodeConfigId :: NodeId
    }

  -- In this example, the test protocol records the (absolute) stake, so we
  -- should be to compute that from the ledger
  type OuroborosLedgerView (TestProtocol p) = (
      OuroborosLedgerView p
    , NodeId -> Int
    )

  -- Payload is the standard payload plus additional fields we want to test for
  data OuroborosPayload (TestProtocol p) ph = TestPayload {
        testPayloadP     :: OuroborosPayload p ph
      , testPayloadStake :: Int
      }
    deriving (Generic)

  -- Proof is the standard proof plus whatever additional info we need to
  -- create the additional fields in the payload
  type ProofIsLeader (TestProtocol p) = (ProofIsLeader p, Int)

  --
  -- The other types are unchanged
  --

  -- Node state is unchanged
  type OuroborosNodeState       (TestProtocol p) = OuroborosNodeState       p
  type OuroborosChainState      (TestProtocol p) = OuroborosChainState      p
  type OuroborosValidationError (TestProtocol p) = OuroborosValidationError p
  type SupportedBlock           (TestProtocol p) = SupportedBlock           p

  mkOuroborosPayload (TestNodeConfig cfg _) (proof, stake) ph = do
      standardPayload <- mkOuroborosPayload cfg proof ph
      return TestPayload {
          testPayloadP     = standardPayload
        , testPayloadStake = stake
        }

  checkIsLeader TestNodeConfig{..} slot (l, stakeOf) cs = do
      mProof <- checkIsLeader testNodeConfigP slot l cs
      case mProof of
        Nothing    -> return $ Nothing
        Just proof -> return $ Just (proof, stakeOf testNodeConfigId)

  selectChain = selectChain . testNodeConfigP

  applyOuroborosChainState TestNodeConfig{..}
                           b
                           (l, _)
                           cs =
      applyOuroborosChainState
        testNodeConfigP
        b
        l
        cs

deriving instance (OuroborosTag p, Show ph) => Show (OuroborosPayload (TestProtocol p) ph)
deriving instance (OuroborosTag p, Eq   ph) => Eq   (OuroborosPayload (TestProtocol p) ph)

instance Condense (OuroborosPayload p ph)
     => Condense (OuroborosPayload (TestProtocol p) ph) where
    condense (TestPayload pld stake) =
        condense pld <> ",stake=" <> condense stake

instance (OuroborosTag p, Serialise ph)
      => Serialise (OuroborosPayload (TestProtocol p) ph) where
  -- use generic instance

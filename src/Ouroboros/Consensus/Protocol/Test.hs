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
    -- * Tags
    TestProtocol
    -- * Classes
  , TestProtocolLedgerView(..)
  , TestProtocolStateView(..)
    -- * Constructors
  , OuroborosState(..)
  , OuroborosLedgerState(..)
  ) where

import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Network.Node (NodeId)
import           Ouroboros.Network.Serialise

-- | Add additional information to the payload of an existing protocol
-- for verification purposes in the unit tests
data TestProtocol p

instance OuroborosTag p => OuroborosTag (TestProtocol p) where
  -- The state is unchanged
  newtype OuroborosState (TestProtocol p)       = TestState (OuroborosState p)
  newtype OuroborosLedgerState (TestProtocol p) = TestLedgerState (OuroborosLedgerState p)

  -- Payload is the standard payload plus additional fields we want to test for
  data OuroborosPayload (TestProtocol p) ph = TestPayload {
        testPayloadStd   :: OuroborosPayload p ph
      , testPayloadStake :: Int
      }
    deriving (Generic)

  -- Proof is the standard proof plus whatever additional info we need to
  -- create the additional fields in the payload
  data ProofIsLeader (TestProtocol p) = TestProof {
        testProofStd   :: ProofIsLeader p
      , testProofStake :: Int
      }

  mkOuroborosPayload TestProof{..} ph = do
      standardPayload <- liftState $ mkOuroborosPayload testProofStd ph
      return TestPayload {
          testPayloadStd   = standardPayload
        , testPayloadStake = testProofStake
        }

  applyOuroborosLedgerState (TestPayload std _) (TestLedgerState ls) =
      TestLedgerState (applyOuroborosLedgerState std ls)

deriving instance (OuroborosTag p) => Show (OuroborosLedgerState (TestProtocol p))

deriving instance Show (OuroborosPayload p ph)
               => Show (OuroborosPayload (TestProtocol p) ph)
deriving instance Eq (OuroborosPayload p ph)
               => Eq (OuroborosPayload (TestProtocol p) ph)

instance Serialise (OuroborosPayload p ph)
      => Serialise (OuroborosPayload (TestProtocol p) ph) where
  -- use generic instance

instance ( RunOuroboros p l
         , TestProtocolLedgerView l
         , TestProtocolStateView p
         ) => RunOuroboros (TestProtocol p) l where
  checkIsLeader slot l = do
      mStandardProof <- liftState $ checkIsLeader slot l
      case mStandardProof of
        Nothing -> return Nothing
        Just standardProof -> do
          TestState st <- getOuroborosState
          -- derive any additional info from the ledger here (eg., stake)
          return $ Just TestProof {
              testProofStd   = standardProof
            , testProofStake = stakeForNode (getOurNodeId st) l
            }

-- | Additional constraints we need for the debugging fields
class TestProtocolLedgerView l where
  stakeForNode :: NodeId -> l -> Int

-- | Constraints on the state of the underlying protocol
class TestProtocolStateView p where
  getOurNodeId :: OuroborosState p -> NodeId

liftState :: MonadOuroborosState (TestProtocol p) m
          => OuroborosStateT p m a
          -> m a
liftState k = do
    TestState st <- getOuroborosState
    (a, st') <- runOuroborosStateT k st
    putOuroborosState (TestState st')
    return a

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.BFT (
    Bft
    -- * Classes
  , BftCrypto(..)
  , BftStandardCrypto
  , BftMockCrypto
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Control.Monad.Except
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Basic BFT
--
-- Basic BFT is very simple:
--
-- * No support for delegation (and hence has no need for a ledger view)
-- * Requires round-robin block signing throughout (and so has no
--   need for any chain state or cryptographic leader proofs).
-- * Does not use any stateful crypto (and so has no need for node state)
data Bft c

instance BftCrypto c => OuroborosTag (Bft c) where
  -- | The BFT payload is just the signature
  newtype Payload (Bft c) ph = BftPayload {
        bftSignature :: SignedDSIGN (BftDSIGN c) ph
      }
    deriving (Generic)

  -- | (Static) node configuration
  data NodeConfig (Bft c) = BftNodeConfig {
        bftNodeId   :: NodeId
      , bftSignKey  :: SignKeyDSIGN (BftDSIGN c)
      , bftNumNodes :: Word
      , bftVerKeys  :: Map NodeId (VerKeyDSIGN (BftDSIGN c))
      }

  type ValidationErr  (Bft c) = BftValidationErr
  type SupportedBlock (Bft c) = HasPayload (Bft c)
  type NodeState      (Bft c) = ()
  type LedgerView     (Bft c) = ()
  type IsLeader       (Bft c) = ()
  type ChainState     (Bft c) = ()

  mkPayload BftNodeConfig{..} _proof preheader = do
      signature <- signedDSIGN preheader bftSignKey
      return $ BftPayload {
          bftSignature = signature
        }

  checkIsLeader BftNodeConfig{..} (Slot n) _l _cs = do
      return $ case bftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` bftNumNodes == fromIntegral i
                                then Just ()
                                else Nothing

  applyChainState BftNodeConfig{..} _l b _cs = do
      -- TODO: Should deal with unknown node IDs
      if verifySignedDSIGN (bftVerKeys Map.! expectedLeader)
                      (blockPreHeader b)
                      (bftSignature (blockPayload (Proxy @(Bft c)) b))
        then return ()
        else throwError BftInvalidSignature
    where
      Slot n         = blockSlot b
      expectedLeader = CoreId $ fromIntegral (n `mod` bftNumNodes)

deriving instance BftCrypto c => Show     (Payload (Bft c) ph)
deriving instance BftCrypto c => Eq       (Payload (Bft c) ph)
deriving instance BftCrypto c => Condense (Payload (Bft c) ph)

instance BftCrypto c => Serialise (Payload (Bft c) ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data BftValidationErr = BftInvalidSignature
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class DSIGNAlgorithm (BftDSIGN c) => BftCrypto c where
  type family BftDSIGN c :: *

data BftStandardCrypto
data BftMockCrypto

instance BftCrypto BftStandardCrypto where
  type BftDSIGN BftStandardCrypto = Ed448DSIGN

instance BftCrypto BftMockCrypto where
  type BftDSIGN BftMockCrypto = MockDSIGN

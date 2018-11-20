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

module Ouroboros.Consensus.Protocol.BFT (
    Bft
    -- * Classes
  , BftCrypto(..)
  , BftStandardCrypto
  , BftMockCrypto
    -- * Type instances
  , OuroborosNodeConfig(..)
  , OuroborosPayload(..)
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

data Bft c

instance BftCrypto c => OuroborosTag (Bft c) where
  -- | The BFT payload is just the signature
  data OuroborosPayload (Bft c) ph = BftPayload {
        bftSignature :: Signed (BftDSIGN c) ph
      }
    deriving (Generic)

  -- | (Static) node configuration
  data OuroborosNodeConfig (Bft c) = BftNodeConfig {
        bftNodeId   :: NodeId
      , bftSignKey  :: SignKeyDSIGN (BftDSIGN c)
      , bftNumNodes :: Word
      , bftVerKeys  :: Map NodeId (VerKeyDSIGN (BftDSIGN c))
      }

  -- | BFT does not need any state
  type OuroborosNodeState (Bft c) = ()

  -- | Ledger view
  --
  -- Basic BFT does not need any info from the ledger.
  -- Cardano BFT will need to know about delegation.
  type OuroborosLedgerView (Bft c) = ()

  -- | For BFT the proof that we are a leader is trivial
  type ProofIsLeader (Bft c) = ()

  type OuroborosValidationError (Bft c) = BftValidationError

  -- | Chain state
  --
  -- Basic BFT does not need to record any chain state.
  -- Cardano BFT will need to record % blocks signed/key, to detect abnomalies.
  type OuroborosChainState (Bft c) = ()

  type SupportedBlock (Bft c) = HasOuroborosPayload (Bft c)

  mkOuroborosPayload BftNodeConfig{..} _proof preheader = do
      signature <- signed preheader bftSignKey
      return $ BftPayload {
          bftSignature = signature
        }

  checkIsLeader BftNodeConfig{..} (Slot n) _l _cs = do
      return $ case bftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` bftNumNodes == fromIntegral i
                                then Just ()
                                else Nothing

  applyOuroborosChainState BftNodeConfig{..} b _l _cs = do
      -- TODO: Should deal with unknown node IDs
      if verifySigned @(BftDSIGN c)
                      (bftVerKeys Map.! expectedLeader)
                      (blockPreHeader b)
                      (bftSignature (blockOuroborosPayload (Proxy @(Bft c)) b))
        then return ()
        else throwError BftInvalidSignature
    where
      Slot n         = blockSlot b
      expectedLeader = CoreId $ fromIntegral (n `mod` bftNumNodes)

deriving instance BftCrypto c => Show (OuroborosPayload (Bft c) ph)
deriving instance BftCrypto c => Eq   (OuroborosPayload (Bft c) ph)

instance BftCrypto c => Condense (OuroborosPayload (Bft c) ph) where
    condense (BftPayload sig) = condense sig

instance BftCrypto c => Serialise (OuroborosPayload (Bft c) ph) where
  -- use generic instance

data BftValidationError = BftInvalidSignature
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class DSIGNAlgorithm (BftDSIGN c) => BftCrypto (c :: *) where
  type family BftDSIGN c :: *

data BftStandardCrypto
data BftMockCrypto

instance BftCrypto BftStandardCrypto where
  type BftDSIGN BftStandardCrypto = Ed448DSIGN

instance BftCrypto BftMockCrypto where
  type BftDSIGN BftMockCrypto = MockDSIGN

{-------------------------------------------------------------------------------
  TestProtocol support
-------------------------------------------------------------------------------}

{-
instance TestProtocolStateView (Bft c) where
  getOurNodeId = bftNodeId
-}

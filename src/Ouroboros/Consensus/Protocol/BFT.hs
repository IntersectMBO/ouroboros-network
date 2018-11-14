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
    -- * Tags
    Bft
  , BftStandardCrypto
  , BftMockCrypto
    -- * Classes
  , BftLedgerView(..)
  , BftCrypto(..)
    -- * BFT specific types
  , OuroborosState(..)
  , OuroborosLedgerState(..)
  ) where

import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util
import           Ouroboros.Network.Block (Slot (..))
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Serialise

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

  -- | We need our own node ID and our key
  data OuroborosState (Bft c) = BftState {
        bftNodeId  :: NodeId
      , bftSignKey :: SignKeyDSIGN (BftDSIGN c)
      }

  -- | For BFT the proof that we are a leader is trivial
  data ProofIsLeader (Bft c) = BftProof

  -- | For BFT the ledger state is trivial.
  -- NOTE: For \"permissive BFT\" this would not be trivial, because validation
  -- would need to know statistical properties about the whole chain.
  data OuroborosLedgerState (Bft c) = BftLedgerState deriving Show

  mkOuroborosPayload BftProof preheader = do
      BftState{..} <- getOuroborosState
      signature <- signed preheader bftSignKey
      return $ BftPayload {
          bftSignature = signature
        }

  applyOuroborosLedgerState _ _ = BftLedgerState

deriving instance BftCrypto c => Show (OuroborosPayload (Bft c) ph)
deriving instance BftCrypto c => Eq   (OuroborosPayload (Bft c) ph)

instance BftCrypto c => Condense (OuroborosPayload (Bft c) ph) where
    condense (BftPayload sig) = condense sig

instance BftCrypto c => Serialise (OuroborosPayload (Bft c) ph) where
  -- use generic instance

instance (BftCrypto c, BftLedgerView l) => RunOuroboros (Bft c) l where
  checkIsLeader (Slot n) l = do
      BftState{..} <- getOuroborosState
      return $ case bftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` numNodes == fromIntegral i
                                then Just BftProof
                                else Nothing
    where
      numNodes = bftNumNodes l

{-------------------------------------------------------------------------------
  Ledger view
-------------------------------------------------------------------------------}

-- | Ledger state that BFT needs
class BftLedgerView l where
  bftNumNodes :: l -> Word

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

instance TestProtocolStateView (Bft c) where
  getOurNodeId = bftNodeId

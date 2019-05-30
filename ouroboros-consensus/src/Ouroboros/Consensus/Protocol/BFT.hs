{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.BFT (
    Bft
  , BftParams(..)
    -- * Classes
  , BftCrypto(..)
  , BftStandardCrypto
  , BftMockCrypto
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except
import           Data.Functor.Identity
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util (Empty)
import           Ouroboros.Consensus.Util.Condense

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

-- | Protocol parameters
data BftParams = BftParams {
      -- | Security parameter
      --
      -- Although the protocol proper does not have such a security parameter,
      -- we insist on it.
      bftSecurityParam :: SecurityParam

      -- | Number of core nodes
    , bftNumNodes      :: Word64
    }

instance (BftCrypto c) => OuroborosTag (Bft c) where
  -- | The BFT payload is just the signature
  newtype Payload (Bft c) ph = BftPayload {
        bftSignature :: SignedDSIGN (BftDSIGN c) ph
      }
    deriving (Generic)

  -- | (Static) node configuration
  data NodeConfig (Bft c) = BftNodeConfig {
        bftParams   :: BftParams
      , bftNodeId   :: NodeId
      , bftSignKey  :: SignKeyDSIGN (BftDSIGN c)
      , bftVerKeys  :: Map NodeId (VerKeyDSIGN (BftDSIGN c))
      }

  type ValidationErr  (Bft c) = BftValidationErr
  type SupportedBlock (Bft c) = HasPayload (Bft c)
  type NodeState      (Bft c) = ()
  type LedgerView     (Bft c) = ()
  type IsLeader       (Bft c) = ()
  type ChainState     (Bft c) = ()

  protocolSecurityParam = bftSecurityParam . bftParams

  mkPayload proxy BftNodeConfig{..} _proof preheader = do
      signature <- signedDSIGN (encodePreHeader proxy) preheader bftSignKey
      return $ BftPayload {
          bftSignature = signature
        }

  checkIsLeader BftNodeConfig{..} (SlotNo n) _l _cs = do
      return $ case bftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` bftNumNodes == fromIntegral i
                                then Just ()
                                else Nothing
    where
      BftParams{..}  = bftParams

  applyChainState cfg@BftNodeConfig{..} _l b _cs = do
      -- TODO: Should deal with unknown node IDs
      let proxy = Identity b
      case verifySignedDSIGN
           (encodePreHeader proxy)
           (bftVerKeys Map.! expectedLeader)
           (blockPreHeader b)
           (bftSignature (blockPayload cfg b)) of
        Right () -> return ()
        Left err -> throwError $ BftInvalidSignature err
    where
      BftParams{..}  = bftParams
      SlotNo n       = blockSlot b
      expectedLeader = CoreId $ fromIntegral (n `mod` bftNumNodes)

  rewindChainState _ _ _ = Just ()

deriving instance BftCrypto c => Show     (Payload (Bft c) ph)
deriving instance BftCrypto c => Eq       (Payload (Bft c) ph)
deriving instance BftCrypto c => Ord      (Payload (Bft c) ph)
deriving instance BftCrypto c => Condense (Payload (Bft c) ph)

instance (DSIGNAlgorithm (BftDSIGN c)) => Serialise (Payload (Bft c) ph) where
  encode (BftPayload sig) = encodeSignedDSIGN sig
  decode = BftPayload <$> decodeSignedDSIGN

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data BftValidationErr = BftInvalidSignature String
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}


-- The equality constraint here is slightly weird; we need it to force GHC to
-- partially apply this constraint in `OuroborosTag` and thus conclude that it
-- can satisfy it universally.
-- | Crypto primitives required by BFT
class (Typeable c, DSIGNAlgorithm (BftDSIGN c), Signable (BftDSIGN c) ~ Empty) => BftCrypto c where
  type family BftDSIGN c :: *

data BftStandardCrypto
data BftMockCrypto

instance BftCrypto BftStandardCrypto where
  type BftDSIGN BftStandardCrypto = Ed448DSIGN

instance BftCrypto BftMockCrypto where
  type BftDSIGN BftMockCrypto = MockDSIGN

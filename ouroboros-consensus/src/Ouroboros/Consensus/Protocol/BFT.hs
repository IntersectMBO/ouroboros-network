{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Ouroboros.Consensus.Protocol.BFT (
    Bft
  , BftFields(..)
  , BftParams(..)
  , BftValidationErr(..)
  , forgeBftFields
    -- * Classes
  , BftCrypto(..)
  , BftStandardCrypto
  , BftMockCrypto
  , BftValidateView(..)
  , bftValidateView
    -- * Type instances
  , ConsensusConfig(..)
  ) where

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Fields BFT requires in a block
-------------------------------------------------------------------------------}

data BftFields c toSign = BftFields {
      bftSignature :: !(SignedDSIGN (BftDSIGN c) toSign)
    }
  deriving (Generic)

deriving instance BftCrypto c => Show (BftFields c toSign)
deriving instance BftCrypto c => Eq   (BftFields c toSign)

-- We use the generic implementation, but override 'showTypeOf' to show @c@
instance (BftCrypto c, Typeable toSign) => NoUnexpectedThunks (BftFields c toSign) where
  showTypeOf _ = show $ typeRep (Proxy @(BftFields c))

data BftValidateView c =
    forall signed. Signable (BftDSIGN c) signed
                => BftValidateView SlotNo (BftFields c signed) signed

-- | Convenience constructor for 'BftValidateView'
bftValidateView :: ( HasHeader    hdr
                   , SignedHeader hdr
                   , Signable (BftDSIGN c) (Signed hdr)
                   )
                => (hdr -> BftFields c (Signed hdr))
                -> (hdr -> BftValidateView c)
bftValidateView getFields hdr =
    BftValidateView (blockSlot hdr) (getFields hdr) (headerSigned hdr)

forgeBftFields :: ( MonadRandom m
                  , BftCrypto c
                  , Signable (BftDSIGN c) toSign
                  )
               => ConsensusConfig (Bft c)
               -> toSign
               -> m (BftFields c toSign)
forgeBftFields BftConfig{..} toSign = do
      signature <- signedDSIGN () toSign bftSignKey
      return $ BftFields {
          bftSignature = signature
        }

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
      bftSecurityParam :: !SecurityParam

      -- | Number of core nodes
    , bftNumNodes      :: !NumCoreNodes
    }
  deriving (Generic, NoUnexpectedThunks)

-- | (Static) node configuration
data instance ConsensusConfig (Bft c) = BftConfig {
      bftParams  :: !BftParams
    , bftNodeId  :: !NodeId
    , bftSignKey :: !(SignKeyDSIGN (BftDSIGN c))
    , bftVerKeys :: !(Map NodeId (VerKeyDSIGN (BftDSIGN c)))
    }
  deriving (Generic)

instance BftCrypto c => ConsensusProtocol (Bft c) where
  type ValidationErr  (Bft c) = BftValidationErr
  type ValidateView   (Bft c) = BftValidateView c
  type LedgerView     (Bft c) = ()
  type IsLeader       (Bft c) = ()
  type ConsensusState (Bft c) = ()

  protocolSecurityParam = bftSecurityParam . bftParams

  checkIfCanBeLeader BftConfig{bftNodeId} =
      case bftNodeId of
        CoreId{}  -> True
        RelayId{} -> False  -- Relays are never leaders

  checkIsLeader BftConfig{..} (SlotNo n) _l _cs = do
      return $ case bftNodeId of
                 RelayId _ -> Nothing
                 CoreId (CoreNodeId i) ->
                   if n `mod` numCoreNodes == i
                     then Just ()
                     else Nothing
    where
      BftParams{..}  = bftParams
      NumCoreNodes numCoreNodes = bftNumNodes

  updateConsensusState BftConfig{..}
                       _l
                       (BftValidateView (SlotNo n) BftFields{..} signed)
                       _cs =
      -- TODO: Should deal with unknown node IDs
      case verifySignedDSIGN
             ()
             (bftVerKeys Map.! expectedLeader)
             signed
             bftSignature of
        Right () -> return ()
        Left err -> throwError $ BftInvalidSignature err
    where
      BftParams{..}  = bftParams
      expectedLeader = CoreId $ CoreNodeId (n `mod` numCoreNodes)
      NumCoreNodes numCoreNodes = bftNumNodes

  rewindConsensusState _ _ _ = Just ()

instance BftCrypto c => NoUnexpectedThunks (ConsensusConfig (Bft c))
  -- use generic instance

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data BftValidationErr = BftInvalidSignature String
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class ( Typeable c
      , DSIGNAlgorithm (BftDSIGN c)
      , Condense (SigDSIGN (BftDSIGN c))
      , NoUnexpectedThunks (SigDSIGN (BftDSIGN c))
      , ContextDSIGN (BftDSIGN c) ~ ()
      ) => BftCrypto c where
  type family BftDSIGN c :: *

data BftStandardCrypto
data BftMockCrypto

instance BftCrypto BftStandardCrypto where
  type BftDSIGN BftStandardCrypto = Ed448DSIGN

instance BftCrypto BftMockCrypto where
  type BftDSIGN BftMockCrypto = MockDSIGN

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance BftCrypto c => Condense (BftFields c toSign) where
  condense BftFields{..} = condense bftSignature

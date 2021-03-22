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
  , BftFields (..)
  , BftParams (..)
  , BftValidationErr (..)
  , forgeBftFields
    -- * Classes
  , BftCrypto (..)
  , BftMockCrypto
  , BftStandardCrypto
  , BftValidateView (..)
  , bftValidateView
    -- * Type instances
  , ConsensusConfig (..)
  ) where

import           Control.Monad.Except
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Ticked
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
instance (BftCrypto c, Typeable toSign) => NoThunks (BftFields c toSign) where
  showTypeOf _ = show $ typeRep (Proxy @(BftFields c))

data BftValidateView c =
    forall signed. Signable (BftDSIGN c) signed
                => BftValidateView (BftFields c signed) signed

-- | Convenience constructor for 'BftValidateView'
bftValidateView :: ( SignedHeader hdr
                   , Signable (BftDSIGN c) (Signed hdr)
                   )
                => (hdr -> BftFields c (Signed hdr))
                -> (hdr -> BftValidateView c)
bftValidateView getFields hdr =
    BftValidateView (getFields hdr) (headerSigned hdr)

forgeBftFields :: ( BftCrypto c
                  , Signable (BftDSIGN c) toSign
                  )
               => ConsensusConfig (Bft c)
               -> toSign
               -> BftFields c toSign
forgeBftFields BftConfig{..} toSign = let
      signature = signedDSIGN () toSign bftSignKey
      in BftFields {
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
  deriving (Generic, NoThunks)

-- | (Static) node configuration
data instance ConsensusConfig (Bft c) = BftConfig {
      bftParams  :: !BftParams
    , bftSignKey :: !(SignKeyDSIGN (BftDSIGN c))
    , bftVerKeys :: !(Map NodeId (VerKeyDSIGN (BftDSIGN c)))
    }
  deriving (Generic)

instance BftCrypto c => ConsensusProtocol (Bft c) where
  type ValidationErr (Bft c) = BftValidationErr
  type ValidateView  (Bft c) = BftValidateView c
  type LedgerView    (Bft c) = ()
  type IsLeader      (Bft c) = ()
  type ChainDepState (Bft c) = ()
  type CanBeLeader   (Bft c) = CoreNodeId

  protocolSecurityParam = bftSecurityParam . bftParams

  checkIsLeader BftConfig{..} (CoreNodeId i) (SlotNo n) _ =
      if n `mod` numCoreNodes == i
      then Just ()
      else Nothing
    where
      BftParams{..} = bftParams
      NumCoreNodes numCoreNodes = bftNumNodes

  updateChainDepState BftConfig{..}
                      (BftValidateView BftFields{..} signed)
                      (SlotNo n)
                      _ =
      -- TODO: Should deal with unknown node IDs
      case verifySignedDSIGN
             ()
             (bftVerKeys Map.! expectedLeader)
             signed
             bftSignature of
        Right () -> return ()
        Left err -> throwError $ BftInvalidSignature err
    where
      BftParams{..} = bftParams
      expectedLeader = CoreId $ CoreNodeId (n `mod` numCoreNodes)
      NumCoreNodes numCoreNodes = bftNumNodes

  reupdateChainDepState _ _ _ _ = ()
  tickChainDepState     _ _ _ _ = TickedTrivial

instance BftCrypto c => NoThunks (ConsensusConfig (Bft c))
  -- use generic instance

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data BftValidationErr = BftInvalidSignature String
  deriving (Show, Eq, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class ( Typeable c
      , DSIGNAlgorithm (BftDSIGN c)
      , Condense (SigDSIGN (BftDSIGN c))
      , NoThunks (SigDSIGN (BftDSIGN c))
      , ContextDSIGN (BftDSIGN c) ~ ()
      ) => BftCrypto c where
  type family BftDSIGN c :: Type

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

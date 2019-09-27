{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Protocol.BFT (
    Bft
  , BftFields(..)
  , BftParams(..)
  , forgeBftFields
    -- * Classes
  , BftCrypto(..)
  , BftStandardCrypto
  , BftMockCrypto
  , HeaderSupportsBft(..)
    -- * Type instances
  , NodeConfig(..)
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
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.NodeId (NodeId (..))
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

class ( HasHeader hdr
      , SignedHeader hdr
      , Signable (BftDSIGN c) (Signed hdr)
      ) => HeaderSupportsBft c hdr where
  headerBftFields :: NodeConfig (Bft c) -> hdr -> BftFields c (Signed hdr)

forgeBftFields :: ( MonadRandom m
                  , BftCrypto c
                  , Signable (BftDSIGN c) toSign
                  )
               => NodeConfig (Bft c)
               -> toSign
               -> m (BftFields c toSign)
forgeBftFields BftNodeConfig{..} toSign = do
      signature <- signedDSIGN toSign bftSignKey
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
      bftSecurityParam :: SecurityParam

      -- | Number of core nodes
    , bftNumNodes      :: Word64
    }

instance BftCrypto c => OuroborosTag (Bft c) where
  -- | (Static) node configuration
  data NodeConfig (Bft c) = BftNodeConfig {
        bftParams   :: BftParams
      , bftNodeId   :: NodeId
      , bftSignKey  :: SignKeyDSIGN (BftDSIGN c)
      , bftVerKeys  :: Map NodeId (VerKeyDSIGN (BftDSIGN c))
      }

  type ValidationErr   (Bft c) = BftValidationErr
  type SupportedHeader (Bft c) = HeaderSupportsBft c
  type NodeState       (Bft c) = ()
  type LedgerView      (Bft c) = ()
  type IsLeader        (Bft c) = ()
  type ChainState      (Bft c) = ()

  protocolSecurityParam = bftSecurityParam . bftParams

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
      case verifySignedDSIGN
           (bftVerKeys Map.! expectedLeader)
           (headerSigned b)
           bftSignature of
        Right () -> return ()
        Left err -> throwError $ BftInvalidSignature err
    where
      BftParams{..}  = bftParams
      BftFields{..}  = headerBftFields cfg b
      SlotNo n       = blockSlot b
      expectedLeader = CoreId $ fromIntegral (n `mod` bftNumNodes)

  rewindChainState _ _ _ = Just ()

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data BftValidationErr = BftInvalidSignature String
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class (Typeable c, DSIGNAlgorithm (BftDSIGN c), Condense (SigDSIGN (BftDSIGN c))) => BftCrypto c where
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

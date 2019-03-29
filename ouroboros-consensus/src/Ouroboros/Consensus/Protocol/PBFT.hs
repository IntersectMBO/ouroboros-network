{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.PBFT (
    PBft
  , PBftParams(..)
    -- * Classes
  , PBftCrypto(..)
  , PBftStandardCrypto
  , PBftMockCrypto
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Cardano.Chain.Update (ProtocolParameters)
import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Tuple (swap)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util.Condense

-- | Invert a map which we assert to be a bijection.
--   If this map is not a bijection, the behaviour is not guaranteed.
--
--   Examples:
--
--   >>> invertBijection (Map.fromList [('a', 1 :: Int), ('b', 2), ('c', 3)])
--   fromList [(1,'a'),(2,'b'),(3,'c')]
invertBijection
  :: Ord v
  => Map k v
  -> Map v k
invertBijection
  = Map.fromListWith const
  . fmap swap
  . Map.toList


{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Permissive BFT
--
-- As defined in https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
data PBft c

-- | Protocol parameters
data PBftParams = PBftParams {
      -- | Security parameter
      --
      -- Although the protocol proper does not have such a security parameter,
      -- we insist on it.
      pbftSecurityParam :: SecurityParam

      -- | Number of core nodes
    , pbftNumNodes      :: Word64

      -- | Size of the window over which to check the proportion of signed keys.
    , pbftSignatureWindow :: Word64

      -- | Signature threshold. This represents the proportion of blocks in a
      -- pbftSignatureWindow-sized window which may be signed by any single key.
    , pbftSignatureThreshold :: Double
    }

instance PBftCrypto c => OuroborosTag (PBft c) where
  -- | The BFT payload is just the issuer and signature
  data Payload (PBft c) ph = PBftPayload {
        pbftIssuer    :: VerKeyDSIGN (PBftDSIGN c)
      , pbftSignature :: SignedDSIGN (PBftDSIGN c) ph
      }
    deriving (Generic)

  -- | (Static) node configuration
  data NodeConfig (PBft c) = PBftNodeConfig {
        pbftParams   :: PBftParams
      , pbftNodeId   :: NodeId
      , pbftSignKey  :: SignKeyDSIGN (PBftDSIGN c)
      , pbftVerKey   :: VerKeyDSIGN (PBftDSIGN c)
      }

  type ValidationErr  (PBft c) = PBftValidationErr
  type SupportedBlock (PBft c) = HasPayload (PBft c)
  type NodeState      (PBft c) = ()

  -- | We require two things from the ledger state:
  --
  --   - Protocol parameters, for the signature window and threshold.
  --   - The delegation map.
  type LedgerView     (PBft c) =
    ( ProtocolParameters
      -- Map from genesis to delegate keys. Note that this map is injective by
      -- construction.
    , Map (VerKeyDSIGN (PBftDSIGN c)) (VerKeyDSIGN (PBftDSIGN c))
    )
  type IsLeader       (PBft c) = ()
  -- | Chain state consists of two things:
  --   - a list of the last 'pbftSignatureWindow' signatures.
  --   - The last seen block slot
  type ChainState     (PBft c) =
    ( Seq (VerKeyDSIGN (PBftDSIGN c))
      -- Last seen block slot.
    , SlotNo
    )

  protocolSecurityParam = pbftSecurityParam . pbftParams

  mkPayload PBftNodeConfig{..} _proof preheader = do
      signature <- signedDSIGN preheader pbftSignKey
      return $ PBftPayload {
          pbftIssuer = pbftVerKey
        , pbftSignature = signature
        }

  checkIsLeader PBftNodeConfig{..} (SlotNo n) _l _cs = do
      return $ case pbftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` pbftNumNodes == fromIntegral i
                                then Just ()
                                else Nothing
    where
      PBftParams{..}  = pbftParams

  applyChainState PBftNodeConfig{..} (_pp, dms) b (signers, lastSlot) = do
      -- Check that the issuer signature verifies, and that it's a delegate of a
      -- genesis key, and that genesis key hasn't voted too many times.

      unless (verifySignedDSIGN (pbftIssuer payload)
                      (blockPreHeader b)
                      (pbftSignature payload))
        $ throwError PBftInvalidSignature

      unless (blockSlot b > lastSlot)
        $ throwError PBftInvalidSlot

      case Map.lookup (pbftIssuer payload) $ invertBijection dms of
        Nothing -> throwError PBftNotGenesisDelegate
        Just gk -> do
          when (Seq.length (Seq.filter (== gk) signers) >= wt)
            $ throwError PBftExceededSignThreshold
          let signers' = Seq.drop (Seq.length signers - fromIntegral pbftSignatureWindow - 1) signers Seq.|> gk
          return (signers', blockSlot b)
    where
      PBftParams{..}  = pbftParams
      payload = blockPayload (Proxy @(PBft c)) b
      wt = floor $ pbftSignatureThreshold * fromIntegral pbftSignatureWindow


deriving instance PBftCrypto c => Show     (Payload (PBft c) ph)
deriving instance PBftCrypto c => Eq       (Payload (PBft c) ph)
deriving instance PBftCrypto c => Ord      (Payload (PBft c) ph)
instance PBftCrypto c => Condense (Payload (PBft c) ph) where
    condense (PBftPayload _ sig) = condense sig

instance PBftCrypto c => Serialise (Payload (PBft c) ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data PBftValidationErr
  = PBftInvalidSignature
  | PBftNotGenesisDelegate
  | PBftExceededSignThreshold
  | PBftInvalidSlot
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class DSIGNAlgorithm (PBftDSIGN c) => PBftCrypto c where
  type family PBftDSIGN c :: *

data PBftStandardCrypto
data PBftMockCrypto

instance PBftCrypto PBftStandardCrypto where
  type PBftDSIGN PBftStandardCrypto = Ed448DSIGN

instance PBftCrypto PBftMockCrypto where
  type PBftDSIGN PBftMockCrypto = MockDSIGN

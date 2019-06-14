{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Ouroboros.Consensus.Update (
    USSArgs(..)
  , MProposalBody(..)
  ) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BSS
import           Data.Map.Strict (Map)
import           Data.Time
import           GHC.Generics (Generic)

import           Codec.Serialise (Serialise (..))
import           Crypto.Hash

import qualified Cardano.Chain.Common as Chain
import qualified Cardano.Chain.Slotting as Chain
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto.Hashing


data USSArgs
  = SubmitVote Update.UpId Bool
  | ProposeSoftware MProposalBody
  | ProposeProtocol MProposalBody
  deriving (Generic, Show)

instance Serialise USSArgs

data MProposalBody = MProposalBody
  { protocolVersion          :: !(Maybe Update.ProtocolVersion)
  , protocolParametersUpdate :: !(Maybe Update.ProtocolParametersUpdate)
  , softwareVersion          :: !(Maybe Update.SoftwareVersion)
  , metadata                 :: !(Map Update.SystemTag Update.InstallerHash)
  } deriving (Eq, Show, Generic)

-- XXX: move those where appropriate.
--
instance HashAlgorithm algo => Serialise (Digest algo) where
  encode digest = encode (BA.convert digest :: BSS.ByteString)
  decode = do
    ba :: BSS.ByteString <- decode
    case digestFromByteString ba of
      Just digest -> pure digest
      Nothing     -> fail "Invalid size for digest."

instance Serialise NominalDiffTime where
  encode t = encode (fromEnum t)
  decode = do
    time :: Int <- decode
    pure $ toEnum time

instance Serialise (Digest algo) => Serialise (AbstractHash algo a)

instance Serialise MProposalBody
instance Serialise Update.ApplicationName
instance Serialise Update.ProtocolVersion
instance Serialise Update.ProtocolParametersUpdate
instance Serialise Update.SoftwareVersion
instance Serialise Update.SystemTag
instance Serialise Update.InstallerHash
instance Serialise Update.SoftforkRule
instance Serialise Chain.EpochNumber
instance Serialise Chain.TxSizeLinear
instance Serialise Chain.LovelacePortion
instance Serialise Chain.Lovelace
instance Serialise Chain.SlotNumber
instance Serialise Chain.TxFeePolicy

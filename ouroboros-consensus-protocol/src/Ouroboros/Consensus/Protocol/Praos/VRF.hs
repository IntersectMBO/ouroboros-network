{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | This module implements VRF range extension as described in
-- https://iohk.io/en/research/library/papers/on-uc-secure-range-extension-and-batch-verification-for-ecvrf/
module Ouroboros.Consensus.Protocol.Praos.VRF (
    InputVRF
  , VRFUsage (..)
  , mkInputVRF
  , vrfLeaderCheckValue
  , vrfNonceValue
  ) where

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.Hash (Blake2b_256, Hash, castHash, hashToBytes,
                     hashWith)
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.Util
                     (SignableRepresentation (getSignableRepresentation),
                     bytesToNatural)
import           Cardano.Crypto.VRF (OutputVRF (..), getOutputVRFBytes)
import           Cardano.Ledger.BaseTypes (Nonce (NeutralNonce, Nonce))
import           Cardano.Ledger.Crypto (Crypto (HASH, VRF))
import           Cardano.Ledger.Serialization (runByteBuilder)
import           Cardano.Ledger.Slot (SlotNo (SlotNo))
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

-- | Input to the verifiable random function. Consists of the hash of the slot
-- and the epoch nonce.
newtype InputVRF = InputVRF {unUnifiedVRF :: Hash Blake2b_256 InputVRF}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NoThunks, ToCBOR)

instance SignableRepresentation InputVRF where
  getSignableRepresentation (InputVRF x) = hashToBytes x

-- | Construct a unified VRF value
mkInputVRF ::
  SlotNo ->
  -- | Epoch nonce
  Nonce ->
  InputVRF
mkInputVRF (SlotNo slot) eNonce =
  InputVRF
    . Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ BS.word64BE slot
      <> ( case eNonce of
             NeutralNonce -> mempty
             Nonce h      -> BS.byteStringCopy (Hash.hashToBytes h)
         )

-- | Indicate the usage of the VRF result.
data VRFUsage
  = -- | The VRF value will be used to establish whether the issuing node is
    -- indeed a leader for this slot.
    VRFLeader
  | -- | The VRF value will be used to contribute to the evolving nonce.
    VRFNonce

-- | Singleton VRF usage
data SVRFUsage a where
  SVRFLeader :: SVRFUsage VRFLeader
  SVRFNonce :: SVRFUsage VRFNonce

class VRFUsageTypeRep t where
  vuTypeRep :: SVRFUsage t

instance VRFUsageTypeRep VRFLeader where
  vuTypeRep = SVRFLeader

instance VRFUsageTypeRep VRFNonce where
  vuTypeRep = SVRFNonce

-- | Indicate the result of the VRF evaluation.
data VRFResult (v :: VRFUsage)

-- | Compute a hash of the unified VRF output appropriate to its usage.
hashVRF ::
  forall (v :: VRFUsage) c proxy.
  (Crypto c, VRFUsageTypeRep v) =>
  proxy c ->
  OutputVRF (VRF c) ->
  Hash (HASH c) (VRFResult v)
hashVRF _ certVRF =
  let vrfOutputAsBytes = getOutputVRFBytes certVRF
   in case vuTypeRep @v of
        SVRFLeader -> castHash $ hashWith id $ "L" <> vrfOutputAsBytes
        SVRFNonce  -> castHash $ hashWith id $ "N" <> vrfOutputAsBytes

-- | Simulate a VRF output to be used for leader checks from the relevant hash.
vrfLeaderCheckValue ::
  forall c proxy.
  Crypto c =>
  proxy c ->
  OutputVRF (VRF c) ->
  Natural
vrfLeaderCheckValue p =
  bytesToNatural . hashToBytes . hashVRF @VRFLeader p

-- | Simulate a VRF output to be used for the evolving nonce.
vrfNonceValue ::
  forall c proxy.
  Crypto c =>
  proxy c ->
  OutputVRF (VRF c) ->
  Nonce
vrfNonceValue p =
  Nonce . castHash . hashWith id . hashToBytes . hashVRF @VRFNonce p

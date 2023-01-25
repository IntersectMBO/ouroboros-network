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
  , vrfLeaderValue
  , vrfNonceValue
  ) where

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.Hash (Blake2b_256, Hash, castHash, hashToBytes,
                     hashWith, sizeHash)
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.Util
                     (SignableRepresentation (getSignableRepresentation),
                     bytesToNatural)
import           Cardano.Crypto.VRF (CertifiedVRF (certifiedOutput),
                     OutputVRF (..), getOutputVRFBytes)
import           Cardano.Ledger.BaseTypes (Nonce (NeutralNonce, Nonce))
import           Cardano.Ledger.Crypto (Crypto (HASH))
import           Cardano.Protocol.HeaderCrypto (HeaderCrypto (VRF))
import           Cardano.Ledger.Serialization (runByteBuilder)
import           Cardano.Ledger.Slot (SlotNo (SlotNo))
import           Cardano.Protocol.TPraos.BHeader (BoundedNatural,
                     assertBoundedNatural)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import           Data.Proxy (Proxy (Proxy))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

-- TODO: The hashing function Blake2b_256 is same as Crypto class
-- Solution 1 - Keep it same as, it is proposed that Crypto will be dissolved and converted to a
--              concrete data type
-- Solution 2 - Parametrise it with Crypto, so as to see its effect, and hence it'd be easier
--              to remove it later as well.
-- | Input to the verifiable random function. Consists of the hash of the slot
-- and the epoch nonce.
newtype InputVRF = InputVRF {unInputVRF :: Hash Blake2b_256 InputVRF}
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

-- | Indicate the result of the VRF evaluation.
data VRFResult (v :: VRFUsage)

-- | Compute a hash of the unified VRF output appropriate to its usage.
hashVRF ::
  forall (v :: VRFUsage) c hc proxy.
  (Crypto c) =>
  proxy c ->
  proxy hc ->
  SVRFUsage v ->
  CertifiedVRF (VRF hc) InputVRF ->
  Hash (HASH c) (VRFResult v)
hashVRF _  _ use certVRF =
  let vrfOutputAsBytes = getOutputVRFBytes $ certifiedOutput certVRF
   in case use of
        SVRFLeader -> castHash $ hashWith id $ "L" <> vrfOutputAsBytes
        SVRFNonce  -> castHash $ hashWith id $ "N" <> vrfOutputAsBytes

-- | Range-extend a VRF output to be used for leader checks from the relevant
-- hash. See section 4.1 of the linked paper for details.
vrfLeaderValue ::
  forall c hc proxy.
  Crypto c =>
  proxy c ->
  proxy hc ->
  CertifiedVRF (VRF hc) InputVRF ->
  BoundedNatural
vrfLeaderValue p hp cvrf =
  assertBoundedNatural
    ((2 :: Natural) ^ (8 * sizeHash (Proxy @(HASH c))))
    (bytesToNatural . hashToBytes $ hashVRF p hp SVRFLeader cvrf)

-- | Range-extend a VRF output to be used for the evolving nonce. See section
-- 4.1 of the linked paper for details.
vrfNonceValue ::
  forall c hc proxy.
  (Crypto c) =>
  proxy c ->
  proxy hc ->
  CertifiedVRF (VRF hc) InputVRF ->
  Nonce
vrfNonceValue p hp =
  -- The double hashing below is perhaps a little confusing. The first hash is
  -- how we do range extension as per the VRF paper. The second hash is how we
  -- generate a nonce value from a VRF output. However, that "VRF output" is now
  -- itself a hash.
  --
  -- However, while the VRF hash is crypto-dependent, for the nonce we use a
  -- fixed `Blake2b_256` hashing function. So this double hashing is still
  -- needed.
  Nonce . castHash . hashWith id . hashToBytes . hashVRF p hp SVRFNonce

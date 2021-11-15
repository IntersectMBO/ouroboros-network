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

module Ouroboros.Consensus.Protocol.Praos.VRF (
    UnifiedVRF
  , VRFUsage (..)
  , hashVRF
  , mkUnifiedVRF
  , vrfLeaderCheckValue
  , vrfNonceValue
  ) where

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.Hash (Blake2b_256, Hash, castHash, hashToBytes,
                     hashWith)
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.Util
                     (SignableRepresentation (getSignableRepresentation))
import           Cardano.Crypto.VRF (OutputVRF (..), getOutputVRFBytes)
import           Cardano.Ledger.BaseTypes (Nonce (NeutralNonce, Nonce))
import           Cardano.Ledger.Crypto (Crypto (HASH, VRF))
import           Cardano.Ledger.Serialization (runByteBuilder)
import           Cardano.Ledger.Slot (SlotNo (SlotNo))
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

-- | Newtype wrapper for a unified VRF value which may then be converted to
-- either of:
--
-- - Leader selection value
-- - Updated Nonce
newtype UnifiedVRF = UnifiedVRF {unUnifiedVRF :: Hash Blake2b_256 UnifiedVRF}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NoThunks, ToCBOR)

instance SignableRepresentation UnifiedVRF where
  getSignableRepresentation (UnifiedVRF x) = hashToBytes x

-- | Construct a unified VRF value
mkUnifiedVRF ::
  SlotNo ->
  -- | Epoch nonce
  Nonce ->
  UnifiedVRF
mkUnifiedVRF (SlotNo slot) eNonce =
  UnifiedVRF
    . Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ BS.word64BE slot
      <> ( case eNonce of
             NeutralNonce -> mempty
             Nonce h      -> BS.byteStringCopy (Hash.hashToBytes h)
         )

-- | Indicate the usage of the VRF result.
data VRFUsage = VRFTest | VRFNonce

-- | Singleton VRF usage
data SVRFUsage a where
  SVRFTest :: SVRFUsage VRFTest
  SVRFNonce :: SVRFUsage VRFNonce

class VRFUsageTypeRep t where
  vuTypeRep :: SVRFUsage t

instance VRFUsageTypeRep VRFTest where
  vuTypeRep = SVRFTest

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
        SVRFTest ->
          castHash $
            Hash.xor
              (hashWith id "TEST")
              (castHash $ hashWith id vrfOutputAsBytes)
        SVRFNonce ->
          castHash $
            Hash.xor
              (hashWith id "NONCE")
              (castHash $ hashWith id vrfOutputAsBytes)

-- | Simulate a VRF output to be used for leader checks from the relevant hash.
vrfLeaderCheckValue ::
  forall c proxy.
  Crypto c =>
  proxy c ->
  OutputVRF (VRF c) ->
  OutputVRF (VRF c)
vrfLeaderCheckValue p =
  OutputVRF . hashToBytes . hashVRF @VRFTest p

-- | Simulate a VRF output to be used for the evolving nonce.
vrfNonceValue ::
  forall c proxy.
  Crypto c =>
  proxy c ->
  OutputVRF (VRF c) ->
  OutputVRF (VRF c)
vrfNonceValue p =
  OutputVRF . hashToBytes . hashVRF @VRFNonce p

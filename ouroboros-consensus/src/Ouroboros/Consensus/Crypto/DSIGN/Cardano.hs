{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Cardano digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Cardano
    ( CardanoDSIGN
    , VerKeyDSIGN(..)
    , SignKeyDSIGN(..)
    , SigDSIGN(..)
    , HasSignTag(..)
    ) where

import           Cardano.Binary
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Crypto (ProtocolMagicId, SignTag (..), Signature,
                     SigningKey, VerificationKey, keyGen, signRaw,
                     toVerification, verifySignatureRaw)
import           Cardano.Crypto.DSIGN.Class
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Prelude (NoUnexpectedThunks)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Constraint
import           Data.Proxy (Proxy (..))
import           Data.Reflection (Given (..))
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Util.Condense

class (HasSignTag a, Decoded a) => ByronSignable a
instance (HasSignTag a, Decoded a) => ByronSignable a

class HasSignTag a where
  signTag :: proxy a -> SignTag

signTagFor :: forall a. HasSignTag a => a -> SignTag
signTagFor _ = signTag (Proxy @a)

instance HasSignTag CC.UTxO.TxSigData where
  signTag = const SignTx

-- Unfortunately, the sign tag for a block also incorporates extra information
-- related to the hybrid lightweight/heavyweight delegation scheme used in the
-- Byron era. Specifically, when signing a block, we also include the
-- verification key of the genesis stakeholder of which the signing node is a delegate.
--
-- In order to avoid this specific case requiring a redesign of the whole system,
-- we do some annoying trickery to make this available:
--
-- - Require a given constraint here containing the relevant verification key
-- - Use a class reifying this instance head/body relationship to propagate this constraint.
instance Given (VerKeyDSIGN CardanoDSIGN)
        => HasSignTag (Annotated CC.Block.ToSign ByteString) where
  signTag = const $ SignBlock vk
    where VerKeyCardanoDSIGN vk = given

-- See https://hackage.haskell.org/package/constraints-0.10.1/docs/Data-Constraint.html#t::-61--62-
--
-- This instance reflects the instance head/body relationship, and allows us to
-- reconstruct the `HasSignTag` constraint using the relevant `Given` constraint.
instance Given (VerKeyDSIGN CardanoDSIGN)
        :=> HasSignTag (Annotated CC.Block.ToSign ByteString) where
  ins = Sub Dict

data CardanoDSIGN

instance Given ProtocolMagicId => DSIGNAlgorithm CardanoDSIGN where

    newtype VerKeyDSIGN CardanoDSIGN = VerKeyCardanoDSIGN VerificationKey
        deriving (Show, Eq, Generic, NoUnexpectedThunks)

    newtype SignKeyDSIGN CardanoDSIGN = SignKeyCardanoDSIGN SigningKey
        deriving (Show, Generic)

    newtype SigDSIGN CardanoDSIGN = SigCardanoDSIGN (Signature CC.Block.ToSign)
        deriving (Show, Eq, Generic, NoUnexpectedThunks)

    type Signable CardanoDSIGN = ByronSignable

    encodeVerKeyDSIGN (VerKeyCardanoDSIGN pk) = toCBOR pk
    decodeVerKeyDSIGN = VerKeyCardanoDSIGN <$> fromCBOR

    encodeSignKeyDSIGN (SignKeyCardanoDSIGN pk) = toCBOR pk
    decodeSignKeyDSIGN = SignKeyCardanoDSIGN <$> fromCBOR

    encodeSigDSIGN (SigCardanoDSIGN pk) = toCBOR pk
    decodeSigDSIGN = SigCardanoDSIGN <$> fromCBOR

    genKeyDSIGN = SignKeyCardanoDSIGN . snd <$> keyGen

    deriveVerKeyDSIGN (SignKeyCardanoDSIGN sk) = VerKeyCardanoDSIGN $ toVerification sk

    signDSIGN a (SignKeyCardanoDSIGN sk) = return
        . SigCardanoDSIGN
        . coerce
        $ signRaw given (Just $ signTagFor a) sk (recoverBytes a)

    verifyDSIGN (VerKeyCardanoDSIGN vk) a (SigCardanoDSIGN sig) =
        if verifySignatureRaw vk (Crypto.signTag given (signTagFor a) <> recoverBytes a) $ coerce sig
          then Right ()
          else Left "Verification failed"

instance Condense (SigDSIGN CardanoDSIGN) where
    condense (SigCardanoDSIGN s) = show s

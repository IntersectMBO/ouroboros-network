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
import           Cardano.Crypto (SignTag (..), Signature, SigningKey,
                     VerificationKey, keyGen, signRaw, toVerification,
                     verifySignatureRaw)
import           Cardano.Crypto.DSIGN.Class
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm (..))
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Util.Condense

class (HasSignTag a, Decoded a) => ByronSignable a
instance (HasSignTag a, Decoded a) => ByronSignable a

class HasSignTag a where
  signTag :: ByronConfig -> VerKeyDSIGN CardanoDSIGN -> proxy a -> SignTag

signTagFor :: forall a. HasSignTag a
           => ByronConfig -> VerKeyDSIGN CardanoDSIGN -> a -> SignTag
signTagFor cfg genKey _ = signTag cfg genKey (Proxy @a)

instance HasSignTag CC.UTxO.TxSigData where
  signTag _ _ = const SignTx

instance HasSignTag (Annotated CC.Block.ToSign ByteString) where
  signTag _ (VerKeyCardanoDSIGN vk) = const $ SignBlock vk

data CardanoDSIGN

instance DSIGNAlgorithm CardanoDSIGN where
    -- Context required for Cardano digital signatures
    --
    -- We require the ByronConfig (from which we just extract the protocol
    -- magic) as well as the verification key of the genesis stakeholder of
    -- which the signing node is a delegate, which is required for signing
    -- blocks.
    type ContextDSIGN CardanoDSIGN = (ByronConfig, VerKeyDSIGN CardanoDSIGN)

    newtype VerKeyDSIGN CardanoDSIGN = VerKeyCardanoDSIGN VerificationKey
        deriving (Show, Eq, Generic)
        deriving NoUnexpectedThunks via UseIsNormalForm (VerKeyDSIGN CardanoDSIGN)

    newtype SignKeyDSIGN CardanoDSIGN = SignKeyCardanoDSIGN SigningKey
        deriving (Show, Generic)
        deriving NoUnexpectedThunks via UseIsNormalForm (SignKeyDSIGN CardanoDSIGN)

    newtype SigDSIGN CardanoDSIGN = SigCardanoDSIGN (Signature CC.Block.ToSign)
        deriving (Show, Eq, Generic)
        deriving NoUnexpectedThunks via UseIsNormalForm (SigDSIGN CardanoDSIGN)

    type Signable CardanoDSIGN = ByronSignable

    encodeVerKeyDSIGN (VerKeyCardanoDSIGN pk) = toCBOR pk
    decodeVerKeyDSIGN = VerKeyCardanoDSIGN <$> fromCBOR

    encodeSignKeyDSIGN (SignKeyCardanoDSIGN pk) = toCBOR pk
    decodeSignKeyDSIGN = SignKeyCardanoDSIGN <$> fromCBOR

    encodeSigDSIGN (SigCardanoDSIGN pk) = toCBOR pk
    decodeSigDSIGN = SigCardanoDSIGN <$> fromCBOR

    genKeyDSIGN = SignKeyCardanoDSIGN . snd <$> keyGen

    deriveVerKeyDSIGN (SignKeyCardanoDSIGN sk) = VerKeyCardanoDSIGN $ toVerification sk

    signDSIGN (cfg, genKey) a (SignKeyCardanoDSIGN sk) = return
        . SigCardanoDSIGN
        . coerce
        $ signRaw (pbftProtocolMagicId cfg) (Just $ signTagFor cfg genKey a) sk (recoverBytes a)

    verifyDSIGN (cfg, genKey) (VerKeyCardanoDSIGN vk) a (SigCardanoDSIGN sig) =
        if verifySignatureRaw vk (Crypto.signTag (pbftProtocolMagicId cfg) (signTagFor cfg genKey a) <> recoverBytes a) $ coerce sig
          then Right ()
          else Left "Verification failed"

instance Condense (SigDSIGN CardanoDSIGN) where
    condense (SigCardanoDSIGN s) = show s

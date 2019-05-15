{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

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
import qualified Cardano.Chain.Txp as CC.Txp
import           Cardano.Crypto
  ( ProtocolMagicId
  , ProxyVerificationKey
  , SignTag(..)
  , PublicKey
  , Signature
  , SecretKey
  , keyGen
  , toPublic
  , signEncoded
  , verifySignature
  )
import           Data.Coerce (coerce)
import           Data.Function (on)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Util.Condense

pm :: ProtocolMagicId
pm = undefined

class HasSignTag a where
  signTag :: a -> SignTag

instance HasSignTag CC.Txp.TxSigData where
  signTag = const SignTx

instance HasSignTag CC.Block.ToSign where
  signTag = const SignMainBlock

instance HasSignTag (ProxyVerificationKey w) where
  signTag = const SignProxyVK

data CardanoDSIGN

instance DSIGNAlgorithm CardanoDSIGN where

    newtype VerKeyDSIGN CardanoDSIGN = VerKeyCardanoDSIGN PublicKey
        deriving (Show, Eq, Generic)

    newtype SignKeyDSIGN CardanoDSIGN = SignKeyCardanoDSIGN SecretKey
        deriving (Show, Eq, Generic)

    newtype SigDSIGN CardanoDSIGN = SigCardanoDSIGN (Signature Encoding)
        deriving (Show, Eq, Generic)

    type Signable CardanoDSIGN = HasSignTag

    encodeVerKeyDSIGN (VerKeyCardanoDSIGN pk) = toCBOR pk
    decodeVerKeyDSIGN = VerKeyCardanoDSIGN <$> fromCBOR

    encodeSignKeyDSIGN (SignKeyCardanoDSIGN pk) = toCBOR pk
    decodeSignKeyDSIGN = SignKeyCardanoDSIGN <$> fromCBOR

    encodeSigDSIGN (SigCardanoDSIGN pk) = toCBOR pk
    decodeSigDSIGN = SigCardanoDSIGN <$> fromCBOR

    genKeyDSIGN = SignKeyCardanoDSIGN . snd <$> keyGen

    deriveVerKeyDSIGN (SignKeyCardanoDSIGN sk) = VerKeyCardanoDSIGN $ toPublic sk

    signDSIGN toEnc a (SignKeyCardanoDSIGN sk) = do
        return $ SigCardanoDSIGN $ signEncoded pm (signTag a) sk (toEnc a)

    verifyDSIGN toEnc (VerKeyCardanoDSIGN vk) a (SigCardanoDSIGN sig)
      = verifySignature toEnc pm (signTag a) vk a $ coerce sig

instance Ord (VerKeyDSIGN CardanoDSIGN) where
    compare = compare `on` show

instance Ord (SignKeyDSIGN CardanoDSIGN) where
    compare = compare `on` show

instance Ord (SigDSIGN CardanoDSIGN) where
    compare = compare `on` show

instance Condense (SigDSIGN CardanoDSIGN) where
    condense (SigCardanoDSIGN s) = show s

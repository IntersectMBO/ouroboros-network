{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | RSAPSS digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.RSAPSS
    ( RSAPSSDSIGN
    ) where

import           Codec.Serialise (Serialise (..))
import           Crypto.PubKey.RSA
import           Crypto.PubKey.RSA.PSS
import           Data.ByteString (unpack)
import           Data.Function (on)
import           GHC.Generics (Generic)
import           Text.Printf (printf)

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Serialise (toBS)

data RSAPSSDSIGN

e :: Integer
e = 0x10001

byteSize :: Int
byteSize = 100

instance DSIGNAlgorithm RSAPSSDSIGN where

    newtype VerKeyDSIGN RSAPSSDSIGN = VerKeyRSAPSSDSIGN PublicKey
        deriving (Show, Eq, Generic)

    newtype SignKeyDSIGN RSAPSSDSIGN = SignKeyRSAPSSDSIGN PrivateKey
        deriving (Show, Eq, Generic)

    newtype SigDSIGN RSAPSSDSIGN = SigRSAPSSDSIGN ByteString
        deriving (Show, Eq, Ord, Generic, Serialise)

    encodeVerKeyDSIGN = encode
    encodeSignKeyDSIGN = encode
    encodeSigDSIGN = encode

    decodeVerKeyDSIGN = decode
    decodeSignKeyDSIGN = decode
    decodeSigDSIGN = decode

    genKeyDSIGN = do
        (_, sk) <- generate byteSize e
        return $ SignKeyRSAPSSDSIGN sk

    deriveVerKeyDSIGN (SignKeyRSAPSSDSIGN sk) = VerKeyRSAPSSDSIGN $ private_pub sk

    signDSIGN toEnc a (SignKeyRSAPSSDSIGN sk) = do
        esig <- signSafer defaultPSSParamsSHA1 sk (toBS $ toEnc a)
        case esig of
            Left err  -> error $ "signDSIGN: " ++ show err
            Right sig -> return $ SigRSAPSSDSIGN sig

    verifyDSIGN toEnc (VerKeyRSAPSSDSIGN vk) a (SigRSAPSSDSIGN sig) =
        verify defaultPSSParamsSHA1 vk (toBS $ toEnc a) sig

instance Ord (VerKeyDSIGN RSAPSSDSIGN) where
    compare = compare `on` show

instance Ord (SignKeyDSIGN RSAPSSDSIGN) where
    compare = compare `on` show

instance Serialise (VerKeyDSIGN RSAPSSDSIGN) where
    encode (VerKeyRSAPSSDSIGN vk) = encode $ vkToTuple vk
    decode                        = (VerKeyRSAPSSDSIGN . vkFromTuple) <$> decode

instance Serialise (SignKeyDSIGN RSAPSSDSIGN) where
    encode (SignKeyRSAPSSDSIGN sk) = encode $ skToTuple sk
    decode                         = (SignKeyRSAPSSDSIGN . skFromTuple) <$> decode

instance Condense (SigDSIGN RSAPSSDSIGN) where
    condense (SigRSAPSSDSIGN b) =
        let (str :: String) = map (toEnum . fromEnum) . unpack $ b
        in printf "%x" str

vkToTuple :: PublicKey -> (Int, Integer, Integer)
vkToTuple vk = (public_size vk, public_n vk, public_e vk)

vkFromTuple :: (Int, Integer, Integer) -> PublicKey
vkFromTuple (size, n, e') = PublicKey size n e'

skToTuple :: PrivateKey
          -> ( (Int, Integer, Integer)
             , Integer
             , Integer
             , Integer
             , Integer
             , Integer
             , Integer
             )
skToTuple sk = ( vkToTuple (private_pub  sk)
               ,            private_d    sk
               ,            private_p    sk
               ,            private_q    sk
               ,            private_dP   sk
               ,            private_dQ   sk
               ,            private_qinv sk
               )

skFromTuple :: ( (Int, Integer, Integer)
               , Integer
               , Integer
               , Integer
               , Integer
               , Integer
               , Integer
               )
            -> PrivateKey
skFromTuple (vk, d, p, q, dp, dq, qinv) = PrivateKey (vkFromTuple vk) d p q dp dq qinv

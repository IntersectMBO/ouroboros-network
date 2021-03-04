{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
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

-- | Byron digital signatures.
module Ouroboros.Consensus.Byron.Crypto.DSIGN
    ( ByronDSIGN
    , VerKeyDSIGN(..)
    , SignKeyDSIGN(..)
    , SigDSIGN(..)
    , HasSignTag(..)
    ) where


import           Control.Exception (throw)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeapNamed (..), NoThunks)

import           Cardano.Binary
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Crypto (ProtocolMagicId, SignTag (..), Signature (..),
                     SigningKey (..), VerificationKey (..), deterministicKeyGen,
                     signRaw, toVerification, verifySignatureRaw)
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.Seed (SeedBytesExhausted (..), getBytesFromSeed)
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Crypto.Wallet as CC

import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.Condense

class (HasSignTag a, Decoded a) => ByronSignable a
instance (HasSignTag a, Decoded a) => ByronSignable a

class HasSignTag a where
  signTag :: VerKeyDSIGN ByronDSIGN -> proxy a -> SignTag

signTagFor :: forall a. HasSignTag a
           => VerKeyDSIGN ByronDSIGN -> a -> SignTag
signTagFor genKey _ = signTag genKey (Proxy @a)

instance HasSignTag CC.UTxO.TxSigData where
  signTag _ = const SignTx

instance HasSignTag (Annotated CC.Block.ToSign ByteString) where
  signTag (VerKeyByronDSIGN vk) = const $ SignBlock vk

data ByronDSIGN

instance DSIGNAlgorithm ByronDSIGN where

    type SeedSizeDSIGN    ByronDSIGN = 32
    type SizeVerKeyDSIGN  ByronDSIGN = 64
    type SizeSignKeyDSIGN ByronDSIGN = 128
    type SizeSigDSIGN     ByronDSIGN = 64

    algorithmNameDSIGN _ = "ByronDSIGN"

    -- Context required for Byron digital signatures
    --
    -- We require the the protocol magic as well as the verification key of the
    -- genesis stakeholder of which the signing node is a delegate, which is
    -- required for signing blocks.
    type ContextDSIGN ByronDSIGN = (ProtocolMagicId, VerKeyDSIGN ByronDSIGN)

    newtype VerKeyDSIGN ByronDSIGN = VerKeyByronDSIGN VerificationKey
        deriving (Show, Eq, Generic)
        deriving NoThunks via InspectHeapNamed "VerKeyDSIGN ByronDSIGN" (VerKeyDSIGN ByronDSIGN)

    newtype SignKeyDSIGN ByronDSIGN = SignKeyByronDSIGN SigningKey
        deriving (Show, Generic)
        deriving NoThunks via InspectHeapNamed "SignKeyDSIGN ByronDSIGN" (SignKeyDSIGN ByronDSIGN)

    newtype SigDSIGN ByronDSIGN = SigByronDSIGN (Signature CC.Block.ToSign)
        deriving (Show, Eq, Generic)
        deriving NoThunks via InspectHeapNamed "SigDSIGN ByronDSIGN" (SigDSIGN ByronDSIGN)

    type Signable ByronDSIGN = ByronSignable

    genKeyDSIGN seed =
        SignKeyByronDSIGN . snd $ deterministicKeyGen seedBytes
      where
        seedBytes = case getBytesFromSeed 32 seed of
          Just (x,_) -> x
          Nothing    -> throw $ SeedBytesExhausted (-1) -- TODO We can't get the seed size!

    deriveVerKeyDSIGN (SignKeyByronDSIGN sk) = VerKeyByronDSIGN $ toVerification sk

    signDSIGN (magic, genKey) a (SignKeyByronDSIGN sk) =
        SigByronDSIGN
        . coerce
        $ signRaw magic (Just $ signTagFor genKey a) sk (recoverBytes a)

    verifyDSIGN (magic, genKey) (VerKeyByronDSIGN vk) a (SigByronDSIGN sig) =
        if verifySignatureRaw vk (Crypto.signTag magic (signTagFor genKey a) <> recoverBytes a) $ coerce sig
          then Right ()
          else Left "Verification failed"

    rawSerialiseVerKeyDSIGN (VerKeyByronDSIGN (VerificationKey vk)) = CC.unXPub vk
    rawSerialiseSignKeyDSIGN (SignKeyByronDSIGN (SigningKey sk)) = CC.unXPrv sk
    rawSerialiseSigDSIGN (SigByronDSIGN (Signature sig)) = CC.unXSignature sig

    rawDeserialiseVerKeyDSIGN bs =
      VerKeyByronDSIGN . VerificationKey <$> (eitherToMaybe $ CC.xpub bs)
    rawDeserialiseSignKeyDSIGN bs =
      SignKeyByronDSIGN . SigningKey <$> (eitherToMaybe $ CC.xprv bs)
    rawDeserialiseSigDSIGN bs =
      SigByronDSIGN . Signature <$> (eitherToMaybe $ CC.xsignature bs)

instance Condense (SigDSIGN ByronDSIGN) where
    condense (SigByronDSIGN s) = show s

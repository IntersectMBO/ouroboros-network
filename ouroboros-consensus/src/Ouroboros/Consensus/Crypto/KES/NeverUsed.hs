{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Crypto.KES.NeverUsed (
    NeverKES
  , VerKeyKES(..)
  , SignKeyKES(..)
  , SigKES(..)
  ) where

import           Codec.Serialise (encode)

import           Ouroboros.Consensus.Crypto.KES.Class
import           Ouroboros.Consensus.Util.Condense

-- | KES never used
--
-- The type of keys and signatures is isomorphic to unit, but when actually
-- trying to sign or verify something a runtime exception will be thrown.
data NeverKES

instance KESAlgorithm NeverKES where
  data VerKeyKES  NeverKES = NeverUsedVerKeyKES  deriving (Show, Eq, Ord)
  data SignKeyKES NeverKES = NeverUsedSignKeyKES deriving (Show, Eq, Ord)
  data SigKES     NeverKES = NeverUsedSigKES     deriving (Show, Eq, Ord)

  encodeVerKeyKES  _ = encode ()
  encodeSignKeyKES _ = encode ()
  encodeSigKES     _ = encode ()

  decodeVerKeyKES  = return NeverUsedVerKeyKES
  decodeSignKeyKES = return NeverUsedSignKeyKES
  decodeSigKES     = return NeverUsedSigKES

  genKeyKES       _ = return NeverUsedSignKeyKES
  deriveVerKeyKES _ = NeverUsedVerKeyKES

  signKES   = error "KES not available"
  verifyKES = error "KES not available"

instance Condense (SigKES NeverKES) where
  condense = show

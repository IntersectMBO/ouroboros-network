{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Crypto.DSIGN.NeverUsed (
    NeverDSIGN
  , VerKeyDSIGN(..)
  , SignKeyDSIGN(..)
  , SigDSIGN(..)
  ) where

import           Codec.Serialise
import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Util.Condense

-- | DSIGN never used
--
-- The type of keys and signatures is isomorphic to unit, but when actually
-- trying to sign or verify something a runtime exception will be thrown.
data NeverDSIGN

instance DSIGNAlgorithm NeverDSIGN where
  data VerKeyDSIGN  NeverDSIGN = NeverUsedVerKeyDSIGN  deriving (Show, Eq, Ord)
  data SignKeyDSIGN NeverDSIGN = NeverUsedSignKeyDSIGN deriving (Show, Eq, Ord)
  data SigDSIGN     NeverDSIGN = NeverUsedSigDSIGN     deriving (Show, Eq, Ord)

  encodeVerKeyDSIGN  _ = encode ()
  encodeSignKeyDSIGN _ = encode ()
  encodeSigDSIGN     _ = encode ()

  decodeVerKeyDSIGN  = return NeverUsedVerKeyDSIGN
  decodeSignKeyDSIGN = return NeverUsedSignKeyDSIGN
  decodeSigDSIGN     = return NeverUsedSigDSIGN

  genKeyDSIGN         = return NeverUsedSignKeyDSIGN
  deriveVerKeyDSIGN _ = NeverUsedVerKeyDSIGN

  signDSIGN   = error "DSIGN not available"
  verifyDSIGN = error "DSIGN not available"

instance Condense (SigDSIGN NeverDSIGN) where
  condense = show

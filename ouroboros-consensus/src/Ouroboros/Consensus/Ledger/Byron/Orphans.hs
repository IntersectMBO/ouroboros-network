{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Ledger.Byron.Orphans () where

import           Codec.Serialise (Serialise, decode, encode)

import           Cardano.Binary (fromCBOR, toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance Serialise CC.Block.ChainValidationState where
  encode = toCBOR
  decode = fromCBOR

instance Serialise CC.Common.KeyHash where
  encode = toCBOR
  decode = fromCBOR

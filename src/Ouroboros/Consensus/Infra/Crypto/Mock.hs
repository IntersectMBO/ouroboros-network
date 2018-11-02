-- | Mock implementations of crypto primitives
--
-- The intend is that the datatypes (and the choice of which constructors to
-- export and which not) and functions defined in this module faithfully
-- model the properties we want from the crypto primitives, but without
-- actually doing any cryptography.
module Ouroboros.Consensus.Infra.Crypto.Mock (
    -- * DSIGN
    module DSIGN
    -- * KES
  , module KES
    -- *VRF
  , module VRF
  ) where

import           Ouroboros.Consensus.Infra.Crypto.Mock.DSIGN as DSIGN
import           Ouroboros.Consensus.Infra.Crypto.Mock.KES as KES
import           Ouroboros.Consensus.Infra.Crypto.Mock.VRF as VRF

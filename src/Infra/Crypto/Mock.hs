-- | Mock implementations of crypto primitives
--
-- The intend is that the datatypes (and the choice of which constructors to
-- export and which not) and functions defined in this module faithfully
-- model the properties we want from the crypto primitives, but without
-- actually doing any cryptography.
module Infra.Crypto.Mock (
    -- * DSIGN
    module Infra.Crypto.Mock.DSIGN
    -- * KES
  , module Infra.Crypto.Mock.KES
    -- *VRF
  , module Infra.Crypto.Mock.VRF
  ) where

import           Infra.Crypto.Mock.DSIGN
import           Infra.Crypto.Mock.KES
import           Infra.Crypto.Mock.VRF

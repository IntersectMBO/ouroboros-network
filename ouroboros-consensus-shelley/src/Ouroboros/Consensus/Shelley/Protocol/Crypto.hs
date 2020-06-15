{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Shelley.Protocol.Crypto (
    module Shelley.Spec.Ledger.Crypto
  , HotKey(..)
  , TPraosCrypto
  , TPraosStandardCrypto
  ) where

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Numeric.Natural

import           Cardano.Binary (ToCBOR)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import           Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Crypto.Hash.Class (Hash)
import           Cardano.Crypto.KES.Class
import qualified Cardano.Crypto.KES.Class as KES
import           Cardano.Crypto.KES.Sum
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Util.Condense (Condense)

import           Shelley.Spec.Ledger.BaseTypes (Seed)
import           Shelley.Spec.Ledger.BlockChain (BHBody)
import           Shelley.Spec.Ledger.Crypto (Crypto (..))
import           Shelley.Spec.Ledger.OCert (KESPeriod)
import           Shelley.Spec.Ledger.TxData (TxBody)

class ( Crypto c
      , Typeable (VRF c)
      , Condense (SigKES (KES c))
      , DSIGN.Signable
          (DSIGN c)
          (VerKeyKES (KES c), Natural, KESPeriod)
      , DSIGN.Signable
          (DSIGN c)
          (Hash (HASH c) (TxBody c))
      , KES.Signable
          (KES c)
          (BHBody c)
      , VRF.Signable (VRF c) Seed
      , ToCBOR (DSIGN.VerKeyDSIGN (DSIGN c))
      ) => TPraosCrypto c

data TPraosStandardCrypto

instance Crypto TPraosStandardCrypto where
  type DSIGN    TPraosStandardCrypto = Ed25519DSIGN
  type KES      TPraosStandardCrypto = Sum7KES Ed25519DSIGN Blake2b_256
  type VRF      TPraosStandardCrypto = SimpleVRF
  type HASH     TPraosStandardCrypto = Blake2b_256
  type ADDRHASH TPraosStandardCrypto = Blake2b_224

instance TPraosCrypto TPraosStandardCrypto

-- | A hot KES key. We store alongside the key the KES period for which it is
-- valid.
data HotKey c = HotKey !Period !(SignKeyKES (KES c))
  deriving Generic

instance Show (HotKey c) where
  show _ = "<HotKey>"

instance TPraosCrypto c => NoUnexpectedThunks (HotKey c)

{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Protocol.TPraos.Crypto
  ( module Cardano.Ledger.Shelley.Crypto
  , TPraosCrypto
  , TPraosStandardCrypto
  , TPraosMockCrypto
  )
where

import           Cardano.Crypto.DSIGN.Class (Signable)
import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash.MD5 (MD5)
import           Cardano.Crypto.Hash.SHA256 (SHA256)
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.KES.Mock
import           Cardano.Crypto.KES.Simple
import           Cardano.Crypto.VRF.Class
import           Cardano.Crypto.VRF.Mock (MockVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           Numeric.Natural
import           Ouroboros.Consensus.Util.Condense
import           Cardano.Ledger.Shelley.Crypto

import BaseTypes (Seed)
import BlockChain (BHBody)
import Keys (VKeyES(..))
import OCert (KESPeriod)

class ( Crypto c
      , Typeable (VRF c)
      , Condense (SigKES (KES c))
      , Cardano.Crypto.DSIGN.Class.Signable
          (DSIGN c)
          (Keys.VKeyES c, Natural, OCert.KESPeriod)
      , Cardano.Crypto.KES.Class.Signable
          (KES c)
          (BlockChain.BHBody c)
      , Cardano.Crypto.VRF.Class.Signable (VRF c) Seed
      ) => TPraosCrypto (c :: Type) where

data TPraosStandardCrypto
data TPraosMockCrypto

instance Crypto TPraosStandardCrypto where
  type DSIGN TPraosStandardCrypto = Ed448DSIGN
  type KES  TPraosStandardCrypto = SimpleKES Ed448DSIGN
  type VRF  TPraosStandardCrypto = SimpleVRF
  type HASH TPraosStandardCrypto = SHA256

instance TPraosCrypto TPraosStandardCrypto

instance Crypto TPraosMockCrypto where
  type DSIGN TPraosMockCrypto = MockDSIGN
  type KES  TPraosMockCrypto = MockKES
  type VRF  TPraosMockCrypto = MockVRF
  type HASH TPraosMockCrypto = MD5

instance TPraosCrypto TPraosMockCrypto

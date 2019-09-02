{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Protocol.TPraos.Crypto where

import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm, Signable)
import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..))
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
import           Ouroboros.Network.Block (SlotNo (..))

import BaseTypes (Seed, UnitInterval)
import BlockChain (BHBody)
import Keys (VKeyES(..))
import OCert (KESPeriod)

class ( DSIGNAlgorithm (TPraosDSIGN c)
      , KESAlgorithm  (TPraosKES  c)
      , VRFAlgorithm  (TPraosVRF  c)
      , HashAlgorithm (TPraosHash c)
      , Typeable c
      , Typeable (TPraosVRF c)
      , Condense (SigKES (TPraosKES c))
      , Cardano.Crypto.DSIGN.Class.Signable
          (TPraosDSIGN c)
          (Keys.VKeyES (TPraosKES c), Natural, OCert.KESPeriod)
      , Cardano.Crypto.KES.Class.Signable
          (TPraosKES c)
          (BlockChain.BHBody (TPraosHash c) (TPraosDSIGN c) (TPraosKES c))
      , Cardano.Crypto.VRF.Class.Signable (TPraosVRF c) (Seed, SlotNo)
      , Cardano.Crypto.VRF.Class.Signable (TPraosVRF c) (UnitInterval, SlotNo)
      ) => TPraosCrypto (c :: Type) where
  type family TPraosDSIGN c :: Type
  type family TPraosKES   c :: Type
  type family TPraosVRF   c :: Type
  type family TPraosHash  c :: Type

data TPraosStandardCrypto
data TPraosMockCrypto

instance TPraosCrypto TPraosStandardCrypto where
  type TPraosDSIGN TPraosStandardCrypto = Ed448DSIGN
  type TPraosKES  TPraosStandardCrypto = SimpleKES Ed448DSIGN
  type TPraosVRF  TPraosStandardCrypto = SimpleVRF
  type TPraosHash TPraosStandardCrypto = SHA256

instance TPraosCrypto TPraosMockCrypto where
  type TPraosDSIGN TPraosMockCrypto = MockDSIGN
  type TPraosKES  TPraosMockCrypto = MockKES
  type TPraosVRF  TPraosMockCrypto = MockVRF
  type TPraosHash TPraosMockCrypto = MD5

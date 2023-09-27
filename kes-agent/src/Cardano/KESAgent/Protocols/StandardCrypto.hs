{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.KESAgent.Protocols.StandardCrypto
where

import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.KES.Crypto

import Cardano.Crypto.KES.Mock
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.KES.CompactSum
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Crypto.Hash.Blake2b

data StandardCrypto

data CompactStandardCrypto

data SingleCrypto

data MockCrypto

instance Crypto StandardCrypto where
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type DSIGN StandardCrypto = Ed25519DSIGN

instance Crypto CompactStandardCrypto where
  type KES CompactStandardCrypto = CompactSum6KES Ed25519DSIGN Blake2b_256
  type DSIGN CompactStandardCrypto = Ed25519DSIGN

instance Crypto SingleCrypto where
  type KES SingleCrypto = SingleKES Ed25519DSIGN
  type DSIGN SingleCrypto = Ed25519DSIGN

instance Crypto MockCrypto where
  type KES MockCrypto = MockKES 128
  type DSIGN MockCrypto = Ed25519DSIGN

instance NamedCrypto StandardCrypto where
  cryptoName _ = CryptoName "StandardCrypto"

instance NamedCrypto CompactStandardCrypto where
  cryptoName _ = CryptoName "CompactStandardCrypto"

instance NamedCrypto SingleCrypto where
  cryptoName _ = CryptoName "SingleCrypto"

instance NamedCrypto MockCrypto where
  cryptoName _ = CryptoName "MockCrypto"

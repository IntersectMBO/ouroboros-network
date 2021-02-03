{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Example.Eras (
    -- * Eras based on the Shelley ledger
    ExampleEra
    -- * Eras instantiated with standard crypto
  , StandardExample
    -- * Re-exports
  , ShelleyBasedEra (..)
  , EraCrypto
  , ShelleyEra
  , StandardShelley
  , StandardCrypto
  ) where

import           Cardano.Ledger.Example (ExampleEra)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Eras
import qualified Shelley.Spec.Ledger.API as SL

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Example era with standard crypto
type StandardExample = ExampleEra StandardCrypto

instance SL.PraosCrypto c => ShelleyBasedEra (ExampleEra c) where
  shelleyBasedEraName _ = "Example"

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Block.Mock where

import           Block               (BodyHash (..), KnownLedgerDomain (..),
                                      LedgerDomain (..))
import           Data.Set            (Set)
import qualified Infra.Crypto.Hash   as H
import           Ouroboros.UTxO.Mock (HasUtxo (..))
import qualified Ouroboros.UTxO.Mock as Mock

instance KnownLedgerDomain 'MockLedgerDomain where
    data BlockBody 'MockLedgerDomain = BlockBody {
        blockData :: Set (Mock.Tx)
      }
    hashBody (BlockBody b) = BodyHash (fromEnum $ H.fromHash $ H.hash b)

instance HasUtxo Mock.Tx => HasUtxo (BlockBody 'MockLedgerDomain) where
  txIns      = txIns      . blockData
  txOuts     = txOuts     . blockData
  updateUtxo = updateUtxo . blockData
  confirmed  = confirmed  . blockData

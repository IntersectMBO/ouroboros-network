{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Block.Mock where

import           Block (Block (..), BlockHeader (..), BodyHash (..),
                     KnownLedgerDomain (..), LedgerDomain (..))
import           Data.Maybe (maybe)
import           Data.Set (Set)
import           GHC.Natural (naturalToWordMaybe)

import qualified Infra.Crypto.Hash as H
import           Infra.Util (Condense (..))
import           Ouroboros.UTxO.Mock (HasUtxo (..))
import qualified Ouroboros.UTxO.Mock as Mock
import           Serialise

instance KnownLedgerDomain 'MockLedgerDomain where
    data BlockBody 'MockLedgerDomain = BlockBody {
          blockData :: Set (Mock.Tx)
        } deriving Show
    hashBody (BlockBody b) =
      BodyHash (maybe maxBound fromEnum $ naturalToWordMaybe $ H.fromHash $ H.hash b)

instance HasUtxo (BlockBody 'MockLedgerDomain) where
  txIns      = txIns      . blockData
  txOuts     = txOuts     . blockData
  updateUtxo = updateUtxo . blockData
  confirmed  = confirmed  . blockData

instance Condense (BlockHeader p) => Condense (Block 'MockLedgerDomain p) where
    condense (Block hdr body) =
        "{hdr: " <> condense hdr
                 <> ", body: "
                 <> condense (hashBody body)
                 <> "}"

--
-- Serialisation
--

instance Serialise (BlockBody 'MockLedgerDomain) where

  encode (BlockBody b) = encodeListLen 1 <> encode b
  decode = do
    decodeListLenOf 1
    BlockBody <$> decode


instance Serialise (Block 'MockLedgerDomain p) where

  encode Block {blockHeader, blockBody} =
      encodeListLen 2
   <> encode blockHeader
   <> encode   blockBody

  decode = do
      decodeListLenOf 2
      Block <$> decode <*> decode

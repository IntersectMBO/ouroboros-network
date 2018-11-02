{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Block.Mock where

import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Natural (naturalToWordMaybe)

import           Block
import           Block.Concrete

import qualified Infra.Crypto.Hash as H
import           Infra.Util (Condense (..))
import           Ouroboros.UTxO.Mock (HasUtxo (..))
import qualified Ouroboros.UTxO.Mock as Mock
import           Serialise

-- Concrete block representation with a bunch of transactions
--
-- For now we just reuse the concrete header from the networking layer.
data SimpleUtxoBlock = SimpleUtxoBlock {
      simpleUtxoHeader :: BlockHeader
    , simpleUtxoBody   :: Set Mock.Tx
    }
  deriving (Generic, Show)

instance Condense SimpleUtxoBlock where
  condense = show -- TODO

simpleUtxoBodyHash :: Set Mock.Tx -> ConcreteBodyHash
simpleUtxoBodyHash = BodyHash
                   . maybe 0 fromIntegral
                   . naturalToWordMaybe
                   . H.fromHash
                   . H.hash
                   . Set.toList

instance HasUtxo SimpleUtxoBlock where
  txIns      = txIns      . simpleUtxoBody
  txOuts     = txOuts     . simpleUtxoBody
  updateUtxo = updateUtxo . simpleUtxoBody
  confirmed  = confirmed  . simpleUtxoBody

instance HasHeader SimpleUtxoBlock where
    type HeaderHash SimpleUtxoBlock = ConcreteHeaderHash
    type BodyHash   SimpleUtxoBlock = ConcreteBodyHash

    blockHash      = headerHash     . simpleUtxoHeader
    blockPrevHash  = headerPrevHash . simpleUtxoHeader
    blockSlot      = headerSlot     . simpleUtxoHeader
    blockNo        = headerBlockNo  . simpleUtxoHeader
    blockSigner    = headerSigner   . simpleUtxoHeader
    blockBodyHash  = headerBodyHash . simpleUtxoHeader

    -- | The block invariant is just that the actual block body hash matches the
    -- body hash listed in the header.
    --
    blockInvariant (SimpleUtxoBlock header body) =
        simpleUtxoBodyHash body == blockBodyHash header

    genesisHash _ = genesisHash (Proxy @BlockHeader)

{-
instance KnownLedgerDomain 'MockLedgerDomain where
    data BlockBody 'MockLedgerDomain = MockBlockBody {
          blockData :: Set (Mock.Tx)
        } deriving (Show, Eq)
    hashBody (MockBlockBody b) =
      BodyHash (maybe maxBound fromEnum $ naturalToWordMaybe $ H.fromHash $ H.hash b)


instance Condense (BlockHeader p) => Condense (Block 'MockLedgerDomain p) where
    condense (Block hdr body) =
        "{hdr: " <> condense hdr
                 <> ", body: "
                 <> condense (hashBody body)
                 <> "}"
-}

instance Serialise SimpleUtxoBlock where
  -- rely on generics instance

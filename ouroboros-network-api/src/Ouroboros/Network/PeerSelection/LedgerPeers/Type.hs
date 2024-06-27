{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Various types related to ledger peers.  This module is re-exported from
-- "Ouroboros.Network.PeerSelection.LedgerPeers".
--
module Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( PoolStake (..)
  , AccPoolStake (..)
  , IsBigLedgerPeer (..)
  , LedgerStateJudgement (..)
  , LedgerPeersConsensusInterface (..)
  , UseLedgerPeers (..)
  , AfterSlot (..)
  , LedgerPeersKind (..)
  , LedgerPeerSnapshot (.., LedgerPeerSnapshot)
  , isLedgerPeersEnabled
  ) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary qualified as Codec
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM
import Control.DeepSeq (NFData (..))
import Data.Aeson
import Data.Aeson.Types
import NoThunks.Class
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)

-- |The type of big ledger peers that is serialised or later
-- provided by node configuration for the networking layer
-- to connect to when syncing.
--
data LedgerPeerSnapshot =
  LedgerPeerSnapshotV1 (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
  -- ^ Internal use for version 1, use pattern synonym for public API
  deriving (Eq, Show)

-- |Public API to access snapshot data. Currently access to only most recent version is available.
-- Nonetheless, serialisation from the node into JSON is supported for older versions via internal
-- api so that newer CLI can still support older node formats.
--
pattern LedgerPeerSnapshot :: (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
                           -> LedgerPeerSnapshot
pattern LedgerPeerSnapshot payload <- LedgerPeerSnapshotV1 payload where
  LedgerPeerSnapshot payload = LedgerPeerSnapshotV1 payload

{-# COMPLETE LedgerPeerSnapshot #-}

-- | In case the format changes in the future, this function provides a migration functionality
-- when possible.
--
migrateLedgerPeerSnapshot :: LedgerPeerSnapshot
                          -> Maybe (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
migrateLedgerPeerSnapshot (LedgerPeerSnapshotV1 lps) = Just lps

instance ToJSON LedgerPeerSnapshot where
  toJSON (LedgerPeerSnapshotV1 (slot, pools)) =
    object [ "Version" .= (1 :: Int)
           , "SlotNo" .= slot
           , "BigLedgerPools" .= [ object [ "AccStake" .= show accStake
                                          , "ReStake"  .= show stake
                                          , "Relays"   .= relays]
                                 | (AccPoolStake accStake, (PoolStake stake, relays)) <- pools]]

instance FromJSON LedgerPeerSnapshot where
  parseJSON = withObject "LedgerPeerSnapshot" $ \v -> do
    vNum <- v .: "Version"
    parsedSnapshot <-
      case (vNum :: Int) of
        1 -> do
          slot <- v .: "SlotNo"
          bigPools <- v .: "BigLedgerPools"
          bigPools' <- (forM bigPools . withObject "BigLedgerPools" $ \poolV -> do
            accStake <-
              poolV .: "AccStake"
              >>= (\case
                    Just accStake -> pure accStake
                    Nothing    ->
                      fail "Invalid AccStake value")
                  . readMaybe
            reStake <-
              poolV .: "ReStake"
              >>= (\case
                    Just reStake -> pure reStake
                    Nothing    ->
                      fail "Invalid ReStake value")
                  . readMaybe
            relays <- poolV .: "Relays"
            return (accStake, (reStake, relays))) <?> Key "BigLedgerPools"

          return $ LedgerPeerSnapshotV1 (slot, bigPools')
        _ -> fail $ "Network.LedgerPeers.Type: parseJSON: failed to parse unsupported version " <> show vNum
    case migrateLedgerPeerSnapshot parsedSnapshot of
      Just payload -> return $ LedgerPeerSnapshot payload
      Nothing      -> fail "Network.LedgerPeers.Type: parseJSON: failed to migrate big ledger peer snapshot"

-- | cardano-slotting provides its own {To,From}CBOR instances for WithOrigin a
-- but to pin down the encoding for CDDL we provide a wrapper with custom
-- instances
--
newtype WithOriginWrapper = WithOriginWrapper (WithOrigin SlotNo)

-- | Hand cranked CBOR instances to facilitate CDDL spec
--
instance ToCBOR WithOriginWrapper where
  toCBOR (WithOriginWrapper Origin) = Codec.encodeListLen 1 <> Codec.encodeWord8 0
  toCBOR (WithOriginWrapper (At (SlotNo slot))) = Codec.encodeListLen 2 <> Codec.encodeWord8 1 <> toCBOR slot

instance FromCBOR WithOriginWrapper where
  fromCBOR = do
    listLen <- Codec.decodeListLen
    tag <- Codec.decodeWord8
    case (listLen, tag) of
      (1, 0) -> pure $ WithOriginWrapper Origin
      (1, _) -> fail "LedgerPeers.Type: Expected tag for Origin constructor"
      (2, 1) -> WithOriginWrapper . At . SlotNo <$> fromCBOR
      (2, _) -> fail "LedgerPeers.Type: Expected tag for At constructor"
      _      -> fail "LedgerPeers.Type: Unrecognized list length while decoding WithOrigin SlotNo"

instance ToCBOR LedgerPeerSnapshot where
  toCBOR (LedgerPeerSnapshotV1 (slot, pools)) =
       Codec.encodeListLen 3
    <> Codec.encodeWord8 1
    <> toCBOR (WithOriginWrapper slot)
    <> toCBOR pools'
    where
      pools' = map (\(AccPoolStake accPoolStake, (PoolStake reStake, relays)) -> (accPoolStake, reStake, relays)) pools

instance FromCBOR LedgerPeerSnapshot where
  fromCBOR = do
    Codec.decodeListLenOf 3
    version <- Codec.decodeWord8
    case version of
      1 -> LedgerPeerSnapshotV1 <$> do
             (WithOriginWrapper slot, pools) <- (,) <$> fromCBOR <*> fromCBOR
             let pools' = [(AccPoolStake accStake, (PoolStake reStake, relays))
                          | (accStake, reStake, relays) <- pools]
             return (slot, pools')
      _ -> fail $ "LedgerPeers.Type: no decoder could be found for version " <> show version

-- | Which ledger peers to pick.
--
data LedgerPeersKind = AllLedgerPeers | BigLedgerPeers
  deriving Show

-- | Only use the ledger after the given slot number.
data UseLedgerPeers = DontUseLedgerPeers
                    | UseLedgerPeers AfterSlot
                    deriving (Eq, Show, Generic, NoThunks)

-- | Only use the ledger after the given slot number.
data AfterSlot = Always
               | After SlotNo
               deriving (Eq, Show, Generic)
               deriving anyclass NoThunks

isLedgerPeersEnabled :: UseLedgerPeers -> Bool
isLedgerPeersEnabled DontUseLedgerPeers = False
isLedgerPeersEnabled UseLedgerPeers {}  = True

-- | The relative stake of a stakepool in relation to the total amount staked.
-- A value in the [0, 1] range.
--
newtype PoolStake = PoolStake { unPoolStake :: Rational }
  deriving (Eq, Ord, Show)
  deriving newtype (Fractional, Num, NFData, Read)

-- | The accumulated relative stake of a stake pool, like PoolStake but it also includes the
-- relative stake of all preceding pools. A value in the range [0, 1].
--
newtype AccPoolStake = AccPoolStake { unAccPoolStake :: Rational }
    deriving (Eq, Ord, Show)
    deriving newtype (Fractional, Num, Read)

-- | A boolean like type.  Big ledger peers are the largest SPOs which control
-- 90% of staked stake.
--
-- Note that 'IsBigLedgerPeer' indicates a role that peer plays in the eclipse
-- evasion, e.g. that a peer was explicitly selected as a big ledger peer, e.g.
-- 'IsNotBigLedgerPeer' does not necessarily mean that the peer isn't a big
-- ledger peer.  This is because we select root peers from all ledger peers
-- (including big ones).
--
data IsBigLedgerPeer
   = IsBigLedgerPeer
   | IsNotBigLedgerPeer
  deriving Eq

-- | Wether the node is caught up or fell too far behind the chain
data LedgerStateJudgement = YoungEnough | TooOld
  deriving (Eq, Show, Generic, NoThunks)

-- | Return ledger state information and ledger peers.
--
data LedgerPeersConsensusInterface m = LedgerPeersConsensusInterface {
    lpGetLatestSlot           :: STM m (WithOrigin SlotNo),
    lpGetLedgerStateJudgement :: STM m LedgerStateJudgement,
    lpGetLedgerPeers          :: STM m [(PoolStake, NonEmpty RelayAccessPoint)]
  }

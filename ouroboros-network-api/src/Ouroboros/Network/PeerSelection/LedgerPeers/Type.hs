{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

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
  , MinBigLedgerPeersForTrustedState (..)
  , isLedgerPeersEnabled
  , compareLedgerPeerSnapshotApproximate
  ) where

import GHC.Generics (Generic)

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary qualified as Codec
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM
import Control.DeepSeq (NFData (..))
import Control.Monad (forM)
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import NoThunks.Class

import Ouroboros.Network.PeerSelection.RelayAccessPoint

-- | Minimum number of hot big ledger peers in Genesis mode
--   for trusted state to be signalled to Consensus. This number
--   should be smaller than the `targetNumberOfActiveBigLedgerPeers`
--   but greater than 1. In Genesis, we may demote a big ledger peer
--   for underperformance, but not promote a replacement immediately
--   to guard against adversaries which may want to slow down our
--   progress.
--
newtype MinBigLedgerPeersForTrustedState =
  MinBigLedgerPeersForTrustedState { getMinBigLedgerPeersForTrustedState :: Int }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)

-- |The type of big ledger peers that is serialised or later
-- provided by node configuration for the networking layer
-- to connect to when syncing.
--
data LedgerPeerSnapshot =
  LedgerPeerSnapshotV2 (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
  -- ^ Internal use for version 1, use pattern synonym for public API
  deriving (Eq, Show)

-- |Public API to access snapshot data. Currently access to only most recent version is available.
-- Nonetheless, serialisation from the node into JSON is supported for older versions via internal
-- api so that newer CLI can still support older node formats.
--
pattern LedgerPeerSnapshot :: (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
                           -> LedgerPeerSnapshot
pattern LedgerPeerSnapshot payload <- LedgerPeerSnapshotV2 payload where
  LedgerPeerSnapshot payload = LedgerPeerSnapshotV2 payload

{-# COMPLETE LedgerPeerSnapshot #-}

-- | Since ledger peer snapshot is serialised with all domain names
--   fully qualified, and all stake values are approximate in floating
--   point, comparison is necessarily approximate as well.
--   The candidate argument is processed here to simulate a round trip
--   by the serialisation mechanism and then compared to the baseline
--   argument, which is assumed that it was actually processed this way
--   when a snapshot was created earlier, and hence it is approximate as well.
--   The two approximate values should be equal if they were created
--   from the same 'faithful' data.
--
compareLedgerPeerSnapshotApproximate :: LedgerPeerSnapshot
                                     -> LedgerPeerSnapshot
                                     -> Bool
compareLedgerPeerSnapshotApproximate baseline candidate =
  case tripIt of
    Success candidate' -> candidate' == baseline
    Error _            -> False
  where
    tripIt = fromJSON . toJSON $ candidate

-- | In case the format changes in the future, this function provides a migration functionality
-- when possible.
--
migrateLedgerPeerSnapshot :: LedgerPeerSnapshot
                          -> Maybe (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
migrateLedgerPeerSnapshot (LedgerPeerSnapshotV2 lps) = Just lps

instance ToJSON LedgerPeerSnapshot where
  toJSON (LedgerPeerSnapshotV2 (slot, pools)) =
    object [ "version" .= (2 :: Int)
           , "slotNo" .= slot
           , "bigLedgerPools" .= [ object
                                   [ "accumulatedStake" .= fromRational @Double accStake
                                   , "relativeStake"  .= fromRational @Double relStake
                                   , "relays"   .= relays]
                                   | (AccPoolStake accStake, (PoolStake relStake, relays)) <- pools
                                   ]]

instance FromJSON LedgerPeerSnapshot where
  parseJSON = withObject "LedgerPeerSnapshot" $ \v -> do
    vNum :: Int <- v .: "version"
    parsedSnapshot <-
      case vNum of
        2 -> do
          slot <- v .: "slotNo"
          bigPools <- v .: "bigLedgerPools"
          bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                         let f poolV = do
                                 AccPoolStakeCoded accStake <- poolV .: "accumulatedStake"
                                 PoolStakeCoded reStake <- poolV .: "relativeStake"
                                 relays <- poolV .: "relays"
                                 return (accStake, (reStake, relays))
                         withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

          return $ LedgerPeerSnapshotV2 (slot, bigPools')
        _ -> fail $ "Network.LedgerPeers.Type: parseJSON: failed to parse unsupported version " <> show vNum
    case migrateLedgerPeerSnapshot parsedSnapshot of
      Just payload -> return $ LedgerPeerSnapshot payload
      Nothing      -> fail "Network.LedgerPeers.Type: parseJSON: failed to migrate big ledger peer snapshot"

-- | cardano-slotting provides its own {To,From}CBOR instances for WithOrigin a
-- but to pin down the encoding for CDDL we provide a wrapper with custom
-- instances
--
newtype WithOriginCoded = WithOriginCoded (WithOrigin SlotNo)

-- | Hand cranked CBOR instances to facilitate CDDL spec
--
instance ToCBOR WithOriginCoded where
  toCBOR (WithOriginCoded Origin) = Codec.encodeListLen 1 <> Codec.encodeWord8 0
  toCBOR (WithOriginCoded (At slotNo)) = Codec.encodeListLen 2 <> Codec.encodeWord8 1 <> toCBOR slotNo

instance FromCBOR WithOriginCoded where
  fromCBOR = do
    listLen <- Codec.decodeListLen
    tag <- Codec.decodeWord8
    case (listLen, tag) of
      (1, 0) -> pure $ WithOriginCoded Origin
      (1, _) -> fail "LedgerPeers.Type: Expected tag for Origin constructor"
      (2, 1) -> WithOriginCoded . At <$> fromCBOR
      (2, _) -> fail "LedgerPeers.Type: Expected tag for At constructor"
      _      -> fail "LedgerPeers.Type: Unrecognized list length while decoding WithOrigin SlotNo"

instance ToCBOR LedgerPeerSnapshot where
  toCBOR (LedgerPeerSnapshotV2 (wOrigin, pools)) =
       Codec.encodeListLen 2
    <> Codec.encodeWord8 2
    <> toCBOR (WithOriginCoded wOrigin, pools')
    where
      pools' =
        [(AccPoolStakeCoded accPoolStake, (PoolStakeCoded relStake, relays))
        | (accPoolStake, (relStake, relays)) <- pools
        ]

instance FromCBOR LedgerPeerSnapshot where
  fromCBOR = do
    Codec.decodeListLenOf 2
    version <- Codec.decodeWord8
    case version of
      2 -> LedgerPeerSnapshotV2 <$> do
             (WithOriginCoded wOrigin, pools) <- fromCBOR
             let pools' = [(accStake, (relStake, relays))
                          | (AccPoolStakeCoded accStake, (PoolStakeCoded relStake, relays)) <- pools
                          ]
             return (wOrigin, pools')
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
  deriving newtype (Fractional, Num, NFData)

newtype PoolStakeCoded = PoolStakeCoded PoolStake
  deriving (ToCBOR, FromCBOR, FromJSON, ToJSON) via Rational

-- | The accumulated relative stake of a stake pool, like PoolStake but it also includes the
-- relative stake of all preceding pools. A value in the range [0, 1].
--
newtype AccPoolStake = AccPoolStake { unAccPoolStake :: Rational }
    deriving (Eq, Ord, Show)
    deriving newtype (Fractional, Num)

newtype AccPoolStakeCoded = AccPoolStakeCoded AccPoolStake
  deriving (ToCBOR, FromCBOR, FromJSON, ToJSON) via Rational

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

instance ToJSON RelayAccessPointCoded where
  toJSON (RelayAccessPointCoded (RelayAccessDomain domain port)) =
    object
      [ "domain" .= decodeUtf8 domain
      , "port"   .= (fromIntegral port :: Int)]

  toJSON (RelayAccessPointCoded (RelayAccessAddress ip port)) =
    object
      [ "address" .= show ip
      , "port" .= (fromIntegral port :: Int)]

instance FromJSON RelayAccessPointCoded where
  parseJSON = withObject "RelayAccessPointCoded" $ \v -> do
    domain <- fmap BS.pack <$> v .:? "domain"
    port <- fromIntegral @Int <$> v .: "port"
    case domain of
      Nothing ->
            v .: "address"
        >>= \case
               Nothing -> fail "RelayAccessPointCoded: invalid IP address"
               Just addr ->
                 return . RelayAccessPointCoded $ RelayAccessAddress addr port
            . readMaybe

      Just domain'
        | Just (_, '.') <- BS.unsnoc domain' ->
          return . RelayAccessPointCoded $ RelayAccessDomain domain' port
        | otherwise ->
          let fullyQualified = domain' `BS.snoc` '.'
          in return . RelayAccessPointCoded $ RelayAccessDomain fullyQualified port

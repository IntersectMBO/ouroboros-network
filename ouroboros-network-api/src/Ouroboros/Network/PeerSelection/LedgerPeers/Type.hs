{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Various types related to ledger peers.  This module is re-exported from
-- "Ouroboros.Network.PeerSelection.LedgerPeers".
--
module Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( PoolStake (..)
  , AccPoolStake (..)
  , IsLedgerPeer (..)
  , IsBigLedgerPeer (..)
  , LedgerPeersConsensusInterface (..)
  , getRelayAccessPointsFromLedger
  , mapExtraAPI
  , UseLedgerPeers (..)
  , AfterSlot (..)
  , LedgerPeersKind (..)
  , LedgerPeerSnapshot (.., LedgerPeerSnapshot)
  , LedgerPeerSnapshotSRVSupport (..)
  , encodeLedgerPeerSnapshot
  , decodeLedgerPeerSnapshot
  , encodeLedgerPeerSnapshotPoint
  , decodeLedgerPeerSnapshotPoint
  , getRelayAccessPointsFromLedgerPeerSnapshot
  , isLedgerPeersEnabled
    -- * Re-exports
  , SRVPrefix
  , RelayAccessPoint (..)
  , LedgerRelayAccessPoint (..)
  , prefixLedgerRelayAccessPoint
  ) where


import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM
import Control.DeepSeq (NFData (..), force)
import Control.Monad (forM)
import Data.Aeson
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics (Generic)
import NoThunks.Class

-- TODO: remove `FromCBOR` and `ToCBOR` instances when ntc V22 is no longer supported
import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary qualified as Codec
import Cardano.Crypto.Hash (Hash, Blake2b_256)
import Ouroboros.Network.Block
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.Point

-- | The type of big ledger peers that is serialised or later
-- provided by node configuration for the networking layer
-- to connect to when syncing. Provided pattern synonym
-- abstracts over the internal representation.
--
data LedgerPeerSnapshot =
    LedgerPeerSnapshotV2
      (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))])
  | LedgerPeerSnapshotV3
      !(Point LedgerPeerSnapshot)
      ![(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))]

instance StandardHash LedgerPeerSnapshot
deriving instance Show (Point LedgerPeerSnapshot) => Show LedgerPeerSnapshot
type instance HeaderHash LedgerPeerSnapshot = Hash Blake2b_256 ByteString


getRelayAccessPointsFromLedgerPeerSnapshot
  :: SRVPrefix
  -> LedgerPeerSnapshot
  -> (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
getRelayAccessPointsFromLedgerPeerSnapshot srvPrefix = \case
  LedgerPeerSnapshotV2 as ->
    fmap (fmap (fmap (fmap (fmap (prefixLedgerRelayAccessPoint srvPrefix))))) as
  LedgerPeerSnapshotV3 pt as ->
    let as' = fmap (fmap (fmap (fmap (prefixLedgerRelayAccessPoint srvPrefix)))) as
     in  (pointSlot pt, as')


-- |Public API to access snapshot data. Currently access to only most recent version is available.
-- Nonetheless, serialisation from the node into JSON is supported for older versions via internal
-- api so that newer CLI can still support older node formats.
--
pattern LedgerPeerSnapshot :: Point LedgerPeerSnapshot
                           -> [(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))]
                           -> LedgerPeerSnapshot
pattern LedgerPeerSnapshot pt as <- LedgerPeerSnapshotV3 pt as
  where
    LedgerPeerSnapshot pt (force -> pools) = LedgerPeerSnapshotV3 pt pools

{-# COMPLETE LedgerPeerSnapshot #-}

-- | In case the format changes in the future, this function provides a migration functionality
-- when possible.
--
migrateLedgerPeerSnapshot
  :: LedgerPeerSnapshot
  -> Maybe LedgerPeerSnapshot
migrateLedgerPeerSnapshot snapshot@LedgerPeerSnapshotV2{} = Just snapshot
migrateLedgerPeerSnapshot snapshot@LedgerPeerSnapshotV3{} = Just snapshot

instance ToJSON LedgerPeerSnapshot where
  toJSON (LedgerPeerSnapshotV2 (slot, pools)) =
    object [ "version" .= (2 :: Int)
           , "slotNo" .= slot
           , "bigLedgerPools" .= [ object
                                   [ "accumulatedStake" .= fromRational @Double accStake
                                   , "relativeStake"    .= fromRational @Double relStake
                                   , "relays"           .= relays]
                                   | (AccPoolStake accStake, (PoolStake relStake, relays)) <- pools
                                   ]]
  toJSON (LedgerPeerSnapshotV3 pt pools) =
    object [ "NodeToClientVersion" .= (23 :: Int)
           , "Point" .= toJSON pt
           , "bigLedgerPools" .= [ object
                                   [ "accumulatedStake" .= fromRational @Double accStake
                                   , "relativeStake"    .= fromRational @Double relStake
                                   , "relays"           .= relays]
                                   | (AccPoolStake accStake, (PoolStake relStake, relays)) <- pools
                                   ]]

instance FromJSON LedgerPeerSnapshot where
  parseJSON = withObject "LedgerPeerSnapshot" $ \v -> do
    vNum :: Maybe Int <- v .:? "version" <|> v.:? "NodeToClientVersion"
    bigPools <- v .: "bigLedgerPools"
    ledgerPeerSnapshot <-
      case vNum of
        Just 1 -> do
          slot <- v .: "slotNo"
          bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                         let f poolV = do
                                 AccPoolStakeCoded accStake <- poolV .: "accumulatedStake"
                                 PoolStakeCoded reStake <- poolV .: "relativeStake"
                                 -- decode using `LedgerRelayAccessPointV1` instance
                                 relays <- fmap getLedgerReelayAccessPointV1 <$> poolV .: "relays"
                                 return (accStake, (reStake, relays))
                         withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

          return $ LedgerPeerSnapshotV2 (slot, bigPools')
        Just 2 -> do
          slot <- v .: "slotNo"
          bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                         let f poolV = do
                                 AccPoolStakeCoded accStake <- poolV .: "accumulatedStake"
                                 PoolStakeCoded reStake <- poolV .: "relativeStake"
                                 relays <- poolV .: "relays"
                                 return (accStake, (reStake, relays))
                         withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

          return $ LedgerPeerSnapshotV2 (slot, bigPools')
        Just 23 -> do
          point     <- v .: "Point"
          bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                         let f poolV = do
                                 AccPoolStakeCoded accStake <- poolV .: "accumulatedStake"
                                 PoolStakeCoded reStake <- poolV .: "relativeStake"
                                 relays <- poolV .: "relays"
                                 return (accStake, (reStake, relays))
                         withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

          return $ LedgerPeerSnapshotV3 point bigPools'
        Just _  ->
          fail $ "Network.LedgerPeers.Type: parseJSON: failed to parse unsupported version "
              <> show vNum
        Nothing ->
          fail $ "Network.LedgerPeers.Type: parseJSON: failed to parse unsupported version "
              <> show vNum
    case migrateLedgerPeerSnapshot ledgerPeerSnapshot of
      Just ledgerPeerSnapshot' -> return ledgerPeerSnapshot'
      Nothing                  ->
        fail "Network.LedgerPeers.Type: parseJSON: failed to migrate big ledger peer snapshot"


encodeWithOrigin :: WithOrigin SlotNo -> Codec.Encoding
encodeWithOrigin Origin      = Codec.encodeListLen 1 <> Codec.encodeWord8 0
encodeWithOrigin (At slotNo) = Codec.encodeListLen 2 <> Codec.encodeWord8 1 <> toCBOR slotNo

decodeWithOrigin :: Codec.Decoder s (WithOrigin SlotNo)
decodeWithOrigin = do
    listLen <- Codec.decodeListLen
    tag <- Codec.decodeWord8
    case (listLen, tag) of
      (1, 0) -> pure $ Origin
      (1, _) -> fail "LedgerPeers.Type: Expected tag for Origin constructor"
      (2, 1) -> At <$> fromCBOR
      (2, _) -> fail "LedgerPeers.Type: Expected tag for At constructor"
      _      -> fail "LedgerPeers.Type: Unrecognized list length while decoding WithOrigin SlotNo"


data LedgerPeerSnapshotSRVSupport
  = LedgerPeerSnapshotSupportsSRV
  -- ^ since `NodeToClientV_22`
  | LedgerPeerSnapshotDoesntSupportSRV
  deriving (Show, Eq)

encodeLedgerPeerSnapshot :: LedgerPeerSnapshotSRVSupport -> LedgerPeerSnapshot -> Codec.Encoding
encodeLedgerPeerSnapshot LedgerPeerSnapshotDoesntSupportSRV (LedgerPeerSnapshotV2 (wOrigin, pools)) =
       Codec.encodeListLen 2
    <> Codec.encodeWord8 1 -- internal version
    <> Codec.encodeListLen 2
    <> encodeWithOrigin wOrigin
    <> toCBOR pools'
    where
      pools' =
        [(AccPoolStakeCoded accPoolStake, (PoolStakeCoded relStake, relays))
        | (accPoolStake, (relStake, relays)) <-
            -- filter out SRV domains, not supported by `< NodeToClientV_22`
            map
              (second $ second $ NonEmpty.filter
                (\case
                    LedgerRelayAccessSRVDomain {} -> False
                    _ -> True)
              )
            pools
        , not (null relays)
        ]
encodeLedgerPeerSnapshot LedgerPeerSnapshotSupportsSRV (LedgerPeerSnapshotV2 (wOrigin, pools)) =
       Codec.encodeListLen 2
    <> Codec.encodeWord8 1 -- internal version
    <> Codec.encodeListLen 2
    <> encodeWithOrigin wOrigin
    <> toCBOR pools'
    where
      pools' =
        [(AccPoolStakeCoded accPoolStake, (PoolStakeCoded relStake, relays))
        | (accPoolStake, (relStake, relays)) <- pools
        ]
encodeLedgerPeerSnapshot _LedgerPeerSnapshotSupportsSRV (LedgerPeerSnapshotV3 pt pools) =
       Codec.encodeListLen 2
    <> Codec.encodeWord8 3 -- internal version
    <> Codec.encodeListLen 2
    <> encodeLedgerPeerSnapshotPoint pt
    <> toCBOR pools'
    where
      pools' =
        [(AccPoolStakeCoded accPoolStake, (PoolStakeCoded relStake, relays))
        | (accPoolStake, (relStake, relays)) <- pools
        ]


encodeLedgerPeerSnapshotPoint :: Point LedgerPeerSnapshot -> Codec.Encoding
encodeLedgerPeerSnapshotPoint = \case
  Point Origin -> Codec.encodeListLen 1 <> Codec.encodeWord8 0
  Point (At Block { blockPointSlot, blockPointHash }) ->
       Codec.encodeListLen 3 <> Codec.encodeWord8 1
    <> Codec.toCBOR blockPointSlot <> Codec.toCBOR blockPointHash


decodeLedgerPeerSnapshot :: LedgerPeerSnapshotSRVSupport -> Codec.Decoder s LedgerPeerSnapshot
decodeLedgerPeerSnapshot _ = do
    Codec.decodeListLenOf 2
    version <- Codec.decodeWord8
    case version of
      1 -> Codec.decodeListLenOf 2 >>
           LedgerPeerSnapshotV2 <$>
             ((,) <$> decodeWithOrigin <*> fromCBOR)
      3 -> Codec.decodeListLenOf 2 >>
           LedgerPeerSnapshotV3 <$> decodeLedgerPeerSnapshotPoint
                                <*> decodeStakePools
      _ -> fail $ "LedgerPeers.Type: no decoder could be found for version " <> show version


decodeLedgerPeerSnapshotPoint :: Codec.Decoder s (Point LedgerPeerSnapshot)
decodeLedgerPeerSnapshotPoint = do
  listLen <- Codec.decodeListLen
  tag <- Codec.decodeWord8
  case (tag, listLen) of
    (0, 1) -> pure $ Point Origin
    (0, n) -> fail $ "LedgerPeers.Type: invalid listLen for Origin tag, expected 1 got " <> show n
    (1, 3) -> Point . At <$> (Block <$> fromCBOR <*> fromCBOR)
    (1, n) -> fail $ "LedgerPeers.Type: invalid listLen for At tag, expected 3 got " <> show n
    _      -> fail "LedgerPeers.Type: Unrecognized CBOR encoding of Point for LedgerPeerSnapshot"


-- | Used by functions to indicate what kind of ledger peer to process
--
data LedgerPeersKind = AllLedgerPeers | BigLedgerPeers
  deriving Show

-- | Only use the ledger after the given slot number.
--
data UseLedgerPeers = DontUseLedgerPeers
                    | UseLedgerPeers AfterSlot
                    deriving (Eq, Show, Generic, NoThunks)

-- | Only use the ledger after the given slot number.
--
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
    deriving newtype (Fractional, Num, NFData)

newtype AccPoolStakeCoded = AccPoolStakeCoded AccPoolStake
  deriving (ToCBOR, FromCBOR, FromJSON, ToJSON) via Rational

-- | Identifies a peer as coming from ledger or not.
data IsLedgerPeer = IsLedgerPeer
                  -- ^ a ledger peer.
                  | IsNotLedgerPeer
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | Return ledger state information and ledger peers.
--
data LedgerPeersConsensusInterface extraAPI m = LedgerPeersConsensusInterface {
    lpGetLatestSlot  :: STM m (WithOrigin SlotNo)
  , lpGetLedgerPeers :: STM m [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
    -- | Extension point so that third party users can add more actions
  , lpExtraAPI       :: extraAPI
  }

getRelayAccessPointsFromLedger
  :: MonadSTM m
  => SRVPrefix
  -> LedgerPeersConsensusInterface extraAPI m
  -> STM m [(PoolStake, NonEmpty RelayAccessPoint)]
getRelayAccessPointsFromLedger
  srvPrefix
  LedgerPeersConsensusInterface {lpGetLedgerPeers}
  =
  fmap (fmap (fmap (fmap (prefixLedgerRelayAccessPoint srvPrefix)))) lpGetLedgerPeers


mapExtraAPI :: (a -> b) -> LedgerPeersConsensusInterface a m -> LedgerPeersConsensusInterface b m
mapExtraAPI f lpci@LedgerPeersConsensusInterface{ lpExtraAPI = api } =
  lpci { lpExtraAPI = f api }

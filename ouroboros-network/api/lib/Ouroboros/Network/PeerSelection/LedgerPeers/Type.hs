{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
  , LedgerPeerSnapshot (..)
  , LedgerPeerSnapshotWithBlock (..)
  , SomeLedgerPeerSnapshot (..)
  , SomeHashableBlock (..)
  , LedgerPeerSnapshotSRVSupport (..)
  , encodeLedgerPeerSnapshot
  , encodeLedgerPeerSnapshot'
  , decodeLedgerPeerSnapshot
  , encodeWithOrigin
  , decodeWithOrigin
  , encodeLedgerPeerSnapshotPoint
  , decodeLedgerPeerSnapshotPoint
  , encodeBigStakePools
  , decodeBigStakePools
  , encodeAllStakePools
  , decodeAllStakePools
  , getRelayAccessPointsFromBigLedgerPeersSnapshot
  , getRelayAccessPointsFromAllLedgerPeersSnapshot
  , isLedgerPeersEnabled
    -- * Re-exports
  , SRVPrefix
  , RelayAccessPoint (..)
  , LedgerRelayAccessPoint (..)
  , prefixLedgerRelayAccessPoint
  ) where


import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM
import Control.DeepSeq (NFData (..))
import Control.Monad (forM)
import Data.Aeson hiding (decode, encode)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class

-- TODO: remove `FromCBOR` and `ToCBOR` instances when ntc V22 is no longer supported
import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary qualified as Codec
import Ouroboros.Network.Block
import Ouroboros.Network.Magic
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.Point

-- | A snapshot of ledger peers extracted from the ledger state at some point
--
data LedgerPeerSnapshot (a :: LedgerPeersKind) where
  LedgerPeerSnapshotV2
    :: (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))])
    -> LedgerPeerSnapshot BigLedgerPeers
  LedgerBigPeerSnapshotV23
    :: !(Point SomeHashableBlock)
    -> !NetworkMagic
    -> ![(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))]
    -> LedgerPeerSnapshot BigLedgerPeers
  LedgerAllPeerSnapshotV23
    :: !(Point SomeHashableBlock)
    -> !NetworkMagic
    -> ![(PoolStake, NonEmpty LedgerRelayAccessPoint)]
    -> LedgerPeerSnapshot AllLedgerPeers

deriving instance Eq (LedgerPeerSnapshot a)
deriving instance Show (LedgerPeerSnapshot a)


-- | We hide the block type to avoid parametrizing ouroboros-network with it
--
data SomeHashableBlock =
  forall blk. ( StandardHash blk
              , ToCBOR (HeaderHash blk)
              , FromCBOR (HeaderHash blk)
              , ToJSON (HeaderHash blk)
              , Typeable blk) => SomeHashableBlock !(Proxy blk) !(HeaderHash blk)

type instance HeaderHash SomeHashableBlock = SomeHashableBlock

-- we need this since `Point` is parameterised with `SomeHashableBlock`
-- in the snapshot
instance StandardHash SomeHashableBlock

-- superclass constraints of `StandardHash` require the following
instance Eq SomeHashableBlock where
  (==) (SomeHashableBlock (_ :: Proxy blk1) h1) (SomeHashableBlock (_ :: Proxy blk2) h2) =
    case eqT :: Maybe (HeaderHash blk1 :~: HeaderHash blk2) of
      Just Refl -> h1 == h2
      Nothing   -> error "impossible! Distinct (HeaderHash blk) types"

instance Show SomeHashableBlock where
  show (SomeHashableBlock _ sbh) = show sbh

instance Ord SomeHashableBlock where
  compare (SomeHashableBlock (_ :: Proxy blk1) h1) (SomeHashableBlock (_ :: Proxy blk2) h2) =
    case eqT :: Maybe (blk1 :~: blk2) of
      Just Refl -> compare h1 h2
      Nothing   -> error "impossible! Distinct (HeaderHash blk) types"

instance NoThunks SomeHashableBlock where
  wNoThunks ctx (SomeHashableBlock _ sbh) = wNoThunks ctx sbh
  showTypeOf _proxy = "SomeHashableBlock"

instance ToJSON SomeHashableBlock where
  toJSON (SomeHashableBlock _proxy hash) = toJSON hash


-- | facility for encoding the snapshot in CBOR for backwards compatibility
--
data SomeLedgerPeerSnapshot = forall k. SomeLedgerPeerSnapshot (LedgerPeerSnapshot k)

deriving instance Show SomeLedgerPeerSnapshot

-- | facility to aid JSON decode instances
--
newtype LedgerPeerSnapshotWithBlock blk a =
  LedgerPeerSnapshotWithBlock { parseLedgerPeerSnapshotWithBlock :: LedgerPeerSnapshot a }


getRelayAccessPointsFromBigLedgerPeersSnapshot
  :: SRVPrefix
  -> LedgerPeerSnapshot BigLedgerPeers
  -> (WithOrigin SlotNo, [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))])
getRelayAccessPointsFromBigLedgerPeersSnapshot srvPrefix = \case
  LedgerPeerSnapshotV2 as ->
    fmap (fmap (fmap (fmap (fmap (prefixLedgerRelayAccessPoint srvPrefix))))) as
  LedgerBigPeerSnapshotV23 pt _magic as ->
    let as' = fmap (fmap (fmap (fmap (prefixLedgerRelayAccessPoint srvPrefix)))) as
     in  (pointSlot pt, as')


getRelayAccessPointsFromAllLedgerPeersSnapshot
  :: SRVPrefix
  -> LedgerPeerSnapshot AllLedgerPeers
  -> (WithOrigin SlotNo, [(PoolStake, NonEmpty RelayAccessPoint)])
getRelayAccessPointsFromAllLedgerPeersSnapshot srvPrefix = \case
  LedgerAllPeerSnapshotV23 pt _magic as ->
    let as' = fmap (fmap (fmap (prefixLedgerRelayAccessPoint srvPrefix))) as
     in  (pointSlot pt, as')


instance ToJSON (LedgerPeerSnapshot a) where
  toJSON (LedgerPeerSnapshotV2 (slot, pools)) =
    object [ "version" .= (2 :: Int)
           , "slotNo" .= slot
           , "bigLedgerPools" .= [ object
                                   [ "accumulatedStake" .= fromRational @Double accStake
                                   , "relativeStake"    .= fromRational @Double relStake
                                   , "relays"           .= relays
                                   ]
                                 | (AccPoolStake accStake, (PoolStake relStake, relays)) <- pools
                                 ]
           ]
  toJSON (LedgerAllPeerSnapshotV23 pt magic pools) =
    object [ "NodeToClientVersion" .= (23 :: Int)
           , "Point" .= toJSON pt
           , "NetworkMagic" .= unNetworkMagic magic
           , "allLedgerPools" .= [ object
                                   [ "relativeStake" .= fromRational @Double relStake
                                   , "relays"        .= relays
                                   ]
                                 | (PoolStake relStake, relays) <- pools
                                 ]
           ]
  toJSON (LedgerBigPeerSnapshotV23 pt magic pools) =
    object [ "NodeToClientVersion" .= (23 :: Int)
           , "Point" .= toJSON pt
           , "NetworkMagic" .= unNetworkMagic magic
           , "bigLedgerPools" .= [ object
                                   [ "accumulatedStake" .= fromRational @Double accStake
                                   , "relativeStake"    .= fromRational @Double relStake
                                   , "relays"           .= relays
                                   ]
                                 | (AccPoolStake accStake, (PoolStake relStake, relays)) <- pools
                                 ]
           ]

instance ( StandardHash blk
         , ToJSON (HeaderHash blk)
         , FromJSON (HeaderHash blk)
         , FromCBOR (HeaderHash blk)
         , ToCBOR (HeaderHash blk)
         , Typeable blk
         ) => FromJSON (LedgerPeerSnapshotWithBlock blk AllLedgerPeers) where
  parseJSON = withObject "LedgerPeerSnapshot" \v -> do
    -- TODO: remove "version" key after NtC V22 support is removed
    vNum :: Int <- v .: "version" <|> v .: "NodeToClientVersion"
    allPools    <- v .: "allLedgerPools"
    case vNum of
      23 -> do
       point     <- v .: "Point" <&> \(pt :: Point blk) ->
         Point $ getPoint pt <&> \blk->
           blk { blockPointHash = SomeHashableBlock (Proxy :: Proxy blk) (blockPointHash blk) }
       magic     <- v .: "NetworkMagic"
       allPools' <- forM (zip [0 :: Int ..] allPools) \(idx, poolO) -> do
                      let f poolV = do
                              reStake  <- poolV .: "relativeStake"
                              relays   <- poolV .: "relays"
                              return (reStake, relays)
                      withObject ("allLedgerPools[" <> show idx <> "]") f (Object poolO)

       return . LedgerPeerSnapshotWithBlock $ LedgerAllPeerSnapshotV23 point (NetworkMagic magic) allPools'
      _ ->
        fail $ "Network.LedgerPeers.Type: parseJSON: failed to parse unsupported version "
            <> show vNum

instance ( StandardHash blk
         , FromJSON (HeaderHash blk)
         , ToJSON (HeaderHash blk)
         -- The CBOR instances are only needed to satisfy these constraints
         -- when constructing `SomeHashableBlock` here.
         , FromCBOR (HeaderHash blk)
         , ToCBOR (HeaderHash blk)
         , Typeable blk
         ) => FromJSON (LedgerPeerSnapshotWithBlock blk BigLedgerPeers) where
  parseJSON = withObject "LedgerPeerSnapshot" \v -> do
    -- TODO: remove "version" key after NtC V22 support is removed
    vNum :: Int <- v .: "version" <|> v .: "NodeToClientVersion"
    case vNum of
      1 -> do
        slot      <- v .: "slotNo"
        bigPools  <- v .: "bigLedgerPools"
        bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                       let f poolV = do
                               accStake <- poolV .: "accumulatedStake"
                               reStake  <- poolV .: "relativeStake"
                               -- decode using `LedgerRelayAccessPointV1` instance
                               relays <- fmap getLedgerReelayAccessPointV1 <$> poolV .: "relays"
                               return (accStake, (reStake, relays))
                       withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

        return . LedgerPeerSnapshotWithBlock $ LedgerPeerSnapshotV2 (slot, bigPools')
      2 -> do
        slot      <- v .: "slotNo"
        bigPools  <- v .: "bigLedgerPools"
        bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                       let f poolV = do
                               accStake <- poolV .: "accumulatedStake"
                               reStake  <- poolV .: "relativeStake"
                               relays   <- poolV .: "relays"
                               return (accStake, (reStake, relays))
                       withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

        return . LedgerPeerSnapshotWithBlock $ LedgerPeerSnapshotV2 (slot, bigPools')
      23 -> do
        point     <- v .: "Point" <&> \(pt :: Point blk) ->
          Point $ getPoint pt <&> \blk->
            blk { blockPointHash = SomeHashableBlock (Proxy :: Proxy blk) (blockPointHash blk) }
        magic     <- v .: "NetworkMagic"
        bigPools  <- v .: "bigLedgerPools"
        bigPools' <- forM (zip [0 :: Int ..] bigPools) \(idx, poolO) -> do
                       let f poolV = do
                               accStake <- poolV .: "accumulatedStake"
                               reStake  <- poolV .: "relativeStake"
                               relays   <- poolV .: "relays"
                               return (accStake, (reStake, relays))
                       withObject ("bigLedgerPools[" <> show idx <> "]") f (Object poolO)

        return . LedgerPeerSnapshotWithBlock $ LedgerBigPeerSnapshotV23 point (NetworkMagic magic) bigPools'
      _ ->
        fail $ "Network.LedgerPeers.Type: parseJSON: failed to parse unsupported version "
            <> show vNum


data LedgerPeerSnapshotSRVSupport
  = LedgerPeerSnapshotSupportsSRV
  -- ^ since `NodeToClientV_22`
  | LedgerPeerSnapshotDoesntSupportSRV
  deriving (Show, Eq)


encodeLedgerPeerSnapshot' :: LedgerPeerSnapshotSRVSupport -> SomeLedgerPeerSnapshot -> Codec.Encoding
encodeLedgerPeerSnapshot' srvSupport (SomeLedgerPeerSnapshot lps) = encodeLedgerPeerSnapshot srvSupport lps
{-# INLINE encodeLedgerPeerSnapshot' #-}


encodeLedgerPeerSnapshot :: LedgerPeerSnapshotSRVSupport -> LedgerPeerSnapshot a -> Codec.Encoding
encodeLedgerPeerSnapshot LedgerPeerSnapshotDoesntSupportSRV (LedgerPeerSnapshotV2 (wOrigin, pools)) =
     Codec.encodeListLen 2
  <> Codec.encodeWord8 1 -- internal version
  <> Codec.encodeListLen 2
  <> encodeWithOrigin wOrigin
  <> toCBOR pools'
  where
    pools' =
      [(accPoolStake, (relStake, NonEmpty.fromList relays))
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
  <> toCBOR pools

encodeLedgerPeerSnapshot _LedgerPeerSnapshotSupportsSRV (LedgerBigPeerSnapshotV23 pt magic pools) =
     Codec.encodeListLen 2
  <> Codec.encodeWord8 2 -- internal version
  <> Codec.encodeListLen 3
  <> encodeLedgerPeerSnapshotPoint pt
  <> Codec.encodeWord32 (unNetworkMagic magic)
  <> encodeBigStakePools pools

encodeLedgerPeerSnapshot _LedgerPeerSnapshotSupportsSRV (LedgerAllPeerSnapshotV23 pt magic pools) =
     Codec.encodeListLen 2
  <> Codec.encodeWord8 3 -- internal version
  <> Codec.encodeListLen 3
  <> encodeLedgerPeerSnapshotPoint pt
  <> Codec.encodeWord32 (unNetworkMagic magic)
  <> encodeAllStakePools pools


decodeLedgerPeerSnapshot :: forall s blk.
                            ( FromCBOR (HeaderHash blk)
                            , ToCBOR (HeaderHash blk)
                            , ToJSON (HeaderHash blk)
                            , StandardHash blk
                            , Typeable blk)
                         => Proxy blk
                         -> Codec.Decoder s (SomeLedgerPeerSnapshot)
decodeLedgerPeerSnapshot proxy = do
    Codec.decodeListLenOf 2
    version <- Codec.decodeWord8
    case version of
      1 -> Codec.decodeListLenOf 2 >>
             SomeLedgerPeerSnapshot .
             LedgerPeerSnapshotV2 <$> ((,) <$> decodeWithOrigin <*> fromCBOR)
      2 -> Codec.decodeListLenOf 3 >>
             SomeLedgerPeerSnapshot <$>
             (LedgerBigPeerSnapshotV23 <$> decodeLedgerPeerSnapshotPoint proxy
                                       <*> (NetworkMagic <$> Codec.decodeWord32)
                                       <*> decodeBigStakePools)
      3 -> Codec.decodeListLenOf 3 >>
             SomeLedgerPeerSnapshot <$>
             (LedgerAllPeerSnapshotV23 <$> decodeLedgerPeerSnapshotPoint proxy
                                       <*> (NetworkMagic <$> Codec.decodeWord32)
                                       <*> decodeAllStakePools)
      _ -> fail $ "LedgerPeers.Type: no decoder could be found for version " <> show version


encodeWithOrigin :: WithOrigin SlotNo -> Codec.Encoding
encodeWithOrigin Origin      = Codec.encodeListLen 1 <> Codec.encodeWord8 0
encodeWithOrigin (At slotNo) = Codec.encodeListLen 2 <> Codec.encodeWord8 1 <> toCBOR slotNo


decodeWithOrigin :: Codec.Decoder s (WithOrigin SlotNo)
decodeWithOrigin = do
    listLen <- Codec.decodeListLen
    tag     <- Codec.decodeWord8
    case (listLen, tag) of
      (1, 0) -> pure Origin
      (1, _) -> fail "LedgerPeers.Type: Expected tag for Origin constructor"
      (2, 1) -> At <$> fromCBOR
      (2, _) -> fail "LedgerPeers.Type: Expected tag for At constructor"
      _      -> fail "LedgerPeers.Type: Unrecognized list length while decoding WithOrigin SlotNo"


encodeLedgerPeerSnapshotPoint :: Point SomeHashableBlock
                              -> Codec.Encoding
encodeLedgerPeerSnapshotPoint = \case
  GenesisPoint -> Codec.encodeListLen 1 <> Codec.encodeWord8 0
  BlockPoint { atSlot, withHash = SomeHashableBlock (Proxy :: Proxy blk) hash} ->
       Codec.encodeListLen 3 <> Codec.encodeWord8 1
    <> Codec.toCBOR atSlot <> toCBOR hash


decodeLedgerPeerSnapshotPoint :: forall blk s.
                                 ( FromCBOR (HeaderHash blk)
                                 , ToCBOR (HeaderHash blk)
                                 , ToJSON (HeaderHash blk)
                                 , Typeable blk
                                 , StandardHash blk)
                              => Proxy blk -> Codec.Decoder s (Point SomeHashableBlock)
decodeLedgerPeerSnapshotPoint _proxy = do
  listLen <- Codec.decodeListLen
  tag     <- Codec.decodeWord8
  case (tag, listLen) of
    (0, 1) -> pure $ Point Origin
    (0, n) -> fail $ "LedgerPeers.Type: invalid listLen for Origin tag, expected 1 got " <> show n
    (1, 3) -> Point . At <$> (Block <$> fromCBOR <*> (SomeHashableBlock (Proxy :: Proxy blk) <$> fromCBOR))
    (1, n) -> fail $ "LedgerPeers.Type: invalid listLen for At tag, expected 3 got " <> show n
    _      -> fail "LedgerPeers.Type: Unrecognized CBOR encoding of Point for LedgerPeerSnapshot"


encodeBigStakePools :: [(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))]
                    -> Codec.Encoding
encodeBigStakePools pools =
     Codec.encodeListLenIndef
  <> foldMap (\(AccPoolStake accPoolStake, (PoolStake poolStake, relays)) ->
                  Codec.encodeListLen 3
               <> toCBOR accPoolStake
               <> toCBOR poolStake
               <> toCBOR relays
             )
             pools
  <> Codec.encodeBreak


decodeBigStakePools :: Codec.Decoder s [(AccPoolStake, (PoolStake, NonEmpty LedgerRelayAccessPoint))]
decodeBigStakePools = do
  Codec.decodeListLenIndef
  Codec.decodeSequenceLenIndef
         (flip (:)) [] reverse
         do
           Codec.decodeListLenOf 3
           accPoolStake <- AccPoolStake <$> fromCBOR
           poolStake    <- PoolStake <$> fromCBOR
           relays       <- fromCBOR
           return (accPoolStake, (poolStake, relays))


encodeAllStakePools :: [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
                    -> Codec.Encoding
encodeAllStakePools pools =
     Codec.encodeListLenIndef
  <> foldMap (\(PoolStake poolStake, relays) ->
                  Codec.encodeListLen 2
               <> toCBOR poolStake
               <> toCBOR relays
             )
             pools
  <> Codec.encodeBreak


decodeAllStakePools :: Codec.Decoder s [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
decodeAllStakePools = do
  Codec.decodeListLenIndef
  Codec.decodeSequenceLenIndef
         (flip (:)) [] reverse
         do
           Codec.decodeListLenOf 2
           poolStake    <- PoolStake <$> fromCBOR
           relays       <- fromCBOR
           return (poolStake, relays)


-- | Used by functions to indicate what kind of ledger peer to process
--
data LedgerPeersKind = AllLedgerPeers | BigLedgerPeers
  deriving (Eq, Show)


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
  deriving newtype (Fractional, Num, NFData, FromJSON, ToJSON, ToCBOR, FromCBOR)
  -- the ToCBOR and FromCBOR instances can be removed once V22 is no longer supported


-- | The accumulated relative stake of a stake pool, like PoolStake but it also includes the
-- relative stake of all preceding pools. A value in the range [0, 1].
--
newtype AccPoolStake = AccPoolStake { unAccPoolStake :: Rational }
  deriving (Eq, Ord, Show)
  deriving newtype (Fractional, Num, NFData, FromJSON, ToJSON, FromCBOR, ToCBOR)
  -- the ToCBOR and FromCBOR instances can be removed once V22 is no longer supported


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

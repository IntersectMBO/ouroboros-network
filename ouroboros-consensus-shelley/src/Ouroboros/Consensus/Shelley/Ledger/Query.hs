{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Query (
    BlockQuery (.., GetUTxO, GetFilteredUTxO)
  , KESConfig (..)
  , NonMyopicMemberRewards (..)
  , StakeSnapshot (..)
  , StakeSnapshots (..)
  , querySupportedVersion
    -- * Serialisation
  , decodeShelleyQuery
  , decodeShelleyResult
  , encodeShelleyQuery
  , encodeShelleyResult
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Control.DeepSeq (NFData)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime)
import           Data.Type.Equality (apply)
import           Data.Typeable (Typeable)
import           Data.UMap (View (..), domRestrictedView)
import           GHC.Generics (Generic)
import           GHC.Word (Word64)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen,
                     enforceSize)
import           Cardano.Slotting.Time (getSystemStart)

import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     encodePoint, mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query hiding (getSystemStart)
import           Ouroboros.Consensus.Util (ShowProxy (..))

import           Cardano.Ledger.Compactible (Compactible (fromCompact))
import qualified Cardano.Ledger.Core as LC
import           Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.EpochBoundary as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL (RewardAccounts)
import qualified Cardano.Ledger.Shelley.RewardProvenance as SL
                     (RewardProvenance)
import qualified Data.VMap as VMap

import           Cardano.Ledger.Crypto (Crypto)
import           Cardano.Ledger.Serialization (decodeRecordNamed,
                     utcTimeFromCBOR, utcTimeToCBOR)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
                     (ShelleyNodeToClientVersion (..))
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

newtype NonMyopicMemberRewards c = NonMyopicMemberRewards {
      unNonMyopicMemberRewards ::
        Map (Either SL.Coin (SL.Credential 'SL.Staking c))
            (Map (SL.KeyHash 'SL.StakePool c) SL.Coin)
    }
  deriving stock   (Show)
  deriving newtype (Eq)

type Delegations c =
  Map (SL.Credential 'SL.Staking c)
      (SL.KeyHash 'SL.StakePool c)

instance Crypto c => Serialise (NonMyopicMemberRewards c) where
  encode = toCBOR . unNonMyopicMemberRewards
  decode = NonMyopicMemberRewards <$> fromCBOR

data KESConfig = KESConfig {
    kcSlotsPerKESPeriod :: Word64
  , kcMaxKESEvolutions  :: Word64
  , kcSystemStart       :: UTCTime
  } deriving (Eq, Show)

instance FromCBOR KESConfig where
  fromCBOR = do
    decodeRecordNamed "KESConfig" (const 3) $ do
      kcSlotsPerKESPeriod <- fromCBOR
      kcMaxKESEvolutions <- fromCBOR
      kcSystemStart <- utcTimeFromCBOR
      pure $ KESConfig
        kcSlotsPerKESPeriod
        kcMaxKESEvolutions
        kcSystemStart

instance ToCBOR KESConfig where
  toCBOR KESConfig { kcSlotsPerKESPeriod, kcMaxKESEvolutions, kcSystemStart } =
    encodeListLen 3
      <> toCBOR kcSlotsPerKESPeriod
      <> toCBOR kcMaxKESEvolutions
      <> utcTimeToCBOR kcSystemStart

data instance BlockQuery (ShelleyBlock proto era) :: Type -> Type where
  GetLedgerTip :: BlockQuery (ShelleyBlock proto era) (Point (ShelleyBlock proto era))
  GetEpochNo :: BlockQuery (ShelleyBlock proto era) EpochNo
  -- | Calculate the Non-Myopic Pool Member Rewards for a set of
  -- credentials. See 'SL.getNonMyopicMemberRewards'
  GetNonMyopicMemberRewards
    :: Set (Either SL.Coin (SL.Credential 'SL.Staking (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era) (NonMyopicMemberRewards (EraCrypto era))
  GetCurrentPParams
    :: BlockQuery (ShelleyBlock proto era) (LC.PParams era)
  GetProposedPParamsUpdates
    :: BlockQuery (ShelleyBlock proto era) (SL.ProposedPPUpdates era)
  -- | This gets the stake distribution, but not in terms of _active_ stake
  -- (which we need for the leader schedule), but rather in terms of _total_
  -- stake, which is relevant for rewards. It is used by the wallet to show
  -- saturation levels to the end user. We should consider refactoring this, to
  -- an endpoint that provides all the information that the wallet wants about
  -- pools, in an extensible fashion.
  GetStakeDistribution
    :: BlockQuery (ShelleyBlock proto era) (SL.PoolDistr (EraCrypto era))

  -- | Get a subset of the UTxO, filtered by address. Although this will
  -- typically return a lot less data than 'GetUTxOWhole', it requires a linear
  -- search over the UTxO and so cost O(n) time.
  --
  -- Only 'GetUTxOByTxIn' is efficient in time and space.
  --
  GetUTxOByAddress
    :: Set (SL.Addr (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)

  -- | Get the /entire/ UTxO. This is only suitable for debug/testing purposes
  -- because otherwise it is far too much data.
  --
  GetUTxOWhole
    :: BlockQuery (ShelleyBlock proto era) (SL.UTxO era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugEpochState
    :: BlockQuery (ShelleyBlock proto era) (SL.EpochState era)

  -- | Wrap the result of the query using CBOR-in-CBOR.
  --
  -- For example, when a client is running a different version than the server
  -- and it sends a 'DebugEpochState' query, the client's decoder might fail to
  -- deserialise the epoch state as it might have changed between the two
  -- different versions. The client will then disconnect.
  --
  -- By using CBOR-in-CBOR, the client always successfully decodes the outer
  -- CBOR layer (so no disconnect) and can then manually try to decode the
  -- inner result. When the client's decoder is able to decode the inner
  -- result, it has access to the deserialised epoch state. When it fails to
  -- decode it, the client can fall back to pretty printing the actual CBOR,
  -- which is better than no output at all.
  GetCBOR
    :: BlockQuery (ShelleyBlock proto era) result
    -> BlockQuery (ShelleyBlock proto era) (Serialised result)

  GetFilteredDelegationsAndRewardAccounts
    :: Set (SL.Credential 'SL.Staking (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era)
             (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))

  GetKESConfig
    :: BlockQuery (ShelleyBlock proto era) KESConfig

  GetGenesisConfig
    :: BlockQuery (ShelleyBlock proto era) (CompactGenesis era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugNewEpochState
    :: BlockQuery (ShelleyBlock proto era) (SL.NewEpochState era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR').
  DebugChainDepState
    :: BlockQuery (ShelleyBlock proto era) (ChainDepState proto)

  GetRewardProvenance
    :: BlockQuery (ShelleyBlock proto era) (SL.RewardProvenance (EraCrypto era))

  -- | Get a subset of the UTxO, filtered by transaction input. This is
  -- efficient and costs only O(m * log n) for m inputs and a UTxO of size n.
  --
  GetUTxOByTxIn
    :: Set (SL.TxIn (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)

  GetStakePools
    :: BlockQuery (ShelleyBlock proto era)
                  (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))

  GetStakePoolParams
    :: Set (SL.KeyHash 'SL.StakePool (EraCrypto era))
    -> BlockQuery (ShelleyBlock proto era)
                  (Map (SL.KeyHash 'SL.StakePool (EraCrypto era))
                       (SL.PoolParams (EraCrypto era)))

  GetRewardInfoPools
    :: BlockQuery (ShelleyBlock proto era)
                  (SL.RewardParams,
                    Map (SL.KeyHash 'SL.StakePool (EraCrypto era))
                        (SL.RewardInfoPool))

  GetPoolState
    :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era)
                  (SL.PState (EraCrypto era))

  GetStakeSnapshots
    :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era)
                  (StakeSnapshots (EraCrypto era))

  GetPoolDistr
    :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
    -> BlockQuery (ShelleyBlock proto era)
                  (SL.PoolDistr (EraCrypto era))

  -- WARNING: please add new queries to the end of the list and stick to this
  -- order in all other pattern matches on queries. This helps in particular
  -- with the en/decoders, as we want the CBOR tags to be ordered.
  --
  -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@ must
  -- be added. See #2830 for a template on how to do this.
  --
  -- WARNING: never modify an existing query that has been incorporated in a
  -- release of the node, as it will break compatibility with deployed nodes.
  -- Instead, add a new query. To remove the old query, first to stop supporting
  -- it by modifying 'querySupportedVersion' (@< X@) and when the version is no
  -- longer used (because mainnet has hard-forked to a newer version), it can be
  -- removed.

pattern GetUTxO :: BlockQuery (ShelleyBlock proto era) (SL.UTxO era)
pattern GetUTxO = GetUTxOWhole
{-# DEPRECATED GetUTxO "Use 'GetUTxOWhole'" #-}

pattern GetFilteredUTxO :: Set (SL.Addr (EraCrypto era))
                        -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)
pattern GetFilteredUTxO x = GetUTxOByAddress x
{-# DEPRECATED GetFilteredUTxO "Use 'GetUTxOByAddress'" #-}


instance (Typeable era, Typeable proto)
  => ShowProxy (BlockQuery (ShelleyBlock proto era)) where

instance (ShelleyCompatible proto era, ProtoCrypto proto ~ crypto) => QueryLedger (ShelleyBlock proto era) where
  answerBlockQuery cfg query ext =
      case query of
        GetLedgerTip ->
          shelleyLedgerTipPoint lst
        GetEpochNo ->
          SL.nesEL st
        GetNonMyopicMemberRewards creds ->
          NonMyopicMemberRewards $
            SL.getNonMyopicMemberRewards globals st creds
        GetCurrentPParams ->
          getPParams st
        GetProposedPParamsUpdates ->
          getProposedPPUpdates st
        GetStakeDistribution ->
          SL.poolsByTotalStakeFraction globals st
        GetUTxOByAddress addrs ->
          SL.getFilteredUTxO st addrs
        GetUTxOWhole ->
          SL.getUTxO st
        DebugEpochState ->
          getEpochState st
        GetCBOR query' ->
          mkSerialised (encodeShelleyResult query') $
            answerBlockQuery cfg query' ext
        GetFilteredDelegationsAndRewardAccounts creds ->
          getFilteredDelegationsAndRewardAccounts st creds
        GetGenesisConfig ->
          compactGenesis $ shelleyGenesis $ topLevelConfigBlock $ getExtLedgerCfg cfg
        DebugNewEpochState ->
          st
        DebugChainDepState ->
          headerStateChainDep hst
        GetRewardProvenance ->
          snd $ SL.getRewardProvenance globals st
        GetUTxOByTxIn txins ->
          SL.getUTxOSubset st txins
        GetStakePools ->
          SL.getPools st
        GetStakePoolParams poolids ->
          SL.getPoolParameters st poolids
        GetRewardInfoPools ->
          SL.getRewardInfoPools globals st
        GetPoolState mPoolIds ->
          let dpsPState = SL.dpsPState . SL.lsDPState . SL.esLState . SL.nesEs $ st in
          case mPoolIds of
            Just poolIds ->
              SL.PState
                { SL._pParams  = Map.restrictKeys (SL._pParams  dpsPState) poolIds
                , SL._fPParams = Map.restrictKeys (SL._fPParams dpsPState) poolIds
                , SL._retiring = Map.restrictKeys (SL._retiring dpsPState) poolIds
                }
            Nothing -> dpsPState
        GetStakeSnapshots mPoolIds ->
          let SL.SnapShots
                { SL._pstakeMark
                , SL._pstakeSet
                , SL._pstakeGo
                } = SL.esSnapshots . SL.nesEs $ st

              -- | Sum all the stake that is held by the pool
              getPoolStake :: KeyHash 'StakePool crypto -> SL.SnapShot crypto -> SL.Coin
              getPoolStake hash ss = VMap.foldMap fromCompact s
                where
                  SL.Stake s = SL.poolStake hash (SL._delegations ss) (SL._stake ss)

              getPoolStakes :: Set (KeyHash 'StakePool crypto) -> Map (KeyHash 'StakePool crypto) (StakeSnapshot crypto)
              getPoolStakes poolIds = Map.fromSet mkStakeSnapshot poolIds
                where mkStakeSnapshot poolId = StakeSnapshot
                        { ssMarkPool = getPoolStake poolId _pstakeMark
                        , ssSetPool  = getPoolStake poolId _pstakeSet
                        , ssGoPool   = getPoolStake poolId _pstakeGo
                        }

              getAllStake :: SL.SnapShot crypto -> SL.Coin
              getAllStake (SL.SnapShot stake _ _) = VMap.foldMap fromCompact (SL.unStake stake)
          in

          case mPoolIds of
            Nothing ->
              StakeSnapshots
                { ssStakeSnapshots = getPoolStakes (Set.fromList (VMap.elems (SL._delegations _pstakeMark)))
                , ssMarkTotal      = getAllStake _pstakeMark
                , ssSetTotal       = getAllStake _pstakeSet
                , ssGoTotal        = getAllStake _pstakeGo
                }
            Just poolIds ->
              StakeSnapshots
                { ssStakeSnapshots = getPoolStakes poolIds
                , ssMarkTotal      = getAllStake _pstakeMark
                , ssSetTotal       = getAllStake _pstakeSet
                , ssGoTotal        = getAllStake _pstakeGo
                }

        GetPoolDistr mPoolIds ->
          let stakeSet = SL._pstakeSet . SL.esSnapshots $ getEpochState st in
          SL.calculatePoolDistr' (maybe (const True) (flip Set.member) mPoolIds) stakeSet
    where
      lcfg    = configLedger $ getExtLedgerCfg cfg
      globals = shelleyLedgerGlobals lcfg
      -- NOTE: we are not pattern matching on @ext@ but using the accessors
      -- here. The reason for that is that that pattern match blows up the
      -- compile time (in particular the time spent desugaring, which is when
      -- the compiler looks at pattern matches) to 2m30s! We don't really
      -- understand why, but our guess is that it has to do with the combination
      -- of the strictness of 'ExtLedgerState', the fact that @LedgerState@ is a
      -- data family, and the 'ShelleyBasedEra' constraint.
      lst = ledgerState ext
      hst = headerState ext
      st  = shelleyLedgerState lst

instance SameDepIndex (BlockQuery (ShelleyBlock proto era)) where
  sameDepIndex GetLedgerTip GetLedgerTip
    = Just Refl
  sameDepIndex GetLedgerTip _
    = Nothing
  sameDepIndex GetEpochNo GetEpochNo
    = Just Refl
  sameDepIndex GetEpochNo _
    = Nothing
  sameDepIndex (GetNonMyopicMemberRewards creds) (GetNonMyopicMemberRewards creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetNonMyopicMemberRewards _) _
    = Nothing
  sameDepIndex GetCurrentPParams GetCurrentPParams
    = Just Refl
  sameDepIndex GetCurrentPParams _
    = Nothing
  sameDepIndex GetProposedPParamsUpdates GetProposedPParamsUpdates
    = Just Refl
  sameDepIndex GetProposedPParamsUpdates _
    = Nothing
  sameDepIndex GetStakeDistribution GetStakeDistribution
    = Just Refl
  sameDepIndex GetStakeDistribution _
    = Nothing
  sameDepIndex (GetUTxOByAddress addrs) (GetUTxOByAddress addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetUTxOByAddress _) _
    = Nothing
  sameDepIndex GetUTxOWhole GetUTxOWhole
    = Just Refl
  sameDepIndex GetUTxOWhole _
    = Nothing
  sameDepIndex DebugEpochState DebugEpochState
    = Just Refl
  sameDepIndex DebugEpochState _
    = Nothing
  sameDepIndex (GetCBOR q) (GetCBOR q')
    = apply Refl <$> sameDepIndex q q'
  sameDepIndex (GetCBOR _) _
    = Nothing
  sameDepIndex (GetFilteredDelegationsAndRewardAccounts creds)
               (GetFilteredDelegationsAndRewardAccounts creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetFilteredDelegationsAndRewardAccounts _) _
    = Nothing
  sameDepIndex GetGenesisConfig GetGenesisConfig
    = Just Refl
  sameDepIndex GetGenesisConfig _
    = Nothing
  sameDepIndex DebugNewEpochState DebugNewEpochState
    = Just Refl
  sameDepIndex DebugNewEpochState _
    = Nothing
  sameDepIndex DebugChainDepState DebugChainDepState
    = Just Refl
  sameDepIndex DebugChainDepState _
    = Nothing
  sameDepIndex GetRewardProvenance GetRewardProvenance
    = Just Refl
  sameDepIndex GetRewardProvenance _
    = Nothing
  sameDepIndex (GetUTxOByTxIn addrs) (GetUTxOByTxIn addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetUTxOByTxIn _) _
    = Nothing
  sameDepIndex GetStakePools GetStakePools
    = Just Refl
  sameDepIndex GetStakePools _
    = Nothing
  sameDepIndex (GetStakePoolParams poolids) (GetStakePoolParams poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetStakePoolParams _) _
    = Nothing
  sameDepIndex GetRewardInfoPools GetRewardInfoPools
    = Just Refl
  sameDepIndex GetRewardInfoPools _
    = Nothing
  sameDepIndex (GetPoolState poolids) (GetPoolState poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetPoolState _) _
    = Nothing
  sameDepIndex (GetStakeSnapshots poolid) (GetStakeSnapshots poolid')
    | poolid == poolid'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetStakeSnapshots _) _
    = Nothing
  sameDepIndex (GetPoolDistr poolids) (GetPoolDistr poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetPoolDistr _) _
    = Nothing
  sameDepIndex GetKESConfig GetKESConfig
    = Just Refl
  sameDepIndex GetKESConfig _
    = Nothing

deriving instance Eq   (BlockQuery (ShelleyBlock proto era) result)
deriving instance Show (BlockQuery (ShelleyBlock proto era) result)

instance ShelleyCompatible proto era => ShowQuery (BlockQuery (ShelleyBlock proto era)) where
  showResult = \case
      GetLedgerTip                               -> show
      GetEpochNo                                 -> show
      GetNonMyopicMemberRewards {}               -> show
      GetCurrentPParams                          -> show
      GetProposedPParamsUpdates                  -> show
      GetStakeDistribution                       -> show
      GetUTxOByAddress {}                        -> show
      GetUTxOWhole                               -> show
      DebugEpochState                            -> show
      GetCBOR {}                                 -> show
      GetFilteredDelegationsAndRewardAccounts {} -> show
      GetGenesisConfig                           -> show
      DebugNewEpochState                         -> show
      DebugChainDepState                         -> show
      GetRewardProvenance                        -> show
      GetUTxOByTxIn {}                           -> show
      GetStakePools                              -> show
      GetStakePoolParams {}                      -> show
      GetRewardInfoPools                         -> show
      GetPoolState {}                            -> show
      GetStakeSnapshots {}                       -> show
      GetPoolDistr {}                            -> show
      GetKESConfig                               -> show

-- | Is the given query supported by the given 'ShelleyNodeToClientVersion'?
querySupportedVersion :: BlockQuery (ShelleyBlock proto era) result -> ShelleyNodeToClientVersion -> Bool
querySupportedVersion = \case
    GetLedgerTip                               -> (>= v1)
    GetEpochNo                                 -> (>= v1)
    GetNonMyopicMemberRewards {}               -> (>= v1)
    GetCurrentPParams                          -> (>= v1)
    GetProposedPParamsUpdates                  -> (>= v1)
    GetStakeDistribution                       -> (>= v1)
    GetUTxOByAddress {}                        -> (>= v1)
    GetUTxOWhole                               -> (>= v1)
    DebugEpochState                            -> (>= v1)
    GetCBOR q                                  -> querySupportedVersion q
    GetFilteredDelegationsAndRewardAccounts {} -> (>= v1)
    GetGenesisConfig                           -> (>= v2)
    DebugNewEpochState                         -> (>= v2)
    DebugChainDepState                         -> (>= v2)
    GetRewardProvenance                        -> (>= v3)
    GetUTxOByTxIn {}                           -> (>= v4)
    GetStakePools                              -> (>= v4)
    GetStakePoolParams {}                      -> (>= v4)
    GetRewardInfoPools                         -> (>= v5)
    GetPoolState {}                            -> (>= v6)
    GetStakeSnapshots {}                       -> (>= v6)
    GetPoolDistr {}                            -> (>= v6)
    GetKESConfig                               -> undefined -- TODO
    -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@
    -- must be added. See #2830 for a template on how to do this.
  where
    v1 = ShelleyNodeToClientVersion1
    v2 = ShelleyNodeToClientVersion2
    v3 = ShelleyNodeToClientVersion3
    v4 = ShelleyNodeToClientVersion4
    v5 = ShelleyNodeToClientVersion5
    v6 = ShelleyNodeToClientVersion6

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getKESConfig :: ExtLedgerCfg (ShelleyBlock proto era) -> KESConfig
getKESConfig (ExtLedgerCfg extLedgerCfg) =
  let blockCfg = topLevelConfigBlock extLedgerCfg
  in KESConfig {
    kcSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod $ shelleyGenesis blockCfg
  , kcMaxKESEvolutions = SL.sgMaxKESEvolutions $ shelleyGenesis blockCfg
  , kcSystemStart = getSystemStart $ shelleySystemStart blockCfg
  }

getProposedPPUpdates ::
     ShelleyBasedEra era
  => SL.NewEpochState era -> SL.ProposedPPUpdates era
getProposedPPUpdates = SL.proposals . SL._ppups
                     . SL.lsUTxOState . SL.esLState . SL.nesEs

-- Get the current 'EpochState.' This is mainly for debugging.
getEpochState :: SL.NewEpochState era -> SL.EpochState era
getEpochState = SL.nesEs

getDState :: SL.NewEpochState era -> SL.DState (EraCrypto era)
getDState = SL.dpsDState . SL.lsDPState . SL.esLState . SL.nesEs

getFilteredDelegationsAndRewardAccounts ::
     SL.NewEpochState era
  -> Set (SL.Credential 'SL.Staking (EraCrypto era))
  -> (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))
getFilteredDelegationsAndRewardAccounts ss creds =
    (filteredDelegations, filteredRwdAcnts)
  where
    u = SL._unified $ getDState ss

    filteredDelegations = domRestrictedView creds $ Delegations u
    filteredRwdAcnts = domRestrictedView creds $ Rewards u

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeShelleyQuery ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock proto era) result -> Encoding
encodeShelleyQuery query = case query of
    GetLedgerTip ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 0
    GetEpochNo ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 1
    GetNonMyopicMemberRewards creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 2 <> toCBOR creds
    GetCurrentPParams ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 3
    GetProposedPParamsUpdates ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 4
    GetStakeDistribution ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 5
    GetUTxOByAddress addrs ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 6 <> toCBOR addrs
    GetUTxOWhole ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 7
    DebugEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 8
    GetCBOR query' ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 9 <> encodeShelleyQuery query'
    GetFilteredDelegationsAndRewardAccounts creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 10 <> toCBOR creds
    GetGenesisConfig ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 11
    DebugNewEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 12
    DebugChainDepState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 13
    GetRewardProvenance ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 14
    GetUTxOByTxIn txins ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 15 <> toCBOR txins
    GetStakePools ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 16
    GetStakePoolParams poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 17 <> toCBOR poolids
    GetRewardInfoPools ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 18
    GetPoolState poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 19 <> toCBOR poolids
    GetStakeSnapshots poolId ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 20 <> toCBOR poolId
    GetPoolDistr poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 21 <> toCBOR poolids
    GetKESConfig ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 22

decodeShelleyQuery ::
     ShelleyBasedEra era
  => Decoder s (SomeSecond BlockQuery (ShelleyBlock proto era))
decodeShelleyQuery = do
    len <- CBOR.decodeListLen
    tag <- CBOR.decodeWord8
    case (len, tag) of
      (1, 0)  -> return $ SomeSecond GetLedgerTip
      (1, 1)  -> return $ SomeSecond GetEpochNo
      (2, 2)  -> SomeSecond . GetNonMyopicMemberRewards <$> fromCBOR
      (1, 3)  -> return $ SomeSecond GetCurrentPParams
      (1, 4)  -> return $ SomeSecond GetProposedPParamsUpdates
      (1, 5)  -> return $ SomeSecond GetStakeDistribution
      (2, 6)  -> SomeSecond . GetUTxOByAddress <$> fromCBOR
      (1, 7)  -> return $ SomeSecond GetUTxOWhole
      (1, 8)  -> return $ SomeSecond DebugEpochState
      (2, 9)  -> (\(SomeSecond q) -> SomeSecond (GetCBOR q)) <$> decodeShelleyQuery
      (2, 10) -> SomeSecond . GetFilteredDelegationsAndRewardAccounts <$> fromCBOR
      (1, 11) -> return $ SomeSecond GetGenesisConfig
      (1, 12) -> return $ SomeSecond DebugNewEpochState
      (1, 13) -> return $ SomeSecond DebugChainDepState
      (1, 14) -> return $ SomeSecond GetRewardProvenance
      (2, 15) -> SomeSecond . GetUTxOByTxIn <$> fromCBOR
      (1, 16) -> return $ SomeSecond GetStakePools
      (2, 17) -> SomeSecond . GetStakePoolParams <$> fromCBOR
      (1, 18) -> return $ SomeSecond GetRewardInfoPools
      (2, 19) -> SomeSecond . GetPoolState <$> fromCBOR
      (2, 20) -> SomeSecond . GetStakeSnapshots <$> fromCBOR
      (2, 21) -> SomeSecond . GetPoolDistr <$> fromCBOR
      _       -> fail $
        "decodeShelleyQuery: invalid (len, tag): (" <>
        show len <> ", " <> show tag <> ")"

encodeShelleyResult ::
     ShelleyCompatible proto era
  => BlockQuery (ShelleyBlock proto era) result -> result -> Encoding
encodeShelleyResult query = case query of
    GetLedgerTip                               -> encodePoint encode
    GetEpochNo                                 -> encode
    GetNonMyopicMemberRewards {}               -> encode
    GetCurrentPParams                          -> toCBOR
    GetProposedPParamsUpdates                  -> toCBOR
    GetStakeDistribution                       -> toCBOR
    GetUTxOByAddress {}                        -> toCBOR
    GetUTxOWhole                               -> toCBOR
    DebugEpochState                            -> toCBOR
    GetCBOR {}                                 -> encode
    GetFilteredDelegationsAndRewardAccounts {} -> toCBOR
    GetGenesisConfig                           -> toCBOR
    DebugNewEpochState                         -> toCBOR
    DebugChainDepState                         -> encode
    GetRewardProvenance                        -> toCBOR
    GetUTxOByTxIn {}                           -> toCBOR
    GetStakePools                              -> toCBOR
    GetStakePoolParams {}                      -> toCBOR
    GetRewardInfoPools                         -> toCBOR
    GetPoolState {}                            -> toCBOR
    GetStakeSnapshots {}                       -> toCBOR
    GetPoolDistr {}                            -> toCBOR
    GetKESConfig                               -> toCBOR

decodeShelleyResult ::
     ShelleyCompatible proto era
  => BlockQuery (ShelleyBlock proto era) result
  -> forall s. Decoder s result
decodeShelleyResult query = case query of
    GetLedgerTip                               -> decodePoint decode
    GetEpochNo                                 -> decode
    GetNonMyopicMemberRewards {}               -> decode
    GetCurrentPParams                          -> fromCBOR
    GetProposedPParamsUpdates                  -> fromCBOR
    GetStakeDistribution                       -> fromCBOR
    GetUTxOByAddress {}                        -> fromCBOR
    GetUTxOWhole                               -> fromCBOR
    DebugEpochState                            -> fromCBOR
    GetCBOR {}                                 -> decode
    GetFilteredDelegationsAndRewardAccounts {} -> fromCBOR
    GetGenesisConfig                           -> fromCBOR
    DebugNewEpochState                         -> fromCBOR
    DebugChainDepState                         -> decode
    GetRewardProvenance                        -> fromCBOR
    GetUTxOByTxIn {}                           -> fromCBOR
    GetStakePools                              -> fromCBOR
    GetStakePoolParams {}                      -> fromCBOR
    GetRewardInfoPools                         -> fromCBOR
    GetPoolState {}                            -> fromCBOR
    GetStakeSnapshots {}                       -> fromCBOR
    GetPoolDistr {}                            -> fromCBOR
    GetKESConfig                               -> fromCBOR

-- | The stake snapshot returns information about the mark, set, go ledger snapshots for a pool,
-- plus the total active stake for each snapshot that can be used in a 'sigma' calculation.
--
-- Each snapshot is taken at the end of a different era. The go snapshot is the current one and
-- was taken two epochs earlier, set was taken one epoch ago, and mark was taken immediately
-- before the start of the current epoch.
data StakeSnapshot crypto = StakeSnapshot
  { ssMarkPool :: !SL.Coin
  , ssSetPool  :: !SL.Coin
  , ssGoPool   :: !SL.Coin
  } deriving (Eq, Show, Generic)

instance NFData (StakeSnapshot crypto)

instance
  Crypto crypto =>
  ToCBOR (StakeSnapshot crypto)
  where
  toCBOR
    StakeSnapshot
    { ssMarkPool
    , ssSetPool
    , ssGoPool
    } = encodeListLen 3
      <> toCBOR ssMarkPool
      <> toCBOR ssSetPool
      <> toCBOR ssGoPool

instance
  Crypto crypto =>
  FromCBOR (StakeSnapshot crypto)
  where
  fromCBOR = do
    enforceSize "StakeSnapshot" 3
    StakeSnapshot
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

data StakeSnapshots crypto = StakeSnapshots
  { ssStakeSnapshots  :: !(Map (SL.KeyHash 'SL.StakePool crypto) (StakeSnapshot crypto))
  , ssMarkTotal       :: !SL.Coin
  , ssSetTotal        :: !SL.Coin
  , ssGoTotal         :: !SL.Coin
  } deriving (Eq, Show, Generic)

instance NFData (StakeSnapshots crypto)

instance
  Crypto crypto =>
  ToCBOR (StakeSnapshots crypto)
  where
  toCBOR
    StakeSnapshots
    { ssStakeSnapshots
    , ssMarkTotal
    , ssSetTotal
    , ssGoTotal
    } = encodeListLen 4
      <> toCBOR ssStakeSnapshots
      <> toCBOR ssMarkTotal
      <> toCBOR ssSetTotal
      <> toCBOR ssGoTotal

instance
  Crypto crypto =>
  FromCBOR (StakeSnapshots crypto)
  where
  fromCBOR = do
    enforceSize "StakeSnapshots" 4
    StakeSnapshots
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

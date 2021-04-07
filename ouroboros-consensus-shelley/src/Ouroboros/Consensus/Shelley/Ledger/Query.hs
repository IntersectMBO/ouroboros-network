{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Query (
    BlockQuery (..)
  , NonMyopicMemberRewards (..)
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
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.Type.Equality (apply)
import           Data.Typeable (Typeable)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     encodePoint, mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Util (ShowProxy (..))

import qualified Cardano.Ledger.Core as LC
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL (RewardAccounts)
import qualified Shelley.Spec.Ledger.RewardProvenance as SL (RewardProvenance)

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
                     (ShelleyNodeToClientVersion (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosState (..))

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

instance SL.PraosCrypto c => Serialise (NonMyopicMemberRewards c) where
  encode = toCBOR . unNonMyopicMemberRewards
  decode = NonMyopicMemberRewards <$> fromCBOR

data instance BlockQuery (ShelleyBlock era) :: Type -> Type where
  GetLedgerTip :: BlockQuery (ShelleyBlock era) (Point (ShelleyBlock era))
  GetEpochNo :: BlockQuery (ShelleyBlock era) EpochNo
  -- | Calculate the Non-Myopic Pool Member Rewards for a set of
  -- credentials. See 'SL.getNonMyopicMemberRewards'
  GetNonMyopicMemberRewards
    :: Set (Either SL.Coin (SL.Credential 'SL.Staking (EraCrypto era)))
    -> BlockQuery (ShelleyBlock era) (NonMyopicMemberRewards (EraCrypto era))
  GetCurrentPParams
    :: BlockQuery (ShelleyBlock era) (LC.PParams era)
  GetProposedPParamsUpdates
    :: BlockQuery (ShelleyBlock era) (SL.ProposedPPUpdates era)
  -- | This gets the stake distribution, but not in terms of _active_ stake
  -- (which we need for the leader schedule), but rather in terms of _total_
  -- stake, which is relevant for rewards. It is used by the wallet to show
  -- saturation levels to the end user. We should consider refactoring this, to
  -- an endpoint that provides all the information that the wallet wants about
  -- pools, in an extensible fashion.
  GetStakeDistribution
    :: BlockQuery (ShelleyBlock era) (SL.PoolDistr (EraCrypto era))
  GetFilteredUTxO
    :: Set (SL.Addr (EraCrypto era))
    -> BlockQuery (ShelleyBlock era) (SL.UTxO era)
  GetUTxO
    :: BlockQuery (ShelleyBlock era) (SL.UTxO era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugEpochState
    :: BlockQuery (ShelleyBlock era) (SL.EpochState era)

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
    :: BlockQuery (ShelleyBlock era) result
    -> BlockQuery (ShelleyBlock era) (Serialised result)

  GetFilteredDelegationsAndRewardAccounts
    :: Set (SL.Credential 'SL.Staking (EraCrypto era))
    -> BlockQuery (ShelleyBlock era)
             (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))

  GetGenesisConfig
    :: BlockQuery (ShelleyBlock era) (CompactGenesis era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugNewEpochState
    :: BlockQuery (ShelleyBlock era) (SL.NewEpochState era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR').
  DebugChainDepState
    :: BlockQuery (ShelleyBlock era) (SL.ChainDepState (EraCrypto era))

  GetRewardProvenance
    :: BlockQuery (ShelleyBlock era) (SL.RewardProvenance (EraCrypto era))

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

instance Typeable era => ShowProxy (BlockQuery (ShelleyBlock era)) where

instance ShelleyBasedEra era => QueryLedger (ShelleyBlock era) where
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
        GetFilteredUTxO addrs ->
          SL.getFilteredUTxO st addrs
        GetUTxO ->
          SL.getUTxO st
        DebugEpochState ->
          getEpochState st
        GetCBOR query' ->
          mkSerialised (encodeShelleyResult query') $
            answerBlockQuery cfg query' ext
        GetFilteredDelegationsAndRewardAccounts creds ->
          getFilteredDelegationsAndRewardAccounts st creds
        GetGenesisConfig ->
          shelleyLedgerCompactGenesis lcfg
        DebugNewEpochState ->
          st
        DebugChainDepState ->
          tpraosStateChainDepState (headerStateChainDep hst)
        GetRewardProvenance ->
          snd $ SL.getRewardInfo globals st
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

instance SameDepIndex (BlockQuery (ShelleyBlock era)) where
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
  sameDepIndex (GetFilteredUTxO addrs) (GetFilteredUTxO addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetFilteredUTxO _) _
    = Nothing
  sameDepIndex GetUTxO GetUTxO
    = Just Refl
  sameDepIndex GetUTxO _
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

deriving instance Eq   (BlockQuery (ShelleyBlock era) result)
deriving instance Show (BlockQuery (ShelleyBlock era) result)

instance ShelleyBasedEra era => ShowQuery (BlockQuery (ShelleyBlock era)) where
  showResult = \case
      GetLedgerTip                               -> show
      GetEpochNo                                 -> show
      GetNonMyopicMemberRewards {}               -> show
      GetCurrentPParams                          -> show
      GetProposedPParamsUpdates                  -> show
      GetStakeDistribution                       -> show
      GetFilteredUTxO {}                         -> show
      GetUTxO                                    -> show
      DebugEpochState                            -> show
      GetCBOR {}                                 -> show
      GetFilteredDelegationsAndRewardAccounts {} -> show
      GetGenesisConfig                           -> show
      DebugNewEpochState                         -> show
      DebugChainDepState                         -> show
      GetRewardProvenance                        -> show

-- | Is the given query supported by the given 'ShelleyNodeToClientVersion'?
querySupportedVersion :: BlockQuery (ShelleyBlock era) result -> ShelleyNodeToClientVersion -> Bool
querySupportedVersion = \case
    GetLedgerTip                               -> (>= v1)
    GetEpochNo                                 -> (>= v1)
    GetNonMyopicMemberRewards {}               -> (>= v1)
    GetCurrentPParams                          -> (>= v1)
    GetProposedPParamsUpdates                  -> (>= v1)
    GetStakeDistribution                       -> (>= v1)
    GetFilteredUTxO {}                         -> (>= v1)
    GetUTxO                                    -> (>= v1)
    DebugEpochState                            -> (>= v1)
    GetCBOR q                                  -> querySupportedVersion q
    GetFilteredDelegationsAndRewardAccounts {} -> (>= v1)
    GetGenesisConfig                           -> (>= v2)
    DebugNewEpochState                         -> (>= v2)
    DebugChainDepState                         -> (>= v2)
    GetRewardProvenance                        -> (>= v3)
    -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@
    -- must be added. See #2830 for a template on how to do this.
  where
    v1 = ShelleyNodeToClientVersion1
    v2 = ShelleyNodeToClientVersion2
    v3 = ShelleyNodeToClientVersion3

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getProposedPPUpdates ::
     ShelleyBasedEra era
  => SL.NewEpochState era -> SL.ProposedPPUpdates era
getProposedPPUpdates = SL.proposals . SL._ppups
                     . SL._utxoState . SL.esLState . SL.nesEs

-- Get the current 'EpochState.' This is mainly for debugging.
getEpochState :: SL.NewEpochState era -> SL.EpochState era
getEpochState = SL.nesEs

getDState :: SL.NewEpochState era -> SL.DState (EraCrypto era)
getDState = SL._dstate . SL._delegationState . SL.esLState . SL.nesEs

getFilteredDelegationsAndRewardAccounts ::
     SL.NewEpochState era
  -> Set (SL.Credential 'SL.Staking (EraCrypto era))
  -> (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))
getFilteredDelegationsAndRewardAccounts ss creds =
    (filteredDelegations, filteredRwdAcnts)
  where
    SL.DState { _rewards = rewards, _delegations = delegations } = getDState ss
    filteredDelegations = Map.restrictKeys delegations creds
    filteredRwdAcnts = Map.restrictKeys rewards creds

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeShelleyQuery ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock era) result -> Encoding
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
    GetFilteredUTxO addrs ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 6 <> toCBOR addrs
    GetUTxO ->
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

decodeShelleyQuery ::
     ShelleyBasedEra era
  => Decoder s (SomeSecond BlockQuery (ShelleyBlock era))
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
      (2, 6)  -> SomeSecond . GetFilteredUTxO <$> fromCBOR
      (1, 7)  -> return $ SomeSecond GetUTxO
      (1, 8)  -> return $ SomeSecond DebugEpochState
      (2, 9)  -> (\(SomeSecond q) -> SomeSecond (GetCBOR q)) <$> decodeShelleyQuery
      (2, 10) -> SomeSecond . GetFilteredDelegationsAndRewardAccounts <$> fromCBOR
      (1, 11) -> return $ SomeSecond GetGenesisConfig
      (1, 12) -> return $ SomeSecond DebugNewEpochState
      (1, 13) -> return $ SomeSecond DebugChainDepState
      (1, 14) -> return $ SomeSecond GetRewardProvenance
      _       -> fail $
        "decodeShelleyQuery: invalid (len, tag): (" <>
        show len <> ", " <> show tag <> ")"

encodeShelleyResult ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock era) result -> result -> Encoding
encodeShelleyResult query = case query of
    GetLedgerTip                               -> encodePoint encode
    GetEpochNo                                 -> encode
    GetNonMyopicMemberRewards {}               -> encode
    GetCurrentPParams                          -> toCBOR
    GetProposedPParamsUpdates                  -> toCBOR
    GetStakeDistribution                       -> toCBOR
    GetFilteredUTxO {}                         -> toCBOR
    GetUTxO                                    -> toCBOR
    DebugEpochState                            -> toCBOR
    GetCBOR {}                                 -> encode
    GetFilteredDelegationsAndRewardAccounts {} -> toCBOR
    GetGenesisConfig                           -> toCBOR
    DebugNewEpochState                         -> toCBOR
    DebugChainDepState                         -> toCBOR
    GetRewardProvenance                        -> toCBOR

decodeShelleyResult ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock era) result
  -> forall s. Decoder s result
decodeShelleyResult query = case query of
    GetLedgerTip                               -> decodePoint decode
    GetEpochNo                                 -> decode
    GetNonMyopicMemberRewards {}               -> decode
    GetCurrentPParams                          -> fromCBOR
    GetProposedPParamsUpdates                  -> fromCBOR
    GetStakeDistribution                       -> fromCBOR
    GetFilteredUTxO {}                         -> fromCBOR
    GetUTxO                                    -> fromCBOR
    DebugEpochState                            -> fromCBOR
    GetCBOR {}                                 -> decode
    GetFilteredDelegationsAndRewardAccounts {} -> fromCBOR
    GetGenesisConfig                           -> fromCBOR
    DebugNewEpochState                         -> fromCBOR
    DebugChainDepState                         -> fromCBOR
    GetRewardProvenance                        -> fromCBOR

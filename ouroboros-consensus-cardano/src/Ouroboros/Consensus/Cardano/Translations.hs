{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Exposes various translation functions across `CardanoEras`.
--
-- NOTE: Most of these translation functions are temporary stop-gap
-- solutions to enable eras with different cryptos. Translation
-- functions are needed because most of the ledger data structures are
-- defined with a `c` parameter hence cannot be freely used with
-- different instances of `c` even though the underlying types defined
-- in `Crypto` do not change. These functions should disappear once we
-- have a separation between `Crypto` and `HeaderCrypto` such that
-- most ledger data structures do not depend on the latter.
module Ouroboros.Consensus.Cardano.Translations (
    translateCredentials
  , translateHotKey
  , translateInitialFunds
  , translateNewEpochState
  , translateShelleyGenesis
  , translateStaking
  , translateTx
  , translateTxId
  , translateValidatedTx
  ) where

import           Cardano.Ledger.Address (Addr (..), BootstrapAddress (..),
                     RewardAcnt (..))
import           Cardano.Ledger.Alonzo.Core (Tx)
import           Cardano.Ledger.BaseTypes (BlocksMade (..))
import qualified Cardano.Ledger.BaseTypes as BaseTypes
import           Cardano.Ledger.Core (PParams (..))
import           Cardano.Ledger.Credential (StakeReference (..))
import           Cardano.Ledger.Crypto (Crypto (..))
import           Cardano.Ledger.Keys (GenDelegPair (..))
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.PoolParams (PoolParams (..))
import           Cardano.Ledger.Shelley.API (Credential (..))
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Shelley.API.Types (NewEpochState (..))
import           Data.Coerce (coerce)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Block ()
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (translateCanBeLeader)
import           Ouroboros.Consensus.Protocol.Praos.Crypto (CanConvertVRF)
import           Ouroboros.Consensus.Protocol.Praos.Translate (translateKeyHash)
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..),
                     ShelleyGenesisStaking (..), ShelleyLeaderCredentials (..))
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()


translateHotKey :: (KES c1 ~ KES c2) => HotKey.HotKey c1 m -> HotKey.HotKey c2 m
translateHotKey HotKey.HotKey {evolve, getInfo, isPoisoned, sign_} =
  HotKey.HotKey {evolve, getInfo, isPoisoned, sign_}

translateCredentials :: (DSIGN c1 ~ DSIGN c2, KES c1 ~ KES c2, CanConvertVRF (VRF c1) (VRF c2)) => ShelleyLeaderCredentials c1 -> ShelleyLeaderCredentials c2
translateCredentials
  ShelleyLeaderCredentials {shelleyLeaderCredentialsInitSignKey,
                            shelleyLeaderCredentialsCanBeLeader, shelleyLeaderCredentialsLabel}
  = ShelleyLeaderCredentials { shelleyLeaderCredentialsInitSignKey
                             , shelleyLeaderCredentialsCanBeLeader = translateCanBeLeader shelleyLeaderCredentialsCanBeLeader
                             , shelleyLeaderCredentialsLabel}

translateShelleyGenesis ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ShelleyGenesis c1 -> ShelleyGenesis c2
translateShelleyGenesis
  ShelleyGenesis {sgSystemStart, sgNetworkMagic, sgNetworkId,
                  sgActiveSlotsCoeff, sgSecurityParam, sgEpochLength,
                  sgSlotsPerKESPeriod, sgMaxKESEvolutions, sgSlotLength,
                  sgUpdateQuorum, sgMaxLovelaceSupply, sgProtocolParams, sgGenDelegs,
                  sgInitialFunds, sgStaking}
  = ShelleyGenesis {sgSystemStart, sgNetworkMagic, sgNetworkId,
                  sgActiveSlotsCoeff, sgSecurityParam, sgEpochLength,
                  sgSlotsPerKESPeriod, sgMaxKESEvolutions, sgSlotLength,
                  sgUpdateQuorum, sgMaxLovelaceSupply, sgProtocolParams = translatePParams sgProtocolParams, sgGenDelegs = translateGenDelegs sgGenDelegs,
                  sgInitialFunds = translateInitialFunds sgInitialFunds, sgStaking = translateStaking sgStaking}

translateStaking ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ShelleyGenesisStaking c1 -> ShelleyGenesisStaking c2
translateStaking ShelleyGenesisStaking {sgsPools, sgsStake} = ShelleyGenesisStaking { sgsPools = translateStakingPools sgsPools, sgsStake = translateStake sgsStake }
  where
    translateStake :: ListMap.ListMap (SL.KeyHash 'SL.Staking c1) (SL.KeyHash 'SL.StakePool c1) -> ListMap.ListMap (SL.KeyHash 'SL.Staking c2) (SL.KeyHash 'SL.StakePool c2)
    translateStake = ListMap.foldrWithKey translateStakingPair mempty

    translateStakingPair ::
      (SL.KeyHash 'SL.Staking c1, SL.KeyHash 'SL.StakePool c1)
      -> ListMap.ListMap (SL.KeyHash 'SL.Staking c2) (SL.KeyHash 'SL.StakePool c2)
      -> ListMap.ListMap (SL.KeyHash 'SL.Staking c2) (SL.KeyHash 'SL.StakePool c2)
    translateStakingPair (stakeKey, stakePool) (ListMap.ListMap stakes) = ListMap.ListMap $ (coerce stakeKey, coerce stakePool) : stakes

translateStakingPools ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ListMap.ListMap (SL.KeyHash 'SL.StakePool c1) (SL.PoolParams c1)
  -> ListMap.ListMap (SL.KeyHash 'SL.StakePool c2) (SL.PoolParams c2)
translateStakingPools = ListMap.foldrWithKey f mempty
 where
   f :: (SL.KeyHash 'SL.StakePool c1, SL.PoolParams c1)
     -> ListMap.ListMap (SL.KeyHash 'SL.StakePool c2) (SL.PoolParams c2)
     -> ListMap.ListMap (SL.KeyHash 'SL.StakePool c2) (SL.PoolParams c2)
   f (kh, params) (ListMap.ListMap ps) = ListMap.ListMap $ (coerce kh, translatePoolParams params) : ps

   translatePoolParams :: SL.PoolParams c1 -> SL.PoolParams c2
   translatePoolParams
     PoolParams {ppId, ppVrf, ppPledge, ppCost, ppMargin, ppRewardAcnt,
                 ppOwners, ppRelays, ppMetadata}
     = PoolParams {ppId = coerce ppId, ppVrf = coerce ppVrf, ppPledge, ppCost, ppMargin, ppRewardAcnt = translateRewardAcnt ppRewardAcnt,
                 ppOwners = Set.map coerce ppOwners, ppRelays, ppMetadata}

   translateRewardAcnt :: SL.RewardAcnt c1 -> SL.RewardAcnt c2
   translateRewardAcnt (RewardAcnt net cre) = RewardAcnt net (translateCredential cre)

translateCredential ::
  forall c1 c2 r .
  (ADDRHASH c1 ~ ADDRHASH c2)
  => SL.Credential r c1 -> SL.Credential r c2
translateCredential (ScriptHashObj sh) = ScriptHashObj $ coerce sh
translateCredential (KeyHashObj kh)    = KeyHashObj $ coerce kh

translateInitialFunds ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2)
  => ListMap.ListMap (SL.Addr c1) SL.Coin -> ListMap.ListMap (SL.Addr c2) SL.Coin
translateInitialFunds = ListMap.mapKeys translateAddr
  where
    translateAddr :: SL.Addr c1 -> SL.Addr c2
    translateAddr (Addr net cre sr) = Addr net (translateCredential cre) (translateStakeReference sr)
    translateAddr (AddrBootstrap ba) = AddrBootstrap (translateBootstrap ba)

    translateStakeReference :: SL.StakeReference c1 -> SL.StakeReference c2
    translateStakeReference (StakeRefBase cre) = StakeRefBase (translateCredential cre)
    translateStakeReference (StakeRefPtr ptr) = StakeRefPtr ptr
    translateStakeReference StakeRefNull = StakeRefNull

    translateBootstrap :: BootstrapAddress c1 -> BootstrapAddress c2
    translateBootstrap (BootstrapAddress ad) = BootstrapAddress ad

translateGenDelegs ::
  forall c1 c2 .
  (HASH c1 ~ HASH c2, ADDRHASH c1 ~ ADDRHASH c2 )
  => Map.Map (SL.KeyHash 'SL.Genesis c1) (SL.GenDelegPair c1)
  -> Map.Map (SL.KeyHash 'SL.Genesis c2) (SL.GenDelegPair c2)
translateGenDelegs = Map.map g . Map.mapKeysMonotonic f
 where
  g :: SL.GenDelegPair c1 -> SL.GenDelegPair c2
  g GenDelegPair {genDelegKeyHash, genDelegVrfHash} =
    GenDelegPair {genDelegKeyHash = coerce genDelegKeyHash, genDelegVrfHash = coerce genDelegVrfHash}

  f :: SL.KeyHash 'SL.Genesis c1 -> SL.KeyHash 'SL.Genesis c2
  f = coerce

translatePParams :: PParams (ShelleyEra c1) -> PParams (ShelleyEra c2)
translatePParams (PParams pph) = PParams (coerce pph)

translateNewEpochState :: (ADDRHASH c1 ~  ADDRHASH c2, DSIGN c1 ~ DSIGN c2) => SL.NewEpochState (BabbageEra c1) -> SL.NewEpochState (BabbageEra c2)
translateNewEpochState
  SL.NewEpochState {nesEL, nesBprev, nesBcur, nesEs, nesRu, nesPd,
                 stashedAVVMAddresses}
  = SL.NewEpochState {nesEL, nesBprev = translateBlocksMade nesBprev, nesBcur = translateBlocksMade nesBcur, nesEs = translateEpochState nesEs, nesRu = undefined , nesPd = undefined ,
                 stashedAVVMAddresses}

translateBlocksMade :: (ADDRHASH c1 ~  ADDRHASH c2, DSIGN c1 ~ DSIGN c2) => BaseTypes.BlocksMade c1 -> BaseTypes.BlocksMade c2
translateBlocksMade BaseTypes.BlocksMade {unBlocksMade} =
  BaseTypes.BlocksMade $ Map.mapKeysMonotonic translateKeyHash unBlocksMade

translateEpochState :: SL.EpochState (BabbageEra c1) -> SL.EpochState (BabbageEra c2)
translateEpochState = undefined

translateTx :: Tx (BabbageEra c1) -> Tx (BabbageEra c2)
translateTx = undefined

translateValidatedTx :: SL.Validated (Tx (BabbageEra c1)) -> SL.Validated (Tx (BabbageEra c2))
translateValidatedTx = undefined

translateTxId :: SL.TxId c1 -> SL.TxId c2
translateTxId = undefined

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.PeerSelection () where

import           Data.Bifunctor (second)
import           Data.Foldable (toList)
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Ord (Down (..))
import           Data.Text.Encoding (encodeUtf8)

import           Ouroboros.Consensus.Ledger.SupportsPeerSelection

import           Shelley.Spec.Ledger.BaseTypes
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.TxBody as SL

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

instance LedgerSupportsPeerSelection (ShelleyBlock era) where
  getPeers ShelleyLedgerState { shelleyLedgerState } = catMaybes
      [ (poolStake,) <$> Map.lookup stakePool poolRelayAddresses
      | (stakePool, poolStake) <- orderByStake poolDistr
      ]
    where
      poolDistr :: SL.PoolDistr (EraCrypto era)
      poolDistr = SL.nesPd shelleyLedgerState

      -- | Sort stake pools by descending stake
      orderByStake ::
           SL.PoolDistr (EraCrypto era)
        -> [(SL.KeyHash 'SL.StakePool (EraCrypto era), PoolStake)]
      orderByStake =
            sortOn (Down . snd)
          . map (second SL.individualPoolStake)
          . Map.toList
          . SL.unPoolDistr

      futurePoolParams, poolParams ::
           Map (SL.KeyHash 'SL.StakePool (EraCrypto era)) (SL.PoolParams era)
      (futurePoolParams, poolParams) =
          (SL._fPParams pstate, SL._pParams pstate)
        where
          pstate :: SL.PState era
          pstate =
                SL._pstate
              . SL._delegationState
              . SL.esLState
              . SL.nesEs
              $ shelleyLedgerState

      relayToRelayAddress :: SL.StakePoolRelay -> Maybe RelayAddress
      relayToRelayAddress (SL.SingleHostAddr (SJust (Port port)) (SJust ipv4) _) =
          Just $ RelayAddressAddr (IPv4 ipv4) (fromIntegral port)
      relayToRelayAddress (SL.SingleHostAddr (SJust (Port port)) SNothing (SJust ipv6)) =
          Just $ RelayAddressAddr (IPv6 ipv6) (fromIntegral port)
      relayToRelayAddress (SL.SingleHostName (SJust (Port port)) dnsName) =
          Just $ RelayAddressDomain $ DomainAddress (encodeUtf8 $ dnsToText dnsName) (fromIntegral port)
      relayToRelayAddress _ =
          -- This could be unsupported relays (SRV records) or an unusable relay such as
          -- a relay with an ip address but without a port number.
          Nothing

      -- | Note that a stake pool can have multiple registered relays
      pparamsRelayAddresses ::
           (RelayAddress -> StakePoolRelay)
        -> SL.PoolParams era
        -> Maybe (NonEmpty StakePoolRelay)
      pparamsRelayAddresses injStakePoolRelay =
            NE.nonEmpty
          . mapMaybe (fmap injStakePoolRelay . relayToRelayAddress)
          . toList
          . SL._poolRelays

      -- | Combine the stake pools registered in the future and the current pool
      -- parameters, and remove duplicates.
      poolRelayAddresses ::
           Map (SL.KeyHash 'SL.StakePool (EraCrypto era)) (NonEmpty StakePoolRelay)
      poolRelayAddresses =
          Map.unionWith
            (\futureRelays currentRelays -> NE.nub (futureRelays <> currentRelays))
            (Map.mapMaybe (pparamsRelayAddresses FutureRelay)  futurePoolParams)
            (Map.mapMaybe (pparamsRelayAddresses CurrentRelay) poolParams)

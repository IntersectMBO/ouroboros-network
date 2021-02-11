{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.PeerSelection () where

import           Cardano.Prelude (forceElemsToWHNF)

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

instance c ~ EraCrypto era
      => LedgerSupportsPeerSelection (ShelleyBlock era) where
  getPeers ShelleyLedgerState { shelleyLedgerState } = catMaybes
      [ (poolStake,) <$> Map.lookup stakePool poolRelayAddresses
      | (stakePool, poolStake) <- orderByStake poolDistr
      ]
    where
      poolDistr :: SL.PoolDistr c
      poolDistr = SL.nesPd shelleyLedgerState

      -- | Sort stake pools by descending stake
      orderByStake ::
           SL.PoolDistr c
        -> [(SL.KeyHash 'SL.StakePool c, PoolStake)]
      orderByStake =
            sortOn (Down . snd)
          . map (second (PoolStake . SL.individualPoolStake))
          . Map.toList
          . SL.unPoolDistr

      futurePoolParams, poolParams ::
           Map (SL.KeyHash 'SL.StakePool c) (SL.PoolParams c)
      (futurePoolParams, poolParams) =
          (SL._fPParams pstate, SL._pParams pstate)
        where
          pstate :: SL.PState c
          pstate =
                SL._pstate
              . SL._delegationState
              . SL.esLState
              . SL.nesEs
              $ shelleyLedgerState

      relayToRelayAddress :: SL.StakePoolRelay -> Maybe RelayAddress
      relayToRelayAddress (SL.SingleHostAddr (SJust (Port port)) (SJust ipv4) _) =
          Just $ RelayAddress (IPv4 ipv4) (fromIntegral port)
      relayToRelayAddress (SL.SingleHostAddr (SJust (Port port)) SNothing (SJust ipv6)) =
          Just $ RelayAddress (IPv6 ipv6) (fromIntegral port)
      relayToRelayAddress (SL.SingleHostName (SJust (Port port)) dnsName) =
          Just $ RelayDomain $ DomainAddress (encodeUtf8 $ dnsToText dnsName) (fromIntegral port)
      relayToRelayAddress _ =
          -- This could be an unsupported relay (SRV records) or an unusable
          -- relay such as a relay with an IP address but without a port number.
          Nothing

      -- | Note that a stake pool can have multiple registered relays
      pparamsRelayAddresses ::
           (RelayAddress -> StakePoolRelay)
        -> SL.PoolParams c
        -> Maybe (NonEmpty StakePoolRelay)
      pparamsRelayAddresses injStakePoolRelay =
            NE.nonEmpty
          . forceElemsToWHNF
          . mapMaybe (fmap injStakePoolRelay . relayToRelayAddress)
          . toList
          . SL._poolRelays

      -- | Combine the stake pools registered in the future and the current pool
      -- parameters, and remove duplicates.
      poolRelayAddresses ::
           Map (SL.KeyHash 'SL.StakePool c) (NonEmpty StakePoolRelay)
      poolRelayAddresses =
          Map.unionWith
            (\futureRelays currentRelays -> NE.nub (futureRelays <> currentRelays))
            (Map.mapMaybe (pparamsRelayAddresses FutureRelay)  futurePoolParams)
            (Map.mapMaybe (pparamsRelayAddresses CurrentRelay) poolParams)

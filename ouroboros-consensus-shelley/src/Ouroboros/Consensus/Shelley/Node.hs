{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node (
    MaxMajorProtVer (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsAlonzo (..)
  , ProtocolParamsMary (..)
  , ProtocolParamsShelley (..)
  , ProtocolParamsShelleyBased (..)
  , SL.Nonce (..)
  , SL.ProtVer (..)
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , SL.emptyGenesisStaking
  , ShelleyLeaderCredentials (..)
  , protocolClientInfoShelley
  , protocolInfoShelley
  , protocolInfoTPraosShelleyBased
  , registerGenesisStaking
  , registerInitialFunds
  , validateGenesis
  ) where



import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run

import qualified Cardano.Ledger.Shelley.API as SL

import qualified Data.UMap as UM
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect ()
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Node.TPraos
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (pHeaderIssuer)
{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}



protocolClientInfoShelley :: ProtocolClientInfo (ShelleyBlock proto era)
protocolClientInfoShelley =
    ProtocolClientInfo {
      -- No particular codec configuration is needed for Shelley
      pClientInfoCodecConfig = ShelleyCodecConfig
    }

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => BlockSupportsMetrics (ShelleyBlock proto era) where
  -- | Premature optimisation: we assume everywhere that metrics are
  -- cheap, so micro-optimise checking whether the issuer vkey is one of our
  -- own vkeys.
  --
  -- * Equality of vkeys takes roughly 40ns
  -- * Hashing a vkey takes roughly 850ns
  -- * Equality of hashes takes roughly 10ns
  --
  -- We want to avoid the hashing of a vkey as it is more expensive than
  -- simply doing a linear search, comparing vkeys for equality. Only when
  -- we have to do a linear search across a large number of vkeys does it
  -- become more efficient to first hash the vkey and look up its hash in
  -- the map.
  --
  -- We could try to be clever and estimate the number of keys after which
  -- we switch from a linear search to hashing + a O(log n) map lookup, but
  -- we keep it (relatively) simple and optimise for the common case: 0 or 1
  -- key.
  isSelfIssued cfg (ShelleyHeader shdr _) = case Map.size issuerVKeys of
      -- The most common case: a non-block producing node
      0 -> IsNotSelfIssued
      -- A block producing node with a single set of credentials: just do an
      -- equality check of the single VKey, skipping the more expensive
      -- computation of the hash.
      1 | pHeaderIssuer shdr `elem` issuerVKeys
        -> IsSelfIssued
        | otherwise
        -> IsNotSelfIssued
      -- When we are running with multiple sets of credentials, which should
      -- only happen when benchmarking, do a hash lookup, as the number of
      -- keys can grow to 100-250.
      _ | SL.hashKey (pHeaderIssuer shdr) `Map.member` issuerVKeys
        -> IsSelfIssued
        | otherwise
        -> IsNotSelfIssued
    where

      issuerVKeys :: Map (SL.KeyHash 'SL.BlockIssuer (EraCrypto era))
                         (SL.VKey 'SL.BlockIssuer (EraCrypto era))
      issuerVKeys = shelleyBlockIssuerVKeys cfg


instance ShelleyBasedEra era => RunNode (ShelleyBlock era)

{-------------------------------------------------------------------------------
  Register genesis staking
-------------------------------------------------------------------------------}

-- | Register the initial staking information in the 'SL.NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial staking.
--
-- Any existing staking information is overridden, but the UTxO is left
-- untouched.
--
-- TODO adapt and reuse @registerGenesisStaking@ from @cardano-ledger-specs@.
registerGenesisStaking ::
     forall era. ShelleyBasedEra era
  => SL.ShelleyGenesisStaking (EraCrypto era)
  -> SL.NewEpochState era
  -> SL.NewEpochState era
registerGenesisStaking staking nes = nes {
      SL.nesEs = epochState {
          SL.esLState = ledgerState {
          SL.lsDPState = dpState {
              SL.dpsDState = dState'
            , SL.dpsPState = pState'
            }
        }
        , SL.esSnapshots = (SL.esSnapshots epochState) {
              SL._pstakeMark = initSnapShot
            }
        }

    -- Note that this is only applicable in the initial configuration where
    -- there is no existing stake distribution, since it would completely
    -- overwrite any such thing.
    , SL.nesPd = SL.calculatePoolDistr initSnapShot
    }
  where
    SL.ShelleyGenesisStaking { sgsPools, sgsStake } = staking
    SL.NewEpochState { nesEs = epochState } = nes
    ledgerState = SL.esLState epochState
    dpState = SL.lsDPState ledgerState

    -- New delegation state. Since we're using base addresses, we only care
    -- about updating the '_delegations' field.
    --
    -- See STS DELEG for details
    dState' :: SL.DState (EraCrypto era)
    dState' = (SL.dpsDState dpState) {
          SL._unified = UM.unify
            ( Map.map (const $ SL.Coin 0)
                      . Map.mapKeys SL.KeyHashObj
                      $ sgsStake)
            ( Map.mapKeys SL.KeyHashObj sgsStake )
            mempty
        }
    -- We consider pools as having been registered in slot 0
    -- See STS POOL for details
    pState' :: SL.PState (EraCrypto era)
    pState' = (SL.dpsPState dpState) {
          SL._pParams = sgsPools
        }

    -- The new stake distribution is made on the basis of a snapshot taken
    -- during the previous epoch. We create a "fake" snapshot in order to
    -- establish an initial stake distribution.
    initSnapShot :: SL.SnapShot (EraCrypto era)
    initSnapShot =
        -- Since we build a stake from nothing, we first initialise an
        -- 'IncrementalStake' as empty, and then:
        --
        -- 1. Add the initial UTxO, whilst deleting nothing.
        -- 2. Update the stake map given the initial delegation.
        SL.incrementalStakeDistr
          -- Note that 'updateStakeDistribution' takes first the set of UTxO to
          -- delete, and then the set to add. In our case, there is nothing to
          -- delete, since this is an initial UTxO set.
          (SL.updateStakeDistribution mempty mempty (SL._utxo (SL.lsUTxOState ledgerState)))
          dState'
          pState'

-- | Register the initial funds in the 'SL.NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial funds.
--
-- The given funds are /added/ to the existing UTxO.
--
-- PRECONDITION: the given funds must not be part of the existing UTxO.
-- > forall (addr, _) in initialFunds.
-- >    Map.notElem (SL.initialFundsPseudoTxIn addr) existingUTxO
--
-- PROPERTY:
-- >    genesisUTxO genesis
-- > == <genesisUTxO'> (sgInitialFunds genesis)
-- > == <extractUTxO> (registerInitialFunds (sgInitialFunds genesis)
-- >                                        <empty NewEpochState>)
--
-- TODO move to @cardano-ledger-specs@.
registerInitialFunds ::
     forall era.
     ( ShelleyBasedEra era
     , HasCallStack
     )
  => Map (SL.Addr (EraCrypto era)) SL.Coin
  -> SL.NewEpochState era
  -> SL.NewEpochState era
registerInitialFunds initialFunds nes = nes {
      SL.nesEs = epochState {
          SL.esAccountState = accountState'
        , SL.esLState       = ledgerState'
        }
    }
  where
    epochState   = SL.nesEs          nes
    accountState = SL.esAccountState epochState
    ledgerState  = SL.esLState       epochState
    utxoState    = SL.lsUTxOState    ledgerState
    utxo         = SL._utxo          utxoState
    reserves     = SL._reserves      accountState

    initialFundsUtxo :: SL.UTxO era
    initialFundsUtxo = SL.UTxO $ Map.fromList [
          (txIn, txOut)
        | (addr, amount) <- Map.toList initialFunds
        ,  let txIn  = SL.initialFundsPseudoTxIn addr
               txOut = SL.makeTxOut (Proxy @era) addr (inject amount)
        ]

    utxo' = mergeUtxoNoOverlap utxo initialFundsUtxo

    -- Update the reserves
    accountState' = accountState {
          SL._reserves = reserves <-> coin (SL.balance initialFundsUtxo)
        }

    -- Since we only add entries to our UTxO, rather than spending them, there
    -- is nothing to delete in the incremental update.
    utxoToDel     = SL.UTxO mempty
    ledgerState'  = ledgerState {
          SL.lsUTxOState = utxoState {
              SL._utxo        = utxo',
              -- Normally we would incrementally update here. But since we pass
              -- the full UTxO as "toAdd" rather than a delta, we simply
              -- reinitialise the full incremental stake.
              SL._stakeDistro = SL.updateStakeDistribution mempty utxoToDel utxo'
            }
        }

    -- | Merge two UTxOs, throw an 'error' in case of overlap
    mergeUtxoNoOverlap ::
         HasCallStack
      => SL.UTxO era -> SL.UTxO era -> SL.UTxO era
    mergeUtxoNoOverlap (SL.UTxO m1) (SL.UTxO m2) = SL.UTxO $
        Map.unionWithKey
          (\k _ _ -> error $ "initial fund part of UTxO: " <> show k)
          m1
          m2

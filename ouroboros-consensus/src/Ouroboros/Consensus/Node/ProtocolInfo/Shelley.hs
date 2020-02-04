{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Node.ProtocolInfo.Shelley
  ( ShelleyGenesis (..),
    ProtVer (..),
    protocolInfoShelley,
  )
where

import BaseTypes (Globals (..), ShelleyBase, truncateUnitInterval)
import BlockChain (ProtVer (..))
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Crypto.Hash (hash)
import Cardano.Ledger.Shelley.Crypto
import Cardano.Slotting.EpochInfo.Impl (fixedSizeEpochInfo)
import Cardano.Slotting.Slot
  ( EpochNo (..),
    EpochSize,
    WithOrigin (Origin),
    genesisSlotNo,
  )
import Coin (Coin (..))
import Control.Monad.Trans.Reader (runReader)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Keys
import LedgerState (overlaySchedule)
import Ouroboros.Consensus.BlockchainTime (SlotLength, SystemStart)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Shelley
import Ouroboros.Consensus.Ledger.Shelley.Forge (shelleyGenesisHash)
import qualified Ouroboros.Consensus.Ledger.Shelley.History as Shelley.History
import Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import Ouroboros.Consensus.Protocol.TPraos
import qualified Ouroboros.Consensus.Protocol.TPraos.ChainState as TPraos.ChainState
import Ouroboros.Network.Block (genesisPoint)
import Ouroboros.Network.Magic (NetworkMagic)
import PParams (PParams (..), emptyPParams)
import STS.Chain (ChainState (..), initialShelleyState)
import STS.Prtcl (PrtclState (..))
import TxData (Addr, TxId (..), TxIn (..), TxOut (..))
import UTxO (UTxO (..))
import qualified Updates

-- | Shelley genesis information
--
--   Note that this is needed only for a pure Shelley network, hence it being
--   defined here rather than in its own module. In mainnet, Shelley will
--   transition naturally from Byron, and thus will never have its own genesis
--   information.
data ShelleyGenesis
  = ShelleyGenesis
      { sgStartTime :: SystemStart,
        sgNetworkMagic :: NetworkMagic,
        sgProtocolMagicId :: ProtocolMagicId,
        sgActiveSlotsCoeff :: Double,
        sgDecentralisationParam :: Double,
        sgSecurityParam :: SecurityParam,
        sgEpochLength :: EpochSize,
        sgKESPeriod :: Word64,
        sgMaxKESEvolutions :: Word64,
        sgSlotLength :: SlotLength,
        sgUpdateQuorum :: Word64,
        sgReserves :: Coin,
        sgGenDelegs ::
          Map.Map
            (Keys.GenKeyHash TPraosStandardCrypto)
            (Keys.KeyHash TPraosStandardCrypto),
        sgInitialFunds :: Map.Map (Addr TPraosStandardCrypto) Coin
      }
  deriving (Eq, Show, Generic)

protocolInfoShelley ::
  ShelleyGenesis ->
  ProtVer ->
  Maybe (TPraosIsCoreNode TPraosStandardCrypto) ->
  ProtocolInfo ShelleyBlock
protocolInfoShelley sg pv micn =
  ProtocolInfo
    { pInfoConfig =
        TPraosNodeConfig
          { tpraosParams =
              TPraosParams
                { tpraosLeaderF = sgActiveSlotsCoeff sg,
                  tpraosSecurityParam = sgSecurityParam sg,
                  tpraosEpochInfo = epochInfo',
                  tpraosKESPeriod = sgKESPeriod sg,
                  tpraosSlotLength = sgSlotLength sg
                },
            tpraosExtraConfig =
              ShelleyNodeConfig
                { sncProtocolVersion = pv,
                  sncStartTime = sgStartTime sg,
                  sncNetworkMagic = sgNetworkMagic sg,
                  sncProtocolMagicId = sgProtocolMagicId sg
                }
          },
      pInfoInitState = micn,
      pInfoInitLedger =
        ExtLedgerState
          { ledgerState =
              ShelleyLedgerState
                { ledgerTip = genesisPoint,
                  history = Shelley.History.empty,
                  shelleyLedgerState =
                    chainNes initialChainState
                },
            ouroborosChainState =
              TPraos.ChainState.init
                Origin
                ( PrtclState
                    (chainOCertIssue initialChainState)
                    (chainHashHeader initialChainState)
                    (chainSlotNo initialChainState)
                    (chainEpochNonce initialChainState)
                    (chainEvolvingNonce initialChainState)
                    (chainCandidateNonce initialChainState)
                    (chainPrevEpochNonce initialChainState)
                )
          }
    }
  where
    runShelleyBase :: ShelleyBase a -> a
    runShelleyBase sbThing = runReader sbThing shelleyGlobs
    oSched =
      runShelleyBase $
        overlaySchedule
          (EpochNo 0)
          (Map.keysSet $ sgGenDelegs sg)
          pparams
    pparams =
      emptyPParams
        { _activeSlotCoeff =
            truncateUnitInterval
              . realToFrac
              $ sgActiveSlotsCoeff sg,
          _d =
            truncateUnitInterval
              . realToFrac
              $ sgDecentralisationParam sg,
          _maxBHSize = 1000
        }
    (SecurityParam k) = sgSecurityParam sg
    epochInfo' = fixedSizeEpochInfo $ sgEpochLength sg
    shelleyGlobs =
      Globals
        { epochInfo = epochInfo',
          slotsPerKESPeriod = sgKESPeriod sg,
          securityParameter = k,
          startRewards = 3 * k,
          slotsPrior = 3 * k,
          maxKESEvo = sgMaxKESEvolutions sg,
          quorum = sgUpdateQuorum sg
        }
    utxo :: UTxO TPraosStandardCrypto
    utxo =
      UTxO . Map.fromList $
        [ (magicTxIn, txOut)
          | (addr, amount) <- Map.toList (sgInitialFunds sg),
            let txOut = TxOut addr amount
        ]
      where
        magicTxIn =
          TxIn
            ( TxId
                ( coerce $
                    hash
                      @(HASH TPraosStandardCrypto)
                      @ByteString
                      "In the beginning"
                )
            )
            0
    initialChainState =
      initialShelleyState
        genesisSlotNo
        (EpochNo 0)
        shelleyGenesisHash
        utxo
        (sgReserves sg)
        (sgGenDelegs sg)
        oSched
        (Updates.Applications Map.empty) -- apps
        pparams

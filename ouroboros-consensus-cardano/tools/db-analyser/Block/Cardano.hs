{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Cardano (
    Args (..)
  , CardanoBlockArgs
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Options.Applicative

import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.Update as Byron.Update

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock (..),
                     OneEraBlock (..), OneEraHash (..), getHardForkState,
                     hardForkLedgerStatePerEra)
import           Ouroboros.Consensus.HardFork.Combinator.State (currentState)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node.ProtocolInfo

import qualified Cardano.Ledger.Alonzo.Genesis as SL (AlonzoGenesis)
import           Cardano.Ledger.Crypto

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)

import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block (CardanoEras)
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano)
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo,
                     StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import           Block.Alonzo (Args (..))
import           Block.Byron (Args (..), openGenesisByron)
import           Block.Shelley (Args (..))
import           Data.Maybe (fromJust)
import           HasAnalysis

analyseBlock ::
     (forall blk. HasAnalysis blk => blk -> a)
  -> CardanoBlock StandardCrypto -> a
analyseBlock f =
      hcollapse
    . hcmap p (K . f . unI)
    . getOneEraBlock
    . getHardForkBlock
  where
    p :: Proxy HasAnalysis
    p = Proxy

-- | Lift a function polymorphic over all block types supporting `HasAnalysis`
-- into a corresponding function over `CardanoBlock.`
analyseWithLedgerState ::
  forall a.
  (forall blk. HasAnalysis blk => WithLedgerState blk -> a) ->
  WithLedgerState (CardanoBlock StandardCrypto) ->
  a
analyseWithLedgerState f (WithLedgerState cb sb sa) =
  hcollapse
    . hcmap p (K . f)
    . fromJust
    . hsequence'
    $ hzipWith3 zipLS (goLS sb) (goLS sa) oeb
  where
    p :: Proxy HasAnalysis
    p = Proxy

    zipLS (Comp (Just sb')) (Comp (Just sa')) (I blk) =
      Comp . Just $ WithLedgerState blk sb' sa'
    zipLS _ _ _ = Comp Nothing

    oeb = getOneEraBlock . getHardForkBlock $ cb

    goLS ::
      LedgerState (CardanoBlock StandardCrypto) ->
      NP (Maybe :.: LedgerState) (CardanoEras StandardCrypto)
    goLS =
      hexpand (Comp Nothing)
        . hmap (Comp . Just . currentState)
        . Telescope.tip
        . getHardForkState
        . hardForkLedgerStatePerEra

instance HasProtocolInfo (CardanoBlock StandardCrypto) where
  data Args (CardanoBlock StandardCrypto) =
    CardanoBlockArgs {
        byronArgs   :: Args ByronBlock
      , shelleyArgs :: Args (ShelleyBlock StandardShelley)
      , alonzoArgs  :: Args (ShelleyBlock StandardAlonzo)
      }
  argsParser _ = parseCardanoArgs
  mkProtocolInfo CardanoBlockArgs {..} = do
    let ByronBlockArgs {..}   = byronArgs
    let ShelleyBlockArgs {..} = shelleyArgs
    let AlonzoBlockArgs {..}  = alonzoArgs
    genesisByron <- openGenesisByron configFileByron genesisHash requiresNetworkMagic
    genesisShelley <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    genesisAlonzo <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileAlonzo
    return $ mkCardanoProtocolInfo genesisByron threshold genesisShelley genesisAlonzo initialNonce

instance HasAnalysis (CardanoBlock StandardCrypto) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes   = analyseBlock blockTxSizes
  knownEBBs _    =
      Map.mapKeys castHeaderHash . Map.map castChainHash $
        knownEBBs (Proxy @ByronBlock)

  emitTraces = analyseWithLedgerState emitTraces

type CardanoBlockArgs = Args (CardanoBlock StandardCrypto)

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs = CardanoBlockArgs
    <$> argsParser Proxy
    <*> argsParser Proxy
    <*> argsParser Proxy

mkCardanoProtocolInfo ::
     Byron.Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> ShelleyGenesis StandardShelley
  -> SL.AlonzoGenesis
  -> Nonce
  -> ProtocolInfo IO (CardanoBlock StandardCrypto)
mkCardanoProtocolInfo genesisByron signatureThreshold genesisShelley genesisAlonzo initialNonce =
    protocolInfoCardano
      ProtocolParamsByron {
          byronGenesis                = genesisByron
        , byronPbftSignatureThreshold = signatureThreshold
        , byronProtocolVersion        = Byron.Update.ProtocolVersion 1 2 0
        , byronSoftwareVersion        = Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "db-analyser") 2
        , byronLeaderCredentials      = Nothing
        , byronMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsShelleyBased {
          shelleyBasedGenesis           = genesisShelley
        , shelleyBasedInitialNonce      = initialNonce
        , shelleyBasedLeaderCredentials = []
        }
      ProtocolParamsShelley {
          shelleyProtVer                = ProtVer 3 0
        , shelleyMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsAllegra {
          allegraProtVer                = ProtVer 4 0
        , allegraMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsMary {
          maryProtVer                   = ProtVer 5 0
        , maryMaxTxCapacityOverrides    = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsAlonzo {
          alonzoProtVer                 = ProtVer 6 0
        , alonzoMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = ()
        , transitionTrigger            = TriggerHardForkAtVersion 2
        }
      ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = ()
        , transitionTrigger            = TriggerHardForkAtVersion 3
        }
      ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = ()
        , transitionTrigger            = TriggerHardForkAtVersion 4
        }
      ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = genesisAlonzo
        , transitionTrigger            = TriggerHardForkAtVersion 5
        }

castHeaderHash ::
     HeaderHash ByronBlock
  -> HeaderHash (CardanoBlock StandardCrypto)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash ::
     ChainHash ByronBlock
  -> ChainHash (CardanoBlock StandardCrypto)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h

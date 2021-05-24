{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

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
                     OneEraBlock (..), OneEraHash (..))
import           Ouroboros.Consensus.Node.ProtocolInfo

import qualified Cardano.Ledger.Alonzo.Genesis as SL (AlonzoGenesis)
import           Cardano.Ledger.Crypto

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano)

import           Block.Byron (Args (..), openGenesisByron)
import           Block.Shelley (Args (..))
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

instance HasProtocolInfo (CardanoBlock StandardCrypto) where
  data Args (CardanoBlock StandardCrypto) =
    CardanoBlockArgs {
        byronArgs   :: Args ByronBlock
      , shelleyArgs :: Args (ShelleyBlock StandardShelley)
      , alonzoArgs  :: FilePath
      }
  argsParser _ = parseCardanoArgs
  mkProtocolInfo CardanoBlockArgs {..} = do
    let ByronBlockArgs {..}   = byronArgs
    let ShelleyBlockArgs {..} = shelleyArgs
    genesisByron <- openGenesisByron configFileByron genesisHash requiresNetworkMagic
    genesisShelley <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    genesisAlonzo <- undefined alonzoArgs
    return $ mkCardanoProtocolInfo genesisByron threshold genesisShelley genesisAlonzo initialNonce

instance HasAnalysis (CardanoBlock StandardCrypto) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes   = analyseBlock blockTxSizes
  knownEBBs _    =
      Map.mapKeys castHeaderHash . Map.map castChainHash $
        knownEBBs (Proxy @ByronBlock)

type CardanoBlockArgs = Args (CardanoBlock StandardCrypto)

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs = CardanoBlockArgs
    <$> argsParser Proxy
    <*> argsParser Proxy
    <*> strOption (mconcat [
            long "configAlonzo"
          , help "Path to config file"
          , metavar "PATH"
          ])

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
        }
      ProtocolParamsShelleyBased {
          shelleyBasedGenesis           = genesisShelley
        , shelleyBasedInitialNonce      = initialNonce
        , shelleyBasedLeaderCredentials = []
        }
      ProtocolParamsShelley {
          shelleyProtVer = ProtVer 2 0
        }
      ProtocolParamsAllegra {
          allegraProtVer = ProtVer 3 0
        }
      ProtocolParamsMary {
          maryProtVer    = ProtVer 4 0
        }
      ProtocolParamsAlonzo {
          alonzoProtVer  = ProtVer 5 0
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

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
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
import           Options.Applicative

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Shelley.Node (Nonce (..), ShelleyGenesis)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold)

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano)

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import           Block.Byron (Args (..), openGenesisByron)
import           Block.Shelley (Args (..))
import           HasAnalysis

instance HasAnalysis (CardanoBlock TPraosStandardCrypto) where
  data Args (CardanoBlock TPraosStandardCrypto) =
    CardanoBlockArgs {
        byronArgs   :: Args ByronBlock
      , shelleyArgs :: Args (ShelleyBlock TPraosStandardCrypto)
      }
  argsParser _ = parseCardanoArgs
  mkProtocolInfo CardanoBlockArgs {..} = do
    let ByronBlockArgs {..}   = byronArgs
    let ShelleyBlockArgs {..} = shelleyArgs
    byronConfig   <- openGenesisByron configFileByron genesisHash requiresNetworkMagic
    shelleyConfig <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    return $ mkCardanoProtocolInfo byronConfig shelleyConfig threshold initialNonce
  countTxOutputs blk = case blk of
    Cardano.BlockByron b    -> countTxOutputs b
    Cardano.BlockShelley sh -> countTxOutputs sh
  blockHeaderSize blk = case blk of
    Cardano.BlockByron b    -> blockHeaderSize b
    Cardano.BlockShelley sh -> blockHeaderSize sh
  blockTxSizes blk = case blk of
    Cardano.BlockByron b    -> blockTxSizes b
    Cardano.BlockShelley sh -> blockTxSizes sh
  knownEBBs _ = Map.mapKeys castHeaderHash . Map.map castChainHash $
    knownEBBs (Proxy @ByronBlock)

type CardanoBlockArgs = Args (CardanoBlock TPraosStandardCrypto)

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs = CardanoBlockArgs
    <$> argsParser Proxy
    <*> argsParser Proxy

mkCardanoProtocolInfo :: forall c. TPraosCrypto c
                      => Genesis.Config
                      -> ShelleyGenesis c
                      -> Maybe PBftSignatureThreshold
                      -> Nonce
                      -> ProtocolInfo IO (CardanoBlock c)
mkCardanoProtocolInfo byronConfig shelleyConfig signatureThreshold initialNonce =
    protocolInfoCardano
      byronConfig
      signatureThreshold
      (Update.ProtocolVersion 2 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "db-analyser") 2)
      Nothing
      shelleyConfig
      initialNonce
      (SL.ProtVer 2 0)
      2000
      Nothing
      Nothing
      (TriggerHardForkAtVersion 2)

castHeaderHash :: HeaderHash ByronBlock -> HeaderHash (CardanoBlock c)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash :: ChainHash ByronBlock -> ChainHash (CardanoBlock c)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h

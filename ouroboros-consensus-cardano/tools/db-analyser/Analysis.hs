{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Analysis (HasAnalysis (..), mkTopLevelConfig) where

import           Control.Monad.Except
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Natural (Natural)

import           Cardano.Binary (unAnnotated)
import qualified Cardano.Crypto as Crypto

import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as Chain

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Tx as SL

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config hiding (mkTopLevelConfig)
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Node (protocolInfoByron)

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Node (Nonce (..), ShelleyGenesis,
                     protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

class GetPrevHash blk => HasAnalysis blk where
    countTxOutputs   :: blk -> Int
    blockHeaderSize  :: blk -> SizeInBytes
    blockTxSizes     :: blk -> [SizeInBytes]
    knownEBBs        :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)
    protocolInfo     :: [FilePath] -- Genesis file or files.
                     -> Bool       -- is it mainnet?
                     -> IO (ProtocolInfo IO blk)

mkTopLevelConfig :: forall blk. HasAnalysis blk
                 => [FilePath] -- Genesis file or files.
                 -> Bool       -- is it mainnet?
                 -> IO (TopLevelConfig blk)
mkTopLevelConfig fps onMainNet = pInfoConfig <$> protocolInfo @blk fps onMainNet

{-------------------------------------------------------------------------------
  ByronBlock instance
-------------------------------------------------------------------------------}

instance HasAnalysis ByronBlock where
    countTxOutputs = aBlockOrBoundary (const 0) countTxOutputsByron
    blockHeaderSize = fromIntegral .
      aBlockOrBoundary blockBoundaryHeaderSize blockHeaderSizeByron
    blockTxSizes = aBlockOrBoundary (const []) blockTxSizesByron
    knownEBBs = const Byron.knownEBBs
    protocolInfo [configFile] onMainNet = do
      genesisConfig <- openGenesisByron configFile onMainNet
      return $ mkByronProtocolInfo genesisConfig
    protocolInfo ls _onMainNet =
      error $
        "a single genesis file is needed for pure Byron. Given " ++ show ls

-- | Equivalent of 'either' for 'ABlockOrBoundary'.
aBlockOrBoundary :: (Chain.ABoundaryBlock ByteString -> a)
                 -> (Chain.ABlock ByteString -> a)
                 -> ByronBlock -> a
aBlockOrBoundary fromBoundary fromRegular blk = case blk of
    Byron.ByronBlock (Chain.ABOBBoundary boundaryBlock) _ _
      -> fromBoundary boundaryBlock
    Byron.ByronBlock (Chain.ABOBBlock regularBlk) _ _
      -> fromRegular regularBlk

countTxOutputsByron :: Chain.ABlock ByteString -> Int
countTxOutputsByron Chain.ABlock{..} = countTxPayload bodyTxPayload
  where
    Chain.AHeader{..} = blockHeader
    Chain.ABody{..}   = blockBody

    countTxPayload :: Chain.ATxPayload a -> Int
    countTxPayload = sum
                   . map (countTx . unAnnotated . Chain.aTaTx)
                   . Chain.aUnTxPayload

    countTx :: Chain.Tx -> Int
    countTx = length . Chain.txOutputs

blockBoundaryHeaderSize ::  Chain.ABoundaryBlock ByteString -> Natural
blockBoundaryHeaderSize =
    fromIntegral . BS.length . Chain.boundaryHeaderAnnotation . Chain.boundaryHeader

blockHeaderSizeByron ::  Chain.ABlock ByteString -> Natural
blockHeaderSizeByron = Chain.headerLength . Chain.blockHeader

blockTxSizesByron :: Chain.ABlock ByteString -> [SizeInBytes]
blockTxSizesByron block =
    map (fromIntegral . BL.length . BL.fromStrict . Chain.aTaAnnotation) blockTxAuxs
  where
    Chain.ABlock{ blockBody } = block
    Chain.ABody{ bodyTxPayload } = blockBody
    Chain.ATxPayload{ aUnTxPayload = blockTxAuxs } = bodyTxPayload

openGenesisByron :: FilePath -> Bool -> IO Genesis.Config
openGenesisByron configFile onMainNet = do
    genesisHash <- either (error . show) return =<< runExceptT
      (snd <$> Genesis.readGenesisData configFile)
    genesisConfig <- either (error . show) return =<< runExceptT
      (Genesis.mkConfigFromFile
        (if onMainNet -- transactions on testnet include magic number
          then Crypto.RequiresNoMagic
          else Crypto.RequiresMagic)
        configFile
        (Genesis.unGenesisHash genesisHash))
    return genesisConfig

mkByronProtocolInfo :: Genesis.Config -> ProtocolInfo IO ByronBlock
mkByronProtocolInfo genesisConfig =
    protocolInfoByron
      genesisConfig
      Nothing
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "db-analyse") 2)
      Nothing

{-------------------------------------------------------------------------------
  ShelleyBlock instance
-------------------------------------------------------------------------------}

instance TPraosCrypto c => HasAnalysis (ShelleyBlock c) where
    countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) -> sum $ fmap countOutputs txs
    blockHeaderSize =
      fromIntegral . SL.bHeaderSize . SL.bheader . Shelley.shelleyBlockRaw
    blockTxSizes blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) ->
        toList $ fmap (fromIntegral . BL.length . SL.txFullBytes) txs
    knownEBBs = const Map.empty
    protocolInfo [configFile] _onMainNet = do
      genesis  <- either (error . show) return =<< Aeson.eitherDecodeFileStrict' configFile
      return $ mkShelleyProtocolInfo genesis
    protocolInfo ls _onMainNet =
      error $
        "A single genesis file is needed for pure Shelley. Given " ++ show ls

mkShelleyProtocolInfo :: forall c. TPraosCrypto c
                      => ShelleyGenesis c
                      -> ProtocolInfo IO (ShelleyBlock c)
mkShelleyProtocolInfo genesis =
    protocolInfoShelley
      genesis
      NeutralNonce -- TODO
      2000
      (SL.ProtVer 0 0)
      Nothing

countOutputs :: Shelley.Crypto c => SL.Tx c -> Int
countOutputs tx  = length $ SL._outputs $ SL._body tx

{-------------------------------------------------------------------------------
  CardanoBlock instance
-------------------------------------------------------------------------------}

instance TPraosCrypto c => HasAnalysis (CardanoBlock c) where
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
  protocolInfo [byronGenesis, shelleyGenesis] onMainNet = do
    byronConfig <- openGenesisByron byronGenesis onMainNet
    shelleyConfig <- either (error . show) return =<< Aeson.eitherDecodeFileStrict' shelleyGenesis
    return $ mkCardanoProtocolInfo byronConfig shelleyConfig
  protocolInfo ls _onMainNet =
    error $
      "Two genesis files are needed for Cardano. Given " ++ show ls

mkCardanoProtocolInfo :: forall c. TPraosCrypto c
                      => Genesis.Config
                      -> ShelleyGenesis c
                      -> ProtocolInfo IO (CardanoBlock c)
mkCardanoProtocolInfo byronConfig shelleyConfig =
    protocolInfoCardano
      byronConfig
      Nothing
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "db-analyse") 2)
      Nothing
      shelleyConfig
      NeutralNonce -- TODO
      (SL.ProtVer 2 0)
      2000
      Nothing
      Nothing
      (TriggerHardForkAtVersion 2)

castHeaderHash :: HeaderHash ByronBlock -> HeaderHash (CardanoBlock c)
castHeaderHash = OneEraHash . toRawHash (Proxy @ByronBlock)

castChainHash :: ChainHash ByronBlock -> ChainHash (CardanoBlock c)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h

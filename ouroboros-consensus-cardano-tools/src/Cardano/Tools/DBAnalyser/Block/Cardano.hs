{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBAnalyser.Block.Cardano (
    Args (configFile, threshold, CardanoBlockArgs)
  , CardanoBlockArgs
  ) where

import           Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.SOP.Strict
import           Data.Word (Word16)
import           System.Directory (makeAbsolute)
import           System.FilePath (takeDirectory, (</>))

import           Cardano.Binary (Raw)
import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.Update as Byron.Update
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import qualified Cardano.Ledger.Alonzo.Genesis as SL (AlonzoGenesis)
import           Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Era as Core
import qualified Cardano.Tools.DBAnalyser.Block.Byron as BlockByron
import           Cardano.Tools.DBAnalyser.Block.Shelley ()

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block (CardanoEras,
                     CardanoShelleyEras)
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano)
import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock (..),
                     OneEraBlock (..), OneEraHash (..), getHardForkState,
                     hardForkLedgerStatePerEra)
import           Ouroboros.Consensus.HardFork.Combinator.State (currentState)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.Shelley.Node.Praos

import           Cardano.Node.Types (AdjustFilePaths (..))
import           Cardano.Tools.DBAnalyser.Block.Shelley ()
import           Cardano.Tools.DBAnalyser.HasAnalysis

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

    zipLS (Comp (Just (Flip sb'))) (Comp (Just (Flip sa'))) (I blk) =
      Comp . Just $ WithLedgerState blk sb' sa'
    zipLS _ _ _ = Comp Nothing

    oeb = getOneEraBlock . getHardForkBlock $ cb

    goLS ::
      LedgerState (CardanoBlock StandardCrypto) Canonical ->
      NP (Maybe :.: (Flip LedgerState Canonical)) (CardanoEras StandardCrypto)
    goLS =
      hexpand (Comp Nothing)
        . hmap (Comp . Just . currentState)
        . Telescope.tip
        . getHardForkState
        . hardForkLedgerStatePerEra

instance HasProtocolInfo (CardanoBlock StandardCrypto) where
  data Args (CardanoBlock StandardCrypto) = CardanoBlockArgs {
          configFile           :: FilePath
        , threshold            :: Maybe PBftSignatureThreshold
        }

  mkProtocolInfo CardanoBlockArgs{configFile, threshold} = do
    relativeToConfig :: (FilePath -> FilePath) <-
        (</>) . takeDirectory <$> makeAbsolute configFile

    cc :: CardanoConfig <-
      either (error . show) (return . adjustFilePaths relativeToConfig) =<<
        Aeson.eitherDecodeFileStrict' configFile

    genesisByron   <-
      BlockByron.openGenesisByron (byronGenesisPath cc) (byronGenesisHash cc) (requiresNetworkMagic cc)
    genesisShelley <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' (shelleyGenesisPath cc)
    genesisAlonzo  <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' (alonzoGenesisPath cc)

    initialNonce <- case shelleyGenesisHash cc of
      Just h  -> pure h
      Nothing -> do
        content <- BS.readFile (shelleyGenesisPath cc)
        pure
          $ Nonce
          $ CryptoClass.castHash
          $ CryptoClass.hashWith id
          $ content

    return
      $ mkCardanoProtocolInfo
          genesisByron
          threshold
          genesisShelley
          genesisAlonzo
          initialNonce
          (hardForkTriggers cc)

data CardanoConfig = CardanoConfig {
    -- | @RequiresNetworkMagic@ field
    requiresNetworkMagic :: RequiresNetworkMagic

     -- | @ByronGenesisFile@ field
  , byronGenesisPath     :: FilePath
    -- | @ByronGenesisHash@ field
  , byronGenesisHash     :: Maybe (Crypto.Hash Raw)

    -- | @ShelleyGenesisFile@ field
    -- | @ShelleyGenesisHash@ field
  , shelleyGenesisPath   :: FilePath
  , shelleyGenesisHash   :: Maybe Nonce

    -- | @AlonzoGenesisFile@ field
  , alonzoGenesisPath    :: FilePath

    -- | @Test*HardForkAtEpoch@ for each Shelley era
  , hardForkTriggers     :: NP STA (CardanoShelleyEras StandardCrypto)
  }

instance AdjustFilePaths CardanoConfig where
    adjustFilePaths f cc =
        cc {
            byronGenesisPath    = f $ byronGenesisPath cc
          , shelleyGenesisPath  = f $ shelleyGenesisPath cc
          , alonzoGenesisPath   = f $ alonzoGenesisPath cc
          }

-- | Shelley transition arguments
data STA :: Type -> Type where
  STA ::
       -- so far, the context is either () or AlonzoGenesis
       (SL.AlonzoGenesis -> Core.TranslationContext era)
    -> TriggerHardFork
    -> STA (ShelleyBlock proto era)

instance Aeson.FromJSON CardanoConfig where
  parseJSON = Aeson.withObject "CardanoConfigFile" $ \v -> do

    requiresNetworkMagic <- v Aeson..: "RequiresNetworkMagic"

    byronGenesisPath <- v Aeson..:  "ByronGenesisFile"
    byronGenesisHash <- v Aeson..:? "ByronGenesisHash"

    shelleyGenesisPath <- v Aeson..: "ShelleyGenesisFile"
    shelleyGenesisHash <- v Aeson..:? "ShelleyGenesisHash" >>= \case
      Nothing -> pure Nothing
      Just hex -> case CryptoClass.hashFromTextAsHex hex of
        Nothing -> fail "could not parse ShelleyGenesisHash as a hex string"
        Just h  -> pure $ Just $ Nonce h

    alonzoGenesisPath <- v Aeson..: "AlonzoGenesisFile"

    hardForkTriggers <- do
      let f ::
               Aeson.Key
            -> Word16   -- ^ the argument to 'TriggerHardForkAtVersion'
            -> (SL.AlonzoGenesis -> Core.TranslationContext era)
            -> (Aeson.Parser :.: STA) (ShelleyBlock proto era)
          f nm majProtVer ctxt = Comp $
              fmap (STA ctxt)
            $           (fmap TriggerHardForkAtEpoch <$> (v Aeson..:? nm))
              Aeson..!= (TriggerHardForkAtVersion majProtVer)

      stas <- hsequence' $
        f "TestShelleyHardForkAtEpoch" 2 (\_ -> ()) :*
        f "TestAllegraHardForkAtEpoch" 3 (\_ -> ()) :*
        f "TestMaryHardForkAtEpoch"    4 (\_ -> ()) :*
        f "TestAlonzoHardForkAtEpoch"  5 id         :*
        f "TestBabbageHardForkAtEpoch" 7 id         :*
        Nil

      let isBad :: NP STA xs -> Bool
          isBad = \case
            STA _ TriggerHardForkAtVersion{} :* STA _ TriggerHardForkAtEpoch{} :* _ -> True

            STA{} :* np -> isBad np
            Nil          -> False
      fmap (\() -> stas) $ when (isBad stas) $ fail $
           "if the Cardano config file sets a Test*HardForkEpoch,"
        <> " it must also set it for all previous eras."

    pure $
      CardanoConfig
        { requiresNetworkMagic = requiresNetworkMagic
        , byronGenesisPath = byronGenesisPath
        , byronGenesisHash = byronGenesisHash
        , shelleyGenesisPath = shelleyGenesisPath
        , shelleyGenesisHash = shelleyGenesisHash
        , alonzoGenesisPath = alonzoGenesisPath
        , hardForkTriggers = hardForkTriggers
        }

instance (HasAnnTip (CardanoBlock StandardCrypto), GetPrevHash (CardanoBlock StandardCrypto)) => HasAnalysis (CardanoBlock StandardCrypto) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes   = analyseBlock blockTxSizes
  knownEBBs _    =
      Map.mapKeys castHeaderHash . Map.map castChainHash $
        knownEBBs (Proxy @ByronBlock)

  emitTraces = analyseWithLedgerState emitTraces

  blockStats = analyseBlock blockStats

type CardanoBlockArgs = Args (CardanoBlock StandardCrypto)

mkCardanoProtocolInfo ::
     Byron.Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> ShelleyGenesis StandardShelley
  -> SL.AlonzoGenesis
  -> Nonce
  -> NP STA (CardanoShelleyEras StandardCrypto)
  -> ProtocolInfo IO (CardanoBlock StandardCrypto)
mkCardanoProtocolInfo genesisByron signatureThreshold genesisShelley genesisAlonzo initialNonce hardForkTriggers =
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
      ProtocolParamsBabbage {
          babbageProtVer                 = ProtVer 7 0
        , babbageMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      (unSTA shelleyTransition)
      (unSTA allegraTransition)
      (unSTA maryTransition)
      (unSTA alonzoTransition)
      (unSTA babbageTransition)
  where
    ( shelleyTransition :*
      allegraTransition :*
      maryTransition    :*
      alonzoTransition  :*
      babbageTransition :*
      Nil
      ) = hardForkTriggers

    unSTA :: STA (ShelleyBlock proto era) -> ProtocolTransitionParamsShelleyBased era
    unSTA (STA ctxt trigger) = ProtocolTransitionParamsShelleyBased (ctxt genesisAlonzo) trigger

castHeaderHash ::
     HeaderHash ByronBlock
  -> HeaderHash (CardanoBlock StandardCrypto)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash ::
     ChainHash ByronBlock
  -> ChainHash (CardanoBlock StandardCrypto)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h

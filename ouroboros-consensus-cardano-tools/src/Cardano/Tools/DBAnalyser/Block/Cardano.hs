{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
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

import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.Update as Byron.Update
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import           Cardano.Crypto.Raw (Raw)
import qualified Cardano.Ledger.Alonzo.Genesis as SL (AlonzoGenesis)
import qualified Cardano.Ledger.BaseTypes as SL (natVersion)
import qualified Cardano.Ledger.Conway.Genesis as SL
                     (ConwayGenesis (cgGenDelegs))
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Shelley.Translation as SL
                     (toFromByronTranslationContext)
import           Cardano.Node.Types (AdjustFilePaths (..))
import qualified Cardano.Tools.DBAnalyser.Block.Byron as BlockByron
import           Cardano.Tools.DBAnalyser.Block.Shelley ()
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import           Data.Word (Word16)
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
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.Shelley.Node.Praos
import           System.Directory (makeAbsolute)
import           System.FilePath (takeDirectory, (</>))

type CBlock = CardanoBlock StandardCrypto StandardCrypto

analyseBlock ::
     (forall blk. HasAnalysis blk => blk -> a)
  -> CBlock -> a
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
  WithLedgerState (CBlock) ->
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
      LedgerState CBlock ->
      NP (Maybe :.: LedgerState) (CardanoEras StandardCrypto StandardCrypto)
    goLS =
      hexpand (Comp Nothing)
        . hmap (Comp . Just . currentState)
        . Telescope.tip
        . getHardForkState
        . hardForkLedgerStatePerEra

instance HasProtocolInfo (CBlock) where
  data Args (CBlock) = CardanoBlockArgs {
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
    genesisConway  <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' (conwayGenesisPath cc)

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
          genesisConway
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

    -- | @ConwayGenesisFile@ field
  , conwayGenesisPath    :: FilePath

    -- | @Test*HardForkAtEpoch@ for each Shelley era
  , hardForkTriggers     :: NP ShelleyTransitionArguments (CardanoShelleyEras StandardCrypto StandardCrypto)
  }

instance AdjustFilePaths CardanoConfig where
    adjustFilePaths f cc =
        cc {
            byronGenesisPath    = f $ byronGenesisPath cc
          , shelleyGenesisPath  = f $ shelleyGenesisPath cc
          , alonzoGenesisPath   = f $ alonzoGenesisPath cc
          , conwayGenesisPath   = f $ conwayGenesisPath cc
          -- Byron, Shelley, Alonzo, and Conway are the only eras that have genesis
          -- data. The actual genesis block is a Byron block, therefore we needed a
          -- genesis file. To transition to Shelley, we needed to add some additional
          -- genesis data (eg some initial values of new protocol parametrers like
          -- @d@). Similarly in Alonzo (eg Plutus interpreter parameters/limits) and
          -- in Conway too (ie keys of the new genesis delegates).
          --
          -- In contrast, the Allegra, Mary, and Babbage eras did not introduce any new
          -- genesis data.
          }

-- | Shelley transition arguments
data ShelleyTransitionArguments :: Type -> Type where
  ShelleyTransitionArguments ::
       -- so far, the context is either () or AlonzoGenesis or ConwayGenesis
       ((ShelleyGenesis StandardCrypto, SL.AlonzoGenesis, SL.ConwayGenesis StandardCrypto) ->
        Core.TranslationContext era)
    -> TriggerHardFork
    -> ShelleyTransitionArguments (ShelleyBlock proto era)

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

    conwayGenesisPath <- v Aeson..: "ConwayGenesisFile"

    hardForkTriggers <- do
      let f ::
               Aeson.Key
            -> Word16   -- ^ the argument to 'TriggerHardForkAtVersion'
            -> ((ShelleyGenesis StandardCrypto, SL.AlonzoGenesis, SL.ConwayGenesis StandardCrypto) ->
                Core.TranslationContext era)
            -> (Aeson.Parser :.: ShelleyTransitionArguments) (ShelleyBlock proto era)
          f nm majProtVer ctxt = Comp $
              fmap (ShelleyTransitionArguments ctxt)
            $           (fmap TriggerHardForkAtEpoch <$> (v Aeson..:? nm))
              Aeson..!= (TriggerHardForkAtVersion majProtVer)

      stas <- hsequence'
        $  f "TestShelleyHardForkAtEpoch" 2 (\(sg, _, _) -> SL.toFromByronTranslationContext sg)
        :* f "TestAllegraHardForkAtEpoch" 3 (\_ -> ())
        :* f "TestMaryHardForkAtEpoch"    4 (\_ -> ())
        :* f "TestAlonzoHardForkAtEpoch"  5 ((\(_, ag, _) -> ag))
        :* f "TestBabbageHardForkAtEpoch" 7 (\_ -> ())
        :* f "TestConwayHardForkAtEpoch"  9 (\(_, _, cg) -> SL.cgGenDelegs cg)
        :* Nil

      let isBad :: NP ShelleyTransitionArguments xs -> Bool
          isBad = \case
            ShelleyTransitionArguments _ TriggerHardForkAtVersion{} :* ShelleyTransitionArguments _ TriggerHardForkAtEpoch{} :* _ -> True

            ShelleyTransitionArguments{} :* np -> isBad np
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
        , conwayGenesisPath = conwayGenesisPath
        , hardForkTriggers = hardForkTriggers
        }

instance (HasAnnTip (CBlock), GetPrevHash (CBlock)) => HasAnalysis (CBlock) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes   = analyseBlock blockTxSizes
  knownEBBs _    =
      Map.mapKeys castHeaderHash . Map.map castChainHash $
        knownEBBs (Proxy @ByronBlock)

  emitTraces = analyseWithLedgerState emitTraces

  blockStats = analyseBlock blockStats

type CardanoBlockArgs = Args (CBlock)

mkCardanoProtocolInfo ::
     Byron.Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> ShelleyGenesis StandardCrypto
  -> SL.AlonzoGenesis
  -> SL.ConwayGenesis StandardCrypto
  -> Nonce
  -> NP ShelleyTransitionArguments (CardanoShelleyEras StandardCrypto StandardCrypto)
  -> ProtocolInfo IO (CBlock)
mkCardanoProtocolInfo genesisByron signatureThreshold genesisShelley genesisAlonzo genesisConway initialNonce hardForkTriggers =
    protocolInfoCardano
      ProtocolParamsByron {
          byronGenesis                = genesisByron
        , byronPbftSignatureThreshold = signatureThreshold
        , byronProtocolVersion        = Byron.Update.ProtocolVersion 1 2 0
        , byronSoftwareVersion        = Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "db-analyser") 2
        , byronLeaderCredentials      = Nothing
        , byronMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      ProtocolParamsShelleyBased {
          shelleyBasedGenesis           = genesisShelley
        , shelleyBasedInitialNonce      = initialNonce
        , shelleyBasedLeaderCredentials = []
        }
      ProtocolParamsShelley {
          -- Note that this is /not/ the Shelley protocol version, see
          -- https://github.com/input-output-hk/cardano-node/blob/daeae61a005776ee7b7514ce47de3933074234a8/cardano-node/src/Cardano/Node/Protocol/Cardano.hs#L167-L170
          -- and the succeeding comments.
          shelleyProtVer                = ProtVer (SL.natVersion @3) 0
        , shelleyMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      ProtocolParamsAllegra {
          allegraProtVer                = ProtVer (SL.natVersion @4) 0
        , allegraMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      ProtocolParamsMary {
          maryProtVer                   = ProtVer (SL.natVersion @5) 0
        , maryMaxTxCapacityOverrides    = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      ProtocolParamsAlonzo {
          alonzoProtVer                 = ProtVer (SL.natVersion @7) 0
        , alonzoMaxTxCapacityOverrides  = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      ProtocolParamsBabbage {
          babbageProtVer                 = ProtVer (SL.natVersion @9) 0
        , babbageMaxTxCapacityOverrides  = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      ProtocolParamsConway {
          conwayProtVer                  = ProtVer (SL.natVersion @9) 0
        , conwayMaxTxCapacityOverrides   = Mempool.mkOverrides Mempool.noOverridesMeasure
        }
      (unShelleyTransitionArguments shelleyTransition)
      (unShelleyTransitionArguments allegraTransition)
      (unShelleyTransitionArguments maryTransition)
      (unShelleyTransitionArguments alonzoTransition)
      (unShelleyTransitionArguments babbageTransition)
      (unShelleyTransitionArguments conwayTransition)
  where
    ( shelleyTransition :*
      allegraTransition :*
      maryTransition    :*
      alonzoTransition  :*
      babbageTransition :*
      conwayTransition  :*
      Nil
      ) = hardForkTriggers

    unShelleyTransitionArguments ::
         ShelleyTransitionArguments (ShelleyBlock proto era)
      -> ProtocolTransitionParamsShelleyBased era
    unShelleyTransitionArguments (ShelleyTransitionArguments ctxt trigger) =
      ProtocolTransitionParamsShelleyBased
      (ctxt (genesisShelley, genesisAlonzo, genesisConway))
      trigger

castHeaderHash ::
     HeaderHash ByronBlock
  -> HeaderHash (CBlock)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash ::
     ChainHash ByronBlock
  -> ChainHash (CBlock)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h

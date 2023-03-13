{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tools.DBSynthesizer.Orphans () where

import qualified Cardano.Chain.Update as Byron (ApplicationName (..))
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Node.Types (AdjustFilePaths (..),
                     NodeByronProtocolConfiguration (..),
                     NodeHardForkProtocolConfiguration (..))
import           Cardano.Tools.DBSynthesizer.Types
import           Control.Monad (when)
import           Data.Aeson as Aeson (FromJSON (..), withObject, (.!=), (.:),
                     (.:?))


instance FromJSON NodeConfigStub where
    parseJSON val = withObject "NodeConfigStub" (parse' val) val
      where
        parse' o v = do
          proto <- v .: "Protocol"
          when (proto /= ("Cardano" :: String)) $
            fail $ "nodeConfig.Protocol expected: Cardano; found: " ++ proto
          NodeConfigStub o
            <$> v .: "AlonzoGenesisFile"
            <*> v .: "ShelleyGenesisFile"
            <*> v .: "ByronGenesisFile"
            <*> v .: "ConwayGenesisFile"

instance AdjustFilePaths NodeConfigStub where
    adjustFilePaths f nc =
        nc {
            ncsAlonzoGenesisFile    = f $ ncsAlonzoGenesisFile nc
          , ncsShelleyGenesisFile   = f $ ncsShelleyGenesisFile nc
          , ncsByronGenesisFile     = f $ ncsByronGenesisFile nc
          , ncsConwayGenesisFile    = f $ ncsConwayGenesisFile nc
          }

instance AdjustFilePaths NodeCredentials where
    adjustFilePaths f nc =
        nc {
            credCertFile  = f <$> credCertFile nc
          , credVRFFile   = f <$> credVRFFile nc
          , credKESFile   = f <$> credKESFile nc
          , credBulkFile  = f <$> credBulkFile nc
          }

-- DUPLICATE: mirroring parsers from cardano-node/src/Cardano/Node/Configuration/POM.hs

instance FromJSON NodeHardForkProtocolConfiguration where
    parseJSON = withObject "NodeHardForkProtocolConfiguration" $ \v ->
        NodeHardForkProtocolConfiguration
          <$> v .:? "TestEnableDevelopmentHardForkEras"
                .!= False
          <*> v .:? "TestShelleyHardForkAtEpoch"
          <*> v .:? "TestShelleyHardForkAtVersion"
          <*> v .:? "TestAllegraHardForkAtEpoch"
          <*> v .:? "TestAllegraHardForkAtVersion"
          <*> v .:? "TestMaryHardForkAtEpoch"
          <*> v .:? "TestMaryHardForkAtVersion"
          <*> v .:? "TestAlonzoHardForkAtEpoch"
          <*> v .:? "TestAlonzoHardForkAtVersion"
          <*> v .:? "TestBabbageHardForkAtEpoch"
          <*> v .:? "TestBabbageHardForkAtVersion"
          <*> v .:? "TestConwayHardForkAtEpoch"
          <*> v .:? "TestConwayHardForkAtVersion"

instance FromJSON NodeByronProtocolConfiguration where
    parseJSON = withObject "NodeByronProtocolConfiguration" $ \v ->
        NodeByronProtocolConfiguration
          <$> v .: "ByronGenesisFile"
          <*> v .:? "ByronGenesisHash"
          <*> v .:? "RequiresNetworkMagic"
                .!= RequiresNoMagic
          <*> v .:? "PBftSignatureThreshold"
          <*> pure (Byron.ApplicationName "cardano-sl")
          <*> v .:? "ApplicationVersion"
                .!= 1
          <*> v .: "LastKnownBlockVersion-Major"
          <*> v .: "LastKnownBlockVersion-Minor"
          <*> v .: "LastKnownBlockVersion-Alt"
                .!= 0

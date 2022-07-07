{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Tools.DBSynthesizer.Orphans () where

import           Cardano.Node.Types (AdjustFilePaths (..),
                     NodeByronProtocolConfiguration (..),
                     NodeHardForkProtocolConfiguration (..))
import           Cardano.Tools.DBSynthesizer.Types

import qualified Cardano.Chain.Update as Byron (ApplicationName (..))
import           Cardano.Crypto (RequiresNetworkMagic (..))

import           Data.Aeson as Aeson (FromJSON (..), withObject, (.!=), (.:),
                     (.:?))

import           Control.Monad (when)


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

instance AdjustFilePaths NodeConfigStub where
    adjustFilePaths f nc@NodeConfigStub{..} =
        nc {
            ncsAlonzoGenesisFile    = f ncsAlonzoGenesisFile
          , ncsShelleyGenesisFile   = f ncsShelleyGenesisFile
          , ncsByronGenesisFile     = f ncsByronGenesisFile
          }

instance AdjustFilePaths NodeCredentials where
    adjustFilePaths f nc@NodeCredentials{..} =
        nc {
            credCertFile  = f <$> credCertFile
          , credVRFFile   = f <$> credVRFFile
          , credKESFile   = f <$> credKESFile
          , credBulkFile  = f <$> credBulkFile
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

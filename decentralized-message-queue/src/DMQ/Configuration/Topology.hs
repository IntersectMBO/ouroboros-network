{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Configuration.Topology where

import Control.Exception (Exception (..), IOException, try)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Ouroboros.Network.Diffusion.Topology (NetworkTopology (..))
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot)

-- | Read the `NetworkTopology` configuration from the specified file.
--
readTopologyFile
  :: ( FromJSON extraConfig
     , FromJSON extraFlags
     )
  => FilePath
  -> IO (Either Text (NetworkTopology extraConfig extraFlags))
readTopologyFile nc = do
  eBs <- try $ BS.readFile nc

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        case eitherDecode bs' of
          Left err -> return $ Left (handlerJSON err)
          Right t  -> return $ Right t
  where
    handler :: IOException -> Text
    handler e = Text.pack $ "DMQ.Topology.readTopologyFile: "
                          ++ displayException e
    handlerJSON :: String -> Text
    handlerJSON err = mconcat
      [ "Is your topology file formatted correctly? "
      , "Expecting P2P Topology file format. "
      , "The port and valency fields should be numerical. "
      , Text.pack err
      ]

readTopologyFileOrError
  :: ( FromJSON extraConfig
     , FromJSON extraFlags
     )
  => FilePath
  -> IO (NetworkTopology extraConfig extraFlags)
readTopologyFileOrError nc =
      readTopologyFile nc
  >>= either (\err -> error $ "DMQ.Topology.readTopologyFile: "
                           <> Text.unpack err)
             pure

readPeerSnapshotFile :: FilePath -> IO (Either Text LedgerPeerSnapshot)
readPeerSnapshotFile psf = do
  eBs <- try $ BS.readFile psf
  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        case eitherDecode bs' of
          Left err -> return $ Left (handlerJSON err)
          Right t  -> return $ Right t
  where
    handler :: IOException -> Text
    handler e = Text.pack $ "DMQ.Topology.readPeerSnapshotFile: "
                          ++ displayException e
    handlerJSON :: String -> Text
    handlerJSON err = mconcat
      [ "Is your snapshot file formatted correctly? "
      , Text.pack err
      ]

readPeerSnapshotFileOrError :: FilePath -> IO LedgerPeerSnapshot
readPeerSnapshotFileOrError nc =
      readPeerSnapshotFile nc
  >>= either (\err -> error $ "DMQ.Topology.readTopologyFile: "
                           <> Text.unpack err)
             pure

{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Ouroboros.Network.PeerSelection.Governor
        ( PeerSelectionTargets(..), TracePeerSelection(..) )
import Ouroboros.Network.PeerSelection.Types
import Ouroboros.Network.PeerSelection.Test hiding (visualisationTrace)

import Control.Monad.Class.MonadTime
import Control.Exception

import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON, ToJSONKey)
import qualified Test.QuickCheck as QC


main :: IO ()
main = do
  args <- execParser cliDescription
  dumpSimVizLog args


dumpSimVizLog :: SimVizArgs -> IO ()
dumpSimVizLog SimVizArgs{simSize, simTime} = do
    env <- QC.generate (QC.resize simSize QC.arbitrary)
    let trace = visualisationTrace simTime env
    BS.putStrLn $ JS.encode (simplifyEnv env)
    mapM_ (BS.putStrLn . JS.encode) trace

visualisationTrace :: Int
                   -> GovernorMockEnvironment
                   -> [(Time, TracePeerSelection PeerAddr)]
visualisationTrace n =
    takeFirstNHours n
  . selectGovernorEvents
  . selectPeerSelectionTraceEvents
  . runGovernorInMockEnvironment

data SimEnv = SimEnv {
       simPeerGraph       :: Map PeerAddr [PeerAddr],
       simLocalRootPeers  :: [PeerAddr],
       simPublicRootPeers :: [PeerAddr],
       simTargets         :: PeerSelectionTargets
     }
  deriving Generic

simplifyEnv :: GovernorMockEnvironment -> SimEnv
simplifyEnv GovernorMockEnvironment {
              peerGraph,
              localRootPeers,
              publicRootPeers,
              targets
            } =
    SimEnv {..}
  where
    simPeerGraph :: Map PeerAddr [PeerAddr]
    simPeerGraph = Map.fromList [ (addr, peers)
                                | let (PeerGraph g) = peerGraph
                                , (addr, peers, _) <- g ]

    simLocalRootPeers :: [PeerAddr]
    simLocalRootPeers = Map.keys localRootPeers

    simPublicRootPeers :: [PeerAddr]
    simPublicRootPeers = Set.elems publicRootPeers

    simTargets :: PeerSelectionTargets
    simTargets = fst (scriptHead targets)


cliDescription :: ParserInfo SimVizArgs
cliDescription =
  info (parseSimVizArgs <**> helper)
       (fullDesc
     <> progDesc "A tool to dump the log of the Cardano node peer-to-peer network simulation.")

data SimVizArgs = SimVizArgs {
       simSize :: Int, -- QC Gen size
       simTime :: Int  -- Simulated runtime in hours
     }

parseSimVizArgs :: Parser SimVizArgs
parseSimVizArgs =
    SimVizArgs
      <$> parseSize
      <*> parseTime

parseSize :: Parser Int
parseSize =
    option auto (
         long "size"
      <> short 's'
      <> metavar "n"
      <> help "Size of the randomly generated sim environment."
      <> value 30
      <> showDefault
    )

parseTime :: Parser Int
parseTime =
    option auto (
         long "time"
      <> short 't'
      <> metavar "n"
      <> help "The time limit (hours) on the sim in virtual time."
      <> value 24
      <> showDefault
    )

deriving instance ToJSON SimEnv
deriving instance ToJSON PeerSelectionTargets
deriving instance ToJSON PeerAddr
deriving instance ToJSONKey PeerAddr
deriving instance ToJSON PeerStatus
deriving instance ToJSON PeerAdvertise

deriving instance (ToJSON a, ToJSONKey a) => ToJSON (TracePeerSelection a)

instance ToJSON Time where
  toJSON (Time t) = JS.toJSON t

instance ToJSON SomeException where
  toJSON e = JS.toJSON (displayException e)


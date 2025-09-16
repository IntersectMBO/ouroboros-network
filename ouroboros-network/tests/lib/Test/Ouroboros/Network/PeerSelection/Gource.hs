{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.PeerSelection.Gource (gourceVisualisationScript) where

import Control.Monad.Class.MonadTime.SI
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types

import Test.Ouroboros.Network.PeerSelection.Cardano.MockEnvironment
import Test.Ouroboros.Network.PeerSelection.Instances
import Test.Ouroboros.Network.PeerSelection.Utils

--
-- Visualisation of examples
--

-- | Graph visualisation tool, see <https://github.com/acaudwell/Gource/>
--
-- It's not designed for general graphs, just file hierarchies, but it's got
-- a very convenient input format, so not much effort to visualise.
--
gourceVisualisationScript :: GovernorMockEnvironment -> String
gourceVisualisationScript =
    renderGourceScript
  . toGourceScript Map.empty
  . visualisationTrace

visualisationTrace :: GovernorMockEnvironment
                   -> [(Time, TracePeerSelection () () () PeerAddr)]
visualisationTrace =
    takeFirstNHours 24
  . selectGovernorEvents
  . selectPeerSelectionTraceEvents @() @() @() @()
  . runGovernorInMockEnvironment

toGourceScript :: Map PeerAddr (PeerSource, [PeerAddr])
               -> [(Time, TracePeerSelection () () () PeerAddr)]
               -> [GourceEntry]
toGourceScript peers ((ts, TraceLocalRootPeersChanged _ new):trace) =
    -- link new root peers directly to the root as cold
    [ GourceEntry {
        timestamp = ts,
        path      = peeraddr : [],
        modtype   = NodeAdded,
        username  = "local root"
      }
    | peeraddr <- Set.toList (LocalRootPeers.keysSet new) ]
 ++ toGourceScript peers' trace
  where
    peers' = Map.fromList
               [ (peeraddr, (PeerSourceLocalRoot, []))
               | peeraddr <- Set.toList (LocalRootPeers.keysSet new) ]
          <> peers

toGourceScript peers ((ts, TracePublicRootsRequest _ _):trace) =
    GourceEntry {
      timestamp = ts,
      path      = [],
      modtype   = NodeModified,
      username  = "public roots request"
    }
  : toGourceScript peers trace

toGourceScript peers ((ts, TracePublicRootsResults new _ _):trace) =
    -- link new root peers directly to the root as cold
    [ GourceEntry {
        timestamp = ts,
        path      = peeraddr : [],
        modtype   = NodeAdded,
        username  = "public root"
      }
    | peeraddr <- Set.elems (PublicRootPeers.toSet (\_ -> Set.empty) new)
    , peeraddr `Map.notMember` peers
    ]
 ++ toGourceScript peers' trace
  where
    peers' = Map.fromList
               [ (peeraddr, (PeerSourcePublicRoot, []))
               | peeraddr <- Set.elems (PublicRootPeers.toSet (\_ -> Set.empty) new)
               , peeraddr `Map.notMember` peers
               ]
          <> peers

toGourceScript peers ((ts, TracePeerShareRequests _ _ _ _ selected):trace) =
    [ GourceEntry {
        timestamp = ts,
        path      = discoverypath,
        modtype   = NodeModified,
        username  = "peer-sharing"
      }
    | peeraddr <- Set.elems selected
    , let Just (_, discoverypath) = Map.lookup peeraddr peers ]
 ++ toGourceScript peers trace

toGourceScript peers ((ts, TracePeerShareResults results):trace) =
    [ GourceEntry {
        timestamp = ts,
        path      = dstaddr : snd (peers Map.! srcaddr),
        modtype   = NodeAdded,
        username  = "discovered"
      }
    | (srcaddr, Right (PeerSharingResult dstaddrs)) <- results
    , dstaddr <- dstaddrs
    , dstaddr `Map.notMember` peers
    ]
 ++ toGourceScript peers' trace
  where
    peers' = Map.fromList
               [ (dstaddr, (PeerSourcePeerShare, dstaddr : discoverypath))
               | (srcaddr, Right (PeerSharingResult dstaddrs)) <- results
               , dstaddr <- dstaddrs
               , dstaddr `Map.notMember` peers
               , let Just (_, discoverypath) = Map.lookup srcaddr peers ]
          <> peers


toGourceScript peers (_:trace) = toGourceScript peers trace
toGourceScript _     []        = []

-- | See <https://github.com/acaudwell/Gource/wiki/Custom-Log-Format>
--
-- * timestamp - A unix timestamp of when the update occured.
-- * username - The name of the user who made the update.
-- * type - initial for the update type - (A)dded, (M)odified or (D)eleted.
-- * file - Path of the file updated.
-- * colour - A colour for the file in hex (FFFFFF) format. Optional.
--
data GourceEntry = GourceEntry {
       timestamp :: Time,
       path      :: [PeerAddr],
       modtype   :: NodeModification,
       username  :: String
--       colour    :: Colour
     }

data NodeModification = NodeAdded | NodeModified | NodeDeleted

--data Colour = ColourLocalRoot
--            | ColourPublicRoot

renderGourceScript :: [GourceEntry] -> String
renderGourceScript = unlines . map renderGourceEntry

renderGourceEntry :: GourceEntry -> String
renderGourceEntry GourceEntry {
                    timestamp,
                    path,
                    modtype,
                    username
--                  colour
                  } =
    intercalate "|"
      [ renderTime timestamp
      , username
      , renderModType modtype
      , intercalate "/" ("root" : [ show addr | PeerAddr addr <- reverse path ])
                 ++ ".node"
--      , renderColour colour
      ]
  where
    renderTime :: Time -> String
    renderTime t = show (floor (diffTime t (Time 0)) :: Int)

    renderModType NodeAdded    = "A"
    renderModType NodeModified = "M"
    renderModType NodeDeleted  = "D"

--    renderColour ColourLocalRoot  = "FFFFFF"
--    renderColour ColourPublicRoot = "FFFF00"


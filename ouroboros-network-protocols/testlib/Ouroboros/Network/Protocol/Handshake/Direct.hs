{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.Handshake.Direct (pureHandshake) where

import qualified Data.Map as Map

import           Ouroboros.Network.Protocol.Handshake.Version

-- | Pure computation which serves as a reference implementation of the
-- @'Handshake'@ protocol. Useful for testing
-- @'handshakeClientPeer'@ against @'handshakeServerPeer'@
-- using  @'connect'@
--
pureHandshake
  ::forall vNumber vData r. Ord vNumber
  => (vData -> vData -> Maybe vData)
  -> Versions vNumber vData r -- reponders's \/ server's known versions
  -> Versions vNumber vData r -- initiator's \/ client's known versions
  -> (Maybe r, Maybe r)
pureHandshake acceptVersion (Versions serverVersions) (Versions clientVersions) =

      case Map.toDescList $ serverVersions `Map.intersection` clientVersions of

        []             -> (Nothing, Nothing)

        (vNumber, _):_ ->
          case (serverVersions Map.! vNumber, clientVersions Map.! vNumber) of
            ( version, version' ) ->
                ( versionApplication version
                    <$> acceptVersion (versionData version)  (versionData version')
                , versionApplication version'
                    <$> acceptVersion (versionData version') (versionData version)
                )

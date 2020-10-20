{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.Handshake.Direct
  ( pureHandshake
  ) where

import           Data.Typeable (Typeable, cast)
import qualified Data.Map as Map

import           Ouroboros.Network.Protocol.Handshake.Version

-- | Pure computation which serves as a reference implementation of the
-- @'Handshake'@ protocol. Useful for testing
-- @'handshakeClientPeer'@ against @'handshakeServerPeer'@
-- using  @'connect'@
--
pureHandshake
  ::forall vNumber extra r. Ord vNumber
  => (forall vData. extra vData -> Dict Typeable vData)
  -> (forall vData. extra vData -> vData -> vData -> Bool)
  -> Versions vNumber extra r -- reponders's \/ server's known versions
  -> Versions vNumber extra r -- initiator's \/ client's known versions
  -> (Maybe r, Maybe r)
pureHandshake isTypeable acceptVersion (Versions serverVersions) (Versions clientVersions) =

      case Map.toDescList $ serverVersions `Map.intersection` clientVersions of

        []             -> (Nothing, Nothing)

        (vNumber, _):_ -> 
          case (serverVersions Map.! vNumber, clientVersions Map.! vNumber) of

            ( Sigma vData version, Sigma vData' version' ) ->
                  case (isTypeable (versionExtra version), isTypeable (versionExtra version')) of
                        (Dict, Dict) -> case (cast vData, cast vData') of
                          (Just d, Just d') ->
                            ( if acceptVersion (versionExtra version) vData d'
                                then Just $ runApplication (versionApplication version) d'
                                else Nothing

                            , Just $ runApplication (versionApplication version') d
                            )
                          _ -> (Nothing, Nothing)

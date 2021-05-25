{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ouroboros.Network.Protocol.Handshake.Type
  (
  -- * Handshake Protocol
    Handshake (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  , RefuseReason (..)
  , HandshakeProtocolError (..)
  )
  where


import           Control.Exception
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Map (Map)

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))


-- |
-- The handshake mini-protocol is used initially to agree the version and
-- associated parameters of the protocol to use for all subsequent
-- communication.
--
data Handshake vNumber vParams where
    StPropose :: Handshake vNumber vParams
    StConfirm :: Handshake vNumber vParams
    StDone    :: Handshake vNumber vParams

instance ShowProxy (Handshake vNumber vParams) where
    showProxy _ = "Handshake"

-- |
-- Reasons by which a server can refuse proposed version.
--
data RefuseReason vNumber
  -- | All of the prosed versions where not known to the server.
  -- Since the server sends all versions that it can knows about, some of them
  -- we might not be able to decode, so we include raw tags @[Int]@.
  --
  = VersionMismatch [vNumber] [Int]

  -- | The server failed to decode version parameters.
  --
  | HandshakeDecodeError vNumber Text

  -- | The server refused to run the proposed version parameters
  --
  | Refused vNumber Text
  deriving (Eq, Show)

instance (Typeable vNumber, Show vNumber) => Exception (RefuseReason vNumber)


instance Protocol (Handshake vNumber vParams) where

    data Message (Handshake vNumber vParams) from to where

      -- |
      -- Propose versions together with version parameters.  It must be
      -- encoded to a sorted list.
      --
      MsgProposeVersions
        :: Map vNumber vParams
        -> Message (Handshake vNumber vParams) StPropose StConfirm

      -- |
      -- The remote end decides which version to use and sends chosen version.
      -- The server is allowed to modify version parameters. 
      --
      MsgAcceptVersion
        :: vNumber
        -> vParams
        -> Message (Handshake vNumber vParams) StConfirm StDone

      -- |
      -- or it refuses to run any version,
      --
      MsgRefuse
        :: RefuseReason vNumber
        -> Message (Handshake vNumber vParams) StConfirm StDone

    data ClientHasAgency st where
      TokPropose :: ClientHasAgency StPropose

    data ServerHasAgency st where
      TokConfirm :: ServerHasAgency StConfirm

    data NobodyHasAgency st where
      TokDone    :: NobodyHasAgency StDone

    exclusionLemma_ClientAndServerHaveAgency TokPropose tok = case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone    tok = case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone    tok = case tok of {}

deriving instance (Show vNumber, Show vParams)
    => Show (Message (Handshake vNumber vParams) from to)

instance Show (ClientHasAgency (st :: Handshake vNumber vParams)) where
    show TokPropose = "TokPropose"

instance Show (ServerHasAgency (st :: Handshake vNumber vParams)) where
    show TokConfirm = "TokConfirm"

-- | Extends handshake error @'RefuseReason'@ type, by client specific errors.
--
data HandshakeProtocolError vNumber
  = HandshakeError (RefuseReason vNumber)
  | NotRecognisedVersion vNumber
  | InvalidServerSelection vNumber Text
  deriving (Eq, Show)

instance (Typeable vNumber, Show vNumber)
    => Exception (HandshakeProtocolError vNumber)

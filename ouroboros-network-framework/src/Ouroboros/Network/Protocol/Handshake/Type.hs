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
    -- $simultanous-open
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
      -- `MsgProposeVersions'` received as a response to 'MsgProposeVersions'.
      -- It is not supported to explicitly send this message. It can only be
      -- received as a copy of 'MsgProposeVersions' in a simultanous open
      -- scenario.
      --
      MsgProposeVersions'
        :: Map vNumber vParams
        -> Message (Handshake vNumber vParams) StConfirm StDone

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

-- $simultanous-open
--
-- On simultanous open both sides will send `MsgProposeVersions`, which will be
-- decoded as `MsgProposeVersions'` which is a terminal message of the
-- protocol.  It is important to stress that in this case both sides will make
-- the choice which version and parameters to pick.  Our algorithm for picking
-- version is symmetric, which ensures that both sides will endup with the same
-- choice.  If one side decides to refuse the version it will close the
-- connection, without sending the reason to the other side.

deriving instance (Show vNumber, Show vParams)
    => Show (Message (Handshake vNumber vParams) from to)

instance Show (ClientHasAgency (st :: Handshake vNumber vParams)) where
    show TokPropose       = "TokPropose"

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

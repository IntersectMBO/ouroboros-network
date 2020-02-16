{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Network.Mux
  ( AppType (..)
  , OuroborosApplication (..)
  , MuxPeer (..)
  , ProtocolEnum (..)
  , MiniProtocolLimits (..)
  , MiniProtocolNum (..)
  , fromInitiatorAndResponderToResponder
  , runMuxPeer
  , simpleInitiatorApplication
  , simpleResponderApplication

  , toApplication
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Exception (Exception)
import           Control.Tracer (Tracer)

import           Data.Void (Void)
import           Data.Int (Int64)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Network.Mux.Types hiding (MiniProtocolLimits(..))
import qualified Network.Mux.Types as Mux

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver


-- |  Like 'MuxApplication' but using a more polymorphic 'Channel' type.
--
data OuroborosApplication (appType :: AppType) ptcl bytes m a b where
     OuroborosInitiatorApplication
       :: (ptcl -> Channel m bytes -> m a)
       -> OuroborosApplication InitiatorApp ptcl bytes m a Void

     OuroborosResponderApplication
       :: (ptcl -> Channel m bytes -> m b)
       -> OuroborosApplication ResponderApp ptcl bytes m Void b

     OuroborosInitiatorAndResponderApplication
       :: (ptcl -> Channel m bytes -> m a)
       -> (ptcl -> Channel m bytes -> m b)
       -> OuroborosApplication InitiatorAndResponderApp ptcl bytes m a b

--TODO: the OuroborosApplication type needs to be updated to follow the new
-- structure of the MuxApplication, which no longer uses the bounded enumeration
-- idiom. For now, toApplication converts from the bounded enumeration style to
-- the simple list style that MuxApplication now uses.

class ProtocolEnum ptcl where
    fromProtocolEnum :: ptcl -> MiniProtocolNum

class MiniProtocolLimits ptcl where
    maximumIngressQueue :: ptcl -> Int64

toApplication :: (Enum ptcl, Bounded ptcl,
                  ProtocolEnum ptcl, MiniProtocolLimits ptcl)
              => OuroborosApplication appType ptcl LBS.ByteString m a b
              -> MuxApplication appType m a b
toApplication (OuroborosInitiatorApplication f) =
    MuxApplication
      [ MuxMiniProtocol {
          miniProtocolNum    = fromProtocolEnum ptcl,
          miniProtocolLimits = Mux.MiniProtocolLimits {
                                 Mux.maximumIngressQueue = maximumIngressQueue ptcl
                               },
          miniProtocolRun    =
            InitiatorProtocolOnly
              (\channel -> f ptcl (fromChannel channel))
        }
      | ptcl <- [minBound..maxBound] ]

toApplication (OuroborosResponderApplication f) =
    MuxApplication
      [ MuxMiniProtocol {
          miniProtocolNum    = fromProtocolEnum ptcl,
          miniProtocolLimits = Mux.MiniProtocolLimits {
                                 Mux.maximumIngressQueue = maximumIngressQueue ptcl
                               },
          miniProtocolRun    =
            ResponderProtocolOnly
              (\channel -> f ptcl (fromChannel channel))
        }
      | ptcl <- [minBound..maxBound] ]

toApplication (OuroborosInitiatorAndResponderApplication f g) =
    MuxApplication
      [ MuxMiniProtocol {
          miniProtocolNum    = fromProtocolEnum ptcl,
          miniProtocolLimits = Mux.MiniProtocolLimits {
                                 Mux.maximumIngressQueue = maximumIngressQueue ptcl
                               },
          miniProtocolRun    =
            InitiatorAndResponderProtocol
              (\channel -> f ptcl (fromChannel channel))
              (\channel -> g ptcl (fromChannel channel))
        }
      | ptcl <- [minBound..maxBound] ]


-- |
-- Extract the responder part of an InitiatorAndResponderApp.
fromInitiatorAndResponderToResponder
    :: OuroborosApplication InitiatorAndResponderApp ptcl bytes m a b
    -> OuroborosApplication ResponderApp ptcl bytes m Void b
fromInitiatorAndResponderToResponder
  (OuroborosInitiatorAndResponderApplication _ g ) = OuroborosResponderApplication g


-- |
-- This type is only necessary to use the @'simpleMuxClient'@ and
-- @'simpleMuxServer'@ smart constructors.
--
data MuxPeer failure m bytes a where
    MuxPeer :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer failure m bytes a

    MuxPeerPipelined
            :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer failure m bytes a


-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadThrow m
     , MonadCatch m
     , MonadAsync m
     , Exception failure
     )
  => MuxPeer failure m bytes a
  -> Channel m bytes
  -> m a
runMuxPeer (MuxPeer tracer codec peer) channel =
    runPeer tracer codec channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) channel =
    runPipelinedPeer tracer codec channel peer


-- |
-- Smart constructor for @'MuxInitiatorApplication'@.  It is a simple client, since
-- none of the applications requires resource handling to run in the monad @m@.
-- Each one is simply run either by @'runPeer'@ or @'runPipelinedPeer'@.
--
simpleInitiatorApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer failure m bytes a)
  -> OuroborosApplication InitiatorApp ptcl bytes m a Void
simpleInitiatorApplication fn = OuroborosInitiatorApplication $ \ptcl channel ->
  runMuxPeer (fn ptcl) channel


-- |
-- Smart constructor for @'MuxResponderApplicatin'@, similar to @'simpleMuxInitiator'@.
--
simpleResponderApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer failure m bytes a)
  -> OuroborosApplication ResponderApp ptcl bytes m Void a
simpleResponderApplication fn = OuroborosResponderApplication $ \ptcl channel ->
  runMuxPeer (fn ptcl) channel

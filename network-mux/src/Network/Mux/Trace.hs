{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ViewPatterns              #-}

-- TODO: needed with GHC-8.10
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Network.Mux.Trace
  ( -- * Exceptions
    Error (..)
  , handleIOException
    -- * Trace events
  , Trace (..)
  , ChannelTrace (..)
  , BearerTrace (..)
    -- * Tracers
  , Tracers' (.., TracersI, tracer_, channelTracer_, bearerTracer_)
  , contramapTracers'
  , Tracers
  , nullTracers
  , tracersWith
  , TracersWithBearer
  , tracersWithBearer
    -- * Tracing wrappers
  , WithBearer (..)
  , TraceLabelPeer (..)
    -- * State
  , State (..)
  ) where

import Prelude hiding (read)

import Formatting (formatToString, (%+))
import Formatting qualified as F

import Control.Exception hiding (throwIO)
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, nullTracer)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Functor.Identity
import GHC.Generics (Generic (..))
import Quiet (Quiet (..))

import Network.Mux.Types


--
-- Errors
--

-- | Enumeration of error conditions.
--
data Error = UnknownMiniProtocol MiniProtocolNum
           -- ^ returned by 'decodeSDUHeader', thrown by 'Bearer'.
           | BearerClosed String
           -- ^ thrown by 'Bearer' when received a null byte.
           | IngressQueueOverRun MiniProtocolNum MiniProtocolDir
           -- ^ thrown by 'demux' when violating 'maximumIngressQueue'
           -- byte limit.
           | InitiatorOnly MiniProtocolNum
           -- ^ thrown when data arrives on a responder channel when the
           -- mux was set up as an 'InitiatorApp'.
           | IOException IOException String
           -- ^ 'IOException' thrown by

           | SDUDecodeError String
           -- ^ return by 'decodeSDUHeader', thrown by 'Bearer'.
           | SDUReadTimeout
           -- ^ thrown when reading of a single SDU takes too long
           | SDUWriteTimeout
           -- ^ thrown when writing a single SDU takes too long

           | Shutdown (Maybe SomeException) Status
           -- ^ Result of runMiniProtocol's completionAction in case of
           -- an error or mux being closed while a mini-protocol was
           -- still running, this is not a clean exit.
           deriving Show

instance Exception Error where
  displayException = \case
    UnknownMiniProtocol pnum      -> formatToString ("unknown mini-protocol" %+ F.shown) pnum
    BearerClosed msg              -> formatToString ( "bearer closed:" %+ F.shown) msg
    IngressQueueOverRun pnum pdir -> formatToString ("ingress queue overrun for" %+ F.shown %+ F.shown) pnum pdir
    InitiatorOnly pnum            -> formatToString ("received data on initiator only protocol" %+ F.shown) pnum
    IOException e msg             -> formatToString (F.string F.% ":" %+ F.string) (displayException e) msg
    SDUDecodeError msg            -> formatToString ("SDU decode error:" %+ F.string) msg
    SDUReadTimeout                -> "SDU read timeout expired"
    SDUWriteTimeout               -> "SDU write timeout expired"
    Shutdown Nothing st           -> formatToString ("mux shutdown error in state" %+ F.shown) st
    Shutdown (Just e) st          -> formatToString ("mux shutdown error" %+ F.parenthesised F.string %+ "in state" %+ F.shown) (displayException e) st

-- | Handler for 'IOException's which wraps them in 'Error'.
--
-- It is used various 'Bearer' implementations:
-- * 'socketAsBearer'
-- * 'pipeAsBearer'
--
handleIOException :: MonadThrow m => String -> IOException -> m a
handleIOException msg e = throwIO (IOException e msg)


--
-- Tracing
--

-- | A peer label for use in 'Tracer's. This annotates tracer output as being
-- associated with a given peer identifier.
--
data TraceLabelPeer peerid a = TraceLabelPeer peerid a
  deriving (Eq, Functor, Show)

instance Bifunctor TraceLabelPeer where
  bimap f g (TraceLabelPeer a b) = TraceLabelPeer (f a) (g b)

-- | Type used for tracing mux events.
--
data WithBearer peerid a = WithBearer {
      wbPeerId :: !peerid
      -- ^ A tag that should identify a specific mux bearer.
    , wbEvent  :: !a
  }
  deriving (Generic)
  deriving Show via (Quiet (WithBearer peerid a))
--TODO: probably remove this type


-- | Mid-level channel events traced independently by each mini protocol job.
--
data ChannelTrace =
      TraceChannelRecvStart MiniProtocolNum
    | TraceChannelRecvEnd MiniProtocolNum Int
    | TraceChannelSendStart MiniProtocolNum Int
    | TraceChannelSendEnd MiniProtocolNum

instance Show ChannelTrace where
    show (TraceChannelRecvStart mid) =
      formatToString ("Channel Receive Start on" %+ F.shown) mid
    show (TraceChannelRecvEnd mid len)   =
      formatToString ("Channel Receive End on" %+ F.parenthesised F.shown %+ F.int) mid len
    show (TraceChannelSendStart mid len) =
      formatToString ("Channel Send Start on" %+ F.parenthesised F.shown %+ F.int) mid len
    show (TraceChannelSendEnd mid) =
      formatToString ("Channel Send End on" %+ F.shown) mid


data State = Mature
             -- ^ `Mux started ingress, and egress threads
           | Dead
             -- ^ Mux is being shutdown.
           deriving (Eq, Show)

-- | High-level mux events.
--
data Trace =
      TraceState State
    | TraceCleanExit MiniProtocolNum MiniProtocolDir
    | TraceExceptionExit MiniProtocolNum MiniProtocolDir SomeException
    | TraceStartEagerly MiniProtocolNum MiniProtocolDir
    | TraceStartOnDemand MiniProtocolNum MiniProtocolDir
    | TraceStartOnDemandAny MiniProtocolNum MiniProtocolDir
    | TraceStartedOnDemand MiniProtocolNum MiniProtocolDir
    | TraceTerminating MiniProtocolNum MiniProtocolDir
    | forall mode. TraceNewMux [MiniProtocolInfo mode]
    | TraceStarting
    | TraceStopping
    | TraceStopped

instance Show Trace where
    show (TraceState new) =
      formatToString ("State:" %+ F.shown) new
    show (TraceCleanExit mid dir) =
      formatToString
        ("Miniprotocol" %+ F.parenthesised F.shown %+ F.shown %+ "terminated cleanly")
        mid dir
    show (TraceExceptionExit mid dir e) =
      formatToString
        ("Miniprotocol" %+ F.shown %+ F.shown %+ "terminated with exception" %+ F.shown)
        mid dir e
    show (TraceStartEagerly mid dir) =
      formatToString
        ("Eagerly started" %+ F.parenthesised F.shown %+ "in" %+ F.shown)
        mid dir
    show (TraceStartOnDemand mid dir) =
      formatToString
        ("Preparing to start" %+ F.parenthesised F.shown %+ "in" %+ F.shown)
        mid dir
    show (TraceStartOnDemandAny mid dir) =
      formatToString
        ("Preparing to start on any" %+ F.parenthesised F.shown %+ "in" %+ F.shown)
        mid dir
    show (TraceStartedOnDemand mid dir) =
      formatToString
        ("Started on demand" %+ F.parenthesised F.shown %+ "in" %+ F.shown)
        mid dir
    show (TraceTerminating mid dir) =
      formatToString
        ("Terminating" %+ F.parenthesised F.shown %+ "in" %+ F.shown)
        mid dir
    show (TraceNewMux infos) =
      formatToString
        ("New mux with protocols:" %+ F.commaSpaceSep F.shown)
        infos
    show TraceStarting = "Mux starting"
    show TraceStopping = "Mux stopping"
    show TraceStopped  = "Mux stoppped"


-- | Bundle of tracers used directly by mux.
--
data Tracers' m f = Tracers {
    tracer        :: Tracer m (f Trace),
    -- ^ high-level tracer of mux state events

    channelTracer :: Tracer m (f ChannelTrace),
    -- ^ channel tracer

    bearerTracer  :: Tracer m (f BearerTrace)
    -- ^ high-frequency tracer
  }

type Tracers m = Tracers' m Identity


-- | Trace all events through one polymorphic tracer.
--
tracersWith :: (forall x. Tracer m x) -> Tracers' m f
tracersWith tr = Tracers {
    tracer        = tr,
    channelTracer = tr,
    bearerTracer  = tr
  }


nullTracers :: Monad m => Tracers' m f
nullTracers = tracersWith nullTracer


-- | A convenient bidirectional pattern synonym which (un)wraps the `Identity`
-- functor in the `Tracer` type.
--
pattern TracersI :: forall m.
                    Monad m =>
                    Tracer m Trace
                 -> Tracer m ChannelTrace
                 -> Tracer m BearerTrace
                 -> Tracers m
pattern TracersI { tracer_, channelTracer_, bearerTracer_ } <-
    Tracers { tracer        = contramap Identity -> tracer_,
              channelTracer = contramap Identity -> channelTracer_,
              bearerTracer  = contramap Identity -> bearerTracer_
            }
  where
    TracersI tracer' channelTracer' bearerTracer' =
      Tracers {
         tracer        = runIdentity >$< tracer',
         channelTracer = runIdentity >$< channelTracer',
         bearerTracer  = runIdentity >$< bearerTracer'
       }

{-# COMPLETE TracersI #-}

-- | Contravariant natural transformation of `Tracers' m`.
--
contramapTracers' :: Monad m
                  => (forall x. f' x -> f x)
                  -> Tracers' m f -> Tracers' m f'
contramapTracers'
  f
  Tracers { tracer,
            channelTracer,
            bearerTracer
          }
  =
  Tracers { tracer        = f >$< tracer,
            channelTracer = f >$< channelTracer,
            bearerTracer  = f >$< bearerTracer
          }


type TracersWithBearer connId m = Tracers' m (WithBearer connId)

tracersWithBearer :: Monad m => peerId -> TracersWithBearer peerId m -> Tracers m
tracersWithBearer peerId = contramapTracers' (WithBearer peerId . runIdentity)

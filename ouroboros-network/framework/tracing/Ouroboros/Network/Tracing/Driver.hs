{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Tracing.Driver () where

import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.Aeson (Value (String), (.=), (.?=))

import Cardano.Logging
import Ouroboros.Network.Driver.Simple qualified as Simple
import Ouroboros.Network.Driver.Stateful qualified as Stateful

import Network.TypedProtocol.Codec qualified as Simple
import Network.TypedProtocol.Stateful.Codec qualified as Stateful

-- | Render the mux bearer time as seconds since process start. This is the
-- same shape the immdb-server's hand-rolled SendRecv tracer uses (before the
-- 'tmf' conversion to UTCTime); downstream analysis joins by sequence number,
-- so an unconverted Double is sufficient.
jsonMuxAt :: Time -> Double
jsonMuxAt (Time t) = realToFrac t

-------------------------------------------------------------------------------
-- Driver Simple.
-------------------------------------------------------------------------------

instance LogFormatting (Simple.AnyMessage ps)
      => LogFormatting (Simple.TraceSendRecv ps) where
  forMachine dtal (Simple.TraceSendMsg tm m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m, "mux_at" .= jsonMuxAt tm ]
  forMachine dtal (Simple.TraceRecvMsg mbTm m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m, "mux_at" .?= fmap jsonMuxAt mbTm ]

  forHuman (Simple.TraceSendMsg _ m) = "Send: " <> forHuman m
  forHuman (Simple.TraceRecvMsg _ m) = "Receive: " <> forHuman m

  asMetrics (Simple.TraceSendMsg _ m) = asMetrics m
  asMetrics (Simple.TraceRecvMsg _ m) = asMetrics m

instance LogFormatting (Stateful.AnyMessage ps f)
      => LogFormatting (Stateful.TraceSendRecv ps f) where
  forMachine dtal (Stateful.TraceSendMsg tm m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m, "mux_at" .= jsonMuxAt tm ]
  forMachine dtal (Stateful.TraceRecvMsg mbTm m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m, "mux_at" .?= fmap jsonMuxAt mbTm ]

  forHuman (Stateful.TraceSendMsg _ m) = "Send: " <> forHuman m
  forHuman (Stateful.TraceRecvMsg _ m) = "Receive: " <> forHuman m

  asMetrics (Stateful.TraceSendMsg _ m) = asMetrics m
  asMetrics (Stateful.TraceRecvMsg _ m) = asMetrics m

instance MetaTrace (Simple.AnyMessage ps) =>
            MetaTrace (Simple.TraceSendRecv ps) where
  namespaceFor (Simple.TraceSendMsg _ msg) =
    nsPrependInner "Send" (namespaceFor msg)
  namespaceFor (Simple.TraceRecvMsg _ msg) =
    nsPrependInner "Receive" (namespaceFor msg)

  severityFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg _ msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Send" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  severityFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg _ msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Receive" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg _ msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Send" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  privacyFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg _ msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Receive" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg _ msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Send" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  detailsFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg _ msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Receive" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  detailsFor _ _ = Nothing

  metricsDocFor (Namespace out ("Send" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  metricsDocFor (Namespace out ("Receive" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  metricsDocFor _ = []

  documentFor (Namespace out ("Send" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  documentFor (Namespace out ("Receive" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  documentFor _ = Nothing

  allNamespaces =
    let cn = allNamespaces :: [Namespace (Simple.AnyMessage ps)]
    in fmap (nsPrependInner "Send") cn ++ fmap (nsPrependInner "Receive") cn

instance MetaTrace (Stateful.AnyMessage ps f) =>
            MetaTrace (Stateful.TraceSendRecv ps f) where
  namespaceFor (Stateful.TraceSendMsg _ msg) =
    nsPrependInner "Send" (namespaceFor msg)
  namespaceFor (Stateful.TraceRecvMsg _ msg) =
    nsPrependInner "Receive" (namespaceFor msg)

  severityFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg _ msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Send" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing

  severityFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg _ msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Receive" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg _ msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Send" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  privacyFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg _ msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Receive" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg _ msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Send" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  detailsFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg _ msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Receive" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  detailsFor _ _ = Nothing

  metricsDocFor (Namespace out ("Send" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  metricsDocFor (Namespace out ("Receive" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  metricsDocFor _ = []

  documentFor (Namespace out ("Send" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  documentFor (Namespace out ("Receive" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  documentFor _ = Nothing

  allNamespaces =
    let cn = allNamespaces :: [Namespace (Stateful.AnyMessage ps f)]
    in fmap (nsPrependInner "Send") cn ++ fmap (nsPrependInner "Receive") cn

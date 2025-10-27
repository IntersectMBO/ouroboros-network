{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Ouroboros.Network.Logging.Framework.Driver () where

import           Cardano.Logging
import qualified Ouroboros.Network.Driver.Simple as Simple
import qualified Ouroboros.Network.Driver.Stateful as Stateful
-- Package: aeson.
import           Data.Aeson (Value (String), (.=))
-- Package: typed-protocols.
import qualified Network.TypedProtocol.Codec as Simple
import qualified Network.TypedProtocol.Stateful.Codec as Stateful

-------------------------------------------------------------------------------
-- Driver Simple.
-------------------------------------------------------------------------------

-- From `Cardano.Node.Tracing.Tracers.NodeToClient`
-- Branch

instance LogFormatting (Simple.AnyMessage ps)
      => LogFormatting (Simple.TraceSendRecv ps) where
  forMachine dtal (Simple.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (Simple.TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (Simple.TraceSendMsg m) = "Send: " <> forHumanOrMachine m
  forHuman (Simple.TraceRecvMsg m) = "Receive: " <> forHumanOrMachine m

  asMetrics (Simple.TraceSendMsg m) = asMetrics m
  asMetrics (Simple.TraceRecvMsg m) = asMetrics m

instance LogFormatting (Stateful.AnyMessage ps f)
      => LogFormatting (Stateful.TraceSendRecv ps f) where
  forMachine dtal (Stateful.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (Stateful.TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (Stateful.TraceSendMsg m) = "Send: " <> forHumanOrMachine m
  forHuman (Stateful.TraceRecvMsg m) = "Receive: " <> forHumanOrMachine m

  asMetrics (Stateful.TraceSendMsg m) = asMetrics m
  asMetrics (Stateful.TraceRecvMsg m) = asMetrics m

instance MetaTrace (Simple.AnyMessage ps) =>
            MetaTrace (Simple.TraceSendRecv ps) where
  namespaceFor (Simple.TraceSendMsg msg) =
    nsPrependInner "Send" (namespaceFor msg)
  namespaceFor (Simple.TraceRecvMsg msg) =
    nsPrependInner "Receive" (namespaceFor msg)

  severityFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Send" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  severityFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Receive" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Send" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  privacyFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Receive" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Send" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  detailsFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg msg)) =
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
  namespaceFor (Stateful.TraceSendMsg msg) =
    nsPrependInner "Send" (namespaceFor msg)
  namespaceFor (Stateful.TraceRecvMsg msg) =
    nsPrependInner "Receive" (namespaceFor msg)

  severityFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Send" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  severityFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Receive" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Send" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  privacyFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Receive" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Send" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  detailsFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg msg)) =
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


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.KESAgent.Processes.Agent.CommonActions
where

import Cardano.Crypto.KES.Class (
  KESAlgorithm (..),
  SignKeyWithPeriodKES (..),
  deriveVerKeyKES,
  forgetSignKeyKES,
 )
import Control.Concurrent.Class.MonadSTM (MonadSTM, atomically)
import Control.Concurrent.Class.MonadSTM.TChan (
  newBroadcastTChan,
  writeTChan,
 )
import Control.Concurrent.Class.MonadSTM.TMVar (
  TMVar,
  newTMVarIO,
  putTMVar,
  takeTMVar,
 )
import Control.Monad (void)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Tracer (Tracer (..), traceWith)
import Data.Functor.Contravariant (contramap)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Snocket (..))
import Text.Printf

import Cardano.KESAgent.KES.Bundle (Bundle (..), TaggedBundle (..), Timestamp (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.Evolution (
  getCurrentKESPeriodWith,
  updateKESTo,
 )
import Cardano.KESAgent.KES.OCert (
  KESPeriod (..),
  OCert (..),
  validateOCert,
 )
import Cardano.KESAgent.Processes.Agent.Context
import Cardano.KESAgent.Processes.Agent.Type
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import Cardano.KESAgent.Util.RefCounting (
  CRef,
  CRefEvent (..),
  acquireCRef,
  newCRefWith,
  releaseCRef,
  withCRefValue,
 )

checkEvolution :: AgentContext m c => Agent c m fd addr -> m ()
checkEvolution agent = do
  p' <-
    getCurrentKESPeriodWith
      (agentGetCurrentTime $ agentOptions agent)
      (agentEvolutionConfig $ agentOptions agent)
  agentTrace agent $ AgentCheckEvolution p'
  alterBundle agent "checkEvolution" $ \bundleMay -> do
    case bundleMay of
      Nothing -> do
        agentTrace agent AgentNoKeyToEvolve
        return (Nothing, ())
      Just TaggedBundle {taggedBundle = Nothing} -> do
        agentTrace agent AgentNoKeyToEvolve
        return (Nothing, ())
      Just
        TaggedBundle
          { taggedBundle = Just (Bundle keyVar oc)
          , taggedBundleTimestamp = timestamp
          } ->
          withCRefValue keyVar $ \key -> do
            let p = KESPeriod $ unKESPeriod (ocertKESPeriod oc) + periodKES key
            if p < p'
              then do
                keyMay' <- updateKESTo () p' oc key
                case keyMay' of
                  Nothing -> do
                    agentTrace agent $ AgentKeyExpired p p'
                    releaseCRef keyVar
                    return (Nothing, ())
                  Just key' -> do
                    agentTrace agent $ AgentKeyEvolved p p'
                    keyVar' <- newCRefWith (agentCRefTracer agent) (forgetSignKeyKES . skWithoutPeriodKES) key'
                    releaseCRef keyVar
                    return
                      ( Just
                          TaggedBundle
                            { taggedBundle = Just (Bundle keyVar' oc)
                            , taggedBundleTimestamp = timestamp
                            }
                      , ()
                      )
              else do
                agentTrace agent $ AgentKeyNotEvolved p p'
                return (bundleMay, ())

agentTrace :: Agent c m fd addr -> AgentTrace -> m ()
agentTrace agent = traceWith (agentTracer . agentOptions $ agent)

agentCRefTracer :: Agent c m fd addr -> Tracer m CRefEvent
agentCRefTracer = contramap AgentCRefEvent . agentTracer . agentOptions

alterBundle ::
  (MonadSTM m, MonadThrow m) =>
  Agent c m fd addr ->
  String ->
  (Maybe (TaggedBundle m c) -> m (Maybe (TaggedBundle m c), a)) ->
  m a
alterBundle agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentCurrentKeyVar agent)) >>= \bundle -> do
    agentTrace agent (AgentLockAcquired context)
    (bundle', retval) <- f bundle
    atomically $ putTMVar (agentCurrentKeyVar agent) bundle'
    agentTrace agent (AgentLockReleased context)
    return retval

withBundle ::
  (MonadSTM m, MonadThrow m) =>
  Agent c m fd addr ->
  String ->
  (Maybe (TaggedBundle m c) -> m a) ->
  m a
withBundle agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentCurrentKeyVar agent)) >>= \bundle -> do
    agentTrace agent (AgentLockAcquired context)
    retval <- f bundle
    atomically $ putTMVar (agentCurrentKeyVar agent) bundle
    agentTrace agent (AgentLockReleased context)
    return retval

alterStagedKey ::
  (MonadSTM m, MonadThrow m) =>
  Agent c m fd addr ->
  String ->
  ( Maybe (CRef m (SignKeyWithPeriodKES (KES c))) ->
    m (Maybe (CRef m (SignKeyWithPeriodKES (KES c))), a)
  ) ->
  m a
alterStagedKey agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentStagedKeyVar agent)) >>= \skp -> do
    agentTrace agent (AgentLockAcquired context)
    (skp', retval) <- f skp
    atomically $ putTMVar (agentStagedKeyVar agent) skp'
    agentTrace agent (AgentLockReleased context)
    return retval

withStagedKey ::
  (MonadSTM m, MonadThrow m) =>
  Agent c m fd addr ->
  String ->
  (Maybe (CRef m (SignKeyWithPeriodKES (KES c))) -> m a) ->
  m a
withStagedKey agent context f = do
  agentTrace agent $ AgentLockRequest context
  atomically (takeTMVar (agentStagedKeyVar agent)) >>= \skp -> do
    agentTrace agent (AgentLockAcquired context)
    retval <- f skp
    atomically $ putTMVar (agentStagedKeyVar agent) skp
    agentTrace agent (AgentLockReleased context)
    return retval

formatMaybeKey :: Timestamp -> Maybe (OCert c) -> String
formatMaybeKey ts Nothing =
  printf "DELETE (%lu)" (timestampValue ts)
formatMaybeKey ts (Just ocert) =
  formatKey ts ocert

formatKey :: Timestamp -> OCert c -> String
formatKey ts ocert =
  let serialNumber = ocertN ocert
  in printf "%i (%lu)" serialNumber (timestampValue ts)

newAgent ::
  forall c m fd addr.
  Monad m =>
  MonadSTM m =>
  Show addr =>
  Show fd =>
  Proxy c ->
  Snocket m fd addr ->
  MakeRawBearer m fd ->
  AgentOptions m addr c ->
  m (Agent c m fd addr)
newAgent _p s mrb options = do
  stagedKeyVar :: TMVar m (Maybe (CRef m (SignKeyWithPeriodKES (KES c)))) <-
    newTMVarIO Nothing
  currentKeyVar :: TMVar m (Maybe (TaggedBundle m c)) <-
    newTMVarIO Nothing
  nextKeyChan <- atomically newBroadcastTChan
  bootstrapConnectionsVar <- newTMVarIO mempty

  serviceFD <- openFD (agentServiceAddr options)
  controlFD <- mapM openFD (agentControlAddr options)

  return
    Agent
      { agentSnocket = s
      , agentMRB = mrb
      , agentOptions = options
      , agentStagedKeyVar = stagedKeyVar
      , agentCurrentKeyVar = currentKeyVar
      , agentNextKeyChan = nextKeyChan
      , agentServiceFD = serviceFD
      , agentControlFD = controlFD
      , agentBootstrapConnections = bootstrapConnectionsVar
      }
  where
    openFD addr = do
      fd <- open s (addrFamily s addr)
      bind s fd addr
      return fd

finalizeAgent :: Monad m => Agent c m fd addr -> m ()
finalizeAgent agent = do
  let s = agentSnocket agent
  close s (agentServiceFD agent)
  mapM_ (close s) (agentControlFD agent)

data PushKeyResult c
  = PushKeyOK (Maybe (OCert c))
  | PushKeyInvalidOCert String
  | PushKeyTooOld

pushKeyResultToRecvResult :: PushKeyResult c -> RecvResult
pushKeyResultToRecvResult PushKeyOK {} = RecvOK
pushKeyResultToRecvResult PushKeyInvalidOCert {} = RecvErrorInvalidOpCert
pushKeyResultToRecvResult PushKeyTooOld = RecvErrorKeyOutdated

pushKeyResultOCert :: PushKeyResult c -> Maybe (OCert c)
pushKeyResultOCert (PushKeyOK ocm) = ocm
pushKeyResultOCert _ = Nothing

pushKeyResultVKey :: PushKeyResult c -> Maybe (VerKeyKES (KES c))
pushKeyResultVKey pkr = ocertVkHot <$> pushKeyResultOCert pkr

validateBundle ::
  AgentContext m c =>
  Agent c m fd add ->
  Bundle m c ->
  m (Either String ())
validateBundle agent bundle = do
  vkKES <- withCRefValue (bundleSKP bundle) (deriveVerKeyKES . skWithoutPeriodKES)
  return $
    validateOCert
      (agentColdVerKey (agentOptions agent))
      vkKES
      (bundleOC bundle)

validateTaggedBundle ::
  AgentContext m c =>
  Agent c m fd add ->
  TaggedBundle m c ->
  m (Either String ())
validateTaggedBundle _ TaggedBundle {taggedBundle = Nothing} = return (Right ())
validateTaggedBundle agent TaggedBundle {taggedBundle = Just bundle} = validateBundle agent bundle

isTaggedBundleAgeValid ::
  TaggedBundle m c ->
  TaggedBundle m c ->
  Bool
isTaggedBundleAgeValid current new =
  checkTimestamp && checkOCerts
  where
    checkTimestamp
      -- When both timestamps match, deletions trump creation
      | isNothing (taggedBundle new)
      , isJust (taggedBundle current) =
          taggedBundleTimestamp current <= taggedBundleTimestamp new
      | otherwise =
          taggedBundleTimestamp current < taggedBundleTimestamp new
    checkOCerts =
      let snCurrentMay = (ocertN . bundleOC) <$> taggedBundle current
          snNewMay = (ocertN . bundleOC) <$> taggedBundle new
      in case (snCurrentMay, snNewMay) of
          (Just snCurrent, Just snNew) ->
            snCurrent <= snNew
          _ -> True

pushKey ::
  forall c m fd addr.
  AgentContext m c =>
  Agent c m fd addr ->
  TaggedBundle m c ->
  m (PushKeyResult c)
pushKey agent tbundle = do
  validateTaggedBundle agent tbundle
    >>= either
      handleInvalidBundle
      (\() -> (handleValidBundle <* checkEvolution agent))
  where
    handleInvalidBundle :: String -> m (PushKeyResult c)
    handleInvalidBundle err = do
      agentTrace agent $ AgentRejectingKey err
      return (PushKeyInvalidOCert err)

    releaseBundle :: Bundle m c -> m (OCert c)
    releaseBundle (Bundle oldKeyVar oldOC) = do
      let serialNumber = ocertN oldOC
      oldVk <- withCRefValue oldKeyVar (deriveVerKeyKES . skWithoutPeriodKES)
      releaseCRef oldKeyVar
      return oldOC

    releaseTaggedBundle :: TaggedBundle m c -> m (Maybe (OCert c))
    releaseTaggedBundle tbundle =
      maybe (return Nothing) (fmap Just . releaseBundle) $ taggedBundle tbundle

    acquireBundle :: Bundle m c -> m ()
    acquireBundle bundle =
      void $ acquireCRef (bundleSKP bundle)

    acquireTaggedBundle :: TaggedBundle m c -> m ()
    acquireTaggedBundle tbundle =
      maybe (return ()) acquireBundle (taggedBundle tbundle)

    handleValidBundle :: m (PushKeyResult c)
    handleValidBundle = do
      result <- setValidBundle
      case result of
        PushKeyOK {} ->
          broadcastUpdate tbundle
        _ ->
          pure ()
      return result

    setValidBundle :: m (PushKeyResult c)
    setValidBundle = do
      alterBundle agent "pushKey" $ \oldTBundleMay -> do
        case oldTBundleMay of
          Nothing -> do
            case taggedBundle tbundle of
              Nothing ->
                agentTrace agent $ AgentInstallingKeyDrop
              Just bundle ->
                agentTrace agent $
                  AgentInstallingNewKey (formatKey (taggedBundleTimestamp tbundle) (bundleOC bundle))
            return (Just tbundle, PushKeyOK Nothing)
          Just oldTBundle -> do
            if isTaggedBundleAgeValid oldTBundle tbundle
              then do
                releaseResult <- releaseTaggedBundle oldTBundle
                case (releaseResult, taggedBundle tbundle) of
                  (Nothing, Just bundle) ->
                    agentTrace agent $
                      AgentInstallingNewKey (formatKey (taggedBundleTimestamp tbundle) (bundleOC bundle))
                  (Nothing, Nothing) ->
                    agentTrace agent $
                      AgentInstallingKeyDrop
                  (Just oldOC, Nothing) ->
                    agentTrace agent $
                      AgentDroppingKey (formatKey (taggedBundleTimestamp tbundle) oldOC)
                  (Just oldOC, Just bundle) ->
                    agentTrace agent $
                      AgentReplacingPreviousKey
                        (formatKey (taggedBundleTimestamp oldTBundle) oldOC)
                        (formatKey (taggedBundleTimestamp tbundle) (bundleOC bundle))
                return (Just tbundle, PushKeyOK releaseResult)
              else do
                releaseResult <- releaseTaggedBundle tbundle
                agentTrace agent $
                  AgentRejectingKey $
                    "Key is too old: "
                      ++ (show . timestampValue $ taggedBundleTimestamp oldTBundle)
                      ++ " > "
                      ++ (show . timestampValue $ taggedBundleTimestamp tbundle)
                return (Just oldTBundle, PushKeyTooOld)

    broadcastUpdate :: TaggedBundle m c -> m ()
    broadcastUpdate tbundle = do
      agentTrace agent $
        AgentRequestingKeyUpdate
          ( formatMaybeKey
              (taggedBundleTimestamp tbundle)
              (bundleOC <$> taggedBundle tbundle)
          )
      atomically $ writeTChan (agentNextKeyChan agent) tbundle

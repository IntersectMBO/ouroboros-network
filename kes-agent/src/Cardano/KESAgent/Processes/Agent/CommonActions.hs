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

-- | Common operations used inside the KES agent process.
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
import Cardano.KESAgent.Util.Formatting
import Cardano.KESAgent.Processes.Agent.Context
import Cardano.KESAgent.Processes.Agent.Type
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting (
  CRef,
  CRefEvent (..),
  acquireCRef,
  newCRefWith,
  releaseCRef,
  withCRefValue,
 )

-- | Check whether the current active key bundle need to be evolved, and if so,
-- evolve it.
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
        -- No bundle, no need to evolve anything.
        agentTrace agent AgentNoKeyToEvolve
        return (Nothing, ())
      Just TaggedBundle {taggedBundle = Nothing} -> do
        -- Active bundle is a key deletion, which we don't need to evolve.
        agentTrace agent AgentNoKeyToEvolve
        return (Nothing, ())
      Just
        TaggedBundle
          { taggedBundle = Just (Bundle keyVar oc)
          , taggedBundleTimestamp = timestamp
          } ->
          -- We have an active bundle, so we may need to evolve it.
          withCRefValue keyVar $ \key -> do
            let p = KESPeriod $ unKESPeriod (ocertKESPeriod oc) + periodKES key
            if p < p'
              then do
                -- We need to evolve.
                keyMay' <- updateKESTo () p' oc key
                case keyMay' of
                  Nothing -> do
                    -- No more evolutions available, remove the key.
                    -- It's OK to just leave the key store empty, rather than
                    -- storing a key deletion, because the key is expired
                    -- anyway, so any peer or client who still has that key will
                    -- also evolve and expire it.
                    agentTrace agent $ AgentKeyExpired p p'
                    releaseCRef keyVar
                    return (Nothing, ())
                  Just key' -> do
                    -- Key evolved successfully, replace it in our key store,
                    -- and release the old key.
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
                -- Key bundle is up to date, no need to evolve.
                agentTrace agent $ AgentKeyNotEvolved p p'
                return (bundleMay, ())

-- | Convenience wrapper for sending 'AgentTrace' messages to an agent's
-- default 'Tracer'.
agentTrace :: Agent c m fd addr -> AgentTrace -> m ()
agentTrace agent = traceWith (agentTracer . agentOptions $ agent)

-- | Convenience wrapper for tracing refcounting events in an agent context.
agentCRefTracer :: Agent c m fd addr -> Tracer m CRefEvent
agentCRefTracer = contramap AgentCRefEvent . agentTracer . agentOptions

-- | Modify the bundle stored in a KES agent in a thread-safe manner.
-- The key modification function takes and returns a 'Maybe', where 'Nothing'
-- means that there is no key; returning 'Nothing' will erase the stored key.
-- Broadcasting the key update (if appropriate), evolving the key to the current
-- evolution, and securely erasing any old keys, is the responsibility of the
-- caller.
-- See 'Agent' for explanations of how keys are stored in a KES agent.
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

-- | Perform an action on the key bundle stored in a KES agent in a thread-safe
-- manner. The key bundle should be considered read-only; to manipulate the key
-- bundle, use 'alterBundle'.
-- See 'Agent' for explanations of how keys are stored in a KES agent.
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

-- | Modify a staged key stored in a KES agent.
-- The key modification function takes and returns a 'Maybe', where 'Nothing'
-- means that there is no key; returning 'Nothing' will erase the stored key.
-- See 'Agent' for explanations of how keys are stored in a KES agent.
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

-- | Access a staged key stored in a KES agent. The staged key is read-only
-- during this operation.
-- See 'Agent' for explanations of how keys are stored in a KES agent.
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

-- | Initialize a new 'Agent'. This will not spawn the actual agent process,
-- just set up everything it needs.
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
newAgent
  _p
  -- \| 'Snocket' for both service and control connections
  s
  -- \| Raw-bearer factory, needed for the RawBearer protocols
  mrb
  -- \| Agent options
  options = do
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

-- | Clean up after an 'Agent'.
finalizeAgent :: Monad m => Agent c m fd addr -> m ()
finalizeAgent agent = do
  let s = agentSnocket agent
  close s (agentServiceFD agent)
  mapM_ (close s) (agentControlFD agent)

-- | The result of pushing a key into an agent.
data PushKeyResult c
  = -- | Push was successful, key is now current.
    PushKeyOK (Maybe (OCert c))
  | -- | Push failed due to an invalid operational certificate.
    PushKeyInvalidOCert String
  | -- | Push failed because the pushed key is older than the current one.
    PushKeyTooOld

-- | Convert a 'PushKeyResult' to a 'RecvResult' to report back to a connected
-- client.
pushKeyResultToRecvResult :: PushKeyResult c -> RecvResult
pushKeyResultToRecvResult PushKeyOK {} = RecvOK
pushKeyResultToRecvResult PushKeyInvalidOCert {} = RecvErrorInvalidOpCert
pushKeyResultToRecvResult PushKeyTooOld = RecvErrorKeyOutdated

-- | Extract the 'OCert' from a 'PushKeyResult', if any.
pushKeyResultOCert :: PushKeyResult c -> Maybe (OCert c)
pushKeyResultOCert (PushKeyOK ocm) = ocm
pushKeyResultOCert _ = Nothing

-- | Extract the KES verification key from a 'PushKeyResult', if any.
pushKeyResultVKey :: PushKeyResult c -> Maybe (VerKeyKES (KES c))
pushKeyResultVKey pkr = ocertVkHot <$> pushKeyResultOCert pkr

-- | Validate a key bundle. This will validate the operational certificate in
-- the bundle against the cold key configured for the agent, and a
-- verification key derived from the signing key in the bundle.
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

-- | Validate a tagged bundle. This will perform a validation as per
-- 'validateBundle' if the tagged bundle contains a key; if the tagged bundle
-- is a deletion (i.e., contains no key), then it always validates.
validateTaggedBundle ::
  AgentContext m c =>
  Agent c m fd add ->
  TaggedBundle m c ->
  m (Either String ())
validateTaggedBundle _ TaggedBundle {taggedBundle = Nothing} = return (Right ())
validateTaggedBundle agent TaggedBundle {taggedBundle = Just bundle} = validateBundle agent bundle

-- Perform an age check on a tagged bundle vs. a current bundle.
-- The new bundle is considered valid if it is newer than the current bundle,
-- according to the following checks:
-- - If both old and new bundles contain keys, the new bundle's
--   operational certificate serial number must be higher than the old one, and
--   the timestamp must be strictly newer.
-- - If only the new bundle contains a key, then the timestamp must be strictly
--   newer.
-- - If only the old bundle contains a key, then the timestamp of the new bundle
--   must be newer or the same: deletions trump creations.
-- - If neither bundle contains a key, then the timestamp must be strictly
--   newer.
isTaggedBundleAgeValid ::
  TaggedBundle m c ->
  TaggedBundle m c ->
  Bool
isTaggedBundleAgeValid
  -- \| The currently installed bundle.
  current
  -- \| The candidate bundle to install; this one must be newer than the
  -- installed bundle.
  new =
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

-- | Handle an incoming 'TaggedBundle'. This will perform the following steps:
-- - Validate the tagged bundle against the currently installed bundle, if any.
--   If validation fails, abort.
-- - Update the agent's active key store to hold the new bundle.
-- - Enqueue the key update for broadcasting to connected clients.
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
            -- No old bundle present; we can just install the new one without
            -- an age check.
            case taggedBundle tbundle of
              Nothing ->
                agentTrace agent $ AgentInstallingKeyDrop
              Just bundle ->
                agentTrace agent $
                  AgentInstallingNewKey (formatTaggedBundle (taggedBundleTimestamp tbundle) (bundleOC bundle))
            return (Just tbundle, PushKeyOK Nothing)
          Just oldTBundle -> do
            -- We have an old bundle, so we need to do an age check.
            if isTaggedBundleAgeValid oldTBundle tbundle
              then do
                -- Age check passed, which means we need to release the old
                -- bundle and install the new one.
                releaseResult <- releaseTaggedBundle oldTBundle
                case (releaseResult, taggedBundle tbundle) of
                  (Nothing, Just bundle) ->
                    agentTrace agent $
                      AgentInstallingNewKey (formatTaggedBundle (taggedBundleTimestamp tbundle) (bundleOC bundle))
                  (Nothing, Nothing) ->
                    agentTrace agent $
                      AgentInstallingKeyDrop
                  (Just oldOC, Nothing) ->
                    agentTrace agent $
                      AgentDroppingKey (formatTaggedBundle (taggedBundleTimestamp tbundle) oldOC)
                  (Just oldOC, Just bundle) ->
                    agentTrace agent $
                      AgentReplacingPreviousKey
                        (formatTaggedBundle (taggedBundleTimestamp oldTBundle) oldOC)
                        (formatTaggedBundle (taggedBundleTimestamp tbundle) (bundleOC bundle))
                return (Just tbundle, PushKeyOK releaseResult)
              else do
                -- Age check failed, so we keep the old bundle and release the
                -- new one.
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
          ( formatTaggedBundleMaybe
              (taggedBundleTimestamp tbundle)
              (bundleOC <$> taggedBundle tbundle)
          )
      atomically $ writeTChan (agentNextKeyChan agent) tbundle

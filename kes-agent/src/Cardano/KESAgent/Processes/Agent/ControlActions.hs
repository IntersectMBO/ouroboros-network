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

-- | Functions for handling commands issued by a command client within a KES
-- agent process.
module Cardano.KESAgent.Processes.Agent.ControlActions
where

import Cardano.Crypto.KES.Class (
  KESAlgorithm (..),
  SignKeyWithPeriodKES (..),
  deriveVerKeyKES,
  forgetSignKeyKES,
  genKeyKES,
 )
import Cardano.Crypto.Libsodium.MLockedSeed
import Control.Concurrent.Class.MonadSTM.TMVar (
  putTMVar,
  readTMVar,
  takeTMVar,
 )
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadThrow (
  bracket,
  finally,
 )
import Control.Monad.Class.MonadTime (MonadTime (..))
import qualified Data.Map.Strict as Map

import Cardano.KESAgent.KES.Bundle (
  Bundle (..),
  TaggedBundle (..),
  timestampFromUTC,
  timestampToUTC,
 )
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.Evolution (
  getCurrentKESPeriodWith,
 )
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Processes.Agent.CommonActions
import Cardano.KESAgent.Processes.Agent.Context
import Cardano.KESAgent.Processes.Agent.Type
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import Cardano.KESAgent.Util.RefCounting (
  newCRef,
  releaseCRef,
  withCRefValue,
 )

-- | Generate a new KES key and store it in the staged key slot.
-- @kes-agent-control gen-key@ command.
genKey ::
  AgentContext m c =>
  Agent c m fd addr ->
  m (Maybe (VerKeyKES (KES c)))
genKey agent = do
  vk <- bracket
    (agentGenSeed . agentOptions $ agent)
    mlockedSeedFinalize
    $ \seed -> do
      sk <- genKeyKES seed
      oldSKVarMay <- atomically $ takeTMVar (agentStagedKeyVar agent)
      maybe (return ()) releaseCRef oldSKVarMay
      newSKVar <- newCRef (forgetSignKeyKES . skWithoutPeriodKES) (SignKeyWithPeriodKES sk 0)
      atomically $ putTMVar (agentStagedKeyVar agent) (Just newSKVar)
      vk <- deriveVerKeyKES sk
      return $ Just vk
  agentTrace agent $ maybe AgentCouldNotGenerateStagedKey (AgentGeneratedStagedKey . formatVK) vk
  return vk

-- | Drop the currently installed KES key.
-- @kes-agent-control drop-key@ command.
dropKey ::
  AgentContext m c =>
  Agent c m fd addr ->
  m RecvResult
dropKey agent =
  do
    timestamp <- timestampFromUTC <$> getCurrentTime
    pushKeyResultToRecvResult
      <$> pushKey
        agent
        TaggedBundle
          { taggedBundle = Nothing
          , taggedBundleTimestamp = timestamp
          }

-- | Drop the staged KES key.
-- @kes-agent-control drop-staged-key@ command.
dropStagedKey ::
  AgentContext m c =>
  Agent c m fd addr ->
  m (Maybe (VerKeyKES (KES c)))
dropStagedKey agent =
  do
    keyMay <- atomically $ takeTMVar (agentStagedKeyVar agent)
    vkMay <- mapM (flip withCRefValue (deriveVerKeyKES . skWithoutPeriodKES)) keyMay
    maybe (return ()) releaseCRef keyMay
    agentTrace agent $ maybe AgentNoStagedKeyToDrop (AgentDroppedStagedKey . formatVK) vkMay
    return Nothing
    `finally` do
      atomically $ putTMVar (agentStagedKeyVar agent) Nothing

-- | Get the verification key of the staged key.
-- @kes-agent-control export-staged-key@ command.
queryKey ::
  AgentContext m c =>
  Agent c m fd addr ->
  m (Maybe (VerKeyKES (KES c)))
queryKey agent = do
  withStagedKey agent "queryKey" $ \keyMay -> do
    case keyMay of
      Nothing -> return Nothing
      Just skpVar -> withCRefValue skpVar $ \skp -> do
        vk <- deriveVerKeyKES (skWithoutPeriodKES skp)
        return $ Just vk

-- | Add an operational certificate to the staged key, and, if valid, move it
-- into the active key slot.
-- @kes-agent-control install-key@ command.
installKey ::
  AgentContext m c =>
  Agent c m fd addr ->
  OCert c ->
  m RecvResult
installKey agent oc = do
  newKeyMay <- alterStagedKey agent "install staged key" $ \keyMay -> do
    case keyMay of
      Nothing -> do
        return (Nothing, Nothing)
      Just skpVar -> do
        return (Nothing, Just skpVar)
  maybe
    (return RecvErrorNoKey)
    ( \newKey -> do
        utcTime <- getCurrentTime
        pushKeyResultToRecvResult
          <$> pushKey
            agent
            TaggedBundle
              { taggedBundle = Just (Bundle newKey oc)
              , taggedBundleTimestamp = timestampFromUTC utcTime
              }
    )
    newKeyMay

-- | Get information about the agent state.
-- @kes-agent-control info@ command.
getInfo ::
  AgentContext m c =>
  Agent c m fd addr ->
  m (AgentInfo c)
getInfo agent = do
  bundleInfoMay <- do
    withBundle agent "get info" $ \case
      Nothing ->
        return Nothing
      Just (TaggedBundle {taggedBundle = Nothing, taggedBundleTimestamp = ts}) ->
        return $
          Just
            TaggedBundleInfo
              { taggedBundleInfo = Nothing
              , taggedBundleInfoTimestamp = Just (timestampToUTC ts)
              }
      Just (TaggedBundle {taggedBundle = Just bundle, taggedBundleTimestamp = ts}) ->
        withCRefValue (bundleSKP bundle) $ \skp -> do
          return $
            Just
              TaggedBundleInfo
                { taggedBundleInfo =
                    Just
                      BundleInfo
                        { bundleInfoEvolution = fromIntegral $ periodKES skp
                        , bundleInfoStartKESPeriod = ocertKESPeriod (bundleOC bundle)
                        , bundleInfoOCertN = ocertN (bundleOC bundle)
                        , bundleInfoVK = ocertVkHot (bundleOC bundle)
                        , bundleInfoSigma = ocertSigma (bundleOC bundle)
                        }
                , taggedBundleInfoTimestamp = Just (timestampToUTC ts)
                }

  keyInfoMay <- do
    withStagedKey agent "get info" $ \case
      Nothing -> do
        return Nothing
      Just skpVar -> withCRefValue skpVar $ \skp -> do
        vk <- deriveVerKeyKES (skWithoutPeriodKES skp)
        return $ Just (KeyInfo vk)

  now <- agentGetCurrentTime (agentOptions agent)
  kesPeriod <-
    getCurrentKESPeriodWith
      (agentGetCurrentTime $ agentOptions agent)
      (agentEvolutionConfig $ agentOptions agent)
  bootstrapStatusesRaw <- Map.toAscList <$> atomically (readTMVar (agentBootstrapConnections agent))
  let bootstrapStatuses = map (uncurry BootstrapInfo) bootstrapStatusesRaw

  return
    AgentInfo
      { agentInfoCurrentBundle = bundleInfoMay
      , agentInfoStagedKey = keyInfoMay
      , agentInfoCurrentTime = now
      , agentInfoCurrentKESPeriod = kesPeriod
      , agentInfoBootstrapConnections = bootstrapStatuses
      }

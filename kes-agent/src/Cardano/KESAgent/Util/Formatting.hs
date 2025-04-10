{-# LANGUAGE FlexibleContexts #-}

module Cardano.KESAgent.Util.Formatting
where

import Cardano.Crypto.KES.Class (
  KESAlgorithm (..),
  SignKeyWithPeriodKES (..),
  deriveVerKeyKES,
  forgetSignKeyKES,
 )
import Text.Printf

import Cardano.KESAgent.KES.Bundle (Bundle (..), TaggedBundle (..), Timestamp (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Util.Pretty

-- | Format a key bundle, or key deletion, for the purpose of trace logging.
formatTaggedBundleMaybe ::
  KESAlgorithm (KES c) =>
  Timestamp ->
  Maybe (OCert c) ->
  String
formatTaggedBundleMaybe ts Nothing =
  printf "DELETE (%lu)" (timestampValue ts)
formatTaggedBundleMaybe ts (Just ocert) =
  formatTaggedBundle ts ocert

-- | Format a key bundle for the purpose of trace logging.
formatTaggedBundle ::
  KESAlgorithm (KES c) =>
  Timestamp ->
  OCert c ->
  String
formatTaggedBundle ts ocert =
  let serialNumber = ocertN ocert
  in printf "%i (%lu) = %s" serialNumber (timestampValue ts) (formatVK $ ocertVkHot ocert)

formatVK ::
  KESAlgorithm kes =>
  VerKeyKES kes ->
  String
formatVK = pretty . rawSerialiseVerKeyKES

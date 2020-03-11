{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ThreadNet.RealPBFT.ProtocolInfo (
  theProposedProtocolVersion,
  theProposedSoftwareVersion,
  mkProtocolRealPBFT,
  ) where

import           Data.Foldable (find)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Stack (HasCallStack)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Byron.Node

mkProtocolRealPBFT :: HasCallStack
                   => PBftParams
                   -> CoreNodeId
                   -> Genesis.Config
                   -> Genesis.GeneratedSecrets
                   -> ProtocolInfo ByronBlock
mkProtocolRealPBFT params (CoreNodeId i)
                   genesisConfig genesisSecrets =
    protocolInfoByron
      genesisConfig
      (Just $ PBftSignatureThreshold pbftSignatureThreshold)
      theProposedProtocolVersion
      theProposedSoftwareVersion
      (Just leaderCredentials)
  where
    leaderCredentials :: PBftLeaderCredentials
    leaderCredentials = either (error . show) id $
        mkPBftLeaderCredentials
          genesisConfig
          dlgKey
          dlgCert

    PBftParams{pbftSignatureThreshold} = params

    dlgKey :: Crypto.SigningKey
    dlgKey = fromMaybe (error "dlgKey") $
       find (\sec -> Delegation.delegateVK dlgCert == Crypto.toVerification sec)
            $ Genesis.gsRichSecrets genesisSecrets

    dlgCert :: Delegation.Certificate
    dlgCert = snd $ Map.toAscList dlgMap !! (fromIntegral i)

    dlgMap :: Map Common.KeyHash Delegation.Certificate
    dlgMap = Genesis.unGenesisDelegation
           $ Genesis.gdHeavyDelegation
           $ Genesis.configGenesisData genesisConfig

-- | The protocol version proposed as part of the hard-fork smoke test
--
-- The initial Byron ledger state begins with protocol version @0.0.0@. In the
-- smoke test, if the proposal and votes are enabled, then we will be proposing
-- an update to @1.0.0@.
--
-- This value occurs in two crucial places: the proposal and also the
-- 'Byron.byronProtocolVersion' field of the static node config. See the
-- Haddock comment on 'mkProtocolRealPBftAndHardForkTxs'.
--
theProposedProtocolVersion :: Update.ProtocolVersion
theProposedProtocolVersion = Update.ProtocolVersion 1 0 0

-- | The software version proposed as part of the hard-fork smoke test
--
-- We don't actually care about this for the smoke test, but we have to set it
-- both as part of the proposal and also as part of the node's static
-- configuration. Its use in the static configuration is legacy and does not
-- seem to affect anything; see Issue #1732.
--
-- The initial Byron ledger state begins with no recorded software versions.
-- For the addition of a new software version, the Byron ledger rules require
-- that it starts at 0 or 1.
--
theProposedSoftwareVersion :: Update.SoftwareVersion
theProposedSoftwareVersion = Update.SoftwareVersion
  -- appnames must be ASCII and <= 12 characters
  (Update.ApplicationName "Dummy")
  0

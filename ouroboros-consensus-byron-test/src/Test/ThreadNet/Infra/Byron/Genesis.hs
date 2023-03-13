{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.ThreadNet.Infra.Byron.Genesis (
    byronPBftParams
  , generateGenesisConfig
  ) where

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import           Control.Monad.Except (runExceptT)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy
import           Test.Util.Time

{-------------------------------------------------------------------------------
  Generating the genesis configuration
-------------------------------------------------------------------------------}

byronPBftParams :: SecurityParam -> NumCoreNodes -> PBftParams
byronPBftParams paramK numCoreNodes = PBftParams
  { pbftNumNodes           = numCoreNodes
  , pbftSecurityParam      = paramK
  , pbftSignatureThreshold = PBftSignatureThreshold $ (1 / n) + (1 / k) + epsilon
    -- crucially: @floor (k * t) >= ceil (k / n)@
  }
    where
      epsilon = 1/10000   -- avoid problematic floating point round-off

      n :: Num a => a
      n = fromIntegral x where NumCoreNodes x = numCoreNodes

      k :: Num a => a
      k = fromIntegral x where SecurityParam x = paramK

-- Instead of using 'Dummy.dummyConfig', which hard codes the number of rich
-- men (= CoreNodes for us) to 4, we generate a dummy config with the given
-- number of rich men.
generateGenesisConfig :: SlotLength
                      -> PBftParams
                      -> (Genesis.Config, Genesis.GeneratedSecrets)
generateGenesisConfig slotLen params =
    either (error . show) id $
      Crypto.deterministic "this is fake entropy for testing" $
        runExceptT $
          Genesis.generateGenesisConfigWithEntropy dawnOfTime spec
  where
    PBftParams{pbftNumNodes, pbftSecurityParam} = params
    NumCoreNodes numCoreNodes = pbftNumNodes

    spec :: Genesis.GenesisSpec
    spec = Dummy.dummyGenesisSpec
        { Genesis.gsInitializer = Dummy.dummyGenesisInitializer
          { Genesis.giTestBalance =
              (Genesis.giTestBalance Dummy.dummyGenesisInitializer)
                -- The nodes are the richmen
                { Genesis.tboRichmen = fromIntegral numCoreNodes }
          }
        , Genesis.gsK = Common.BlockCount $ maxRollbacks pbftSecurityParam
        , Genesis.gsProtocolParameters = gsProtocolParameters
          { Update.ppSlotDuration = toByronSlotLength slotLen
          }
        }
      where
        gsProtocolParameters = Genesis.gsProtocolParameters Dummy.dummyGenesisSpec

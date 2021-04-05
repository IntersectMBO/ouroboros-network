{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.HardFork.Combinator.Embed.Binary (protocolInfoBinary) where

import           Control.Exception (assert)
import           Data.Align (alignWith)
import           Data.SOP.Strict (NP (..))
import           Data.These (These (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract (protocolSecurityParam)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (exactlyTwo)
import           Ouroboros.Consensus.Util.OptNP (OptNP (..))

import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.History as History

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

protocolInfoBinary ::
     forall m blk1 blk2.
     (CanHardFork '[blk1, blk2], Monad m)
     -- First era
  => ProtocolInfo m blk1
  -> History.EraParams
  -> (ConsensusConfig (BlockProtocol blk1) -> PartialConsensusConfig (BlockProtocol blk1))
  -> (LedgerConfig blk1 -> PartialLedgerConfig blk1)
     -- Second era
  -> ProtocolInfo m blk2
  -> History.EraParams
  -> (ConsensusConfig (BlockProtocol blk2) -> PartialConsensusConfig (BlockProtocol blk2))
  -> (LedgerConfig blk2 -> PartialLedgerConfig blk2)
  -> ProtocolInfo m (HardForkBlock '[blk1, blk2])
protocolInfoBinary protocolInfo1 eraParams1 toPartialConsensusConfig1 toPartialLedgerConfig1
                   protocolInfo2 eraParams2 toPartialConsensusConfig2 toPartialLedgerConfig2 =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = HardForkConsensusConfig {
                hardForkConsensusConfigK      = k
              , hardForkConsensusConfigShape  = shape
              , hardForkConsensusConfigPerEra = PerEraConsensusConfig
                  (  WrapPartialConsensusConfig (toPartialConsensusConfig1 consensusConfig1)
                  :* WrapPartialConsensusConfig (toPartialConsensusConfig2 consensusConfig2)
                  :* Nil
                  )
              }
          , topLevelConfigLedger = HardForkLedgerConfig {
                hardForkLedgerConfigShape  = shape
              , hardForkLedgerConfigPerEra = PerEraLedgerConfig
                  (  WrapPartialLedgerConfig (toPartialLedgerConfig1 ledgerConfig1)
                  :* WrapPartialLedgerConfig (toPartialLedgerConfig2 ledgerConfig2)
                  :* Nil
                  )
              }
          , topLevelConfigBlock =
              HardForkBlockConfig $
                PerEraBlockConfig $
                  (blockConfig1 :* blockConfig2 :* Nil)
          , topLevelConfigCodec =
              HardForkCodecConfig $
                PerEraCodecConfig $
                  (codecConfig1 :* codecConfig2 :* Nil)
          , topLevelConfigStorage =
              HardForkStorageConfig $
                PerEraStorageConfig $
                  (storageConfig1 :* storageConfig2 :* Nil)
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState =
              HardForkLedgerState $
                initHardForkState initLedgerState1
          , headerState =
              genesisHeaderState $
                initHardForkState $
                  WrapChainDepState $
                    headerStateChainDep initHeaderState1
          }
      , pInfoBlockForging =
          alignWith alignBlockForging <$> blockForging1 <*> blockForging2
      }
  where
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = consensusConfig1
          , topLevelConfigLedger   = ledgerConfig1
          , topLevelConfigBlock    = blockConfig1
          , topLevelConfigCodec    = codecConfig1
          , topLevelConfigStorage  = storageConfig1
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = initLedgerState1
          , headerState = initHeaderState1
          }
      , pInfoBlockForging = blockForging1
      } = protocolInfo1

    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = consensusConfig2
          , topLevelConfigLedger   = ledgerConfig2
          , topLevelConfigBlock    = blockConfig2
          , topLevelConfigCodec    = codecConfig2
          , topLevelConfigStorage  = storageConfig2
          }
      , pInfoBlockForging = blockForging2
      } = protocolInfo2

    k1, k2, k :: SecurityParam
    k1 = protocolSecurityParam consensusConfig1
    k2 = protocolSecurityParam consensusConfig2
    k = assert (k1 == k2) k1

    shape :: History.Shape '[blk1, blk2]
    shape = History.Shape $ exactlyTwo eraParams1 eraParams2

    alignBlockForging ::
         These (BlockForging m blk1) (BlockForging m blk2)
      -> BlockForging m (HardForkBlock '[blk1, blk2])
    alignBlockForging = \case
      This bf1 ->
        hardForkBlockForging
          (forgeLabel bf1)
          (OptCons bf1 $ OptSkip OptNil)
      That bf2 ->
        hardForkBlockForging
          (forgeLabel bf2)
          (OptSkip $ OptCons bf2 OptNil)
      These bf1 bf2 ->
        hardForkBlockForging
          (forgeLabel bf1 <> "-" <> forgeLabel bf2)
          (OptCons bf1 $ OptCons bf2 OptNil)

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Demo.Ledger.Byron (
    protocolInfoByron
  , ByronDemoConfig
  ) where

import           Control.Monad.Except
import qualified Data.Bimap as Bimap
import           Data.Coerce
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

import           Formatting (formatToString)

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Cardano.Genesis
import qualified Cardano.Chain.Update as Cardano.Update
import qualified Cardano.Crypto as Cardano
import qualified Cardano.Crypto.Signing as Cardano.KeyGen

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Demo.Ledger.Byron.Config
import           Ouroboros.Consensus.Demo.Ledger.Byron.Elaborate
import           Ouroboros.Consensus.Demo.Ledger.Byron.Forge

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

protocolInfoByron :: NumCoreNodes
                  -> CoreNodeId
                  -> PBftParams
                  -> Cardano.Genesis.Config
                  -> ProtocolInfo (ByronBlock ByronDemoConfig)
protocolInfoByron (NumCoreNodes numCoreNodes) (CoreNodeId nid) params gc =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                  pbftParams  = params
                    { pbftNumNodes = fromIntegral numCoreNodes
                      -- Set the signature window to be short for the demo.
                    , pbftSignatureWindow = 7
                    }
                , pbftNodeId  = CoreId nid
                , pbftSignKey = SignKeyCardanoDSIGN (snd (lookupKey nid))
                , pbftVerKey  = VerKeyCardanoDSIGN  (fst (lookupKey nid))
                , pbftGenVerKey = VerKeyCardanoDSIGN (lookupGenKey nid)
                }
          , encNodeConfigExt = ByronDemoConfig {
                pbftCoreNodes = Bimap.fromList [
                    (fst (lookupKey n), CoreNodeId n)
                    | n <- [0 .. numCoreNodes]
                    ]
              , pbftProtocolMagic   = Cardano.Genesis.configProtocolMagic gc
              , pbftProtocolVersion = Cardano.Update.ProtocolVersion 1 0 0
              , pbftSoftwareVersion = Cardano.Update.SoftwareVersion (Cardano.Update.ApplicationName "Cardano Demo") 1
              , pbftGenesisConfig   = gc
              , pbftGenesisHash     = coerce Cardano.Genesis.configGenesisHeaderHash gc
              , pbftEpochSlots      = Cardano.Genesis.configEpochSlots gc
              , pbftGenesisDlg      = Cardano.Genesis.configHeavyDelegation gc
              , pbftSecrets         = Dummy.dummyGeneratedSecrets
              }
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = ByronLedgerState {
                blsCurrent   = initState
              , blsSnapshots = Seq.empty
              }
          , ouroborosChainState = Seq.empty
          }
      , pInfoInitState  = ()
      }
  where
    initState :: Cardano.Block.ChainValidationState
    Right initState = runExcept $ Cardano.Block.initialChainValidationState gc

    lookupKey :: Int -> (Cardano.VerificationKey, Cardano.SigningKey)
    lookupKey n = (\x -> (Cardano.KeyGen.toVerification x, x))
                . (!! n)
                . Cardano.Genesis.gsRichSecrets
                . fromJust
                $ Cardano.Genesis.configGeneratedSecrets gc

    lookupGenKey :: Int -> Cardano.VerificationKey
    lookupGenKey n = Cardano.KeyGen.toVerification
                   . (!! n)
                   . Cardano.Genesis.gsDlgIssuersSecrets
                   . fromJust
                   $ Cardano.Genesis.configGeneratedSecrets gc


{-------------------------------------------------------------------------------
  RunDemo instance
-------------------------------------------------------------------------------}

instance Condense Cardano.Block.HeaderHash where
  condense = formatToString Cardano.Block.headerHashF

instance DemoHeaderHash Cardano.Block.HeaderHash where
  demoEncodeHeaderHash = encodeByronHeaderHash
  demoDecodeHeaderHash = decodeByronHeaderHash

instance ByronGiven => RunDemo (ByronBlock ByronDemoConfig) where
  demoForgeBlock         = forgeBlock
  demoBlockMatchesHeader = \_hdr _blk -> True -- TODO #595
  demoBlockFetchSize     = const 2000 -- TODO #593
  demoIsEBB              = const False -- TODO #704
  demoEpochSize          = \_ _ -> return 21600 -- TODO #226
  demoEncodeBlock        = encodeByronBlock  . pbftEpochSlots . encNodeConfigExt
  demoEncodeHeader       = encodeByronHeader . pbftEpochSlots . encNodeConfigExt
  demoEncodeGenTx        = encodeByronGenTx
  demoEncodeLedgerState  = const encodeByronLedgerState
  demoEncodeChainState   = const encodeByronChainState
  demoDecodeBlock        = decodeByronBlock  . pbftEpochSlots . encNodeConfigExt
  demoDecodeHeader       = decodeByronHeader . pbftEpochSlots . encNodeConfigExt
  demoDecodeGenTx        = decodeByronGenTx
  demoDecodeLedgerState  = const decodeByronLedgerState
  demoDecodeChainState   = const decodeByronChainState
  demoMockTx             = elaborateTx

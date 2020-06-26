{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Byron.Examples (
    exampleChainDepState
  , exampleLedgerState
  , exampleHeaderState
  , exampleExtLedgerState
  , exampleHeaderHash
  , exampleGenTx
  , exampleGenTxId
  , exampleUPIState
  , exampleTxSizeLinear
  , exampleApplyTxErr
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import qualified Data.Sequence.Strict as Seq

import qualified Cardano.Chain.Block as CC.Block
import           Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.UTxO as CC

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.DelegationHistory as DH

import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'S.WindowSize', because 'decodeByronChainDepState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'S.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam 2

windowSize :: S.WindowSize
windowSize = S.WindowSize 2

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

exampleChainDepState :: ChainDepState (BlockProtocol ByronBlock)
exampleChainDepState = withEBB
  where
    signers = map (`S.PBftSigner` CC.exampleKeyHash) [1..4]

    withoutEBB = S.fromList
      secParam
      windowSize
      (NotOrigin 2, Seq.fromList signers, S.NothingEbbInfo)

    -- info about an arbitrary hypothetical EBB
    exampleEbbSlot            :: SlotNo
    exampleEbbHeaderHashBytes :: HeaderHashBytes
    exampleEbbSlot            = 6
    exampleEbbHeaderHashBytes = mkHeaderHashBytesForTestingOnly
                                  (Lazy8.pack "test_golden_ChainDepState6")

    withEBB = S.appendEBB secParam windowSize
                exampleEbbSlot exampleEbbHeaderHashBytes
                withoutEBB

exampleLedgerState :: LedgerState ByronBlock
exampleLedgerState = ByronLedgerState
    { byronLedgerState       = initState
    , byronDelegationHistory = DH.empty
    }
  where
    initState :: CC.Block.ChainValidationState
    Right initState = runExcept $
      CC.Block.initialChainValidationState CC.dummyConfig

exampleHeaderState :: HeaderState ByronBlock
exampleHeaderState = (genesisHeaderState S.empty)
    { headerStateTips = Seq.singleton annTip }
  where
    annTip = AnnTip {
        annTipSlotNo  = 0
      , annTipBlockNo = 0
      , annTipInfo    = TipInfoIsEBB exampleHeaderHash IsNotEBB
      }

exampleExtLedgerState :: ExtLedgerState ByronBlock
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

exampleHeaderHash :: ByronHash
exampleHeaderHash = ByronHash $ CC.Genesis.configGenesisHeaderHash CC.dummyConfig

exampleGenTx :: GenTx ByronBlock
exampleGenTx = ByronTx CC.exampleTxId (CC.annotateTxAux CC.exampleTxAux)

exampleGenTxId :: TxId (GenTx ByronBlock)
exampleGenTxId = ByronTxId CC.exampleTxId

exampleUPIState :: CC.UPI.State
exampleUPIState = CC.UPI.initialState CC.dummyConfig

exampleTxSizeLinear :: CC.TxSizeLinear
exampleTxSizeLinear = CC.TxSizeLinear (CC.mkKnownLovelace @155381)
                                      (43.946 :: Rational)

exampleApplyTxErr :: ApplyMempoolPayloadErr
exampleApplyTxErr =
      CC.MempoolTxErr
    $ CC.UTxOValidationTxValidationError
    $ CC.TxValidationLovelaceError "a"
    $ CC.LovelaceOverflow 0

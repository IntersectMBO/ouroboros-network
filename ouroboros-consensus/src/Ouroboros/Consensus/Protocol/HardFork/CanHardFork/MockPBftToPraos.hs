{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Protocol.HardFork.CanHardFork.MockPBftToPraos () where

import           Ouroboros.Consensus.Ledger.Mock
import qualified Ouroboros.Consensus.Ledger.Mock.Stake as Stake
import           Ouroboros.Consensus.Protocol.HardFork.CanHardFork
import           Ouroboros.Consensus.Protocol.HardFork.Config
import           Ouroboros.Consensus.Protocol.PBFT hiding (pbftParams)
import           Ouroboros.Consensus.Protocol.Praos

{-------------------------------------------------------------------------------
  Simple Hard Fork Instance
-------------------------------------------------------------------------------}

type MockPBft  = PBft (PBftLedgerView PBftMockCrypto) PBftMockCrypto
type MockPraos = Praos AddrDist PraosMockCrypto

instance CanHardFork MockPBft MockPraos where

  segmentLength _ = undefined -- TODO

  chainStateAfterFork _ _ = []

  ledgerViewAfterFork cfg _ = Stake.equalStakeDist addrDist
    where
      addrDist :: AddrDist
      addrDist = praosExtConfig (nodeConfigAfterFork cfg)

{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module Ouroboros.Consensus.Protocol (
    -- * Supported protocols
    ProtocolMockBFT
  , ProtocolMockPraos
  , ProtocolLeaderSchedule
  , ProtocolMockPBFT
  , ProtocolRealPBFT
    -- * Abstract over the various protocols
  , Protocol(..)
    -- * Evidence that we can run all the supported protocols
  , runProtocol
  , module X
  ) where

import           Data.Coerce
import           Data.Reflection (give)

import qualified Cardano.Chain.Block as Block
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT ()
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos ()
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.BFT as X
import           Ouroboros.Consensus.Protocol.LeaderSchedule as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Protocol.Praos as X
import           Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  Supported protocols
-------------------------------------------------------------------------------}

type ProtocolMockBFT        = Bft BftMockCrypto
type ProtocolMockPraos      = Praos AddrDist PraosMockCrypto
type ProtocolLeaderSchedule = WithLeaderSchedule (Praos () PraosCryptoUnused)
type ProtocolMockPBFT       = PBft (PBftLedgerView PBftMockCrypto) PBftMockCrypto
type ProtocolRealPBFT       = PBft ByronConfig PBftCardanoCrypto

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol blk where
  -- | Run BFT against the mock ledger
  ProtocolMockBFT
    :: SecurityParam
    -> Protocol (SimpleBftBlock SimpleMockCrypto BftMockCrypto)

  -- | Run Praos against the mock ledger
  ProtocolMockPraos
    :: PraosParams
    -> Protocol (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  ProtocolLeaderSchedule
    :: PraosParams
    -> LeaderSchedule
    -> Protocol (SimplePraosRuleBlock SimpleMockCrypto)

  -- | Run PBFT against the mock ledger
  ProtocolMockPBFT
    :: PBftParams
    -> Protocol (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)

  -- | Run PBFT against the real ledger
  ProtocolRealPBFT
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
    -> Protocol ByronBlockOrEBB

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol blk -> Dict (RunNode blk)
runProtocol ProtocolMockBFT{}        = Dict
runProtocol ProtocolMockPraos{}      = Dict
runProtocol ProtocolLeaderSchedule{} = Dict
runProtocol ProtocolMockPBFT{}       = Dict
runProtocol (ProtocolRealPBFT
             gc@Genesis.Config{ Genesis.configGenesisData
                              , Genesis.configGenesisHash}
             _msigthd _pv _sv _mplc) =
  let Genesis.GenesisData{Genesis.gdProtocolMagicId} = configGenesisData
  in give (Genesis.configEpochSlots gc)
     $ give gdProtocolMagicId
     $ give (coerce @_ @Block.HeaderHash configGenesisHash)
     $ Dict

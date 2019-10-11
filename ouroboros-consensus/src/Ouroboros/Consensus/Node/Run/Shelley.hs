module Ouroboros.Consensus.Node.Run.Shelley () where

import           Ouroboros.Consensus.Ledger.Shelley
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.TPraos


instance RunNode ShelleyBlock where
  nodeForgeBlock = forgeShelleyBlock

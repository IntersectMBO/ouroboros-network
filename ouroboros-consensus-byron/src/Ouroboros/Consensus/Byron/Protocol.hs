{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Ouroboros.Consensus.Byron.Protocol (
    PBftByronCrypto
  , genesisKeyCoreNodeId
  , nodeIdToGenesisKey
  ) where

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Control.Monad (guard)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT

{-------------------------------------------------------------------------------
  Crypto
-------------------------------------------------------------------------------}

data PBftByronCrypto

instance PBftCrypto PBftByronCrypto where
  type PBftDSIGN          PBftByronCrypto = ByronDSIGN
  type PBftDelegationCert PBftByronCrypto = CC.Delegation.Certificate
  type PBftVerKeyHash     PBftByronCrypto = CC.Common.KeyHash

  dlgCertGenVerKey = VerKeyByronDSIGN . CC.Delegation.issuerVK
  dlgCertDlgVerKey = VerKeyByronDSIGN . CC.Delegation.delegateVK
  hashVerKey (VerKeyByronDSIGN pk) = CC.Common.hashKey pk

{-------------------------------------------------------------------------------
  PBFT node order
-------------------------------------------------------------------------------}

-- | Determine the 'CoreNodeId' for a code node, based on the genesis key it
-- will sign blocks on behalf of.
--
-- In PBFT, the 'CoreNodeId' index is determined by the 0-based position in
-- the sort order of the genesis key hashes.
genesisKeyCoreNodeId :: CC.Genesis.Config
                     -> VerKeyDSIGN ByronDSIGN
                        -- ^ The genesis verification key
                     -> Maybe CoreNodeId
genesisKeyCoreNodeId gc vkey =
    CoreNodeId . fromIntegral <$>
      Set.lookupIndex (hashVerKey vkey) (genesisKeyHashes gc)

-- | Inverse of 'genesisKeyCoreNodeId'
nodeIdToGenesisKey :: CC.Genesis.Config
                   -> CoreNodeId
                   -> Maybe CC.Common.KeyHash
nodeIdToGenesisKey gc (CoreNodeId nid) = do
    guard $ nid < fromIntegral (Set.size (genesisKeyHashes gc))
    return $ Set.elemAt (fromIntegral nid) (genesisKeyHashes gc)

genesisKeyHashes :: CC.Genesis.Config -> Set CC.Common.KeyHash
genesisKeyHashes = CC.Genesis.unGenesisKeyHashes
                 . CC.Genesis.configGenesisKeyHashes

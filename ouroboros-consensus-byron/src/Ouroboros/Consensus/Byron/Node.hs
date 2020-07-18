{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Node (
    protocolInfoByron
  , protocolClientInfoByron
  , mkByronConfig
  , PBftSignatureThreshold(..)
  , defaultPBftSignatureThreshold
    -- * Secrets
  , PBftLeaderCredentials
  , PBftLeaderCredentialsError
  , mkPBftLeaderCredentials
  , mkPBftIsLeader
    -- * For testing
  , plcCoreNodeId
  ) where

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Maybe
import           Data.Void

import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Node.Serialisation ()
import           Ouroboros.Consensus.Byron.Protocol

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data PBftLeaderCredentials = PBftLeaderCredentials {
      plcSignKey    :: Crypto.SigningKey
    , plcDlgCert    :: Delegation.Certificate
    , plcCoreNodeId :: CoreNodeId
    } deriving Show

-- | Make the 'PBftLeaderCredentials', with a couple sanity checks:
--
-- * That the block signing key and the delegation certificate match.
-- * That the delegation certificate does correspond to one of the genesis
--   keys from the genesis file.
--
mkPBftLeaderCredentials :: Genesis.Config
                        -> Crypto.SigningKey
                        -> Delegation.Certificate
                        -> Either PBftLeaderCredentialsError
                                  PBftLeaderCredentials
mkPBftLeaderCredentials gc sk cert = do
    guard (Delegation.delegateVK cert == Crypto.toVerification sk)
      ?! NodeSigningKeyDoesNotMatchDelegationCertificate

    let vkGenesis = Delegation.issuerVK cert
    nid <- genesisKeyCoreNodeId gc (VerKeyByronDSIGN vkGenesis)
             ?! DelegationCertificateNotFromGenesisKey

    return PBftLeaderCredentials {
      plcSignKey     = sk
    , plcDlgCert     = cert
    , plcCoreNodeId  = nid
    }
  where
    (?!) :: Maybe a -> e -> Either e a
    Just x  ?! _ = Right x
    Nothing ?! e = Left  e

data PBftLeaderCredentialsError =
       NodeSigningKeyDoesNotMatchDelegationCertificate
     | DelegationCertificateNotFromGenesisKey
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Signature threshold. This represents the proportion of blocks in a
--   pbftSignatureWindow-sized window which may be signed by any single key.
newtype PBftSignatureThreshold =
        PBftSignatureThreshold { unSignatureThreshold :: Double }

-- | See chapter 4.1 of
--   https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
defaultPBftSignatureThreshold :: PBftSignatureThreshold
defaultPBftSignatureThreshold = PBftSignatureThreshold 0.22

protocolInfoByron :: forall m. Monad m
                  => Genesis.Config
                  -> Maybe PBftSignatureThreshold
                  -> Update.ProtocolVersion
                  -> Update.SoftwareVersion
                  -> Maybe PBftLeaderCredentials
                  -> ProtocolInfo m ByronBlock
protocolInfoByron genesisConfig mSigThresh pVer sVer mLeader =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = FullProtocolConfig {
                protocolConfigConsensus = PBftConfig {
                    pbftParams = byronPBftParams genesisConfig mSigThresh
                  }
              , protocolConfigIndep = ()
              }
          , topLevelConfigBlock = FullBlockConfig {
                blockConfigLedger = genesisConfig
              , blockConfigBlock  = byronConfig
              , blockConfigCodec  = mkByronCodecConfig genesisConfig
              }
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = initByronLedgerState genesisConfig Nothing
          , headerState = genesisHeaderState S.empty
          }
      , pInfoLeaderCreds = mkCreds <$> mLeader
      }
  where
    byronConfig = mkByronConfig genesisConfig pVer sVer

    mkCreds :: PBftLeaderCredentials
            -> (PBftIsLeader PBftByronCrypto, MaintainForgeState m ByronBlock)
    mkCreds cred = (mkPBftIsLeader cred, defaultMaintainForgeState)

protocolClientInfoByron :: EpochSlots
                        -> SecurityParam
                        -> ProtocolClientInfo ByronBlock
protocolClientInfoByron epochSlots securityParam  =
    ProtocolClientInfo {
      pClientInfoCodecConfig = ByronCodecConfig {
          getByronEpochSlots    = epochSlots
        , getByronSecurityParam = securityParam
        }
    }

byronPBftParams :: Genesis.Config -> Maybe PBftSignatureThreshold -> PBftParams
byronPBftParams cfg threshold = PBftParams {
      pbftSecurityParam      = genesisSecurityParam cfg
    , pbftNumNodes           = genesisNumCoreNodes  cfg
    , pbftSignatureThreshold = unSignatureThreshold
                             $ fromMaybe defaultPBftSignatureThreshold threshold
    }

mkPBftIsLeader :: PBftLeaderCredentials -> PBftIsLeader PBftByronCrypto
mkPBftIsLeader (PBftLeaderCredentials sk cert nid) = PBftIsLeader {
      pbftCoreNodeId = nid
    , pbftSignKey    = SignKeyByronDSIGN sk
    , pbftDlgCert    = cert
    }

mkByronConfig :: Genesis.Config
              -> Update.ProtocolVersion
              -> Update.SoftwareVersion
              -> BlockConfig ByronBlock
mkByronConfig genesisConfig pVer sVer = ByronConfig {
      byronGenesisConfig   = genesisConfig
    , byronProtocolVersion = pVer
    , byronSoftwareVersion = sVer
    }

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

instance InspectLedger ByronBlock where
  type LedgerWarning ByronBlock = Void
  inspectLedger _ _ = []

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode ByronBlock where
  getSystemStart =
      SystemStart
    . Genesis.gdStartTime
    . extractGenesisData

  getNetworkMagic =
      NetworkMagic
    . Crypto.unProtocolMagicId
    . Genesis.gdProtocolMagicId
    . extractGenesisData

  getProtocolMagicId = byronProtocolMagicId

extractGenesisData :: BlockConfig ByronBlock -> Genesis.GenesisData
extractGenesisData = Genesis.configGenesisData . byronGenesisConfig

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance RunNode ByronBlock where
  nodeBlockFetchSize        = byronHeaderBlockSizeHint

  -- The epoch size is fixed and can be derived from @k@ by the ledger
  -- ('kEpochSlots').
  nodeImmDbChunkInfo        = \cfg ->
                                  simpleChunkInfo
                                . (coerce :: EpochSlots -> EpochSize)
                                . kEpochSlots
                                . Genesis.gdK
                                . extractGenesisData
                                . configBlock
                                $ cfg

  -- If the current chain is empty, produce a genesis EBB and add it to the
  -- ChainDB. Only an EBB can have Genesis (= empty chain) as its predecessor.
  nodeInitChainDB cfg chainDB = do
      empty <- InitChainDB.checkEmpty chainDB
      when empty $ InitChainDB.addBlock chainDB genesisEBB
    where
      genesisEBB = forgeEBB (configBlock cfg) (SlotNo 0) (BlockNo 0) GenesisHash

  nodeCheckIntegrity     = verifyBlockIntegrity . configBlock

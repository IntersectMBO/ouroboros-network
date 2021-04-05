{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Node (
    PBftSignatureThreshold (..)
  , ProtocolParamsByron (..)
  , byronBlockForging
  , defaultPBftSignatureThreshold
  , mkByronConfig
  , protocolClientInfoByron
  , protocolInfoByron
    -- * Secrets
  , ByronLeaderCredentials (..)
  , ByronLeaderCredentialsError
  , mkByronLeaderCredentials
  , mkPBftCanBeLeader
  ) where

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Void (Void)

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
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Util ((.....:))

import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Ledger.Inspect ()
import           Ouroboros.Consensus.Byron.Node.Serialisation ()
import           Ouroboros.Consensus.Byron.Protocol

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

-- | Credentials needed to produce blocks in the Byron era.
data ByronLeaderCredentials = ByronLeaderCredentials {
      blcSignKey    :: Crypto.SigningKey
    , blcDlgCert    :: Delegation.Certificate
      -- | Only core nodes can produce blocks. The 'CoreNodeId' is used to
      -- determine the order (round-robin) in which core nodes produce blocks.
    , blcCoreNodeId :: CoreNodeId
      -- | Identifier for this set of credentials.
      --
      -- Useful when the node is running with multiple sets of credentials.
    , blcLabel      :: Text
    }
  deriving (Show)

-- | Make the 'ByronLeaderCredentials', with a couple sanity checks:
--
-- * That the block signing key and the delegation certificate match.
-- * That the delegation certificate does correspond to one of the genesis
--   keys from the genesis file.
--
mkByronLeaderCredentials ::
     Genesis.Config
  -> Crypto.SigningKey
  -> Delegation.Certificate
  -> Text
  -> Either ByronLeaderCredentialsError ByronLeaderCredentials
mkByronLeaderCredentials gc sk cert lbl = do
    guard (Delegation.delegateVK cert == Crypto.toVerification sk)
      ?! NodeSigningKeyDoesNotMatchDelegationCertificate

    let vkGenesis = Delegation.issuerVK cert
    nid <- genesisKeyCoreNodeId gc (VerKeyByronDSIGN vkGenesis)
             ?! DelegationCertificateNotFromGenesisKey

    return ByronLeaderCredentials {
      blcSignKey     = sk
    , blcDlgCert     = cert
    , blcCoreNodeId  = nid
    , blcLabel       = lbl
    }
  where
    (?!) :: Maybe a -> e -> Either e a
    Just x  ?! _ = Right x
    Nothing ?! e = Left  e

data ByronLeaderCredentialsError =
       NodeSigningKeyDoesNotMatchDelegationCertificate
     | DelegationCertificateNotFromGenesisKey
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge ByronBlock = PBftCannotForge PBftByronCrypto

type instance ForgeStateInfo ByronBlock = ()

type instance ForgeStateUpdateError ByronBlock = Void

byronBlockForging
  :: Monad m
  => ByronLeaderCredentials
  -> BlockForging m ByronBlock
byronBlockForging creds = BlockForging {
      forgeLabel       = blcLabel creds
    , canBeLeader
    , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
    , checkCanForge    = \cfg slot tickedPBftState _isLeader () ->
                             pbftCheckCanForge
                               (configConsensus cfg)
                               canBeLeader
                               slot
                               tickedPBftState
    , forgeBlock       = return .....: forgeByronBlock
    }
  where
    canBeLeader = mkPBftCanBeLeader creds

mkPBftCanBeLeader :: ByronLeaderCredentials -> CanBeLeader (PBft PBftByronCrypto)
mkPBftCanBeLeader (ByronLeaderCredentials sk cert nid _) = PBftCanBeLeader {
      pbftCanBeLeaderCoreNodeId = nid
    , pbftCanBeLeaderSignKey    = SignKeyByronDSIGN sk
    , pbftCanBeLeaderDlgCert    = cert
    }

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | See chapter 4.1 of
--   https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
defaultPBftSignatureThreshold :: PBftSignatureThreshold
defaultPBftSignatureThreshold = PBftSignatureThreshold 0.22

-- | Parameters needed to run Byron
data ProtocolParamsByron = ProtocolParamsByron {
      byronGenesis                :: Genesis.Config
    , byronPbftSignatureThreshold :: Maybe PBftSignatureThreshold
    , byronProtocolVersion        :: Update.ProtocolVersion
    , byronSoftwareVersion        :: Update.SoftwareVersion
    , byronLeaderCredentials      :: Maybe ByronLeaderCredentials
    }

protocolInfoByron ::
     forall m. Monad m
  => ProtocolParamsByron
  -> ProtocolInfo m ByronBlock
protocolInfoByron ProtocolParamsByron {
                      byronGenesis                = genesisConfig
                    , byronPbftSignatureThreshold = mSigThresh
                    , byronProtocolVersion        = pVer
                    , byronSoftwareVersion        = sVer
                    , byronLeaderCredentials      = mLeaderCreds
                    } =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PBftConfig {
                pbftParams = byronPBftParams compactedGenesisConfig mSigThresh
              }
          , topLevelConfigLedger  = compactedGenesisConfig
          , topLevelConfigBlock   = blockConfig
          , topLevelConfigCodec   = mkByronCodecConfig compactedGenesisConfig
          , topLevelConfigStorage = ByronStorageConfig blockConfig
          }
      , pInfoInitLedger = ExtLedgerState {
            -- Important: don't pass the compacted genesis config to
            -- 'initByronLedgerState', it needs the full one, including the AVVM
            -- balances.
            ledgerState = initByronLedgerState genesisConfig Nothing
          , headerState = genesisHeaderState S.empty
          }
      , pInfoBlockForging =
          return $ byronBlockForging <$> maybeToList mLeaderCreds
      }
  where
    compactedGenesisConfig = compactGenesisConfig genesisConfig

    blockConfig = mkByronConfig compactedGenesisConfig pVer sVer

protocolClientInfoByron :: EpochSlots -> ProtocolClientInfo ByronBlock
protocolClientInfoByron epochSlots =
    ProtocolClientInfo {
      pClientInfoCodecConfig = ByronCodecConfig {
          getByronEpochSlots = epochSlots
        }
    }

byronPBftParams :: Genesis.Config -> Maybe PBftSignatureThreshold -> PBftParams
byronPBftParams cfg threshold = PBftParams {
      pbftSecurityParam      = genesisSecurityParam cfg
    , pbftNumNodes           = genesisNumCoreNodes  cfg
    , pbftSignatureThreshold = fromMaybe defaultPBftSignatureThreshold threshold
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

extractGenesisData :: BlockConfig ByronBlock -> Genesis.GenesisData
extractGenesisData = Genesis.configGenesisData . byronGenesisConfig

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance NodeInitStorage ByronBlock where
  -- The epoch size is fixed and can be derived from @k@ by the ledger
  -- ('kEpochSlots').
  nodeImmutableDbChunkInfo =
        simpleChunkInfo
      . (coerce :: EpochSlots -> EpochSize)
      . kEpochSlots
      . Genesis.gdK
      . extractGenesisData
      . getByronBlockConfig

  -- If the current chain is empty, produce a genesis EBB and add it to the
  -- ChainDB. Only an EBB can have Genesis (= empty chain) as its predecessor.
  nodeInitChainDB cfg InitChainDB { getCurrentLedger, addBlock } = do
      tip <- ledgerTipPoint (Proxy @ByronBlock) <$> getCurrentLedger
      case tip of
        BlockPoint {} -> return ()
        GenesisPoint  -> addBlock genesisEBB
    where
      genesisEBB =
        forgeEBB (getByronBlockConfig cfg) (SlotNo 0) (BlockNo 0) GenesisHash

  nodeCheckIntegrity = verifyBlockIntegrity . getByronBlockConfig

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance BlockSupportsMetrics ByronBlock where
  isSelfIssued = isSelfIssuedConstUnknown

instance RunNode ByronBlock

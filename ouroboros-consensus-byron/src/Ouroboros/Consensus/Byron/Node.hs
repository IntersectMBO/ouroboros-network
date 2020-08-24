{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Node (
    protocolInfoByron
  , protocolClientInfoByron
  , mkByronConfig
  , PBftSignatureThreshold(..)
  , defaultPBftSignatureThreshold
  , byronBlockForging
    -- * Secrets
  , ByronLeaderCredentials(..)
  , ByronLeaderCredentialsError
  , mkByronLeaderCredentials
  , mkPBftCanBeLeader
  ) where

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Maybe
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
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
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

data ByronLeaderCredentials = ByronLeaderCredentials {
      blcSignKey    :: Crypto.SigningKey
    , blcDlgCert    :: Delegation.Certificate
    , blcCoreNodeId :: CoreNodeId
    } deriving Show

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
  -> Either ByronLeaderCredentialsError ByronLeaderCredentials
mkByronLeaderCredentials gc sk cert = do
    guard (Delegation.delegateVK cert == Crypto.toVerification sk)
      ?! NodeSigningKeyDoesNotMatchDelegationCertificate

    let vkGenesis = Delegation.issuerVK cert
    nid <- genesisKeyCoreNodeId gc (VerKeyByronDSIGN vkGenesis)
             ?! DelegationCertificateNotFromGenesisKey

    return ByronLeaderCredentials {
      blcSignKey     = sk
    , blcDlgCert     = cert
    , blcCoreNodeId  = nid
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
      canBeLeader
    , updateForgeState = \_ -> return $ ForgeStateUpdateInfo $ Unchanged ()
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
mkPBftCanBeLeader (ByronLeaderCredentials sk cert nid) = PBftCanBeLeader {
      pbftCanBeLeaderCoreNodeId = nid
    , pbftCanBeLeaderSignKey    = SignKeyByronDSIGN sk
    , pbftCanBeLeaderDlgCert    = cert
    }

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
                  -> Maybe ByronLeaderCredentials
                  -> ProtocolInfo m ByronBlock
protocolInfoByron genesisConfig mSigThresh pVer sVer mLeader =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PBftConfig {
                pbftParams = byronPBftParams compactedGenesisConfig mSigThresh
              }
          , topLevelConfigBlock = FullBlockConfig {
                blockConfigLedger = compactedGenesisConfig
              , blockConfigBlock  = mkByronConfig compactedGenesisConfig pVer sVer
              , blockConfigCodec  = mkByronCodecConfig compactedGenesisConfig
              }
          }
      , pInfoInitLedger = ExtLedgerState {
            -- Important: don't pass the compacted genesis config to
            -- 'initByronLedgerState', it needs the full one, including the AVVM
            -- balances.
            ledgerState = initByronLedgerState genesisConfig Nothing
          , headerState = genesisHeaderState S.empty
          }
      , pInfoBlockForging =
          return . byronBlockForging <$> mLeader
      }
  where
    compactedGenesisConfig = compactGenesisConfig genesisConfig

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

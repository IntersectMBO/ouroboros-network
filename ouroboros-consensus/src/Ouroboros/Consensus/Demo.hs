{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Instantiations of the protocol stack used in tests and demos
module Ouroboros.Consensus.Demo (
    -- * Abstract over protocols
    DemoProtocol(..)
  , DemoBFT
  , DemoPraos
  , DemoLeaderSchedule
  , DemoMockPBFT
  , DemoRealPBFT
  , NumCoreNodes(..)
  , ProtocolInfo(..)
  , protocolInfo
  , RunDemo(..)
  , DemoBlock(..)
  , DemoHeader(..)
  , DemoHeaderHash(..)
  , runDemo
    -- * Support for runnig the demos
  , defaultSecurityParam
  , defaultDemoPraosParams
  , defaultDemoPBftParams
  , enumCoreNodes
  , HasCreator(..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import qualified Data.Bimap as Bimap
import           Data.Coerce
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Reflection (Given (..), give)
import qualified Data.Sequence as Seq

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Cardano.Genesis
import qualified Cardano.Chain.Slotting as Cardano.Slot
import qualified Cardano.Chain.Update as Cardano.Update
import qualified Cardano.Crypto as Cardano
import qualified Cardano.Crypto.Signing as Cardano.KeyGen

import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HasHeader,
                     HeaderHash, SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (verKeyIdFromSigned)
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.KES
import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import qualified Ouroboros.Consensus.Ledger.Byron.Demo as ByronDemo
import           Ouroboros.Consensus.Ledger.Mock (SimpleBlock,
                     SimpleBlockMockCrypto, SimpleHeader, SimplePreHeader)
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

type DemoBFT            = Bft BftMockCrypto
type DemoPraos          = ExtNodeConfig Mock.AddrDist (Praos PraosMockCrypto)
type DemoLeaderSchedule = WithLeaderSchedule (Praos PraosMockCrypto)
type DemoMockPBFT       = ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto)
type DemoRealPBFT       = ExtNodeConfig ByronDemo.Config (PBft PBftCardanoCrypto)

-- | Consensus protocol to use
data DemoProtocol blk hdr where
  -- | Run BFT against the mock ledger
  DemoBFT
    :: SecurityParam
    -> DemoProtocol (SimpleBlock  DemoBFT SimpleBlockMockCrypto)
                    (SimpleHeader DemoBFT SimpleBlockMockCrypto)

  -- | Run Praos against the mock ledger
  DemoPraos
    :: PraosParams
    -> DemoProtocol (SimpleBlock  DemoPraos SimpleBlockMockCrypto)
                    (SimpleHeader DemoPraos SimpleBlockMockCrypto)

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  DemoLeaderSchedule
    :: LeaderSchedule
    -> PraosParams
    -> DemoProtocol (SimpleBlock  DemoLeaderSchedule SimpleBlockMockCrypto)
                    (SimpleHeader DemoLeaderSchedule SimpleBlockMockCrypto)

  -- | Run PBFT against the mock ledger
  DemoMockPBFT
    :: PBftParams
    -> DemoProtocol (SimpleBlock  DemoMockPBFT SimpleBlockMockCrypto)
                    (SimpleHeader DemoMockPBFT SimpleBlockMockCrypto)

  -- | Run PBFT against the real ledger
  DemoRealPBFT
    :: PBftParams
    -> Cardano.Genesis.Config
    -> DemoProtocol (ByronBlock  ByronDemo.Config)
                    (ByronHeader ByronDemo.Config)

-- | Data required to run the specified protocol.
data ProtocolInfo b = ProtocolInfo {
        pInfoConfig     :: NodeConfig (BlockProtocol b)
      , pInfoInitState  :: NodeState  (BlockProtocol b)
        -- | The ledger state at genesis
      , pInfoInitLedger :: ExtLedgerState b
      }

newtype NumCoreNodes = NumCoreNodes Int
  deriving (Show)

-- | Info needed to run the selected protocol
protocolInfo :: DemoProtocol blk hdr -> NumCoreNodes -> CoreNodeId -> ProtocolInfo blk
protocolInfo (DemoBFT securityParam) (NumCoreNodes numCoreNodes) (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = BftNodeConfig {
            bftParams   = BftParams {
                              bftNumNodes      = fromIntegral numCoreNodes
                            , bftSecurityParam = securityParam
                            }
          , bftNodeId   = CoreId nid
          , bftSignKey  = SignKeyMockDSIGN nid
          , bftVerKeys  = Map.fromList [
                (CoreId n, VerKeyMockDSIGN n)
              | n <- [0 .. numCoreNodes - 1]
              ]
          }
      , pInfoInitLedger = ExtLedgerState (Mock.genesisLedgerState addrDist) ()
      , pInfoInitState  = ()
      }
  where
    addrDist :: Mock.AddrDist
    addrDist = Mock.mkAddrDist numCoreNodes
protocolInfo (DemoPraos params) (NumCoreNodes numCoreNodes) (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PraosNodeConfig {
                praosParams        = params
              , praosNodeId        = CoreId nid
              , praosSignKeyVRF    = SignKeyMockVRF nid
              , praosInitialEta    = 0
              , praosInitialStake  = Mock.genesisStakeDist addrDist
              , praosVerKeys       = verKeys
              }
          , encNodeConfigExt = addrDist
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState         = Mock.genesisLedgerState addrDist
          , ouroborosChainState = []
          }
      , pInfoInitState = SignKeyMockKES (
             fst $ verKeys IntMap.! nid   -- key ID
           , 0                            -- KES initial slot
           , praosLifetimeKES params      -- KES lifetime
           )
      }
  where
    addrDist :: Mock.AddrDist
    addrDist = Mock.mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList [ (nd, (VerKeyMockKES nd, VerKeyMockVRF nd))
                              | nd <- [0 .. numCoreNodes - 1]
                              ]
protocolInfo (DemoLeaderSchedule schedule params)
             (NumCoreNodes numCoreNodes)
             (CoreNodeId nid) =
    ProtocolInfo
    { pInfoConfig    = WLSNodeConfig
        { lsNodeConfigSchedule = schedule
        , lsNodeConfigP        = PraosNodeConfig
            { praosParams       = params
            , praosNodeId       = CoreId nid
            , praosSignKeyVRF   = SignKeyMockVRF nid
            , praosInitialEta   = 0
            , praosInitialStake = Mock.genesisStakeDist addrDist
            , praosVerKeys      = verKeys
            }
        , lsNodeConfigNodeId   = CoreNodeId nid
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState         = Mock.genesisLedgerState addrDist
        , ouroborosChainState = ()
        }
    , pInfoInitState  = ()
    }
  where
    addrDist = Mock.mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList [ (nd, (VerKeyMockKES nd, VerKeyMockVRF nd))
                              | nd <- [0 .. numCoreNodes - 1]
                              ]
protocolInfo (DemoMockPBFT params)
             (NumCoreNodes numCoreNodes)
             (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                pbftParams   = params {pbftNumNodes = fromIntegral numCoreNodes}
              , pbftNodeId   = CoreId nid
              , pbftSignKey  = SignKeyMockDSIGN nid
              , pbftVerKey   = VerKeyMockDSIGN nid
              }
            , encNodeConfigExt = PBftLedgerView
                (Bimap.fromList [(VerKeyMockDSIGN n, VerKeyMockDSIGN n) | n <- [0 .. numCoreNodes - 1]])
          }
      , pInfoInitLedger = ExtLedgerState (Mock.genesisLedgerState addrDist) Seq.empty
      , pInfoInitState  = ()
      }
  where
    addrDist :: Mock.AddrDist
    addrDist = Mock.mkAddrDist numCoreNodes

protocolInfo (DemoRealPBFT params gc)
             (NumCoreNodes numCoreNodes)
             (CoreNodeId nid) =
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
                }
          , encNodeConfigExt = ByronDemo.Config {
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

{-------------------------------------------------------------------------------
  Support for running the demos
-------------------------------------------------------------------------------}

defaultSecurityParam :: SecurityParam
defaultSecurityParam = SecurityParam 5

defaultDemoPraosParams :: PraosParams
defaultDemoPraosParams = PraosParams {
      praosSecurityParam = defaultSecurityParam
    , praosSlotsPerEpoch = 3
    , praosLeaderF       = 0.5
    , praosLifetimeKES   = 1000000
    }

defaultDemoPBftParams :: PBftParams
defaultDemoPBftParams = PBftParams {
      pbftSecurityParam      = defaultSecurityParam
    , pbftNumNodes           = nn
    , pbftSignatureWindow    = fromIntegral $ nn * 10
    , pbftSignatureThreshold = (1.0 / fromIntegral nn) + 0.1
    }
  where
    nn = 3

enumCoreNodes :: NumCoreNodes -> [CoreNodeId]
enumCoreNodes (NumCoreNodes numNodes) = [ CoreNodeId n
                                        | n <- [0 .. numNodes - 1]
                                        ]

{-------------------------------------------------------------------------------
  Who created a block?
-------------------------------------------------------------------------------}

class HasCreator b where
    getCreator :: NodeConfig (BlockProtocol b) -> b -> CoreNodeId

instance HasCreator (SimpleBlock DemoBFT c) where
    getCreator _ = CoreNodeId
                 . verKeyIdFromSigned
                 . bftSignature
                 . Mock.headerOuroboros
                 . Mock.simpleHeader

instance HasCreator (SimpleBlock DemoPraos c) where
    getCreator _ = praosCreator
                 . praosExtraFields
                 . encPayloadP
                 . Mock.headerOuroboros
                 . Mock.simpleHeader

instance HasCreator (SimpleBlock DemoLeaderSchedule c) where
    getCreator _ = getWLSPayload
                 . Mock.headerOuroboros
                 . Mock.simpleHeader

instance HasCreator (SimpleBlock DemoMockPBFT c) where
    getCreator _ = CoreNodeId
                 . verKeyIdFromSigned
                 . pbftSignature
                 . encPayloadP
                 . Mock.headerOuroboros
                 . Mock.simpleHeader

instance HasCreator (ByronBlock ByronDemo.Config) where
    getCreator (EncNodeConfig _ ByronDemo.Config{..}) (ByronBlock b) =
        fromMaybe (error "getCreator: unknown key") $ Bimap.lookup key pbftCoreNodes
     where
       key :: Cardano.VerificationKey
       key = Cardano.pskIssuerVK
             . Cardano.psigPsk
             . Cardano.Block.unBlockSignature
             . Cardano.Block.headerSignature
             . Cardano.Block.blockHeader
             $ b

{-------------------------------------------------------------------------------
  Additional functions needed to run the demo
-------------------------------------------------------------------------------}

class DemoHeaderHash hh where
  demoEncodeHeaderHash :: hh -> Encoding
  demoDecodeHeaderHash :: Decoder s hh

class ( DemoHeaderHash (HeaderHash hdr)
      , SupportedBlock (BlockProtocol hdr) hdr
      , HasHeader hdr
      , Condense hdr
      , Condense (ChainHash hdr)
      ) => DemoHeader hdr where
  demoEncodeHeader   :: NodeConfig (BlockProtocol hdr) -> hdr -> Encoding
  demoDecodeHeader   :: NodeConfig (BlockProtocol hdr) -> Decoder s hdr
  demoBlockFetchSize :: hdr -> SizeInBytes

class ( ProtocolLedgerView blk
      , LedgerConfigView   blk
      , Condense           blk
      , Condense          [blk]
      , ApplyTx            blk
      , Show (Payload (BlockProtocol blk) (PreHeader blk))
      ) => DemoBlock blk where
  demoEncodeBlock :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  demoDecodeBlock :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s blk

  -- | Construct transaction from mock transaction
  --
  -- When we run the demo, for convenience we submit mock transactions from
  -- the command line. These then need to be translated to "real" transactions
  -- for the ledger that we are running. Of course, this translation will
  -- necessarily be limited and will rely on things like 'generatedSecrets'.
  demoMockTx :: NodeConfig (BlockProtocol blk) -> Mock.Tx -> GenTx blk

class ( DemoHeader hdr
      , DemoBlock blk
      , BlockProtocol blk ~ BlockProtocol hdr
      , HeaderHash    blk ~ HeaderHash    hdr
      ) => RunDemo blk hdr where
  demoForgeBlock         :: (HasNodeState (BlockProtocol blk) m, MonadRandom m)
                         => NodeConfig (BlockProtocol blk)
                         -> SlotNo         -- ^ Current slot
                         -> BlockNo        -- ^ Current block number
                         -> ChainHash hdr  -- ^ Previous hash
                         -> [GenTx blk]    -- ^ Txs to add in the block
                         -> IsLeader (BlockProtocol blk)
                         -> m blk
  demoGetHeader          :: blk -> hdr
  demoBlockMatchesHeader :: hdr -> blk -> Bool

{-------------------------------------------------------------------------------
  RunDemo instance for the mock ledger
-------------------------------------------------------------------------------}

instance HashAlgorithm h => DemoHeaderHash (Hash h a) where
  demoEncodeHeaderHash = Serialise.encode
  demoDecodeHeaderHash = Serialise.decode

instance ( OuroborosTag p
         , SupportedBlock p (SimpleHeader p SimpleBlockMockCrypto)
         , Serialise (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         , Condense  (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         ) => DemoHeader (SimpleHeader p SimpleBlockMockCrypto) where
  demoEncodeHeader   = const Serialise.encode
  demoDecodeHeader   = const Serialise.decode
  demoBlockFetchSize = Mock.headerBlockSize . Mock.headerPreHeader

instance ( OuroborosTag p
         , ProtocolLedgerView (SimpleBlock p SimpleBlockMockCrypto)
         , Condense  (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         , Serialise (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         , Show      (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         ) => DemoBlock (SimpleBlock p SimpleBlockMockCrypto) where
  demoEncodeBlock = const Serialise.encode
  demoDecodeBlock = const Serialise.decode
  demoMockTx      = \_ -> Mock.SimpleGenTx

instance ( OuroborosTag p
         , ProtocolLedgerView (SimpleBlock  p SimpleBlockMockCrypto)
         , SupportedBlock p   (SimpleHeader p SimpleBlockMockCrypto)
         , Condense  (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         , Serialise (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         , Show      (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
         ) => RunDemo (SimpleBlock  p SimpleBlockMockCrypto)
                      (SimpleHeader p SimpleBlockMockCrypto) where
  demoForgeBlock         = Mock.forgeSimpleBlock
  demoGetHeader          = Mock.simpleHeader
  demoBlockMatchesHeader = Mock.blockMatchesHeader

{-------------------------------------------------------------------------------
  RunDemo instance for PBFT with the real ledger
-------------------------------------------------------------------------------}

instance DemoHeaderHash Cardano.Block.HeaderHash where
  demoEncodeHeaderHash = ByronDemo.encodeHeaderHash
  demoDecodeHeaderHash = ByronDemo.decodeHeaderHash

instance ( Given Cardano.Block.HeaderHash
         , Given Cardano.Slot.EpochSlots
         ) => DemoHeader (ByronHeader ByronDemo.Config) where
  demoEncodeHeader   = ByronDemo.encodeHeader
  demoDecodeHeader   = ByronDemo.decodeHeader
  demoBlockFetchSize = const 2000 -- TODO #593

instance ( Given Cardano.Block.HeaderHash
         , Given Cardano.ProtocolMagicId
         , Given Cardano.Slot.EpochSlots
         ) => DemoBlock (ByronBlock ByronDemo.Config) where
  demoEncodeBlock = ByronDemo.encodeBlock
  demoDecodeBlock = ByronDemo.decodeBlock
  demoMockTx      = ByronDemo.elaborateTx

instance ( Given Cardano.Block.HeaderHash
         , Given Cardano.ProtocolMagicId
         , Given Cardano.Slot.EpochSlots
         ) => RunDemo (ByronBlock  ByronDemo.Config)
                      (ByronHeader ByronDemo.Config) where
  demoForgeBlock         = ByronDemo.forgeBlock
  demoGetHeader          = byronHeader
  demoBlockMatchesHeader = \_hdr _blk -> True -- TODO #595

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported demos
-------------------------------------------------------------------------------}

runDemo :: DemoProtocol blk hdr -> Dict (RunDemo blk hdr)
runDemo DemoBFT{}            = Dict
runDemo DemoPraos{}          = Dict
runDemo DemoLeaderSchedule{} = Dict
runDemo DemoMockPBFT{}       = Dict
runDemo DemoRealPBFT{}       = give (Dummy.dummyEpochSlots)
                             $ give (Cardano.Genesis.gdProtocolMagicId Dummy.dummyGenesisData)
                             $ give (coerce @_ @Cardano.Block.HeaderHash Dummy.dummyGenesisHash)
                             $ Dict

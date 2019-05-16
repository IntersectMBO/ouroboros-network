{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
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
  , Block
  , Header
  , NumCoreNodes(..)
  , ProtocolInfo(..)
  , protocolInfo
  , RunDemo(..)
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
import qualified Codec.Serialise as Serialise
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString as BS
import           Data.Either (fromRight)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Cardano.Genesis
import qualified Cardano.Crypto as Cardano
import qualified Cardano.Crypto.Signing as Cardano.KeyGen

import           Ouroboros.Network.Block (BlockNo, ChainHash, HasHeader,
                     HeaderHash, SlotNo, StandardHash)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (verKeyIdFromSigned)
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.KES
import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

type DemoBFT            = Bft BftMockCrypto
type DemoPraos          = ExtNodeConfig AddrDist (Praos PraosMockCrypto)
type DemoLeaderSchedule = WithLeaderSchedule (Praos PraosMockCrypto)
type DemoMockPBFT       = ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto)
type DemoRealPBFT       = ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto)

-- | Consensus protocol to use
data DemoProtocol p where
  -- | Run BFT against the mock ledger
  DemoBFT            :: SecurityParam -> DemoProtocol DemoBFT

  -- | Run Praos against the mock ledger
  DemoPraos          :: PraosParams -> DemoProtocol DemoPraos

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  DemoLeaderSchedule :: LeaderSchedule -> PraosParams -> DemoProtocol DemoLeaderSchedule

  -- | Run PBFT against the mock ledger
  DemoMockPBFT       :: PBftParams -> DemoProtocol DemoMockPBFT

  -- | Run PBFT against the real ledger
  DemoRealPBFT       :: PBftParams -> DemoProtocol DemoRealPBFT

type family Block p = b | b -> p where
  Block DemoRealPBFT = ByronBlock ByronDemoConfig

  -- Demos using mock ledger/block
  Block p = SimpleBlock p SimpleBlockMockCrypto

type family Header p :: * where
  Header DemoRealPBFT = ByronHeader

  -- Demos using mock ledger/block
  Header p = SimpleHeader p SimpleBlockMockCrypto

-- | Data required to run the specified protocol.
data ProtocolInfo p = ProtocolInfo {
        pInfoConfig     :: NodeConfig p
        -- | The ledger state at genesis
      , pInfoInitLedger :: ExtLedgerState (Block p)
      , pInfoInitState  :: NodeState p
      }

newtype NumCoreNodes = NumCoreNodes Int
  deriving (Show)

-- | Info needed to run the selected protocol
protocolInfo :: DemoProtocol p -> NumCoreNodes -> CoreNodeId -> ProtocolInfo p
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
      , pInfoInitLedger = ExtLedgerState (genesisLedgerState addrDist) ()
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
protocolInfo (DemoPraos params) (NumCoreNodes numCoreNodes) (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PraosNodeConfig {
                praosParams        = params
              , praosNodeId        = CoreId nid
              , praosSignKeyVRF    = SignKeyMockVRF nid
              , praosInitialEta    = 0
              , praosInitialStake  = genesisStakeDist addrDist
              , praosVerKeys       = verKeys
              }
          , encNodeConfigExt = addrDist
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState         = genesisLedgerState addrDist
          , ouroborosChainState = []
          }
      , pInfoInitState = SignKeyMockKES (
             fst $ verKeys IntMap.! nid   -- key ID
           , 0                            -- KES initial slot
           , praosLifetimeKES params      -- KES lifetime
           )
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

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
            , praosInitialStake = genesisStakeDist addrDist
            , praosVerKeys      = verKeys
            }
        , lsNodeConfigNodeId   = CoreNodeId nid
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState         = genesisLedgerState addrDist
        , ouroborosChainState = ()
        }
    , pInfoInitState  = ()
    }
  where
    addrDist = mkAddrDist numCoreNodes

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
      , pInfoInitLedger = ExtLedgerState (genesisLedgerState addrDist) Seq.empty
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
protocolInfo (DemoRealPBFT params)
             (NumCoreNodes numCoreNodes)
             (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                  pbftParams  = params {pbftNumNodes = fromIntegral numCoreNodes}
                , pbftNodeId  = CoreId nid
                , pbftSignKey = SignKeyCardanoDSIGN (snd (mkKey nid))
                , pbftVerKey  = VerKeyCardanoDSIGN  (fst (mkKey nid))
                }
          , encNodeConfigExt = ByronDemoConfig {
                pbftNodes = Map.fromList [
                                (fst (mkKey n), CoreNodeId n)
                              | n <- [0 .. numCoreNodes]
                              ]
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
    Right initState = runExcept $
        Cardano.Block.initialChainValidationState (pbftGenesisConfig params)

    mkKey :: Int -> (Cardano.VerificationKey, Cardano.SigningKey)
    mkKey n =
        Cardano.KeyGen.deterministicKeyGen $
          BS.pack $ replicate 32 (fromIntegral n)

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

defaultDemoPBftParams :: Cardano.Genesis.Config -> PBftParams
defaultDemoPBftParams genesisConfig = PBftParams {
      pbftSecurityParam      = defaultSecurityParam
    , pbftNumNodes           = nn
    , pbftSignatureWindow    = fromIntegral $ nn * 10
    , pbftSignatureThreshold = (1.0 / fromIntegral nn) + 0.1
    , pbftGenesisConfig      = genesisConfig
    }
  where
    nn = 3

enumCoreNodes :: NumCoreNodes -> [CoreNodeId]
enumCoreNodes (NumCoreNodes numNodes) = [ CoreNodeId n
                                        | n <- [0 .. numNodes - 1]
                                        ]

{-------------------------------------------------------------------------------
  Parameters common to all protocols
-------------------------------------------------------------------------------}

-- | Construct address to node ID mapping
mkAddrDist :: Int -- ^ Number of nodes
           -> AddrDist
mkAddrDist numCoreNodes =
    Map.fromList $ zip [[addr]   | addr <- ['a'..]]
                       [CoreId n | n    <- [0  .. numCoreNodes - 1]]

-- | Transaction giving initial stake to the nodes
genesisTx :: AddrDist -> Tx
genesisTx addrDist = Tx mempty [(addr, 1000) | addr <- Map.keys addrDist]

genesisUtxo :: AddrDist -> Utxo
genesisUtxo addrDist =
    fromRight (error "genesisLedger: invalid genesis tx") $
      runExcept (utxo (genesisTx addrDist))

genesisLedgerState :: AddrDist -> LedgerState (SimpleBlock p c)
genesisLedgerState addrDist = SimpleLedgerState {
      slsUtxo      = genesisUtxo addrDist
    , slsConfirmed = Set.singleton (hash (genesisTx addrDist))
    }

-- | Genesis stake distribution
genesisStakeDist :: AddrDist -> StakeDist
genesisStakeDist addrDist =
    relativeStakes (totalStakes addrDist (genesisUtxo addrDist))

{-------------------------------------------------------------------------------
  Who created a block?
-------------------------------------------------------------------------------}

class HasCreator p where
    getCreator :: NodeConfig p -> Block p -> CoreNodeId

instance HasCreator DemoBFT where
    getCreator _ = CoreNodeId
                 . verKeyIdFromSigned
                 . bftSignature
                 . headerOuroboros
                 . simpleHeader

instance HasCreator DemoPraos where
    getCreator _ = praosCreator
                 . praosExtraFields
                 . encPayloadP
                 . headerOuroboros
                 . simpleHeader

instance HasCreator DemoLeaderSchedule where
    getCreator _ = getWLSPayload
                 . headerOuroboros
                 . simpleHeader

instance HasCreator DemoMockPBFT where
    getCreator _ = CoreNodeId
                 . verKeyIdFromSigned
                 . pbftSignature
                 . encPayloadP
                 . headerOuroboros
                 . simpleHeader

instance HasCreator DemoRealPBFT where
    getCreator (EncNodeConfig _ ByronDemoConfig{..}) (ByronBlock b) =
        Map.findWithDefault (error "getCreator: unknown key") key pbftNodes
     where
       key :: Cardano.VerificationKey
       key = Cardano.Block.headerGenesisKey
           . Cardano.Block.blockHeader
           $ b


{-
type DemoProtocolConstraints p =

demoProtocolConstraints :: DemoProtocol p -> Dict (DemoProtocolConstraints p)
demoProtocolConstraints DemoBFT{}            = Dict
demoProtocolConstraints DemoPraos{}          = Dict
demoProtocolConstraints DemoLeaderSchedule{} = Dict
demoProtocolConstraints DemoMockPBFT{}       = Dict
demoProtocolConstraints DemoRealPBFT{}       = Dict

-}

{-------------------------------------------------------------------------------
  Additional functions needed to run the demo
-------------------------------------------------------------------------------}

class ( OuroborosTag p
      , ProtocolLedgerView (Block p)
      , HasCreator p
      , Condense  (Payload p (PreHeader (Block p)))
      , Eq        (Payload p (PreHeader (Block p)))
      , Show      (Payload p (PreHeader (Block p)))
      , BlockProtocol (Block p) ~ p
      , HeaderHash (Block p) ~ HeaderHash (Header p)
      , StandardHash (Header p)
      , HasHeader (Header p)
      , LedgerConfigView (Block p)
      , SupportedPreHeader p (PreHeader (Block p))
      , Condense (Block p)
      , Condense [Block p]
      ) => RunDemo p where
  demoForgeBlock :: (HasNodeState p m, MonadRandom m)
                 => NodeConfig p
                 -> SlotNo                      -- ^ Current slot
                 -> BlockNo                     -- ^ Current block number
                 -> ChainHash (Header p)        -- ^ Previous hash
                 -> Map (Hash ShortHash Tx) Tx  -- ^ Txs to add in the block
                 -> IsLeader p
                 -> m (Block p)

  demoGetHeader        :: Block p -> Header p

  -- We provide context for the encoders and decoders in case they need access
  -- to stuff like "number of slots in an epoch"

  demoEncodeHeader     :: NodeConfig p -> Header p -> Encoding
  demoEncodeHeaderHash :: NodeConfig p -> HeaderHash (Header p) -> Encoding
  demoEncodeBlock      :: NodeConfig p -> Block p -> Encoding
  demoDecodeHeader     :: forall s. NodeConfig p -> Decoder s (Header p)
  demoDecodeHeaderHash :: forall s. NodeConfig p -> Decoder s (HeaderHash (Header p))
  demoDecodeBlock      :: forall s. NodeConfig p -> Decoder s (Block p)

runDemo :: DemoProtocol p -> Dict (RunDemo p)
runDemo DemoBFT{}            = Dict
runDemo DemoPraos{}          = Dict
runDemo DemoLeaderSchedule{} = Dict
runDemo DemoMockPBFT{}       = Dict
runDemo DemoRealPBFT{}       = Dict

instance RunDemo DemoBFT where
  demoForgeBlock       = forgeSimpleBlock
  demoGetHeader        = simpleHeader
  demoEncodeHeader     = const Serialise.encode
  demoEncodeHeaderHash = const Serialise.encode
  demoEncodeBlock      = const Serialise.encode
  demoDecodeHeader     = const Serialise.decode
  demoDecodeHeaderHash = const Serialise.decode
  demoDecodeBlock      = const Serialise.decode

instance RunDemo DemoPraos where
  demoForgeBlock       = forgeSimpleBlock
  demoGetHeader        = simpleHeader
  demoEncodeHeader     = const Serialise.encode
  demoEncodeHeaderHash = const Serialise.encode
  demoEncodeBlock      = const Serialise.encode
  demoDecodeHeader     = const Serialise.decode
  demoDecodeHeaderHash = const Serialise.decode
  demoDecodeBlock      = const Serialise.decode

instance RunDemo DemoLeaderSchedule where
  demoForgeBlock       = forgeSimpleBlock
  demoGetHeader        = simpleHeader
  demoEncodeHeader     = const Serialise.encode
  demoEncodeHeaderHash = const Serialise.encode
  demoEncodeBlock      = const Serialise.encode
  demoDecodeHeader     = const Serialise.decode
  demoDecodeHeaderHash = const Serialise.decode
  demoDecodeBlock      = const Serialise.decode

instance RunDemo DemoMockPBFT where
  demoForgeBlock       = forgeSimpleBlock
  demoGetHeader        = simpleHeader
  demoEncodeHeader     = const Serialise.encode
  demoEncodeHeaderHash = const Serialise.encode
  demoEncodeBlock      = const Serialise.encode
  demoDecodeHeader     = const Serialise.decode
  demoDecodeHeaderHash = const Serialise.decode
  demoDecodeBlock      = const Serialise.decode

instance RunDemo DemoRealPBFT where
  demoForgeBlock       = forgeByronDemoBlock
  demoGetHeader        = byronHeader
  demoEncodeHeader     = encodeByronDemoHeader
  demoEncodeHeaderHash = encodeByronDemoHeaderHash
  demoEncodeBlock      = encodeByronDemoBlock
  demoDecodeHeader     = decodeByronDemoHeader
  demoDecodeHeaderHash = decodeByronDemoHeaderHash
  demoDecodeBlock      = decodeByronDemoBlock

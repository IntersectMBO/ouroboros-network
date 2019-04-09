{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Instantiations of the protocol stack used in tests and demos
module Ouroboros.Consensus.Demo (
    -- * Abstract over protocols
    DemoProtocol(..)
  , DemoBFT
  , DemoPBFT
  , DemoPraos
  , DemoLeaderSchedule
  , Block
  , NumCoreNodes(..)
  , ProtocolInfo(..)
  , protocolInfo
  , DemoProtocolConstraints
  , demoProtocolConstraints
    -- * Support for runnig the demos
  , defaultSecurityParam
  , defaultDemoPraosParams
  , enumCoreNodes
  , HasCreator(..)
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Data.Either (fromRight)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import           Ouroboros.Network.Block (SlotNo(..))
import           Ouroboros.Network.Chain (Chain (..))

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (verKeyIdFromSigned)
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.KES
import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Consensus.Ledger.Abstract
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
type DemoPBFT           = ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto)
type DemoPraos          = ExtNodeConfig AddrDist (Praos PraosMockCrypto)
type DemoLeaderSchedule = WithLeaderSchedule (Praos PraosMockCrypto)

-- | Consensus protocol to use
data DemoProtocol p where
  DemoBFT            :: SecurityParam -> DemoProtocol DemoBFT
  DemoPBFT           :: PBftParams -> DemoProtocol DemoPBFT
  DemoPraos          :: PraosParams -> DemoProtocol DemoPraos
  DemoLeaderSchedule :: LeaderSchedule -> PraosParams -> DemoProtocol DemoLeaderSchedule

-- | Our 'Block' type stays the same.
type Block p = SimpleBlock p SimpleBlockMockCrypto

-- | Data required to run the specified protocol
data ProtocolInfo p = ProtocolInfo {
        pInfoConfig     :: NodeConfig p
      , pInfoInitChain  :: Chain (Block p)
      , pInfoInitLedger :: ExtLedgerState (Block p)
      , pInfoInitState  :: NodeState p
      }

type DemoProtocolConstraints p = (
    OuroborosTag p
  , ProtocolLedgerView (Block p)
  , HasCreator (Block p)
  , Condense  (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  , Eq        (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  , Serialise (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  , Show      (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  )

demoProtocolConstraints :: DemoProtocol p -> Dict (DemoProtocolConstraints p)
demoProtocolConstraints DemoBFT{}            = Dict
demoProtocolConstraints DemoPBFT{}           = Dict
demoProtocolConstraints DemoPraos{}          = Dict
demoProtocolConstraints DemoLeaderSchedule{} = Dict

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
      , pInfoInitChain  = Genesis
      , pInfoInitLedger = ExtLedgerState (genesisLedgerState addrDist) ()
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
protocolInfo (DemoPBFT params) (NumCoreNodes numCoreNodes) (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                pbftParams   = params {
                    pbftNumNodes      = fromIntegral numCoreNodes
                    }
              , pbftNodeId   = CoreId nid
              , pbftSignKey  = SignKeyMockDSIGN nid
              , pbftVerKey   = VerKeyMockDSIGN nid
              }
            , encNodeConfigExt = PBftLedgerView
                (Map.fromList [(VerKeyMockDSIGN n, VerKeyMockDSIGN n) | n <- [0 .. numCoreNodes - 1]])
          }
      , pInfoInitChain  = Genesis
      , pInfoInitLedger = ExtLedgerState (genesisLedgerState addrDist) ( Seq.empty, SlotNo 0 )
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
      , pInfoInitChain  = Genesis
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
    , pInfoInitChain  = Genesis
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

{-
  data NodeConfig (WithLeaderSchedule p) = WLSNodeConfig
    { lsNodeConfigWithLeaderSchedule :: LeaderSchedule
    , lsNodeConfigP                  :: NodeConfig p
    , lsNodeConfigNodeId             :: Int
    }
    -}

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

class HasCreator b where
    getCreator :: b -> CoreNodeId

instance HasCreator (Block DemoBFT) where
    getCreator = CoreNodeId
               . verKeyIdFromSigned
               . bftSignature
               . headerOuroboros
               . simpleHeader

instance HasCreator (Block DemoPBFT) where
    getCreator = CoreNodeId
               . verKeyIdFromSigned
               . pbftSignature
               . encPayloadP
               . headerOuroboros
               . simpleHeader

instance HasCreator (Block DemoPraos) where
    getCreator = praosCreator
               . praosExtraFields
               . encPayloadP
               . headerOuroboros
               . simpleHeader

instance HasCreator (Block DemoLeaderSchedule) where
    getCreator = getWLSPayload
               . headerOuroboros
               . simpleHeader

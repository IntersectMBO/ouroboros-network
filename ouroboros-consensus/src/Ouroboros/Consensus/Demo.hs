{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Instantiations of the protocol stack used in tests and demos
module Ouroboros.Consensus.Demo (
    DemoProtocol(..)
  , DemoBFT
  , DemoPraos
  , Block
  , ProtocolInfo(..)
  , DemoProtocolConstraints
  , demoProtocolConstraints
  , NumCoreNodes(..)
  , CoreNodeId(..)
  , fromCoreNodeId
  , protocolInfo
  ) where

import           Control.Monad.Except
import           Data.Either (fromRight)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import           Data.Ratio ((%))
import qualified Data.Set as Set

import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.KES
import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense

type DemoBFT   = Bft BftMockCrypto
type DemoPraos = ExtNodeConfig AddrDist (Praos PraosMockCrypto)

-- | Consensus protocol to use
data DemoProtocol p where
  DemoBFT   :: DemoProtocol DemoBFT
  DemoPraos :: DemoProtocol DemoPraos

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
  , Condense  (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  , Eq        (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  , Serialise (Payload p (SimplePreHeader p SimpleBlockMockCrypto))
  )

demoProtocolConstraints :: DemoProtocol p -> Dict (DemoProtocolConstraints p)
demoProtocolConstraints DemoBFT   = Dict
demoProtocolConstraints DemoPraos = Dict

newtype CoreNodeId   = CoreNodeId Int
newtype NumCoreNodes = NumCoreNodes Int

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId (CoreNodeId n) = CoreId n

-- | Info needed to run the selected protocol
--
-- TODO: There is overlap between this code and the consensus tests, which
-- we may wish to avoid. That said, the demo and the consensus can pick
-- different values here.
protocolInfo :: DemoProtocol p -> NumCoreNodes -> CoreNodeId -> ProtocolInfo p
protocolInfo DemoBFT (NumCoreNodes numCoreNodes) (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = BftNodeConfig {
            bftNodeId   = CoreId nid
          , bftSignKey  = SignKeyMockDSIGN nid
          , bftNumNodes = fromIntegral numCoreNodes
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
protocolInfo DemoPraos (NumCoreNodes numCoreNodes) (CoreNodeId nid) =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PraosNodeConfig {
                praosNodeId        = CoreId nid
              , praosSignKeyVRF    = SignKeyMockVRF nid
              , praosSlotsPerEpoch = fromIntegral $ k * kPerEpoch
              , praosInitialEta    = 0
              , praosInitialStake  = genesisStakeDist numCoreNodes
              , praosLeaderF       = 0.5
              , praosK             = fromIntegral k
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
             fst $ verKeys IntMap.! nid  -- key ID
           , 0                           -- KES initial slot
           , 1 + fromIntegral numSlots   -- KES lifetime
           )
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList [ (nd, (VerKeyMockKES nd, VerKeyMockVRF nd))
                              | nd <- [0 .. numCoreNodes - 1]
                              ]

    k, kPerEpoch, numSlots :: Int
    k         = 5
    kPerEpoch = 3
    numSlots  = maxBound - 1 -- avoid overflow in nodeState computation

{-------------------------------------------------------------------------------
  Parameters common to all protocols
-------------------------------------------------------------------------------}

-- | Construct address to node ID mapping
mkAddrDist :: Int -- ^ Number of nodes
           -> AddrDist
mkAddrDist numCoreNodes =
    Map.fromList $ zip [[addr]   | addr <- ['a'..]]
                       [CoreId n | n    <- [0  .. numCoreNodes - 1]]

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

genesisStakeDist :: Int -> StakeDist
genesisStakeDist numCoreNodes =
    IntMap.fromList [ (n, 1 % fromIntegral numCoreNodes)
                    | n <- [0 .. numCoreNodes - 1]
                    ]

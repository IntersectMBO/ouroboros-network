{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Mock (
    -- * Basic definitions
    Tx(..)
  , TxIn
  , TxOut
  , Addr
  , Utxo
  , InvalidInputs(..)
    -- * Compute UTxO
  , HasUtxo(..)
  , utxo
    -- * Block crypto
  , SimpleBlockCrypto(..)
  , SimpleBlockStandardCrypto -- just a tag
  , SimpleBlockMockCrypto -- just a tag
    -- * Blocks
  , SimpleBlock(..)
  , SimpleHeader(..)
  , SimplePreHeader(..)
  , SimpleBody(..)
  , forgeSimpleBlock
  , blockMatchesHeader
    -- * Updating the Ledger state
  , LedgerState(..)
  , AddrDist
  , relativeStakes
  , totalStakes
  ) where

import           Codec.CBOR.Decoding (decodeListLenOf)
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as BL
import           Data.FingerTree (Measured (measure))
import qualified Data.IntMap.Strict as IntMap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain, toOldestFirst)

import           Ouroboros.Consensus.Crypto.DSIGN.Class (Empty)
import           Ouroboros.Consensus.Crypto.Hash.Class
import           Ouroboros.Consensus.Crypto.Hash.MD5 (MD5)
import           Ouroboros.Consensus.Crypto.Hash.Short (ShortHash)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.HList (All, HList)
import qualified Ouroboros.Consensus.Util.HList as HList

{-------------------------------------------------------------------------------
  Basic definitions

-------------------------------------------------------------------------------}

data Tx = Tx (Set TxIn) [TxOut]
  deriving (Show, Eq, Ord, Generic)

instance Condense Tx where
  condense (Tx ins outs) = condense (ins, outs)

type TxIn  = (Hash ShortHash Tx, Int)
type TxOut = (Addr, Int)
type Addr  = String
type Utxo  = Map TxIn TxOut

{-------------------------------------------------------------------------------
  Computing UTxO
-------------------------------------------------------------------------------}

newtype InvalidInputs = InvalidInputs (Set TxIn) deriving (Show, Condense)

class HasUtxo a where
  txIns      :: a -> Set TxIn
  txOuts     :: a -> Utxo
  confirmed  :: a -> Set (Hash ShortHash Tx)
  updateUtxo :: Monad m => a -> Utxo -> ExceptT InvalidInputs m Utxo

utxo :: (Monad m, HasUtxo a) => a -> ExceptT InvalidInputs m Utxo
utxo a = updateUtxo a Map.empty

instance HasUtxo Tx where
  txIns     (Tx ins _outs) = ins
  txOuts tx@(Tx _ins outs) =
      Map.fromList $ map aux (zip [0..] outs)
    where
      aux :: (Int, TxOut) -> (TxIn, TxOut)
      aux (ix, out) = ((hash tx, ix), out)

  confirmed       = Set.singleton . hash
  updateUtxo tx u =
      let notInUtxo = txIns tx Set.\\ (Map.keysSet u)
      in case Set.null notInUtxo of
           True  -> return $ (u `Map.union` txOuts tx) `Map.withoutKeys` txIns tx
           False -> throwError $ InvalidInputs notInUtxo

instance HasUtxo a => HasUtxo [a] where
  txIns      = foldr (Set.union . txIns)     Set.empty
  txOuts     = foldr (Map.union . txOuts)    Map.empty
  confirmed  = foldr (Set.union . confirmed) Set.empty
  updateUtxo = repeatedlyM updateUtxo

instance HasUtxo tx => HasUtxo (Map (Hash h tx) tx) where
  txIns           = txIns     . Map.elems
  txOuts          = txOuts    . Map.elems
  confirmed       = confirmed . Map.elems
  updateUtxo as u =
      let notInUtxo = txIns as Set.\\ (Map.keysSet u)
      in case Set.null notInUtxo of
           True  -> return $ (u `Map.union` txOuts as) `Map.withoutKeys` txIns as
           False -> throwError $ InvalidInputs notInUtxo

instance HasUtxo SimpleBody where
  txIns      = txIns      . getSimpleBody
  txOuts     = txOuts     . getSimpleBody
  updateUtxo = updateUtxo . getSimpleBody
  confirmed  = confirmed  . getSimpleBody

instance HasUtxo (SimpleBlock p c) where
  txIns      = txIns      . simpleBody
  txOuts     = txOuts     . simpleBody
  updateUtxo = updateUtxo . simpleBody
  confirmed  = confirmed  . simpleBody

instance HasUtxo a => HasUtxo (Chain a) where
  txIns      = txIns      . toOldestFirst
  txOuts     = txOuts     . toOldestFirst
  updateUtxo = updateUtxo . toOldestFirst
  confirmed  = confirmed  . toOldestFirst

instance All HasUtxo as => HasUtxo (HList as) where
  txIns      = HList.foldr (Proxy @HasUtxo) (Set.union . txIns)     Set.empty
  txOuts     = HList.foldr (Proxy @HasUtxo) (Map.union . txOuts)    Map.empty
  confirmed  = HList.foldr (Proxy @HasUtxo) (Set.union . confirmed) Set.empty
  updateUtxo = HList.repeatedlyM (Proxy @HasUtxo) updateUtxo

{-------------------------------------------------------------------------------
  Crypto needed for simple blocks

-------------------------------------------------------------------------------}

class (HashAlgorithm (SimpleBlockHash c), Typeable c) => SimpleBlockCrypto c where
  type family SimpleBlockHash c :: *

data SimpleBlockStandardCrypto

instance SimpleBlockCrypto SimpleBlockStandardCrypto where
  type SimpleBlockHash SimpleBlockStandardCrypto = MD5

-- A mock crypto using the 'ShortHash' variant.
data SimpleBlockMockCrypto

instance SimpleBlockCrypto SimpleBlockMockCrypto where
  type SimpleBlockHash SimpleBlockMockCrypto = ShortHash

{-------------------------------------------------------------------------------
  Simple blocks

  NOTE: Many of these instances can be made a lot cleaner once we have
  QuantifiedConstraints available (ghc 8.6).
-------------------------------------------------------------------------------}

data SimpleHeader p c = SimpleHeader {
      headerPreHeader :: SimplePreHeader p c
    , headerOuroboros :: Payload p (SimplePreHeader p c)
    , headerHash      :: Hash (SimpleBlockHash c) (SimpleHeader p c)
    }
  deriving (Generic)

deriving instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Show (Payload p (SimplePreHeader p c))) => Show (SimpleHeader p c)
deriving instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Eq   (Payload p (SimplePreHeader p c))) => Eq   (SimpleHeader p c)
deriving instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Ord  (Payload p (SimplePreHeader p c))) => Ord  (SimpleHeader p c)

mkSimpleHeader
  :: (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c)))
  => SimplePreHeader p c
  -> Payload p (SimplePreHeader p c)
  -> SimpleHeader p c
mkSimpleHeader preHeader payload =
    headerWoHash { headerHash = hash headerWoHash }
  where
    headerWoHash = SimpleHeader
      { headerPreHeader = preHeader
      , headerOuroboros = payload
      , headerHash      = error "Hash used before it was computed"
      }

-- | The preheader is the header without the ouroboros protocol specific payload
--
-- This is necessary to be able specify what the signature is over precisely
-- (to wit, the pre header plus some ouroboros specific stuff but, crucially,
-- without the signature itself).
data SimplePreHeader p c = SimplePreHeader {
      headerPrev      :: ChainHash (SimpleHeader p c)
    , headerSlot      :: SlotNo
    , headerBlockNo   :: BlockNo
    , headerBodyHash  :: Hash (SimpleBlockHash c) SimpleBody
    , headerBlockSize :: Word
    }
  deriving (Generic, Show, Eq, Ord)

instance (Typeable p, SimpleBlockCrypto c) => Condense (SimplePreHeader p c) where
    condense = show

data SimpleBody = SimpleBody { getSimpleBody :: [Tx] }
  deriving (Generic, Show, Eq, Ord)

data SimpleBlock p c = SimpleBlock {
      simpleHeader :: SimpleHeader p c
    , simpleBody   :: SimpleBody
    }
  deriving (Generic)

deriving instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Show (Payload p (SimplePreHeader p c))) => Show (SimpleBlock p c)
deriving instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Eq (Payload p (SimplePreHeader p c))) => Eq (SimpleBlock p c)
deriving instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Ord (Payload p (SimplePreHeader p c))) => Ord (SimpleBlock p c)

instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Condense (Payload p (SimplePreHeader p c)), Serialise (Payload p (SimplePreHeader p c))) => Condense (SimpleHeader p c) where
  condense hdr@(SimpleHeader _ pl bh) = mconcat [
        "("
      , condensedHash (blockPrevHash hdr)
      , "->"
      , condense bh
      , ","
      , condense pl
      , ","
      , condense (unSlotNo $ blockSlot hdr)
      , ")"
      ]

instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Condense (Payload p (SimplePreHeader p c)), Serialise (Payload p (SimplePreHeader p c))) => Condense (SimpleBlock p c) where
  condense (SimpleBlock hdr@(SimpleHeader _ pl bh) (SimpleBody txs)) = mconcat [
        "("
      , condensedHash (blockPrevHash hdr)
      , "->"
      , condense bh
      , ","
      , condense pl
      , ","
      , condense (unSlotNo $ blockSlot hdr)
      , ","
      , condense txs
      , ")"
      ]

instance Condense (ChainHash (SimpleHeader p c)) where
  condense = condensedHash

condensedHash :: Show (HeaderHash b) => ChainHash b -> String
condensedHash GenesisHash     = "genesis"
condensedHash (BlockHash hdr) = show hdr

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => Measured BlockMeasure (SimpleHeader p c) where
  measure = blockMeasure

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => Measured BlockMeasure (SimpleBlock p c) where
  measure = blockMeasure


instance (Typeable p, SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => HasHeader (SimpleHeader p c) where
  type HeaderHash (SimpleHeader p c) = Hash (SimpleBlockHash c) (SimpleHeader p c)

  blockHash      = headerHash
  blockPrevHash  = headerPrev    . headerPreHeader
  blockSlot      = headerSlot    . headerPreHeader
  blockNo        = headerBlockNo . headerPreHeader

  blockInvariant _ = True

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => HasHeader (SimpleBlock p c) where
  type HeaderHash (SimpleBlock p c) = Hash (SimpleBlockHash c) (SimpleHeader p c)

  blockHash      = blockHash . simpleHeader
  blockSlot      = blockSlot . simpleHeader
  blockNo        = blockNo   . simpleHeader
  blockPrevHash  = castHash . blockPrevHash . simpleHeader

  blockInvariant SimpleBlock{..} =
       blockInvariant simpleHeader
    && hash simpleBody == headerBodyHash (headerPreHeader simpleHeader)

instance (Typeable p, SimpleBlockCrypto c) => StandardHash (SimpleHeader p c)
instance (Typeable p, SimpleBlockCrypto c) => StandardHash (SimpleBlock  p c)

{-------------------------------------------------------------------------------
  Creating blocks
-------------------------------------------------------------------------------}

forgeSimpleBlock :: forall m p c.
                    ( HasNodeState p m
                    , MonadRandom m
                    , OuroborosTag p
                    , SimpleBlockCrypto c
                    , Serialise (Payload p (SimplePreHeader p c))
                      -- TODO Decide whether we want to fix this constraint here.
                    , SupportedPreHeader p ~ Empty
                    )
                 => NodeConfig p
                 -> SlotNo                          -- ^ Current slot
                 -> BlockNo                         -- ^ Current block number
                 -> ChainHash (SimpleHeader p c)    -- ^ Previous hash
                 -> [Tx]                            -- ^ Txs to add in the block
                 -> IsLeader p
                 -> m (SimpleBlock p c)
forgeSimpleBlock cfg curSlot curNo prevHash txs proof = do
    ouroborosPayload <- mkPayload encode cfg proof preHeader
    return $ SimpleBlock {
        simpleHeader = mkSimpleHeader preHeader ouroborosPayload
      , simpleBody   = body
      }
  where
    body :: SimpleBody
    body = SimpleBody txs

    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    bodySize :: Word
    bodySize = fromIntegral $ BL.length $ serialise body

    preHeader :: SimplePreHeader p c
    preHeader = SimplePreHeader {
          headerPrev      = prevHash
        , headerSlot      = curSlot
        , headerBlockNo   = curNo
        , headerBodyHash  = hash body
        , headerBlockSize = bodySize
        }

-- | Check whether the block matches the header
blockMatchesHeader :: SimpleBlockCrypto c => SimpleHeader p c -> SimpleBlock p c -> Bool
blockMatchesHeader SimpleHeader { headerPreHeader } SimpleBlock { simpleBody } =
    headerBodyHash headerPreHeader == hash simpleBody

{-------------------------------------------------------------------------------
  Updating the Ledger
-------------------------------------------------------------------------------}

type instance BlockProtocol (SimpleBlock p c) = p

type instance BlockProtocol (SimpleHeader p c) = p

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c)))
      => HasPreHeader (SimpleHeader p c) where
  type PreHeader (SimpleHeader p c) = SimplePreHeader p c

  blockPreHeader = headerPreHeader

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c)))
      => HasPreHeader (SimpleBlock p c) where
  type PreHeader (SimpleBlock p c) = SimplePreHeader p c

  blockPreHeader = headerPreHeader . simpleHeader

instance ( SimpleBlockCrypto c
         , OuroborosTag p
         , Serialise (Payload p (SimplePreHeader p c))
         )
      => HasPayload p (SimpleHeader p c) where
  blockPayload _ = headerOuroboros

instance ( SimpleBlockCrypto c
         , OuroborosTag p
         , Serialise (Payload p (SimplePreHeader p c))
         )
      => HasPayload p (SimpleBlock p c) where
  blockPayload _ = headerOuroboros . simpleHeader

-- TODO: This instance is ugly.. can we avoid it?
instance ( OuroborosTag p
         , SimpleBlockCrypto c
         , Serialise (Payload p (SimplePreHeader (ExtNodeConfig cfg p) c))
         , Typeable cfg
         )
      => HasPayload p (SimpleHeader (ExtNodeConfig cfg p) c) where
  blockPayload _ = encPayloadP . headerOuroboros

-- TODO: This instance is ugly.. can we avoid it?
instance ( OuroborosTag p
         , SimpleBlockCrypto c
         , Serialise (Payload p (SimplePreHeader (ExtNodeConfig cfg p) c))
         , Typeable cfg
         )
      => HasPayload p (SimpleBlock (ExtNodeConfig cfg p) c) where
  blockPayload _ = encPayloadP . headerOuroboros . simpleHeader

instance OuroborosTag p => UpdateLedger (SimpleBlock p c) where
  data LedgerState (SimpleBlock p c) =
      SimpleLedgerState {
          slsUtxo      :: Utxo
        , slsConfirmed :: Set (Hash ShortHash Tx)
        }

  data LedgerError (SimpleBlock p c) = LedgerErrorInvalidInputs InvalidInputs
    deriving (Show)
  data LedgerConfig (SimpleBlock p c) = MockLedgerConfig

  applyLedgerHeader _ _ = pure
  applyLedgerBlock  _   = updateSimpleLedgerState

deriving instance OuroborosTag p => Show (LedgerState (SimpleBlock p c))

updateSimpleLedgerState :: (Monad m, HasUtxo a)
                        => a
                        -> LedgerState (SimpleBlock p c)
                        -> ExceptT (LedgerError (SimpleBlock p c))
                                   m
                                   (LedgerState (SimpleBlock p c))
updateSimpleLedgerState b (SimpleLedgerState u c) = do
    u' <- withExceptT LedgerErrorInvalidInputs $ updateUtxo b u
    return $ SimpleLedgerState u' (c `Set.union` confirmed b)

instance OuroborosTag p => LedgerConfigView (SimpleBlock p c) where
  ledgerConfigView = const MockLedgerConfig

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

instance OuroborosTag p => ApplyTx (SimpleBlock p c) where
  type GenTx      (SimpleBlock p c) = Tx
  type ApplyTxErr (SimpleBlock p c) = LedgerError (SimpleBlock p c)

  applyTx            = \_ -> updateSimpleLedgerState
  reapplyTx          = \_ -> updateSimpleLedgerState
  reapplyTxSameState = \_ -> (mustSucceed . runExcept) .: updateSimpleLedgerState
    where
      mustSucceed (Left  _)  = error "reapplyTxSameState: unexpected error"
      mustSucceed (Right st) = st

{-------------------------------------------------------------------------------
  Support for various consensus algorithms
-------------------------------------------------------------------------------}

-- | Mapping from addresses to node IDs
--
-- This is needed in order to assign stake to nodes.
type AddrDist = Map Addr NodeId

instance (BftCrypto c, SimpleBlockCrypto c')
      => ProtocolLedgerView (SimpleBlock (Bft c) c') where
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Just $ slotUnbounded ()

-- | Mock ledger is capable of running PBFT, but we simply assume the delegation
-- map and the protocol parameters can be found statically in the node
-- configuration.
instance (SimpleBlockCrypto c')
  => ProtocolLedgerView (SimpleBlock (ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto)) c') where
  protocolLedgerView (EncNodeConfig _ pbftParams) _ls = pbftParams
  -- This instance is correct, because the delegation map doesn't change in the
  -- node configuration.
  anachronisticProtocolLedgerView (EncNodeConfig _ pbftParams) _ _
    = Just $ slotUnbounded pbftParams

-- | Praos needs a ledger that can give it the "active stake distribution"
--
-- TODO: Currently our mock ledger does not do this, and just assumes that all
-- the leaders have equal stake at all times. In a way this is not wrong: it
-- is just a different instantiation of the same consensus algorithm (see
-- documentation of 'LedgerView'). Ideally we'd change this however, but it
-- may not be worth it; it would be a bit of work, and after we have integrated
-- the Shelley rules, we'll have a proper instance anyway.
instance ( PraosCrypto c, SimpleBlockCrypto c')
      => ProtocolLedgerView (SimpleBlock (ExtNodeConfig AddrDist (Praos c)) c') where
  protocolLedgerView (EncNodeConfig _ addrDist) _ =
      equalStakeDistr addrDist

  anachronisticProtocolLedgerView (EncNodeConfig _ addrDist) _ _ =
      Just $ slotUnbounded $ equalStakeDistr addrDist

nodeStake :: NodeId -> Maybe (Int, Rational)
nodeStake (RelayId _) = Nothing
nodeStake (CoreId i)  = Just (i, 1)

equalStakeDistr :: AddrDist -> StakeDist
equalStakeDistr = IntMap.fromList
                . mapMaybe (nodeStake . snd)
                . Map.toList

instance (PraosCrypto c, SimpleBlockCrypto c')
      => ProtocolLedgerView (SimpleBlock (WithLeaderSchedule (Praos c)) c') where
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Just $ slotUnbounded ()
{-------------------------------------------------------------------------------
  Compute relative stake
-------------------------------------------------------------------------------}

data StakeHolder =
    -- | Stake of a core node
    StakeCore Int

    -- | Stake for everybody else (we don't need to distinguish)
  | StakeEverybodyElse
  deriving (Show, Eq, Ord)

relativeStakes :: Map StakeHolder Int -> StakeDist
relativeStakes m =
   let totalStake    = fromIntegral $ sum $ Map.elems m
   in  IntMap.fromList [ (nid, fromIntegral stake / totalStake)
                       | (StakeCore nid, stake) <- Map.toList m
                       ]

-- | Compute stakes of all nodes
--
-- The 'Nothing' value holds the total stake of all addresses that don't
-- get mapped to a NodeId.
totalStakes :: Map Addr NodeId -> Utxo -> Map StakeHolder Int
totalStakes addrDist = foldl f Map.empty
 where
   f :: Map StakeHolder Int -> TxOut -> Map StakeHolder Int
   f m (a, stake) = case Map.lookup a addrDist of
       Just (CoreId nid) -> Map.insertWith (+) (StakeCore nid)    stake m
       _                 -> Map.insertWith (+) StakeEverybodyElse stake m

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Tx
instance Serialise SimpleBody

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => Serialise (SimplePreHeader p c)
instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => Serialise (SimpleBlock     p c)

instance (SimpleBlockCrypto c, OuroborosTag p, Serialise (Payload p (SimplePreHeader p c))) => Serialise (SimpleHeader    p c) where
  encode SimpleHeader { headerPreHeader = preHeader, headerOuroboros = payload} =
    -- Don't serialise the hash now, because we compute it by hashing the
    -- serialisation of the header.
    encodeListLen 2 <> encode preHeader <> encode payload
  decode = do
    decodeListLenOf 2
    preHeader <- decode
    payload   <- decode
    return $ mkSimpleHeader preHeader payload

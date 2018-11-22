{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
  , forgeBlock
    -- * Updating the Ledger state
  , LedgerState(..)
  ) where

import           Codec.Serialise
import           Crypto.Random (MonadRandom)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Crypto.Hash.Class
import           Ouroboros.Consensus.Crypto.Hash.MD5 (MD5)
import           Ouroboros.Consensus.Crypto.Hash.Short (ShortHash)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.HList (All, HList)
import qualified Ouroboros.Consensus.Util.HList as HList
import           Ouroboros.Network.Block hiding (Hash)
import qualified Ouroboros.Network.Block as Network
import           Ouroboros.Network.Chain (Chain, toOldestFirst)

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

class HasUtxo a where
  txIns      :: a -> Set TxIn
  txOuts     :: a -> Utxo
  confirmed  :: a -> Set Tx
  updateUtxo :: a -> Utxo -> Utxo

utxo :: HasUtxo a => a -> Utxo
utxo a = updateUtxo a Map.empty

instance HasUtxo Tx where
  txIns     (Tx ins _outs) = ins
  txOuts tx@(Tx _ins outs) =
      Map.fromList $ map aux (zip [0..] outs)
    where
      aux :: (Int, TxOut) -> (TxIn, TxOut)
      aux (ix, out) = ((hash tx, ix), out)

  confirmed       = Set.singleton
  updateUtxo tx u = u `Map.union` txOuts tx

instance HasUtxo a => HasUtxo (Set a) where
  txIns           = txIns     . Set.toList
  txOuts          = txOuts    . Set.toList
  confirmed       = confirmed . Set.toList
  updateUtxo as u = (u `Map.union` txOuts as) `withoutKeys` txIns as

instance HasUtxo a => HasUtxo [a] where
  txIns      = foldr (Set.union . txIns)     Set.empty
  txOuts     = foldr (Map.union . txOuts)    Map.empty
  confirmed  = foldr (Set.union . confirmed) Set.empty
  updateUtxo = repeatedly updateUtxo

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
  updateUtxo = HList.repeatedly (Proxy @HasUtxo) updateUtxo

{-------------------------------------------------------------------------------
  Crypto needed for simple blocks

-------------------------------------------------------------------------------}

class HashAlgorithm (SimpleBlockHash c) => SimpleBlockCrypto c where
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
    }
  deriving (Generic)

deriving instance (SimpleBlockCrypto c, OuroborosTag p) => Show (SimpleHeader p c)
deriving instance (SimpleBlockCrypto c, OuroborosTag p) => Eq   (SimpleHeader p c)
deriving instance (SimpleBlockCrypto c, OuroborosTag p) => Ord  (SimpleHeader p c)

-- | The preheader is the header without the ouroboros protocol specific payload
--
-- This is necessary to be able specify what the signature is over precisely
-- (to wit, the pre header plus some ouroboros specific stuff but, crucially,
-- without the signature itself).
data SimplePreHeader p c = SimplePreHeader {
      headerPrev     :: Network.Hash (SimpleHeader p c)
    , headerSlot     :: Slot
    , headerBlockNo  :: BlockNo
    , headerBodyHash :: Hash (SimpleBlockHash c) SimpleBody
    }
  deriving (Generic, Show, Eq, Ord)

instance SimpleBlockCrypto c => Condense (SimplePreHeader p c) where
    condense = show

data SimpleBody = SimpleBody { getSimpleBody :: Set Tx }
  deriving (Generic, Show, Eq, Ord)

data SimpleBlock p c = SimpleBlock {
      simpleHeader :: SimpleHeader p c
    , simpleBody   :: SimpleBody
    }
  deriving (Generic, Show, Eq, Ord)

instance (SimpleBlockCrypto c, OuroborosTag p) => Condense (SimpleBlock p c) where
  condense (SimpleBlock hdr@(SimpleHeader _ pl) (SimpleBody txs)) =
      condensedHash (blockPrevHash hdr)
          <> "-"
          <> condense (blockHash hdr)
          <> ",("
          <> condense pl
          <> ",("
          <> condense (getSlot $ blockSlot hdr)
          <> ","
          <> condense txs
          <> "))])))"

condensedHash :: Show (HeaderHash b) => Network.Hash b -> String
condensedHash GenesisHash     = "genesis"
condensedHash (BlockHash hdr) = show hdr

instance (SimpleBlockCrypto c, OuroborosTag p) => HasHeader (SimpleHeader p c) where
  type HeaderHash (SimpleHeader p c) = Hash (SimpleBlockHash c) (SimpleHeader p c)

  blockHash      = hash
  blockPrevHash  = headerPrev    . headerPreHeader
  blockSlot      = headerSlot    . headerPreHeader
  blockNo        = headerBlockNo . headerPreHeader

  blockInvariant _ = True

instance (SimpleBlockCrypto c, OuroborosTag p) => HasHeader (SimpleBlock p c) where
  type HeaderHash (SimpleBlock p c) = Hash (SimpleBlockHash c) (SimpleHeader p c)

  blockHash      = blockHash . simpleHeader
  blockSlot      = blockSlot . simpleHeader
  blockNo        = blockNo   . simpleHeader
  blockPrevHash  = Network.castHash . blockPrevHash . simpleHeader

  blockInvariant SimpleBlock{..} =
       blockInvariant simpleHeader
    && hash simpleBody == headerBodyHash (headerPreHeader simpleHeader)

instance SimpleBlockCrypto c => StandardHash (SimpleHeader p c)
instance SimpleBlockCrypto c => StandardHash (SimpleBlock  p c)

{-------------------------------------------------------------------------------
  Creating blocks
-------------------------------------------------------------------------------}

forgeBlock :: forall m p c.
              ( HasNodeState p m
              , MonadRandom m
              , OuroborosTag p
              , SimpleBlockCrypto c
              )
           => NodeConfig p
           -> Slot                            -- ^ Current slot
           -> BlockNo                         -- ^ Current block number
           -> Network.Hash (SimpleHeader p c) -- ^ Previous hash
           -> Set Tx                          -- ^ Txs to add in the block
           -> IsLeader p
           -> m (SimpleBlock p c)
forgeBlock cfg curSlot curNo prevHash txs proof = do
    ouroborosPayload <- mkPayload cfg proof preHeader
    return $ SimpleBlock {
        simpleHeader = SimpleHeader preHeader ouroborosPayload
      , simpleBody   = body
      }
  where
    body :: SimpleBody
    body = SimpleBody txs

    preHeader :: SimplePreHeader p c
    preHeader = SimplePreHeader {
          headerPrev     = prevHash
        , headerSlot     = curSlot
        , headerBlockNo  = curNo
        , headerBodyHash = hash body
        }

{-------------------------------------------------------------------------------
  Updating the Ledger
-------------------------------------------------------------------------------}

type instance BlockProtocol (SimpleBlock p c) = p

instance (SimpleBlockCrypto c, OuroborosTag p)
      => HasPreHeader (SimpleBlock p c) where
  type PreHeader (SimpleBlock p c) = SimplePreHeader p c

  blockPreHeader = headerPreHeader . simpleHeader

instance (SimpleBlockCrypto c, OuroborosTag p)
      => HasPayload p (SimpleBlock p c) where
  blockPayload _ = headerOuroboros . simpleHeader

instance OuroborosTag p => UpdateLedger (SimpleBlock p c) where
  data LedgerState (SimpleBlock p c) = SimpleLedgerState Utxo

  -- TODO: Modify UTxO model to return errors
  data LedgerError (SimpleBlock p c) -- no constructors for now
    deriving (Show)

  -- Apply a block to the ledger state
  applyLedgerState b (SimpleLedgerState u) = do
      -- TODO: Updating the UTxO should throw an error also rather than
      -- silently removing transactions that do double spending
      let u' = updateUtxo b u
      return $ SimpleLedgerState u'

deriving instance OuroborosTag p => Show (LedgerState (SimpleBlock p c))

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Tx
instance Serialise SimpleBody

instance (SimpleBlockCrypto c, OuroborosTag p) => Serialise (SimpleHeader    p c)
instance (SimpleBlockCrypto c, OuroborosTag p) => Serialise (SimplePreHeader p c)
instance (SimpleBlockCrypto c, OuroborosTag p) => Serialise (SimpleBlock     p c)

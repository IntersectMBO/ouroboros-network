{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Crypto.Hash.Class
import           Ouroboros.Consensus.Crypto.Hash.MD5 (MD5)
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

  TODO: We should make the hash configurable.
-------------------------------------------------------------------------------}

data Tx = Tx (Set TxIn) [TxOut]
  deriving (Show, Eq, Ord, Generic)

instance Condense Tx where
  condense (Tx ins outs) = condense (ins, outs)

type TxIn  = (Hash MD5 Tx, Int)
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

  TODO: We may want to introduce a "short hash" variation to use in testing.
-------------------------------------------------------------------------------}

class HashAlgorithm (SimpleBlockHash c) => SimpleBlockCrypto c where
  type family SimpleBlockHash c :: *

data SimpleBlockStandardCrypto

instance SimpleBlockCrypto SimpleBlockStandardCrypto where
  type SimpleBlockHash SimpleBlockStandardCrypto = MD5

{-------------------------------------------------------------------------------
  Simple blocks

  NOTE: Many of these instances can be made a lot cleaner once we have
  QuantifiedConstraints available (ghc 8.6).
-------------------------------------------------------------------------------}

data SimpleHeader p c = SimpleHeader {
      headerPreHeader :: SimplePreHeader p c
    , headerOuroboros :: OuroborosPayload p (SimplePreHeader p c)
    }
  deriving (Generic)

deriving instance ( SimpleBlockCrypto c
                  , Show (OuroborosPayload p (SimplePreHeader p c))
                  ) => Show (SimpleHeader p c)
deriving instance ( SimpleBlockCrypto c
                  , Eq (OuroborosPayload p (SimplePreHeader p c))
                  ) => Eq (SimpleHeader p c)

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
  deriving (Generic, Show, Eq)

data SimpleBody = SimpleBody { getSimpleBody :: Set Tx }
  deriving (Generic, Show, Eq)

data SimpleBlock p c = SimpleBlock {
      simpleHeader :: SimpleHeader p c
    , simpleBody   :: SimpleBody
    } deriving (Generic)

deriving instance ( SimpleBlockCrypto c
                  , Show (OuroborosPayload p (SimplePreHeader p c))
                  ) => Show (SimpleBlock p c)
deriving instance ( SimpleBlockCrypto c
                  , Eq (OuroborosPayload p (SimplePreHeader p c))
                  ) => Eq (SimpleBlock p c)

-- TODO: Write a proper condensed instance.
instance ( SimpleBlockCrypto c
         , Show (OuroborosPayload p (SimplePreHeader p c))
         )
        => Condense (SimpleBlock p c) where
  condense b = show b

instance SimpleBlockCrypto c => StandardHash (SimpleHeader p c)
instance SimpleBlockCrypto c => StandardHash (SimpleBlock  p c)

instance ( SimpleBlockCrypto c
         , Serialise (OuroborosPayload p (SimplePreHeader p c))
         ) => HasHeader (SimpleHeader p c) where
  type HeaderHash (SimpleHeader p c) = Hash (SimpleBlockHash c) (SimpleHeader p c)

  blockHash      = hash
  blockPrevHash  = headerPrev    . headerPreHeader
  blockSlot      = headerSlot    . headerPreHeader
  blockNo        = headerBlockNo . headerPreHeader

  blockInvariant _ = True

instance ( SimpleBlockCrypto c
         , Serialise (OuroborosPayload p (SimplePreHeader p c))
         ) => HasHeader (SimpleBlock p c) where
  type HeaderHash (SimpleBlock p c) = Hash (SimpleBlockHash c) (SimpleHeader p c)

  blockHash      = blockHash . simpleHeader
  blockSlot      = blockSlot . simpleHeader
  blockNo        = blockNo   . simpleHeader
  blockPrevHash  = Network.castHash . blockPrevHash . simpleHeader

  blockInvariant SimpleBlock{..} =
       blockInvariant simpleHeader
    && hash simpleBody == headerBodyHash (headerPreHeader simpleHeader)

{-------------------------------------------------------------------------------
  Creating blocks
-------------------------------------------------------------------------------}

forgeBlock :: forall m p c.
              ( MonadOuroborosState p m
              , MonadRandom m
              , OuroborosTag p
              , SimpleBlockCrypto c
              , Serialise (OuroborosPayload p (SimplePreHeader p c))
              )
           => Slot                            -- ^ Current slot
           -> BlockNo                         -- ^ Current block number
           -> Network.Hash (SimpleHeader p c) -- ^ Previous hash
           -> Set Tx                          -- ^ Txs to add in the block
           -> ProofIsLeader p
           -> m (SimpleBlock p c)
forgeBlock curSlot curNo prevHash txs proof = do
    ouroborosPayload <- mkOuroborosPayload proof preHeader
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

instance OuroborosTag p => UpdateLedger (SimpleBlock p c) where
  data LedgerState (SimpleBlock p c) = SimpleLedgerState {
        simpleUtxo :: Utxo
      , simpleProtocolState :: OuroborosLedgerState p
      }

  -- Apply a block to the ledger state
  --
  -- TODO: We need to support rollback, so this probably won't be a pure
  -- function but rather something that lives in a monad with some actions
  -- that we can compute a "running diff" so that we can go back in time.
  applyLedgerState (SimpleBlock hdr (SimpleBody txs)) sls = SimpleLedgerState {
        simpleUtxo = updateUtxo txs (simpleUtxo sls)
      , simpleProtocolState = applyOuroborosLedgerState (headerOuroboros hdr)
                                                        (simpleProtocolState sls)
      }

deriving instance OuroborosTag p => Show (LedgerState (SimpleBlock p c))

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Tx

instance Serialise SimpleBody

instance ( SimpleBlockCrypto c
         , Serialise (OuroborosPayload p (SimplePreHeader p c))
         ) => Serialise (SimpleHeader p c)
instance ( SimpleBlockCrypto c
         , Serialise (OuroborosPayload p (SimplePreHeader p c))
         ) => Serialise (SimplePreHeader p c)
instance ( SimpleBlockCrypto c
         , Serialise (OuroborosPayload p (SimplePreHeader p c))
         ) => Serialise (SimpleBlock p c)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UTxOAPI where

import           Prelude hiding (lookup)

import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)



-- Diffs
--------

class Monoid (Diff a) => ApplyDiff a where
  type Diff a

  applyDiff :: a -> Diff a -> a

  -- Laws:
  --  x ◁ 0 = x
  -- (x ◁ d) ◁ d' =  x ◁ (d <> d')

-- ⊙ ⊕ ∣ |

(◁) :: ApplyDiff a => a -> Diff a -> a
(◁) = applyDiff

prop_applyDiff :: (ApplyDiff a, Eq a) => a -> Diff a -> Diff a -> Bool
prop_applyDiff x d d' =
    (x ◁ d) ◁ d' == x ◁ (d <> d')


instance (Ord k, Monoid a) => ApplyDiff (Map k a) where
    type Diff (Map k a) = Map k (Update a)

    applyDiff :: Map k a -> Map k (Update a) -> Map k a
    applyDiff =
        Map.merge
          Map.preserveMissing
          (Map.mapMaybeMissing      (\_ -> insertUpdate))
          (Map.zipWithMaybeMatched  (\_ -> applyUpdate))
      where
        insertUpdate :: Update a -> Maybe a
        insertUpdate  Delete    = Nothing
        insertUpdate (Insert x) = Just x
        insertUpdate (Update x) = Just x

        applyUpdate :: a -> Update a -> Maybe a
        applyUpdate _  Delete    = Nothing
        applyUpdate _ (Insert y) = Just y
        applyUpdate x (Update y) = Just (x<>y)

data Update a = Delete | Insert !a | Update !a

instance Semigroup a => Semigroup (Update a) where
  Delete   <> Delete   = Delete
  Insert _ <> Insert y = Insert y
  Update x <> Update y = Update (x <> y)

  Insert _ <> Delete   = Delete
  Delete   <> Insert y = Insert y

  Update _ <> Delete   = Delete
  Delete   <> Update y = Insert y

  Insert x <> Update y = Insert (x <> y)
  Update _ <> Insert y = Insert y



-- A partial tracking map is a partial mapping that tracks updates.
--
-- It is partial in the sense that it only contains key value pairs for the
-- keys that will actually be used in a batch of operations. Arranging for this
-- to be true requires knowing the set of keys in advance.
--
-- It tracks updates in the sense that it supports an operation that returns
-- the difference between an original map and the final map after a series of
-- operations.
--
-- Using a partial tracking map allows us to work with a mapping that is
-- primarily kept on-disk, but to write all the operations using only pure
-- functions operating on in-memory data structures. The main trick is that
-- if we know in advance the set of keys we will (or might) need, then we can
-- read them from disk in advance. The second trick is that we can track the
-- updates made to the map and write the differences out to disk at the end.
-- The partial tracking map is the data structure we use to do this.

data PTMap k a = PTMap !(Map k (Maybe a)) !(Map k (Update a))

-- Another trick will be to parametrise code over the map type, to enable
-- us to swap out a normal Data.Map for a PTMap.

class Mapping map where

  lookup :: Ord k => k      -> map k a -> Maybe a
  insert :: Ord k => k -> a -> map k a -> map k a
  update :: Semigroup a
         => Ord k => k -> a -> map k a -> map k a
  delete :: Ord k => k      -> map k a -> map k a


instance Mapping PTMap where
  lookup k (PTMap m _d) =
    case Map.lookup k m of
      Nothing -> error "PTMap.lookup: used a key not fetched from disk"
      Just v  -> v

  insert k v (PTMap m d) =
    PTMap (Map.insert k (Just v) m)
          (Map.insert k (Insert v) d)

  delete k (PTMap m d) =
    PTMap (Map.insert k Nothing m)
          (Map.insert k Delete d)

  update k v (PTMap m d) =
    PTMap (Map.insertWith (<>) k (Just v) m)
          (Map.insertWith (<>) k (Update v) d)


-- Vertical composition
-----------------------
--
-- We don't just write operations over a single mapping. In general we have a
-- bunch of in-memory state and a bundle of on-disk mappings. So we need good
-- ways to manage the composition of program state with many such mappings,
-- and the code that works with that state.
--
-- We define a class of state types that contain mappings. The operations allow
-- us to inject or project just the mappings from the larger state type. We use
-- an associated type that contains just the mappings.
--
-- These operations correspond to what we need to do when we bring in a partial
-- mapping from disk before a series of operations, and when we collect the
-- updates at the end to write out to disk again.

class HasOnDiskMappings state where

  data OnDiskMappings state :: (* -> * -> *) -> *

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  injectOnDiskMappings  :: OnDiskMappings state map -> state any -> state map



-- Horizontal composition
-------------------------
--
-- Horizontal composition refers to the ability to string together multiple
-- operations together.
--
-- With our formulation using tracking maps, this is trivial: it is just
-- composition of functions that use a state containing tracking maps.
--
-- Only at the very beginning, or very end of an atomic batch of operations we
-- have to read the values from disk, and write changes back.


-- ACID or not
--------------
--
-- We intend to provide A, C and I properties, but not D. That is
-- Atomic, Consistent, Isolated but not Durable.


-- Disk operations
------------------

{-
newtype Keys k a = Keys (Set k)

class DB state dbhandle where
  prepare :: dbhandle
          -> OnDiskMappings state Keys
          -> IO (OnDiskMappings state PTMap)

  -- ...


class DBTable tblhandle
-}





-- Pure model
-------------

data DbModel k a = DbModel (Map k a)         -- on-disk
                           (Diff (Map k a))  -- in memory

-- | The data abstraction function: the abstract value that this represents.
--
abs :: (Ord k, Monoid a) => DbModel k a -> Map k a
abs (DbModel d m) = d ◁ m

-- | A flush operation. Abstractly this is an identity operation, but in the
-- representation it moves the in-memory values back to the disk.
--
flush :: (Ord k, Monoid a) => DbModel k a -> DbModel k a
flush (DbModel d m) = DbModel (d ◁ m) mempty

prepare :: DbModel k a -> Set k -> PTMap k a
prepare (DbModel d m)


-- Example
------------

-- | An example of a top level state consisting of a few parts. This
-- demonstrates a top level state type with multiple mappings, and other
-- in-memory state that are not mappings, and is not kept on disk.
--
data LedgerState map =
     LedgerState {

       -- | An example of some nested state
       utxos   :: UTxOState map,

       -- | Something standing in for other state, like protocol parameters.
       pparams :: PParams
     }

-- | The content of this doesn't actually matter. It's just a place-holder.
--
data PParams = PParams

-- | An example sub-part of the state, to demonstrate \"vertical\" composition:
-- that the state and operations can work over components of the overall state.
--
-- In particular this one demonstrates two mappings. The second of the two
-- mappings can benefit from monoidal updates, since it incrementally maintains
-- an aggregation of the first mapping.
--
data UTxOState map =
     UTxOState {
       -- | A simple UTxO structure.
       utxo    :: map TxIn TxOut,

       -- | An aggregation of the UTxO's coins by address.
       utxoagg :: map Addr Coin
     }

data    TxIn  = TxIn !TxId !TxIx   deriving (Eq, Ord, Show)
newtype TxId  = TxId Int           deriving (Eq, Ord, Show)
newtype TxIx  = TxIx Int           deriving (Eq, Ord, Show)
data    TxOut = TxOut !Addr !Coin  deriving (Eq, Show)
newtype Addr  = Addr Int           deriving (Eq, Ord, Show)
newtype Coin  = Coin Int           deriving (Eq, Num, Show)

instance Semigroup Coin where a <> b = a + b

instance HasOnDiskMappings LedgerState where

  data OnDiskMappings LedgerState map =
         LedgerStateMappings {
           utxoMapping    :: map TxIn TxOut,
           utxoaggMapping :: map Addr Coin
         }

  projectOnDiskMappings LedgerState { utxos = UTxOState { utxo, utxoagg } } =
    LedgerStateMappings {
      utxoMapping    = utxo,
      utxoaggMapping = utxoagg
    }

  injectOnDiskMappings
    LedgerStateMappings { utxoMapping, utxoaggMapping }
    LedgerState { pparams } =

    LedgerState {
      utxos   = UTxOState {
                  utxo    = utxoMapping,
                  utxoagg = utxoaggMapping
                },
      pparams
    }

data Tx = Tx [TxIn] [TxOut]

txId :: Tx -> TxId
txId = TxId . H.hash

instance H.Hashable Tx where
  hashWithSalt s (Tx txins txouts) = H.hashWithSalt s (txins, txouts)

instance H.Hashable TxIn where
  hashWithSalt s (TxIn txid txix) = H.hashWithSalt s (txid, txix)

instance H.Hashable TxOut where
  hashWithSalt s (TxOut addr coin) = H.hashWithSalt s (addr, coin)

instance H.Hashable TxId where
  hashWithSalt s (TxId n)          = H.hashWithSalt s n

instance H.Hashable TxIx where
  hashWithSalt s (TxIx n)          = H.hashWithSalt s n

instance H.Hashable Addr where
  hashWithSalt s (Addr n)          = H.hashWithSalt s n

instance H.Hashable Coin where
  hashWithSalt s (Coin n)          = H.hashWithSalt s n

-- Operations on the top level state

type Block = [Tx]
applyBlock :: Mapping map => [Tx] -> LedgerState map -> Maybe (LedgerState map)
applyBlock []       st = Just st
applyBlock (tx:txs) st = applyTx tx st >>= applyBlock txs

applyTx :: Mapping map => Tx -> LedgerState map -> Maybe (LedgerState map)
applyTx tx LedgerState{utxos, pparams} = do
    utxos' <- applyTx' tx utxos
    Just LedgerState {
           utxos = utxos',
           pparams
         }

-- Operations on a sub-component of the state

applyTx' :: Mapping map => Tx -> UTxOState map -> Maybe (UTxOState map)
applyTx' tx@(Tx txins txouts) = fmap (insertOutputs txid txouts)
                              . deleteInputs txins
  where
    txid = txId tx

deleteInputs :: Mapping map => [TxIn] -> UTxOState map -> Maybe (UTxOState map)
deleteInputs []           st = Just st
deleteInputs (txin:txins) st = deleteInput txin st >>= deleteInputs txins

deleteInput :: Mapping map => TxIn -> UTxOState map -> Maybe (UTxOState map)
deleteInput txin UTxOState {utxo, utxoagg} = do
    -- Verify that the input exists, and also update the utxo aggregate
    TxOut addr coin <- lookup txin utxo
    return UTxOState {
      utxo    = delete txin utxo,
      utxoagg = update addr (negate coin) utxoagg
    }

insertOutputs :: Mapping map => TxId -> [TxOut] -> UTxOState map -> UTxOState map
insertOutputs txid txouts st =
    foldr (\(txix, txout) -> insertOutput (TxIn txid (TxIx txix)) txout)
          st (zip [0 ..] txouts)

insertOutput :: Mapping map => TxIn -> TxOut -> UTxOState map -> UTxOState map
insertOutput txin txout@(TxOut addr coin) UTxOState {utxo, utxoagg} =
    -- also update the utxo aggregate
    UTxOState {
      utxo    = insert txin txout utxo,
      utxoagg = update addr coin utxoagg
    }


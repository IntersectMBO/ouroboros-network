{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module UTxOAPI where

import           Prelude hiding (lookup)

import           Data.Kind
import           Data.Functor.Identity

import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.FingerTree as FT

import           Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM



-- Diffs
--------

class Monoid (Diff a) => Changes a where
  data Diff a

  applyDiff :: a -> Diff a -> a

  -- Laws:
  --  x ◁ 0 = x
  -- (x ◁ d) ◁ d' =  x ◁ (d <> d')

-- ⊙ ⊕ ∣ |

(◁) :: Changes a => a -> Diff a -> a
(◁) = applyDiff

prop_applyDiff :: (Changes a, Eq a) => a -> Diff a -> Diff a -> Bool
prop_applyDiff x d d' =
    (x ◁ d) ◁ d' == x ◁ (d <> d')


instance (Ord k, Semigroup a) => Changes (Map k a) where
  newtype Diff (Map k a) = MapDiff (Map k (MapDiffElem a))

  applyDiff :: Map k a -> Diff (Map k a) -> Map k a
  applyDiff m (MapDiff md) =
      Map.merge
        Map.preserveMissing
        (Map.mapMaybeMissing      insert)
        (Map.zipWithMaybeMatched  apply)
        m
        md
    where
      insert :: k -> MapDiffElem a -> Maybe a
      insert _    MapElemDelete     = Nothing
      insert _ (  MapElemInsert x)  = Just x
      insert _ (  MapElemUpdate x)  = Just x

      apply :: k -> a -> MapDiffElem a -> Maybe a
      apply _ _    MapElemDelete     = Nothing
      apply _ _ (  MapElemInsert y)  = Just y
      apply _ x (  MapElemUpdate y)  = Just (x<>y)

instance (Ord k, Semigroup a) => Monoid (Diff (Map k a)) where
  mempty = MapDiff Map.empty

instance (Ord k, Semigroup a) => Semigroup (Diff (Map k a)) where
  MapDiff a <> MapDiff b = MapDiff (Map.unionWith (<>) a b)

data MapDiffElem a = MapElemDelete | MapElemInsert !a | MapElemUpdate !a

instance Semigroup a => Semigroup (MapDiffElem a) where
  MapElemDelete     <> MapElemDelete     = MapElemDelete
  MapElemInsert  _  <> MapElemInsert  y  = MapElemInsert     y
  MapElemUpdate  x  <> MapElemUpdate  y  = MapElemUpdate  (  x <> y)

  MapElemInsert  _  <> MapElemDelete     = MapElemDelete
  MapElemDelete     <> MapElemInsert  y  = MapElemInsert     y

  MapElemUpdate  _  <> MapElemDelete     = MapElemDelete
  MapElemDelete     <> MapElemUpdate  y  = MapElemInsert     y

  MapElemInsert  x  <> MapElemUpdate  y  = MapElemInsert  (  x <> y)
  MapElemUpdate  _  <> MapElemInsert  y  = MapElemInsert     y



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

-- Another trick will be to parametrise code over the map type, to enable
-- us to swap out a normal Data.Map for a PTMap.

class Mapping map where

  lookup :: Ord k => k      -> map k a -> Maybe a
  insert :: Ord k => k -> a -> map k a -> map k a
  update :: Semigroup a
         => Ord k => k -> a -> map k a -> map k a
  delete :: Ord k => k      -> map k a -> map k a


data PMap k a = PMap !(Map k a)  -- The keys asked for that are present
                     !(Set k)    -- The keys asked for that are not present

makePMap :: Ord k => Map k a -> Set k -> PMap k a
makePMap m ks =
    PMap present absent
  where
    present = Map.restrictKeys m ks
    absent  = ks Set.\\ Map.keysSet present

instance Mapping PMap where
  lookup k (PMap present absent) =
    case Map.lookup k present of
      Just v        -> Just v
      Nothing
        | Set.member k absent
                    -> Nothing
        | otherwise -> error "PMap.lookup: used a key not fetched from disk"

  insert k v (PMap present absent) = PMap (Map.insert k v present) absent
  delete k   (PMap present absent) = PMap (Map.delete k   present) absent
  update k v (PMap present absent) = PMap (Map.insertWith (<>) k v present) absent


newtype DiffMap  k v = DiffMap (Diff (Map k v))
  deriving (Semigroup, Monoid)

instance Mapping DiffMap where
  lookup _ _ = error "DiffMap.lookup not supported"

  insert k v (DiffMap (MapDiff d)) = DiffMap (MapDiff (Map.insert k (MapElemInsert v) d))
  delete k   (DiffMap (MapDiff d)) = DiffMap (MapDiff (Map.insert k MapElemDelete     d))
  update k v (DiffMap (MapDiff d)) = DiffMap (MapDiff (Map.insertWith (<>) k (MapElemUpdate v) d))


data PTMap k a = PTMap !(PMap k a) !(DiffMap k a)

makePTMap :: PMap k a -> PTMap k a
makePTMap m = PTMap m (DiffMap (MapDiff Map.empty))

instance Mapping PTMap where
  lookup k (PTMap m _) = lookup k m

  insert k v (PTMap m d) = PTMap (insert k v m) (insert k v d)
  delete k   (PTMap m d) = PTMap (delete k   m) (delete k   d)
  update k v (PTMap m d) = PTMap (update k v m) (update k v d)


applyPTDiffMap :: (Ord k, Semigroup v) => PMap k v -> DiffMap k v -> PTMap k v
applyPTDiffMap (PMap m s) (DiffMap d) = makePTMap (PMap (applyDiff m d) s)


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

data    EmptyMap k v = EmptyMap
newtype KeySet   k v = KeySet (Set k)


class HasMappings (state :: (* -> * -> *) -> *) where
  type StateMappingKeyConstraint   state :: * -> Constraint
  type StateMappingValueConstraint state :: * -> Constraint

  traverseMappings :: Applicative f
                   => (forall k v. Ord k
                                => Semigroup v
                                => StateMappingKeyConstraint   state k
                                => StateMappingValueConstraint state v
                                => map k v -> f (map' k v))
                   -> state map -> f (state map')

  -- The common pure case, not needing Applicative
  fmapMappings :: (forall k v. Ord k
                            => Semigroup v
                            => StateMappingKeyConstraint   state k
                            => StateMappingValueConstraint state v
                            => map k v -> map' k v)
               -> state map -> state map'
  fmapMappings f = runIdentity . traverseMappings (pure . f)

-- | In addition to 'HasMappings', if the state type consists only of mappings
-- then we can zip two states together just using a function on the matching
-- maps. Or similarly we can make one from scratch with constant mappings.
--
-- This zip would not be possible if the state contained any other data as we
-- would need combining functions for the other parts.
--
class HasMappings state => HasOnlyMappings state where
  traverse0Mappings :: Applicative f
                    => (forall k v. Ord k
                                 => Semigroup v
                                 => StateMappingKeyConstraint   state k
                                 => StateMappingValueConstraint state v
                                 => f (map k v))
                    -> f (state map)

  traverse2Mappings :: Applicative f
                    => (forall k v. Ord k
                                 => Semigroup v
                                 => StateMappingKeyConstraint   state k
                                 => StateMappingValueConstraint state v
                                 => map k v -> map' k v -> f (map'' k v))
                    -> state map -> state map' -> f (state map'')

  traverse2Mappings_ :: Applicative f
                     => (forall k v. Ord k
                                  => Semigroup v
                                  => StateMappingKeyConstraint   state k
                                  => StateMappingValueConstraint state v
                                  => map k v -> map' k v -> f ())
                     -> state map -> state map' -> f ()

  -- The common pure case, not needing Applicative
  constMappings :: (forall k v. Ord k
                             => Semigroup v
                             => StateMappingKeyConstraint   state k
                             => StateMappingValueConstraint state v
                             => map k v)
                -> state map
  constMappings f = runIdentity (traverse0Mappings (pure f))

  zipMappings  :: (forall k v. Ord k
                            => Semigroup v
                            => StateMappingKeyConstraint   state k
                            => StateMappingValueConstraint state v
                            => map k v -> map' k v -> map'' k v)
               -> state map -> state map' -> state map''
  zipMappings f a b = runIdentity (traverse2Mappings (\x y -> pure (f x y)) a b)

class (HasMappings state, HasOnlyMappings (OnDiskMappings state))
   => HasOnDiskMappings state where

  data OnDiskMappings state :: (* -> * -> *) -> *

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  injectOnDiskMappings  :: OnDiskMappings state map -> state any -> state map


class MonoidialMapping map where
    memptyMap   :: (Ord k, Semigroup v) => map k v
    mappendMaps :: (Ord k, Semigroup v) => map k v -> map k v -> map k v

instance MonoidialMapping DiffMap where
    memptyMap   = mempty
    mappendMaps = (<>)

instance (HasOnlyMappings (OnDiskMappings state), MonoidialMapping map)
      => Semigroup (OnDiskMappings state map) where
   (<>) = zipMappings mappendMaps

instance (HasOnlyMappings (OnDiskMappings state), MonoidialMapping map)
      => Monoid (OnDiskMappings state map) where
   mempty = constMappings memptyMap


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


-- Diff sequences
-----------------

-- | A DB extension sequence is a sequence of in-memory extensions
-- (i.e. changes) to some other (e.g. disk-based) database.
--
type DbExtensionSeq state =
       FT.FingerTree (DbExtensionsMeasure state)
                     (DbExtension state)

-- | An individual DB extension is a state where the mappings are actually
-- differences on mappings.
--
-- As a potential optimisation (reduced number of potentially-expensive
-- 'projectOnDiskMappings' traversals) we could split it out and keep the
-- in-memory only bits and the on-disk differences separately.
--
data DbExtension state = DbExtension !(state EmptyMap)
                                     !(OnDiskMappings state DiffMap)

data DbExtensionsMeasure state =
       DbExtensionsMeasure !Int !(OnDiskMappings state DiffMap)

instance HasOnDiskMappings state => Semigroup (DbExtensionsMeasure state) where
  DbExtensionsMeasure l1 a1 <> DbExtensionsMeasure l2 a2 =
    DbExtensionsMeasure (l1+l2) (a1 <> a2)

instance HasOnDiskMappings state => Monoid (DbExtensionsMeasure state) where
  mempty = DbExtensionsMeasure 0 mempty

instance HasOnDiskMappings state
      => FT.Measured (DbExtensionsMeasure state) (DbExtension state) where
  measure (DbExtension _s d) = DbExtensionsMeasure 1 d



-- Disk operations
------------------

class DiskDB dbhandle state where
  readDB  :: dbhandle -> OnDiskMappings state KeySet -> IO (OnDiskMappings state PMap)
  writeDB :: dbhandle -> OnDiskMappings state DiffMap -> IO ()

{-
class DBTable tblhandle
-}

newtype TVarDB state = TVarDB (OnDiskMappings state TVarDBTable)

newtype TVarDBTable k v = TVarDBTable (TVar (Map k v))

instance HasOnDiskMappings state => DiskDB (TVarDB state) state where

  readDB  (TVarDB tables) keysets = traverse2Mappings readTVarDBTable  tables keysets
  writeDB (TVarDB tables) diffs   = traverse2Mappings_ writeTVarDBTable tables diffs

readTVarDBTable :: Ord k => TVarDBTable k v -> KeySet k v -> IO (PMap k v)
readTVarDBTable (TVarDBTable tv) (KeySet ks) =
  STM.atomically $ do
    tbl <- STM.readTVar tv
    let !pmap = makePMap tbl ks
    return pmap

writeTVarDBTable :: (Ord k, Semigroup v) => TVarDBTable k v -> DiffMap k v -> IO ()
writeTVarDBTable (TVarDBTable tv) (DiffMap d) =
  STM.atomically $ do
    tbl <- STM.readTVar tv
    let !tbl' = applyDiff tbl d
    STM.writeTVar tv tbl'




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

--prepare :: DbModel k a -> Set k -> PTMap k a
--prepare (DbModel d m)


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
newtype SlotNo= SlotNo Int         deriving (Eq, Enum, Show)

instance Semigroup Coin  where a <> b = a + b
instance Semigroup TxOut where _ <> _ = error "Semigroup TxOut: cannot use updates on the utxo"

class Example a
instance Example TxIn
instance Example TxOut
instance Example Addr
instance Example Coin

instance HasMappings LedgerState where
  type StateMappingKeyConstraint   LedgerState = Example
  type StateMappingValueConstraint LedgerState = Example

  traverseMappings f LedgerState { utxos, pparams } =
    LedgerState <$> traverseMappings f utxos
                <*> pure pparams

instance HasMappings UTxOState where
  type StateMappingKeyConstraint   UTxOState = Example
  type StateMappingValueConstraint UTxOState = Example

  traverseMappings f UTxOState { utxo, utxoagg } =
    UTxOState <$> f utxo
              <*> f utxoagg

instance HasMappings (OnDiskMappings LedgerState) where
  type StateMappingKeyConstraint   (OnDiskMappings LedgerState) = Example
  type StateMappingValueConstraint (OnDiskMappings LedgerState) = Example

  traverseMappings f LedgerStateMappings { utxoMapping, utxoaggMapping } =
    LedgerStateMappings <$> f utxoMapping
                        <*> f utxoaggMapping

instance HasOnlyMappings (OnDiskMappings LedgerState) where
  traverse0Mappings f =
    LedgerStateMappings <$> f
                        <*> f

  traverse2Mappings f st1 st2 =
    LedgerStateMappings <$> f (utxoMapping st1) (utxoMapping st2)
                        <*> f (utxoaggMapping st1) (utxoaggMapping st2)

  traverse2Mappings_ f st1 st2 =
     () <$ f (utxoMapping st1) (utxoMapping st2)
        <* f (utxoaggMapping st1) (utxoaggMapping st2)

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

-- Keys needed for applyBlock
applyBlockKeys :: [Tx] -> OnDiskMappings LedgerState KeySet
applyBlockKeys = undefined


-- Example chain selection
--------------------------

type ChainFragment = [Block]

findIntersection :: ChainFragment -> ChainFragment -> SlotNo
findIntersection = undefined


type LedgerStateSeq = DbExtensionSeq LedgerState

-- | This is a drop \/ split operation, at the given slot number.
-- We use it to rewind to an intersection point.
--
rewindLedgerStateSeq :: SlotNo -> LedgerStateSeq -> LedgerStateSeq
rewindLedgerStateSeq = undefined
    -- This will be a simple fingertree split on the SlotNo

ledgerStateForBlock :: LedgerStateSeq
                    -> OnDiskMappings LedgerState PMap
                    -> LedgerState PTMap
ledgerStateForBlock lseq rs =
    injectOnDiskMappings (zipMappings applyPTDiffMap rs d) s
  where
    s :: LedgerState EmptyMap
    _ FT.:> (DbExtension s _) = FT.viewr lseq

    d :: OnDiskMappings LedgerState DiffMap
    DbExtensionsMeasure _ d = FT.measure lseq

extendLedgerStateSeq :: LedgerStateSeq -> LedgerState PTMap -> LedgerStateSeq
extendLedgerStateSeq lseq ls =
    lseq FT.|> DbExtension s d
  where
    s :: LedgerState EmptyMap
    s = fmapMappings (const EmptyMap) ls

    d :: OnDiskMappings LedgerState DiffMap
    d = fmapMappings (\(PTMap _ diff) -> diff) (projectOnDiskMappings ls)


-- | A highly simplfied chain selection algorithm where we just look at a
-- single candidate chain (which we assume is longer) and just evaluate
-- if the blocks are valid. If they are, we adopt the chain.
--
considerChain :: TVarDB LedgerState  -- ^ The ledger state disk db
              -> TVar ChainFragment  -- ^ Our current chain
              -> TVar LedgerStateSeq -- ^ The ledger state in-mem extension
                                     --   matching the current chain
              -> ChainFragment       -- ^ The candidate chain
              -> IO ()
considerChain db chainvar lseqvar candidate = do
    -- start by reading state and finding the intersection
    (lseq, chain) <- STM.atomically $ (,) <$> STM.readTVar lseqvar
                                          <*> STM.readTVar chainvar
    let intersectSlot = findIntersection chain candidate

    result <- evaluateCandidate (rewindLedgerStateSeq intersectSlot lseq) candidate
    case result of
      Nothing -> return ()
      Just lseq' ->
        -- Adopt the new chain and corresponding ledger state
        STM.atomically $ do
          STM.writeTVar chainvar candidate
          STM.writeTVar lseqvar lseq'
  where
    evaluateCandidate !lseq [] =
      -- If we got to the end without failing, we assume we've got the longer
      -- chain now, so return the corresponding ledger state (seq)
      return (Just lseq)

    evaluateCandidate !lseq (b:bs) = do
      rs <- readDB db (applyBlockKeys b)
      case applyBlock b (ledgerStateForBlock lseq rs) of
        Nothing -> return Nothing
        Just ledgerstate' ->
            evaluateCandidate lseq' bs
          where
            lseq' = extendLedgerStateSeq lseq ledgerstate'


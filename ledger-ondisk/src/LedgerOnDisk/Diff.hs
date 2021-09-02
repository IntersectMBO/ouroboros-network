{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module LedgerOnDisk.Diff where

import Data.Monoid
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Functor.Identity
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.Coerce
import qualified Data.Semigroup as Semi
import Data.Functor ((<&>))
import qualified Data.Monoid as Monoid
import Data.Map
import qualified Data.Map.Strict as Map

-- | The semigroup dictionary in DMappend is unfortunate.
type role Diff nominal
data Diff v where
  DNoChange :: Diff v
  DRemove :: Diff v
  DChangeTo :: v -> Diff v
  DMappend :: Semigroup v => v -> Diff v


deriving stock instance Eq v => Eq (Diff v)
deriving stock instance Show v => Show (Diff v)

{-  (<>) on Diff is not commutative, so I think we should change the order of arguments here for 2 reasons
 1) I like to think of (d1 <> d2) as composition of changes. So in accordance with Haskell's composition
    we should think of addpying the d2 first.  E.g. in Haskell (f . g) x == f (g x). So apply (x <> y) x == apply x (apply y x)
 2) When we lift (Diff v) to Map k (Diff v), we will want to use Map.insertWith (<>) k v x, and Map.unionWith (<>) m1 m2
    and these require to us apply the change on the right first.
So it is best to change the order.

instance Semigroup (Diff v) where
  x <> DNoChange = x
  DMappend x <> DMappend y = DMappend $ x <> y
  DChangeTo x <> DMappend y = DChangeTo $ x <> y
  DRemove <> DMappend y = DChangeTo y
  _ <> y = y

Another thing, is that DRemove (applied first) cancels a subsequent (DMappend v) so the 4th line
when appying things in the eft first should be
   DRemove <> DMappend y = DRemove.
We can also make the function clearer by making it more verbose, so it is easy to see all the cases are covered.
So I suggest this.
-}

-- (<>) is composition of changes. So apply (x <> y) x == apply x (apply y x)
-- Note we apply the change on the right first.
instance Semigroup v => Semigroup (Diff v) where
  DNoChange <> x          = x
  DRemove <> x            = DRemove
  DChangeTo v <> x        = DChangeTo v
  DMappend v <> DNoChange = DMappend v
  DMappend v <> DRemove   = DRemove
  DMappend v <> DChangeTo u = DChangeTo (v <> u)
  DMappend v <> DMappend u = DMappend (v <> u)  

instance Monoid (Diff v) where
  mempty = DNoChange


-- Not sure this is a very useful function, it is just the 'fmap' method of functor.
mapD :: Semigroup b => (a -> b) -> Diff a -> Diff b
mapD f = \case
  DChangeTo x -> DChangeTo $ f x
  -- DMappend x -> DChangeTo $ f x  -- I am also worried about this clause, but it is hard to articulate why.
  --  mapD f (Mappend x) <> mapD f (Mappend y) ---> DChangeTo (f x) <> DChangeTo (f y)
  --  I think I would like this to be Mappend (f(x<>y)), but that is not the case. Of course my intuition might be wrong.
  --  Do you get the same feeing that something is fishy here?
  DRemove -> DRemove
  DNoChange -> DNoChange

{- WHY is the x a maybe type? Every (Diff v) can be appied to any 'v'
applyD :: Semigroup v => Maybe v -> Diff v -> Maybe v
applyD x = \case
  DChangeTo v -> Just v
  DMappend v -> Just $ maybe v (<> v) x
  DRemove -> Nothing
  DNoChange -> x
-}

applyD :: Semigroup v => v -> Diff v -> Maybe v
applyD x = \case
  DChangeTo v -> Just v
  DMappend v -> Just (v <> x)
  DRemove -> Nothing
  DNoChange -> Just x

-- Not sure applyDforK is a usefull function. Once we lift (Diff v) to (Map k (Diff v)), we think of
-- applying the whole d::(Map k (Diff v)) to m::(Map k v). The idea is that any keys in 'm' not in 'd'
-- are not changed. applyDfork is really applyDtoHashMap with a singleton (Map k (Diff v))

applyDforK :: (Eq k, Hashable k, Semigroup v) => k -> Diff v -> HashMap k v -> HashMap k v
applyDforK k = \case
  DChangeTo v -> HashMap.insert k v
  DMappend v -> HashMap.insertWith (<>) k v
  DRemove -> HashMap.delete k
  DNoChange -> id

applyDtoHashMap :: (Semigroup v, Eq k, Hashable k) => HashMap k (Diff v) -> HashMap k v -> HashMap k v
applyDtoHashMap d = appEndo (HashMap.foldMapWithKey go d) where
  go k = Endo . applyDforK k


{- We probably want to use a strict left fold, to avoid creating thunks. Maybe the use of Endo is overkill?
applyDifftoMap :: Semigroup v => Ord k => Map k (Diff v) -> Map k v -> Map k v
applyDifftoMap d = appEndo (Map.foldMapWithKey go d) where
  go k = Endo . \case
    DChangeTo v -> Map.insert k v
    DMappend v -> Map.insertWith (<>) k v
    DRemove -> Map.delete k
    DNoChange -> id
-}    

applyDifftoMap :: (Semigroup v, Ord k) => Map k (Diff v) -> Map k v -> Map k v
applyDifftoMap d m =  Map.foldlWithKey' accum m messages
  where
    accum ans key DRemove = Map.delete key ans
    accum ans key (DChangeTo v) = Map.insert key v ans  -- No need to use (insertWith (<>)) we want to overide whatever is there
    accum ans key (DMappend e) = Map.update (Just . (e <>)) key ans
    accum ans key DNoChange = ans

-- So if (Diff v) represent changesto 'v'
-- then (Map k (Diff v)) represents changes to (Map k v)
-- Just like we want to compose changes to 'v', we want to compose changes to (Map k v)
-- We have (<>) :: Diff v -> Diff v -> Diff v
-- So we want (<>) :: Map k (Diff v) -> Map k (Diff v) -> Map k (Diff v)
-- We would like to write this
-- instance (Ord k,Semigroup v) => Semigroup (Map k (Diff v)) where
--    (<>) x y  = (Map.unionWith <> x y)
-- But this is already instance, Because (Diff v) has a Semigroup instance!

shrinkD :: Arbitrary v => Diff v -> [Diff v]
shrinkD = \case
    DRemove -> [DNoChange]
    DChangeTo v -> [DNoChange] ++ (DChangeTo <$> shrink v)
    DNoChange -> []
    DMappend v -> [DNoChange] ++ (DChangeTo <$> shrink v) ++ [DChangeTo v] ++ (DMappend <$> shrink v)


instance (Semigroup v, Arbitrary v) => Arbitrary (Diff v) where
  arbitrary = oneof
    [ pure DNoChange
    , pure DRemove
    , coerce . DChangeTo <$> arbitrary
    , coerce . DMappend <$> arbitrary
    ]
  shrink = shrinkD

arbitraryNonmonoidalD :: Arbitrary v => Gen (Diff v)
arbitraryNonmonoidalD = arbitrary <&> \case
  DMappend (Semi.Last x) -> DChangeTo x
  DChangeTo (Semi.Last x) -> DChangeTo x
  DRemove -> DRemove
  DNoChange -> DNoChange
  
newtype ArbNonmonoidalD v = ArbNonmonoidalD { unArbNonmonoidalD :: Diff v }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance Arbitrary v => Arbitrary (ArbNonmonoidalD v) where
  arbitrary = coerce <$> arbitraryNonmonoidalD
  shrink = coerce shrinkD

-- we demand that domain keys (applyDToHashMaybeMap x y)  == keys y
applyDtoHashMaybeMap :: (Semigroup v, Eq k, Hashable k) => HashMap k (Diff v) -> HashMap k (Maybe v) -> HashMap k (Maybe v)
applyDtoHashMaybeMap d_map = HashMap.mapWithKey $ \k mb_v -> applyD mb_v (HashMap.lookupDefault mempty k d_map)

applyDforKDMap :: GCompare f => f x -> Diff x -> DMap f Identity -> DMap f Identity
applyDforKDMap k = \case
  DChangeTo x -> DMap.insert k (Identity x)
  -- DMappend x -> DMap.insertWith (<>) k (Identity x)
  DRemove -> DMap.delete k
  DNoChange -> id
  DMappend {} -> error "is this possible?"

applyDtoDMap :: GCompare f => DMap f Diff -> DMap f Identity -> DMap f Identity
applyDtoDMap dm_d = appEndo (DMap.foldrWithKey go mempty dm_d)
  where
    go k d e = Endo (applyDforKDMap k d) <> e

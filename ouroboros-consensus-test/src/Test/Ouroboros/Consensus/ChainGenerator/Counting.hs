{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Very strong types for working with indices, counts, etc within sequences.
module Test.Ouroboros.Consensus.ChainGenerator.Counting (
  -- * general counts
  Count (Count),
  (-),
  (+),
  getCount,
  forgetBase,
  forgetElem,
  -- * indices and sizes
  Preds,
  Index,
  Size,
  Total,
  forRange_,
  lastIndex,
  range, 
  uniformIndex,
  -- * windows
  Lbl (Lbl),
  SomeWindow (SomeWindow),
  Win,
  Contains (Contains, UnsafeContains),
  forgetWindow,
  frWin,
  frWinVar,
  joinWin,
  toWin,
  windowLast,
  windowSize,
  windowStart,
  withSuffixWindow,
  withTopWindow,
  withWindow,
  withWindowBetween,
  -- * vectors
  Vector (Vector),
  MVector (MVector),
  createV,
  getMVector,
  getVector,
  lengthMV,
  lengthV,
  modifyMV,
  readMV,
  readV,
  replicateMV,
  sliceMV,
  sliceV,
  unsafeThawV,
  writeMV,
  -- * variables
  Other,
  Var,
  joinVar,
  toIndex,
  toSize,
  toVar,
  ) where

import           Control.Monad.ST (ST)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Type.Equality as TypeEq
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.OverloadedLabels (IsLabel (fromLabel))
import           Prelude hiding ((+), (-))
import qualified Prelude
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.QuickCheck as QC

-----

infixl 6 .+, .-

(.+) :: Int -> Int -> Int
(.+) = (Prelude.+)

(.-) :: Int -> Int -> Int
(.-) = (Prelude.-)

-----

-- | Indexed by what you're counting from, what you're counting, and which of them you're counting.
newtype Count (base :: Type) (elem :: kelem) (which :: kwhich) = Count Int
  deriving (QC.Arbitrary, Eq, Ord, Read, Show)

getCount :: Count base elem which -> Int
getCount (Count n) = n

infixl 6 +, -

(+) :: Count base elem which -> Int -> Count base elem which
(+) (Count i) j = Count (i .+ j)

(-) :: Count base elem which -> Int -> Count base elem which
(-) (Count i) j = Count (i .- j)

forgetBase :: Count base elem which -> Some.Forgotten (Count () elem which)
forgetBase (Count x) = Some.forgotten $ Count x

forgetElem :: Count base elem which -> Some.Forgotten (Count base () which)
forgetElem (Count x) = Some.forgotten $ Count x

-----

-- | How many preceding elements
data Preds

-- | How many elements in 'Total'
data Total

type Index base elem = Count base elem Preds
type Size  base elem = Count base elem Total

-- | The 'Index' of the rightmost element in the sequence of the given 'Size'
lastIndex :: Size base elem -> Index base elem
lastIndex (Count n) = Count (n .- 1)

range :: Size base elem -> [Index base elem]
range (Count n) = coerce [0 .. max 0 n .- 1]

forRange_ :: Applicative f => Size base elem -> (Index base elem -> f a) -> f ()
forRange_ c = for_ (range c)

uniformIndex :: R.StatefulGen g m => Size base elem -> g -> m (Index base elem)
uniformIndex n g = Count <$> R.uniformRM (0, getCount $ lastIndex n) g

-----

-- | A human-readable label for a 'Win'
data Lbl lbl = Lbl   -- no explicit kind var so that type applications don't
                     -- need to provide the kind
                     --
                     -- TODO as of GHC 9.0, use a standalone kind signature to
                     -- declare k as /inferred/ instead of /specified/
instance (lbl TypeEq.~~ s) => IsLabel s (Lbl lbl) where fromLabel = Lbl

-- | A named window within some containing sequence
data Win (lbl :: klbl) (skolem :: Type)

-- | The correspondence between a window and its containing sequence
data Contains (elem :: kelem) (outer :: Type) (inner :: Type) =
    UnsafeContains
        !(Index outer elem)   -- ^ first slot's offset in containing sequence
        !(Size  inner elem)   -- ^ size of window (INVARIANT: does not reach past end of containing sequence)
  deriving (Eq, Read, Show)

pattern Contains :: Index outer elem -> Size inner elem -> Contains elem outer inner
pattern Contains x y <- UnsafeContains x y

{-# COMPLETE Contains #-}

forgetWindow :: Contains elem outer inner -> Some.Forgotten (Index outer elem, Index outer elem)
forgetWindow win = Some.forgotten $ (windowStart win, windowLast win)

frWin :: Contains elem outer inner -> Index inner elem -> Index outer elem
frWin (Contains (Count i) _n) (Count j) = Count (i .+ j)

frWinVar :: Contains elem outer inner -> Var inner x -> Var outer x
frWinVar _ (Count x) = Count x

toWin :: Contains elem outer inner -> Index outer elem -> Maybe (Index inner elem)
{-# INLINE toWin #-}
toWin (Contains (Count i) (Count n)) (Count j) = if i <= j && j < i .+ n then Just (Count (j .- i)) else Nothing

windowSize :: Contains elem outer inner -> Size inner elem
windowSize (Contains _i (Count n)) = Count n

windowStart :: Contains elem outer inner -> Index outer elem
windowStart win = frWin win (Count 0)

windowLast :: Contains elem outer inner -> Index outer elem
windowLast win = frWin win $ lastIndex $ windowSize win

-- | 'Contains' is a 'Data.Semigroupoid.Semigroupoid'
joinWin :: Contains elem outer mid -> Contains elem mid inner -> Contains elem outer inner
{-# INLINE joinWin #-}
joinWin win win2 = UnsafeContains (frWin win $ windowStart win2) (windowSize win2)

data SomeWindow (lbl :: klbl) (outer :: Type) (elem :: kelem) =
    forall (skolem :: Type).
    SomeWindow
        !(Proxy skolem)
        !(Contains elem outer (Win lbl skolem))

instance Eq (SomeWindow lbl outer elem) where
    SomeWindow l1 l2 == SomeWindow r1 r2 =
        Some.runEq
      $ Some.eqCtor SomeWindow SomeWindow
          `Some.eqArg` (const (), l1, const (), r1)
          `Some.eqArg` (forgetWindow, l2, forgetWindow, r2)

instance Show (SomeWindow lbl outer elem) where
    showsPrec p (SomeWindow prx win) =
        Some.runShowsPrec p
      $ Some.showCtor SomeWindow "SomeWindow"
          `Some.showArg` prx
          `Some.showArg` win

instance Read (SomeWindow lbl outer elem) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeWindow "SomeWindow"
          <*> Some.readArg
          <*> Some.readArg

-- | Create a fresh 'Window' within a given 'Size' that starts with the given 'Index' and contains however many of the given smaller 'Size' of elements exist within the larger 'Size'
--
-- NOTE: the requested window size is truncated if necessary to ensure it fits in the containing sequence
--
-- NOTE: if the the requested index is negative, it's instead taken to be 0
withWindow :: Size outer elem -> Lbl lbl -> Index outer elem -> Size x elem -> SomeWindow lbl outer elem
withWindow (Count n) _lbl (Count i) (Count m) =
    SomeWindow Proxy $ UnsafeContains (Count i') (Count m')
  where
    i' = max 0 i

    pre = i' .- i
    suc = max 0 $ i .+ m .- n

    m' = max 0 $ m .- pre .- suc

withWindowBetween :: Size outer elem -> Lbl lbl -> Index outer elem -> Index outer elem -> SomeWindow lbl outer elem
withWindowBetween n lbl (Count i) (Count j) = withWindow n lbl (Count i) (Count $ j .- i .+ 1)

withSuffixWindow :: Size outer elem -> Lbl lbl -> Index outer elem -> SomeWindow lbl outer elem
withSuffixWindow n lbl i = withWindow n lbl i (Count $ getCount n .- getCount i)

withTopWindow ::
     Lbl lbl
  -> Int
  -> (forall base. Proxy base -> SomeWindow lbl base elem -> ans)
  -> ans
withTopWindow _lbl n k =
      k Proxy $ SomeWindow Proxy $ UnsafeContains (Count 0) (Count n)

-----

-- | Same indices as 'Index' and 'Size'
newtype Vector base elem a = Vector (V.Vector a)
  deriving (Eq, Read, Show)

instance (QC.Arbitrary a, V.Unbox a) => QC.Arbitrary (Vector base elem a) where
    arbitrary = (Vector . V.fromList) <$> QC.arbitrary
    shrink    = map (Vector . V.fromList) . QC.shrink . V.toList . getVector

getVector :: Vector base elem a -> V.Vector a
getVector (Vector v) = v

lengthV :: V.Unbox a => Vector base elem a -> Size base elem
lengthV = Count . V.length . getVector

sliceV :: MV.Unbox a => Contains elem outer inner -> Vector outer elem a -> Vector inner elem a
{-# INLINE sliceV #-}
sliceV win (Vector v) =
    Vector $ V.slice i n v
  where
    Count i = frWin win (Count 0)
    Count n = windowSize win

unsafeThawV :: MV.Unbox a => Vector base elem a -> ST s (MVector base elem s a)
unsafeThawV (Vector v) = MVector <$> V.unsafeThaw v

createV :: MV.Unbox a => (forall s. ST s (MVector base elem s a)) -> Vector base elem a
createV m = Vector $ V.create (getMVector <$> m)

-- | Same indices as 'Index' and 'Size'
newtype MVector base elem s a = MVector (MV.MVector s a)

getMVector :: MVector base elem s a -> MV.MVector s a
getMVector (MVector mv) = mv

lengthMV :: MV.Unbox a => MVector base elem s a -> Size base elem
lengthMV = Count . MV.length . getMVector

sliceMV :: MV.Unbox a => Contains elem outer inner -> MVector outer elem s a -> MVector inner elem s a
{-# INLINE sliceMV #-}
sliceMV win (MVector mv) =
    MVector $ MV.slice i n mv
  where
    Count i = frWin win (Count 0)
    Count n = windowSize win

replicateMV :: MV.Unbox a => Size base elem -> ST s a -> ST s (MVector base elem s a)
replicateMV (Count n) m = fmap MVector $ MV.replicateM n m

readMV   :: MV.Unbox a => MVector base elem s a ->             Index base elem ->      ST s a
writeMV  :: MV.Unbox a => MVector base elem s a ->             Index base elem -> a -> ST s ()
modifyMV :: MV.Unbox a => MVector base elem s a -> (a -> a) -> Index base elem ->      ST s ()

readMV   (MVector mv)   (Count i)   = MV.read   mv i
writeMV  (MVector mv)   (Count i) x = MV.write  mv i x
modifyMV (MVector mv) f (Count i)   = MV.modify mv f i

readV :: MV.Unbox a => Vector base elem a -> Index base elem -> a
readV (Vector v) (Count i) = v V.! i

-----

-- | A count of things that satisfy some predicate other than 'Preds' and 'Total'
data Other

deriving instance (which TypeEq.~~ Other) => Enum (Count base elem which)
deriving instance (which TypeEq.~~ Other) => Num  (Count base elem which)

type Var base elem = Count base elem Other

-- | For initializing a 'Var'
toVar :: Count base elem which -> Var base elem
toVar (Count n) = Count n

-- | When the 'Var' has become a 'Size'
toSize :: Var base elem -> Size base elem
toSize (Count n) = Count n

-- | When the 'Var' has become a 'Index'
toIndex :: Var base elem -> Index base elem
toIndex (Count i) = Count i

-- | A count of things in an element can be lifted to a count of things in a vector
joinVar :: MVector base elem s a -> Var a x -> Var base x
joinVar _ = \(Count n) -> Count n

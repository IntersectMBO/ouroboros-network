{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Testing dependent functions
module Ouroboros.Consensus.Util.DepFn (
    -- * Infrastructure: generic
    All
    -- * Testing dependent functions
  , DepArgs(..)
  , DepFn(..)
  , SingArbitrary(..)
  , SingShow(..)
  ) where

import           Data.Kind (Constraint, Type)
import           Test.QuickCheck

import           Ouroboros.Consensus.Util.Singletons

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
  All _ '[]       = ()
  All f (x ': xs) = (f x, All f xs)

{-------------------------------------------------------------------------------
  'Testable' support for dependently typed functions
-------------------------------------------------------------------------------}

data DepArgs :: [k -> Type] -> k -> Type where
  Nil  :: DepArgs '[] t
  (:*) :: f t -> DepArgs fs t -> DepArgs (f ': fs) t

infixr :*

newtype DepFn (fs :: [k -> Type]) a = DepFn {
    applyDepFn :: forall (t :: k). Sing (t :: k) -> DepArgs fs t -> a
  }

class SingArbitrary (f :: k -> Type) where
  singArbitrary :: Sing t -> Gen (f t)

  singShrink :: Sing t -> f t -> [f t]
  singShrink _ _ = []

class SingShow (f :: k -> Type) where
  singShow :: Sing t -> f t -> String

instance (All SingArbitrary fs, SingI fs) => SingArbitrary (DepArgs fs) where
  singArbitrary (st :: Sing t) = go sing
    where
      go :: All SingArbitrary fs' => Sing fs' -> Gen (DepArgs fs' t)
      go SNil       = return Nil
      go (SCons ss) = (:*) <$> singArbitrary st <*> go ss

  singShrink (st :: Sing t) = go
    where
      go :: All SingArbitrary fs' => DepArgs fs' t -> [DepArgs fs' t]
      go Nil       = return Nil
      go (x :* xs) = (:*) <$> singShrink st x <*> go xs

instance All SingShow fs => SingShow (DepArgs fs) where
  singShow _              Nil       = "[]"
  singShow (st :: Sing t) (x :* xs) = "[" ++ singShow st x ++ go xs
    where
      go :: All SingShow fs' => DepArgs fs' t -> String
      go Nil         = "]"
      go (x' :* xs') = "," ++ singShow st x' ++ go xs'

instance ( SingKind k
         , Arbitrary (Demote k)
         , All SingArbitrary fs
         , All SingShow fs
         , SingI fs
         , Testable a
         ) => Testable (DepFn (fs :: [k -> Type]) a) where
  property (DepFn f) = property $ do
    t :: Demote k <- arbitrary
    withSomeSing t $ \st -> do
      return $ forAllShrink
                 (shownSingAs st <$> singArbitrary st)
                 (liftReshow st $ singShrink st)
                 (f st . fromShownAs)

{-------------------------------------------------------------------------------
  Internal: labelling
-------------------------------------------------------------------------------}

data ShownAs a = ShownAs String a

fromShownAs :: ShownAs a -> a
fromShownAs (ShownAs _ a) = a

shownSingAs :: SingShow f => Sing a -> f a -> ShownAs (f a)
shownSingAs sa fa = ShownAs (singShow sa fa) fa

liftReshow :: (SingShow g, Functor t)
           => Sing b -> (f a -> t (g b)) -> ShownAs (f a) -> t (ShownAs (g b))
liftReshow sb f (ShownAs _ a) = fmap (\b -> ShownAs (singShow sb b) b) (f a)

instance Show (ShownAs a) where
  show (ShownAs l _) = l

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

data Univ = Two | Many

instance Arbitrary Univ where
  arbitrary = elements [Two, Many]

data instance Sing (k :: Univ) where
  SingTwo  :: Sing ('Two  :: Univ)
  SingMany :: Sing ('Many :: Univ)

instance SingI ('Two  :: Univ) where sing = SingTwo
instance SingI ('Many :: Univ) where sing = SingMany

instance SingKind Univ where
  type Demote Univ = Univ

  fromSing SingTwo  = Two
  fromSing SingMany = Many

  toSing Two  = SomeSing SingTwo
  toSing Many = SomeSing SingMany

data Val :: Univ -> Type where
  ValTwo  :: Bool -> Val 'Two
  ValMany :: Int  -> Val 'Many

deriving instance Show (Val t)

instance Eq (Val t) where
  ValTwo  x == ValTwo  y = x == y
  ValMany x == ValMany y = x == y

instance SingShow Val where
  singShow _ = show

instance SingArbitrary Val where
  singArbitrary SingTwo  = ValTwo  <$> arbitrary
  singArbitrary SingMany = ValMany <$> arbitrary

prop_example :: DepFn '[Val, Val] Bool
prop_example = DepFn $ \_sing (x :* y :* Nil) -> x == y

_example :: IO ()
_example = quickCheck prop_example

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Provide generic `Act` instances, in two cases
--
--   data Product a b = Product a b
--
--   instance Act (Last a) a where
--     act (Last Nothing)  a = a
--     act (Last (Just a)) _ = a
--
--   instance Act (Last a) (Product a) where
--     act = gact
--
-- or where two product are matched piecewise (e.g. field by field)
--
--   instance Act (Last a) (Identity a) where
--     act (Last Nothing)  a = a
--     act (Last (Just a)) _ = Identity a
--
--   data Product' f = Product' (f a) (f b)
--     derive Generic
--
--   instance Act (Product' Last) (Product' Identity) where
--     act = gpact
--   
module Data.Act.Generic
  ( gact
  , GenericAct (..)
  , gpact
  ) where

import Data.Act
import GHC.Generics

-- | Action of `s` on `a` via `Rep a`.
--
gact :: (Generic a, GAct s (Rep a))
     => s -> a -> a
gact s a = to (s `gact'` from a)

class GAct s g where
  gact' :: s -> g p -> g p

instance GAct s U1 where
  gact' _ _ = U1
  
instance GAct s V1 where
  gact' s a = s `seq` a `seq` error "GAct.V1: gact'"

instance Act s a => GAct s (K1 i a) where
  gact' s (K1 a) = K1 (s `act` a)

instance GAct s f => GAct s (M1 i' c' f) where
  gact' s (M1 a) = M1 (s `gact'` a)

instance (GAct s f, GAct s g) => GAct s (f :*: g) where
  gact' s (a0 :*: a1) = s `gact'` a0 :*: s `gact'` a1

instance (GAct s f, GAct s g) => GAct s (f :+: g) where
  gact' s (L1 a) = L1 (s `gact'` a)
  gact' s (R1 a) = R1 (s `gact'` a)

-- | A newtype wrapper for deriving via.
--
newtype GenericAct s a = GenericAct { getGenericAct :: a }

instance (Generic s, Generic a, GAct s (Rep a), Semigroup s) => Act s (GenericAct s a) where
  act s (GenericAct a) = GenericAct (s `gact` a)


-- | Action of `s` on `a` via action of `Rep s` on `Rep a`.
--
gpact :: (Generic s, Generic a, GPAct (Rep s) (Rep a))
     => s -> a -> a
gpact s a = to (from s `gpact'` from a)

class GPAct f g where
  gpact' :: f p -> g p -> g p

instance GPAct s U1 where
  gpact' _ _ = U1
  
instance GPAct s V1 where
  gpact' s a = s `seq` a `seq` error "GPAct.V1: gact'"

instance Act s a => GPAct (K1 i' s) (K1 i a) where
  gpact' (K1 s) (K1 a) = K1 (s `act` a)

instance GPAct s f => GPAct (M1 i' c' s) (M1 i' c' f) where
  gpact' (M1 s) (M1 a) = M1 (s `gpact'` a)

instance (GPAct s f, GPAct s' f') => GPAct (s :*: s') (f :*: f') where
  gpact' (s0 :*: s1) (a0 :*: a1) = s0 `gpact'` a0 :*: s1 `gpact'` a1

-- we don't provide `GPAct` for `:+:`

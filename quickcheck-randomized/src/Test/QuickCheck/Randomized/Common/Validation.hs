{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.QuickCheck.Randomized.Common.Validation (
  -- * Validation
  Breadcrumb,
  Invalidities (..),
  Validate (..),
  assertValid,
  nullInvalidities,
  invalidity,
  invalidityUnless,
  innerInvalidities,
  validateViaRep,
  withValid,
  -- * The definition of 'validateViaRep'
  ValidateField (..),
  ValidateFields (..),
  ValidateRep (..),
  ) where

import           Control.Exception (assert)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Semigroup (Max (..), Min (..))
import           GHC.Generics
import           GHC.TypeLits

import           Test.QuickCheck

-- | A data name, a constructor name, a field name, a field number, etc.
type Breadcrumb = String

data Invalidities = Invalidities [String] (Map.Map Breadcrumb Invalidities)
  deriving (Show)

instance Monoid Invalidities where
  mempty = Invalidities [] Map.empty

-- | Accumulative
instance Semigroup Invalidities where
  Invalidities l1 l2 <> Invalidities r1 r2 =
      Invalidities (l1 <> r1) (Map.unionWith (<>) l2 r2)

-- | There are no invalidities.
nullInvalidities :: Invalidities -> Bool
nullInvalidities (Invalidities msgs inners) =
    null msgs && all nullInvalidities inners

-- | @'assert' False@ unless 'nullInvalidities'.
assertValid :: Validate a => a -> b -> b
assertValid = assert . nullInvalidities . validate

-- | @'property' False@ unless 'nullInvalidities'.
withValid :: Validate a => a -> Property -> Property
withValid a prop =
    if   nullInvalidities invalidities
    then prop
    else counterexample (show invalidities) False
  where
    invalidities = validate a

-- | An invalidity.
invalidity :: String -> Invalidities
invalidity msg = Invalidities [msg] Map.empty

-- | An 'invalidity', conditionally.
invalidityUnless :: Bool -> String -> Invalidities
invalidityUnless b msg = if b then mempty else invalidity msg

-- | Include the invalidities of part of a value as sub-invalidities scoped
-- appropriately with the given 'BreadCrumb'.
innerInvalidities :: Breadcrumb -> Invalidities -> Invalidities
innerInvalidities turn invalidities
    | nullInvalidities invalidities = mempty
    | otherwise                     = Invalidities [] (Map.singleton turn invalidities)

class Validate a where
  validate :: a -> Invalidities

  default validate :: (Generic a, ValidateRep (Rep a)) => a -> Invalidities
  validate = validateViaRep

instance Validate ()
instance (Validate a, Validate b) => Validate (a, b)
instance (Validate a, Validate b, Validate c) => Validate (a, b, c)
instance
     (Validate a, Validate b, Validate c, Validate d)
  => Validate (a, b, c, d)
instance
     (Validate a, Validate b, Validate c, Validate d, Validate e)
  => Validate (a, b, c, d, e)
instance
     (Validate a, Validate b, Validate c, Validate d, Validate e, Validate f)
  => Validate (a, b, c, d, e, f)
instance
     (Validate a, Validate b, Validate c, Validate d, Validate e, Validate f, Validate g)
  => Validate (a, b, c, d, e, f, g)

instance Validate a => Validate (Min a)
instance Validate a => Validate (Max a)

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

-- | Use "GHC.Generics" to validate every field of every constructor.
validateViaRep :: (Generic a, ValidateRep (Rep a)) => a -> Invalidities
validateViaRep = validateRep . from

class ValidateRep r where
  validateRep :: r x -> Invalidities

instance (Datatype meta, ValidateRep rep) => ValidateRep (M1 D meta rep) where
  validateRep d@(M1 x) = innerInvalidities ("type " <> datatypeName d) $ validateRep x

instance ValidateRep V1 where validateRep _ = mempty

instance (ValidateRep l, ValidateRep r) => ValidateRep (l :+: r) where
  validateRep = \case
      L1 l -> validateRep l
      R1 r -> validateRep r

instance (Constructor meta, ValidateFields rep) => ValidateRep (M1 C meta rep) where
  validateRep c@(M1 x) = innerInvalidities ("constructor " <> conName c) $ validateFields 0 x

class ValidateFields r where
  validateFields :: Int -> r x -> Invalidities

instance ValidateFields U1 where validateFields _ _ = mempty

instance (ValidateFields l, ValidateFields r) => ValidateFields (l :*: r) where
  validateFields !k (l :*: r) =
      validateFields k l <> validateFields (k + 1) r

instance
     ValidateField rep
  => ValidateFields (M1 S (MetaSel Nothing su ss ds) rep) where
  validateFields !k (M1 x) =
      innerInvalidities ("field #" <> show k) $ validateField x

instance
     (KnownSymbol n, ValidateField rep)
  => ValidateFields (M1 S (MetaSel (Just n) su ss ds) rep) where
  validateFields !_k (M1 x) = innerInvalidities n $ validateField x
    where
      n = "field " <> symbolVal (Proxy :: Proxy n)

class ValidateField r where
  validateField :: r x -> Invalidities

instance Validate a => ValidateField (K1 i a) where
  validateField (K1 x) = validate x

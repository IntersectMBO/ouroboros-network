{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Condensed but human-readable output (like 'Show').
module Util.Condense (
    Condense (..)
  , Condense1 (..)
  , condense1
  ) where

import qualified Data.ByteString as BS.Strict
import qualified Data.ByteString.Lazy as BS.Lazy
import           Data.Int
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text, unpack)
import           Data.Void
import           Data.Word
import           Numeric.Natural
import           Text.Printf (printf)

{-------------------------------------------------------------------------------
  Main class
-------------------------------------------------------------------------------}

-- | Condensed but human-readable output
class Condense a where
  condense :: a -> String

{-------------------------------------------------------------------------------
  Rank-1 types
-------------------------------------------------------------------------------}

class Condense1 f where
  liftCondense :: (a -> String) -> f a -> String

-- | Lift the standard 'condense' function through the type constructor
condense1 :: (Condense1 f, Condense a) => f a -> String
condense1 = liftCondense condense

{-------------------------------------------------------------------------------
  Default instances for deriving-via
-------------------------------------------------------------------------------}

newtype CondenseAsShow a = CondenseAsShow {getCondenseAsShow :: a}
  deriving Show

instance Show a => Condense (CondenseAsShow a) where
  condense = show

{-------------------------------------------------------------------------------
  Instances for standard types
-------------------------------------------------------------------------------}

instance Condense Void where
  condense = absurd

instance Condense Text where
  condense = unpack

deriving via CondenseAsShow Bool instance Condense Bool
deriving via CondenseAsShow Int instance Condense Int
deriving via CondenseAsShow Int64 instance Condense Int64
deriving via CondenseAsShow Word instance Condense Word
deriving via CondenseAsShow Word32 instance Condense Word32
deriving via CondenseAsShow Word64 instance Condense Word64
deriving via CondenseAsShow Natural instance Condense Natural

instance Condense Rational where
  condense = printf "%.8f" . (fromRational :: Rational -> Double)

instance Condense1 [] where
  liftCondense f as = "[" ++ intercalate "," (map f as) ++ "]"

instance Condense1 Set where
  liftCondense f = liftCondense f . Set.toList

instance Condense a => Condense [a] where
  condense = condense1

instance Condense a => Condense (Maybe a) where
  condense (Just a) = "Just " ++ condense a
  condense Nothing  = "Nothing"

instance Condense a => Condense (Set a) where
  condense = condense1

instance (Condense a, Condense b) => Condense (a, b) where
  condense (a, b) = "(" ++ intercalate "," [condense a, condense b] ++ ")"

instance (Condense a, Condense b, Condense c) => Condense (a, b, c) where
  condense (a, b, c) = "(" ++ intercalate "," [condense a, condense b, condense c] ++ ")"

instance (Condense a, Condense b, Condense c, Condense d) => Condense (a, b, c, d) where
  condense (a, b, c, d) = "(" ++ intercalate "," [condense a, condense b, condense c, condense d] ++ ")"

instance (Condense a, Condense b, Condense c, Condense d, Condense e) => Condense (a, b, c, d, e) where
  condense (a, b, c, d, e) = "(" ++ intercalate "," [condense a, condense b, condense c, condense d, condense e] ++ ")"

instance (Condense k, Condense a) => Condense (Map k a) where
  condense = condense . Map.toList

instance Condense BS.Strict.ByteString where
  condense bs = show bs ++ "<" ++ show (BS.Strict.length bs) ++ "b>"

instance Condense BS.Lazy.ByteString where
  condense bs = show bs ++ "<" ++ show (BS.Lazy.length bs) ++ "b>"

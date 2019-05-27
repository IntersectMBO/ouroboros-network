{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Util.Condense (
    Condense(..)
  , Condense1(..)
  , condense1
  ) where

import qualified Data.ByteString as BS.Strict
import qualified Data.ByteString.Lazy as BS.Lazy
import           Data.Int
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Numeric.Natural
import           Text.Printf (printf)

import           Ouroboros.Consensus.Util.HList (All, HList (..))
import qualified Ouroboros.Consensus.Util.HList as HList

import           Ouroboros.Storage.FS.API.Types (AllowExisting (..),
                     OpenMode (..), SeekMode (..))

-- | Condensed but human-readable output
class Condense a where
  condense :: a -> String

class Condense1 f where
  liftCondense :: (a -> String) -> f a -> String

-- | Lift the standard 'condense' function through the type constructor
condense1 :: (Condense1 f, Condense a) => f a -> String
condense1 = liftCondense condense

instance Condense String where
  condense = id

instance Condense Bool where
  condense = show

instance Condense Int where
  condense = show

instance Condense Int64 where
  condense = show

instance Condense Word where
  condense = show

instance Condense Word64 where
  condense = show

instance Condense Natural where
  condense = show

instance Condense Rational where
  condense = printf "%.8f" . (fromRational :: Rational -> Double)

instance Condense1 [] where
  liftCondense f as = "[" ++ intercalate "," (map f as) ++ "]"

instance Condense1 Set where
  liftCondense f = liftCondense f . Set.toList

instance {-# OVERLAPPING #-} Condense [String] where
  condense ss = "[" ++ intercalate "," ss ++ "]"

instance {-# OVERLAPPABLE #-} Condense a => Condense [a] where
  condense = condense1

instance Condense a => Condense (Set a) where
  condense = condense1

instance All Condense as => Condense (HList as) where
  condense as = "(" ++ intercalate "," (HList.collapse (Proxy @Condense) condense as) ++ ")"

instance (Condense a, Condense b) => Condense (a, b) where
  condense (a, b) = condense (a :* b :* Nil)

instance (Condense a, Condense b, Condense c) => Condense (a, b, c) where
  condense (a, b, c) = condense (a :* b :* c :* Nil)

instance (Condense a, Condense b, Condense c, Condense d) => Condense (a, b, c, d) where
  condense (a, b, c, d) = condense (a :* b :* c :* d :* Nil)

instance (Condense a, Condense b, Condense c, Condense d, Condense e) => Condense (a, b, c, d, e) where
  condense (a, b, c, d, e) = condense (a :* b :* c :* d :* e :* Nil)

instance (Condense k, Condense a) => Condense (Map k a) where
  condense = condense . Map.toList

instance Condense SeekMode where
  condense RelativeSeek = "r"
  condense AbsoluteSeek = "a"
  condense SeekFromEnd  = "e"

instance Condense AllowExisting where
  condense AllowExisting = ""
  condense MustBeNew     = "!"

instance Condense OpenMode where
    condense ReadMode           = "r"
    condense (WriteMode     ex) = "w"  ++ condense ex
    condense (ReadWriteMode ex) = "rw" ++ condense ex
    condense (AppendMode    ex) = "a"  ++ condense ex


instance Condense BS.Strict.ByteString where
  condense bs = show bs ++ "<" ++ show (BS.Strict.length bs) ++ "b>"

instance Condense BS.Lazy.ByteString where
  condense bs = show bs ++ "<" ++ show (BS.Lazy.length bs) ++ "b>"

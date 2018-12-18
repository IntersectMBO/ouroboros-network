{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Util.Condense (
    Condense(..)
  ) where

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric.Natural
import           Text.Printf (printf)

import           Ouroboros.Consensus.Util.HList (All, HList (..))
import qualified Ouroboros.Consensus.Util.HList as HList

-- | Condensed but human-readable output
class Condense a where
  condense :: a -> String

instance Condense String where
  condense = id

instance Condense Int where
  condense = show

instance Condense Word where
  condense = show

instance Condense Natural where
  condense = show

instance Condense Rational where
  condense = printf "%.8f" . (fromRational :: Rational -> Double)

instance {-# OVERLAPPING #-} Condense [String] where
  condense ss = "[" ++ intercalate "," ss ++ "]"

instance {-# OVERLAPPABLE #-} Condense a => Condense [a] where
  condense as = "[" ++ intercalate "," (map condense as) ++ "]"

instance All Condense as => Condense (HList as) where
  condense as = "(" ++ intercalate "," (HList.collapse (Proxy @Condense) condense as) ++ ")"

instance (Condense a, Condense b) => Condense (a, b) where
  condense (a, b) = condense (a :* b :* Nil)

instance Condense a => Condense (Set a) where
  condense = condense . Set.toList

instance (Condense k, Condense a) => Condense (Map k a) where
  condense = condense . Map.toList

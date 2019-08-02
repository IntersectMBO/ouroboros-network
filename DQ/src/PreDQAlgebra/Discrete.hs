module PreDQAlgebra.Discrete
    ( Disc
    , singleton
    , exact
    , mix
    , uniformNat
    ) where

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           PreDQAlgebra.Class
import           PreDQAlgebra.NoVariance

newtype Disc p t = Disc (Map (NoVar t) p)
    deriving (Show, Read, Eq, Ord)

instance (Num p, Ord t, Semigroup t) => Semigroup (Disc p t) where
    (<>) = fromBinary (<>)

instance (Num p, Ord t, Monoid t) => Monoid (Disc p t) where
    mempty = Disc $ Map.singleton mempty 1

instance (Num p, Ord t, Monoid t) => PreDQAlgebra (Disc p t) where
    firstToFinish = fromBinary firstToFinish
    lastToFinish = fromBinary lastToFinish
    bottom = singleton bottom

fromBinary :: (Num p, Ord t)
           => (NoVar t -> NoVar t -> NoVar t)
           -> Disc p t
           -> Disc p t
           -> Disc p t
fromBinary op (Disc a) (Disc b) = Disc $ foldl' f Map.empty
    [ (ta `op` tb, pa * pb)
    | (ta, pa) <- Map.toList a
    , (tb, pb) <- Map.toList b
    ]
  where
    f m (t, p) = Map.insertWith (+) t p m

singleton :: Num p => NoVar t -> Disc p t
singleton t = Disc $ Map.singleton t 1

exact :: Num p => t -> Disc p t
exact = singleton . Exact

mix :: (Ord t, Num p, Ord p) => p -> Disc p t -> Disc p t -> Disc p t
mix p x@(Disc a) y@(Disc b)
    | p <= 0    = y
    | p >= 1    = x
    | otherwise = let q = 1 - p in Disc $ Map.unionWith (+) ((* p) <$> a) ((* q) <$> b)


uniformNat :: Natural -> Natural -> Disc Rational Nat
uniformNat a b
    | a > b     = bottom
    | otherwise =
        let n = b - a + 1
            p = recip $ fromIntegral n
        in  Disc $ Map.fromList [(Exact $ Nat t, p) | t <- [a .. b]]

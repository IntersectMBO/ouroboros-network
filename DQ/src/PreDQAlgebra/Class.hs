module PreDQAlgebra.Class
    ( PreDQAlgebra (..)
    , convolve
    , perf
    ) where

class Monoid a => PreDQAlgebra a where
    firstToFinish :: a -> a -> a
    lastToFinish :: a -> a -> a
    bottom :: a

convolve :: PreDQAlgebra a => a -> a -> a
convolve = (<>)

perf :: PreDQAlgebra a => a
perf = mempty

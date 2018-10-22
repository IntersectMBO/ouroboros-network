{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test.ArbitrarySt where

import Test.GlobalState

class ArbitrarySt p a | a -> p where
    arbitrarySt :: GenSt p a

    shrinkSt :: a -> [a]
    shrinkSt _ = []




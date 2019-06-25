{-# LANGUAGE OverloadedStrings #-}
module Shim
where

import DeltaQ.Algebra.Class
import Text.LaTeX hiding (Bottom)
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Base.Class (comm0)

asLaTeX :: DeltaQ p d n -> LaTeX
asLaTeX dq  =  math $ 
  case dq of
    Bottom           -> bot --  '⊥'
    Unit             -> comm0 "emptyset" --  '∅'
    Delay _          -> "DELAY"
    ProbChoice p a b -> undefined
    Convolve a b     -> undefined    

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Shim
where

import DeltaQ.Algebra.Class
import Text.LaTeX hiding (Bottom)
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Base.Class
import Data.Ratio

asLaTeX :: Texy t => t -> LaTeX
asLaTeX = execLaTeXM . mathDisplay . texy

instance (RealFrac p) => Texy (DeltaQ p n d) where
  texy x
   = case x of
       Bottom           -> bot --  '⊥' / bottom
       Unit             -> comm0 "emptyset" --  '∅' / perfection
       Delay _          -> "DELAY"
       ProbChoice p a b -> pchoice p (condBracket a) (condBracket b) -- '⇋' / choice
       Convolve a b     -> oplus (condBracket a) (condBracket b) -- '⊕' / convolve
    where
      pchoice p = between $ (lrharp p)

      lrharp p
        = let r = approxRational p (fromRational $ toRational 1e-6)
          in (operatorname (commS "leftrightharpoons"))
             !^ (texy $ numerator r, texy $ denominator r)

      condBracket x =
        case x of
          ProbChoice _ _ _ -> autoParens $ texy x
          Convolve _ _     -> autoParens $ texy x
          _                -> texy x

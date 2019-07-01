{-# LANGUAGE OverloadedStrings #-}
module Shim 
-- (asICDF, asICDF', DelayExtension(..))
where

import           Control.Monad.Reader
import           Data.Ratio
import           DeltaQ.Algebra.Class
import           DeltaQ.Algebra.DelayModel.SimpleUniform
import qualified DeltaQ.RationalProbabilityDoubleDelay as Concrete
import           DeltaQ.Visualisation.ChartPlot
import           Graphics.Rendering.Chart.Easy
import           IHaskell.Display
import           IHaskell.Display.Hatex ()
import           Numeric
import           System.Random.MWC
import           Text.LaTeX hiding (Bottom)
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Packages.AMSMath

-- | Basic (textual) display of DeltaQ values in expression style
instance (RealFrac p, Texy (n d)) => IHaskellDisplay (DeltaQ p n d) where
  display = display . execLaTeXM . mathDisplay . texy

instance (RealFrac p, Texy (n d)) => Texy (DeltaQ p n d) where
  texy x
   = case x of
       Bottom           -> bot --  '⊥' / bottom
       Unit             -> comm0 "emptyset" --  '∅' / perfection
       Delay d          -> texy d
       ProbChoice p a b -> pchoice p (condBracket a) (condBracket b) -- '⇋' / choice
       Convolve a b     -> oplus (condBracket a) (condBracket b) -- '⊕' / convolve
    where
      pchoice p = between $ (lrharp p)
      
      lrharp p
        = let r = approxRational p (fromRational $ toRational 1e-6)
          in (operatorname (commS "leftrightharpoons"))
             !^ (texy $ denominator r, texy $ numerator r)

      condBracket x =
        case x of
          ProbChoice _ _ _ -> autoParens $ texy x
          Convolve _ _     -> autoParens $ texy x
          _                -> texy x

instance (RealFloat r) => Texy (SimpleUniform r) where
  texy x = 
    case x of
      DiracDelta t -> delta <> lfloor <> fmt t <> rceil
      UniformD   t -> (commS "sqcap") <> autoSquareBrackets (fmt t)
   where
      fmt x = fromString $ showGFloatAlt (Just 2) x ""

{-
-- | Plotting support for DeltaQ

iCDFPlot :: MonadIO m
         => String -- ^ title
         -> String -- ^ x axis label
         -> String -- ^ y axis label
         -> Concrete.DeltaQ
         -> m (Renderable ())
iCDFPlot title x'label y'label dq = do
  gen <- liftIO $ createSystemRandom
  layout_title .= title
  v <- (flip runReaderT) def $ generateCDFPlot gen "" dq
  
  return $ toRenderable v
-}

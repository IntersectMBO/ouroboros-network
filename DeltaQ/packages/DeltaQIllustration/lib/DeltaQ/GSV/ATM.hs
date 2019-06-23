{-# LANGUAGE FlexibleContexts #-}

module DeltaQ.GSV.ATM
       ( atmGSV )
where
  
import DeltaQ.Algebra
import DeltaQ.GSV  

-- | models an ATM data translocation facility where boundaries of
--   interest are cell boundaries, assumes no 'V'
atmGSV :: (DelayModel d Double) =>  Double -- ^ cell rate per second
          -> NetElementGSV p d Double
atmGSV x 
  = EGSV { neG = uniform0 cell'service'time
         , neS = \o -> let (a,b) = quotRem o 48
                       in fixed $ cell'service'time * 
                          fromIntegral (a + signum b)
         , neV = perfection
         , neMTU = Just 48
         }
  where
    cell'service'time = recip x

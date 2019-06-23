{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module DeltaQ.GSV
       ( QualityAttenuator (..)
       , CanonicalGSV (..)
       , NetElementGSV (..)
       , noAttenutation
       , pathGSV
       )
where       
  
import DeltaQ.Algebra
  
-- | G,S and V are the measurements of the ∆Q for this network
-- element, a good initution for the components are:
--
--   * G: The time take for an 'empty' packet (S==0) to traverse this
--        element, given that the element was idle. This measure
--        includes the fixed overheads of any header/framing and also
--        the element of ∆Q that corresponds to getting access to
--        medium (where that ∆Q is load independent - e.g. time for
--        the next ATM cell boundary to arrive)
--
--   * S: The additional time take for extra octets in the packets (it
--        is a piecewise continuous function), typically it is monotonic
--
--   * V: This is the variation due to other contention for the resource
--

class QualityAttenuator (a :: * -> (* -> *) -> * -> * ) where
  toCanonicalGSV :: a p d n -> CanonicalGSV p d n
  toCanonicalGSV x 
    = CGSV (g x) (s x) (v x)
  g :: a p d n -> DeltaQ p d n
  s :: a p d n -> Int -> DeltaQ p d n
  v :: a p d n -> DeltaQ p d n


-- | Canonical attenuator model
data CanonicalGSV p d n
 = CGSV { cG   :: DeltaQ p d n        
        , cS   :: Int -> DeltaQ p d n 
        , cV   :: DeltaQ p d n        
        } 
instance QualityAttenuator CanonicalGSV  where
  g = cG
  s = cS
  v = cV

-- | the model of a single network element
data NetElementGSV p d n 
 = EGSV { neG   :: DeltaQ p d n        -- ^ The 'G' for this element
        , neS   :: Int -> DeltaQ p d n -- ^ the 'S' for this element, given the canonical 'packet' size
        , neV   :: DeltaQ p d n        -- ^ The 'V' for this element
        , neMTU :: Maybe Int           -- ^ The MTU for this network element (if defined)
        } 

instance QualityAttenuator NetElementGSV  where
  g = neG
  s = neS
  v = neV
  
noAttenutation :: CanonicalGSV p d n
noAttenutation 
  = CGSV perfection (const perfection) perfection

pathGSV :: (QualityAttenuator a) => [a p d n] -> CanonicalGSV p d n
pathGSV = foldr norm noAttenutation . map toCanonicalGSV
   where 
    norm :: CanonicalGSV p d n -> CanonicalGSV p d n -> CanonicalGSV p d n
    norm (CGSV g1 s1 v1) (CGSV g2 s2 v2)
      = CGSV (g1 `Convolve` g2)
             (\x -> s1 x `Convolve` s2 x) 
             (v1 `Convolve` v2) 










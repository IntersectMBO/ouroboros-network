module DeltaQ.LinkRestriction where

import Numeric.Natural
import Data.Time.Clock
import Data.Ord

import DeltaQ.Topography
import DeltaQ.SimpleGS

-- | Note that we are primarily interested in resource consumption. So
--   "service time" here should be indicative of the time the resource
--   is occupied servicing the request, rather than the time before it
--   could be said to have been "sent".
data LinkRestriction = LinkR
  { e2ePDUServiceTime :: Natural -> DiffTime
    -- ^ The service rate in bits per second of "the link" (with all
    --   serialisation costs included, where appropriate)
  , e2eMaxSDU         :: Natural
    -- ^ The maximum size of the link SDU, ie maximum size of a IP
    --   packet
  }

instance Show LinkRestriction where
  show x = show (linkRestrictionAsSimpleGS x)
           ++ "[" ++ show (e2eMaxSDU x) ++ "]"

instance Eq LinkRestriction where
  a == b = compare a b == EQ

instance Ord LinkRestriction where
  compare = comparing (\x -> (e2eMaxSDU x, e2ePDUServiceTime x 1))

-- | the most restrictive is the one with the larger service time and
--   the smaller MTU. Note there is a linearity assumption being made
--   here which is a simplification of real world. We are ignoring
--   details of quantization and minimum frame size effects.
instance Semigroup LinkRestriction where
 a <> b
   | (eval a >= eval b) = a { e2eMaxSDU = e2eMaxSDU a `min` e2eMaxSDU b}
   |  otherwise         = b { e2eMaxSDU = e2eMaxSDU a `min` e2eMaxSDU b}
   where
     eval x = e2ePDUServiceTime x 1

instance Monoid LinkRestriction where
  mempty = LinkR (const 0) (10^12) -- way out there, but not quite unbounded!

type BitServiceRate = Double

-- | Express a `LinkRestriction` as a ∆Q by evaluating it for SDUs of
--   size 0 and MTU
linkRestrictionAsSimpleGS :: LinkRestriction -> SimpleGS
linkRestrictionAsSimpleGS lr = r
  where
   g = e2ePDUServiceTime lr 0
   r = SimpleDeltaQ g (\n -> m * (fromIntegral n))
   m = ((e2ePDUServiceTime lr $ e2eMaxSDU lr) - g)
       / (fromIntegral $ e2eMaxSDU lr)

-- | Simple model of bits on a wire, no overheads.
mkRestriction :: BitServiceRate
              -> Natural
              -> LinkRestriction
mkRestriction =  mkRestriction' id 

-- | Generic model with an overhead included.
mkRestriction' :: (Natural -> Natural)
               -> BitServiceRate
               -> Natural
               -> LinkRestriction
mkRestriction' q r s
  = LinkR  (\n -> fromRational $ no'bits n * bps) s
  where
    no'bits x = 8 * fromIntegral (q x)
    bps       = recip $ toRational r
    
ethernetR, ethernetVlanR :: BitServiceRate
          -> Natural
          -> LinkRestriction
ethernetR     = ethernetR' 0
ethernetVlanR = ethernetR' 1

-- | Model of processing time for an ethernet frame with a given
--   number of VLANs
ethernetR' :: Int -- ^ number of VLANs
           -> BitServiceRate
           -> Natural
           -> LinkRestriction
ethernetR' n = mkRestriction' pdh
  where
    pdh = (pdh'size +)
    pdh'size
      = preamble + sof + src'mac + dst'mac
        + (fromIntegral n) * vlan
        + fcs + ipg
    preamble = 7
    sof      = 1
    src'mac  = 6
    dst'mac  = 6
    vlan     = 4
    fcs      = 4
    ipg      = 12

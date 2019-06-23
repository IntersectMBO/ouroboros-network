{-# LANGUAGE FlexibleContexts #-}

module DeltaQ.GSV.Ethernet
       ( ethernetGSV )
where

import DeltaQ.Algebra
import DeltaQ.GSV

-- | models a non-vlan IEEE 802.3 Ethernet frame where the
-- outcome-of-insterest is the start of the frame transmission (i.e
-- preamble) upto the CRC. The simplification is that the maximum
-- frame SDU (the IP PDDU) is 1500.
--
-- This simplified model does not capture the context sensitive nature
-- of Ethernet load. It does not model the inter-packet gap. The
-- service time of ethernet packet (after which it can be forwarded)
-- is not the same as the interval of time for which the ethernet
-- mediuam is 'busy' (i.e. can't be reused.)
ethernetGSV :: (DelayModel d Double)
            =>  Double -- ^ bit rate (per second)
            -> NetElementGSV p d Double
ethernetGSV x
  = EGSV { neG = octets'to'interval fixed'frame'overheads
         , neS = \o -> octets'to'interval (o `max` min'frame'sdu)
         , neV = perfection
         , neMTU = Just 1500
         }
  where
    fixed'frame'overheads :: Int
    fixed'frame'overheads
      = preamble + sof + src'mac + dst'mac + e'type + crc
    min'frame'sdu
      = 64 - (src'mac + dst'mac + e'type + crc)
    octets'to'interval n = δ $ (fromIntegral n) * 8 / x
    -- see https://en.wikipedia.org/wiki/Ethernet_frame#Ethernet_II for illustration
    preamble = 7
    sof      = 1
    src'mac  = 6
    dst'mac  = 6
    e'type   = 2
    crc      = 4

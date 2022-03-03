-- This is a non-portable linux only interface.

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

#include "HsNet.h"

module Network.Mux.TCPInfo
  ( StructTCPInfo (..)
  , SocketOption(TCPInfoSocketOption)
  ) where

import           Foreign.C
import           Foreign.Storable (Storable (..))
import           Network.Socket(SocketOption(..))


pattern TCPInfoSocketOption :: SocketOption
pattern TCPInfoSocketOption = SockOpt 6 11

data StructTCPInfo = StructTCPInfo {
    tcpi_state  :: CUChar,
    tcpi_ca_state :: CUChar,
    tcpi_retransmits :: CUChar,
    tcpi_probes :: CUChar,
    tcpi_backoff :: CUChar,
    tcpi_options :: CUChar,
    tcpi_sndrcv_wscale :: CUChar,
    tcpi_delivery_rate_app_limited_fastopen_client_fail :: CUChar,

    tcpi_rto :: CUInt,
    tcpi_ato :: CUInt,
    tcpi_snd_mss :: CUInt,
    tcpi_rcv_mss :: CUInt,

    tcpi_unacked :: CUInt,
    tcpi_sacked :: CUInt,
    tcpi_lost :: CUInt,
    tcpi_retrans :: CUInt,
    tcpi_fackets :: CUInt,

    tcpi_last_data_sent :: CUInt,
    tcpi_last_ack_sent :: CUInt,
    tcpi_last_data_recv :: CUInt,
    tcpi_last_ack_recv :: CUInt,

    tcpi_pmtu :: CUInt,
    tcpi_rcv_ssthresh :: CUInt,
    tcpi_rtt :: CUInt,
    tcpi_rttvar :: CUInt,
    tcpi_snd_ssthresh :: CUInt,
    tcpi_snd_cwnd :: CUInt,
    tcpi_advmss :: CUInt,
    tcpi_reordering :: CUInt,

    tcpi_rcv_rtt :: CUInt,
    tcpi_rcv_space :: CUInt,

    tcpi_total_retrans :: CUInt
  }
  deriving (Eq, Ord, Show)

instance Storable StructTCPInfo where
    sizeOf    _ = (#const sizeof(struct tcp_info))
    alignment _ = alignment (0 :: CInt)

    peek p = do
      tcpi_state <- (#peek struct tcp_info, tcpi_state) p
      tcpi_ca_state <- (#peek struct tcp_info, tcpi_ca_state) p
      tcpi_retransmits <- (#peek struct tcp_info, tcpi_retransmits) p
      tcpi_probes <- (#peek struct tcp_info, tcpi_probes) p
      tcpi_backoff <- (#peek struct tcp_info, tcpi_backoff) p
      tcpi_options <- (#peek struct tcp_info, tcpi_options) p
      let tcpi_sndrcv_wscale = 0
          -- XXX can't cope with bitfields
          tcpi_delivery_rate_app_limited_fastopen_client_fail = 0

      tcpi_rto <- (#peek struct tcp_info, tcpi_rto) p
      tcpi_ato <- (#peek struct tcp_info, tcpi_ato) p
      tcpi_snd_mss <- (#peek struct tcp_info, tcpi_snd_mss) p
      tcpi_rcv_mss <- (#peek struct tcp_info, tcpi_rcv_mss) p

      tcpi_unacked <- (#peek struct tcp_info, tcpi_unacked) p
      tcpi_sacked <- (#peek struct tcp_info, tcpi_sacked) p
      tcpi_lost <- (#peek struct tcp_info, tcpi_lost) p
      tcpi_retrans <- (#peek struct tcp_info, tcpi_retrans) p
      tcpi_fackets <- (#peek struct tcp_info, tcpi_fackets) p

      tcpi_last_data_sent <- (#peek struct tcp_info, tcpi_last_data_sent) p
      tcpi_last_ack_sent <- (#peek struct tcp_info, tcpi_last_ack_sent) p
      tcpi_last_data_recv <- (#peek struct tcp_info, tcpi_last_data_recv) p
      tcpi_last_ack_recv <- (#peek struct tcp_info, tcpi_last_ack_recv) p

      tcpi_pmtu <- (#peek struct tcp_info, tcpi_pmtu) p
      tcpi_rcv_ssthresh <- (#peek struct tcp_info, tcpi_rcv_ssthresh) p
      tcpi_rtt <- (#peek struct tcp_info, tcpi_rtt) p
      tcpi_rttvar <- (#peek struct tcp_info, tcpi_rttvar) p
      tcpi_snd_ssthresh <- (#peek struct tcp_info, tcpi_snd_ssthresh) p
      tcpi_snd_cwnd <- (#peek struct tcp_info, tcpi_snd_cwnd) p
      tcpi_advmss <- (#peek struct tcp_info, tcpi_advmss) p
      tcpi_reordering <- (#peek struct tcp_info, tcpi_reordering) p

      tcpi_rcv_rtt <- (#peek struct tcp_info, tcpi_rcv_rtt) p
      tcpi_rcv_space <- (#peek struct tcp_info, tcpi_rcv_space) p

      tcpi_total_retrans <- (#peek struct tcp_info, tcpi_total_retrans) p
      return $ StructTCPInfo {..}


    -- This is a readonly kernel interface. We shouldn't be writing to
    -- this structure.
    poke _ _ = error "Writing to StructTCPInfo"

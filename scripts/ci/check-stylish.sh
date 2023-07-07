#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
# TODO CPP pragmas in export lists are not supported by stylish-haskell
fd . './network-mux' -e hs -E Setup.hs -E TCPInfo.hs -E Pipe.hs -E Channel.hs -E Bearer.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-api' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-framework' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-mock' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-protocols' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './cardano-client' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i

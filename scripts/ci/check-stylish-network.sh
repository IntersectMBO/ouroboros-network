#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
fd -p io-sim -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd -p io-classes -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd -p strict-stm -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd -p typed-protocols -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
# TODO CPP pragmas in export lists are not supported by stylish-haskell
fd -p network-mux -e hs -E Setup.hs -E network-mux/src/Network/Mux/TCPInfo.hs -E network-mux/src/Network/Mux/Bearer/Pipe.hs -E network-mux/src/Network/Mux/Channel.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd -p ouroboros-network-framework -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd -p cardano-client -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell-network.yaml -i

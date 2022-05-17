#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
# TODO the export of the <= operator TxLimits crashes stylish-haskell
fd -p ouroboros-consensus -e hs -E Setup.hs -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/TxLimits.hs -X stylish-haskell -c .stylish-haskell.yaml -i

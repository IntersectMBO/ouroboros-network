#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
# TODO the export of the <= operator TxLimits crashes stylish-haskell
# TODO the deprecation warning in the HD module will be removed by
# stylish-haskell, unless we update stylish-haskell to v0.14.3.0
fd -p ouroboros-consensus -e hs \
  -E Setup.hs \
  -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/TxLimits.hs \
  -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/HD.hs \
  -X stylish-haskell -c .stylish-haskell.yaml -i

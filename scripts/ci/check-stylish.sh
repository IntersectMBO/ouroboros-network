#!/usr/bin/env bash

set -euo pipefail

# TODO the export of the <= operator TxLimits crashes stylish-haskell
fd -p ouroboros-consensus -e hs -E Setup.hs -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/TxLimits.hs -X stylish-haskell -i

git diff --exit-code

#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
# TODO the export of the <= operator TxLimits crashes stylish-haskell
fd -p ouroboros-consensus \
    -e hs \
    -E Setup.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/TxLimits.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/InMemory.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/OnDisk.hs \
    -X stylish-haskell \
    -c .stylish-haskell.yaml -i

# We don't want these deprecation warnings to be removed accidentally
grep "DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/InMemory.hs >/dev/null 2>&1
grep "DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/OnDisk.hs   >/dev/null 2>&1

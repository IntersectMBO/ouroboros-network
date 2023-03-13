#!/usr/bin/env bash

set -e

export LC_ALL=C.UTF-8
fd -p ouroboros-consensus \
    -e hs \
    -E Setup.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/InMemory.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/OnDisk.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/Types.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/Impl/Pure.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/Impl/Types.hs\
    -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/Impl.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Mempool/TxLimits.hs \
    -E ouroboros-consensus-test/src/Test/Util/Classify.hs \
    -E ouroboros-consensus-cardano-tools/app/DBAnalyser/Parsers.hs \
    -X stylish-haskell \
    -c .stylish-haskell.yaml -i

# We don't want these deprecation warnings to be removed accidentally
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/InMemory.hs >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/OnDisk.hs   >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/LedgerDB/Types.hs    >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Mempool/Impl/Pure.hs         >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Mempool/Impl/Types.hs        >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Mempool/Impl.hs              >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Mempool/TxLimits.hs          >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/Classify.hs                       >/dev/null 2>&1

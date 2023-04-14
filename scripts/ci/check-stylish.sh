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
    -E ouroboros-consensus/src/Ouroboros/Consensus/Util/Counting.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Util/SOP.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Util/OptNP.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Functors.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/InPairs.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Match.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Telescope.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/DerivingVia.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Tails.hs \
    -E ouroboros-consensus-test/src/Test/Util/Classify.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/API.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/API/Types.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/CRC.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/Handle.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/IO.hs \
    -E ouroboros-consensus/src/Ouroboros/Consensus/Storage/IO.hs \
    -E ouroboros-consensus-test/src/Test/Util/FS/Sim/Error.hs \
    -E ouroboros-consensus-test/src/Test/Util/FS/Sim/FsTree.hs \
    -E ouroboros-consensus-test/src/Test/Util/FS/Sim/MockFS.hs \
    -E ouroboros-consensus-test/src/Test/Util/FS/Sim/Pure.hs \
    -E ouroboros-consensus-test/src/Test/Util/FS/Sim/STM.hs \
    -E ouroboros-consensus-test/src/Test/Util/Blob.hs \
    -E ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs \
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
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Util/Counting.hs                         >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Util/SOP.hs                              >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Util/OptNP.hs                            >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Functors.hs     >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/InPairs.hs      >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Match.hs        >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Telescope.hs    >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/DerivingVia.hs  >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Combinator/Util/Tails.hs        >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/Classify.hs                       >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/API.hs            >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/API/Types.hs      >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/CRC.hs            >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/Handle.hs         >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/FS/IO.hs             >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus/src/Ouroboros/Consensus/Storage/IO.hs                >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/FS/Sim/Error.hs                   >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/FS/Sim/FsTree.hs                  >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/FS/Sim/MockFS.hs                  >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/FS/Sim/Pure.hs                    >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/FS/Sim/STM.hs                     >/dev/null 2>&1
grep "module.*DEPRECATED" ouroboros-consensus-test/src/Test/Util/Blob.hs                           >/dev/null 2>&1

fd -p ouroboros-consensus -e cabal -x cabal-fmt -i

#!/usr/bin/env bash

set -o pipefail

# remove network packages from the list of local packages

sed 's/\.\/.*network.*//g' cabal.project \
    | sed 's/\.\/monoidal.*//g' \
    | sed 's/\.\/ntp.*//g' \
    | sed 's/\.\/cardano.*//g' \
    | sed 's/packages:.*/packages:/g' \
    > cabal.project.consensus

cabal build --project-file cabal.project.consensus --builddir dist-chap all 2>&1 | ts "> "

rm cabal.project.consensus

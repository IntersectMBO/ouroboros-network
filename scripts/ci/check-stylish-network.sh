#!/usr/bin/env bash

set -euo pipefail

stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'io-sim/*.hs' | grep -v -e 'Setup.hs')
stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'io-classes/*.hs' | grep -v -e 'Setup.hs')
stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'strict-stm/*.hs' | grep -v -e 'Setup.hs')
stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'typed-protocols*/*.hs' | grep -v -e 'Setup.hs')
# TODO CPP pragmas in export lists are not supported by stylish-haskell
stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'network-mux/*.hs' | grep -v -e 'Setup.hs' -e 'Network/Mux/Bearer/Pipe\.hs' -e 'Network/Mux/Channel\.hs')
stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'ouroboros-network*/*.hs' | grep -v -e 'Setup.hs')
stylish-haskell -c .stylish-haskell-network.yaml -i $(git ls-files -- 'cardano-client/*.hs' | grep -v -e 'Setup.hs')

git diff --exit-code

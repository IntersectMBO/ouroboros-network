#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
# TODO CPP pragmas in export lists are not supported by stylish-haskell
fd . './quickcheck-monoids' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './network-mux' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-api' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-framework' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-mock' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network-protocols' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './ouroboros-network' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i
fd . './cardano-client' -e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell -c .stylish-haskell-network.yaml -i

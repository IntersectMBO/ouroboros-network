#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8
# TODO the export of the <= operator TxLimits crashes stylish-haskell

fd -p anti-diff -e hs -X stylish-haskell -c .stylish-haskell.yaml -i
#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8

fd -p semigroupoid -e hs -X stylish-haskell -c .stylish-haskell.yaml -i
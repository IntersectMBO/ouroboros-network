#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C.UTF-8

fd -p anti-diff -e hs -X stylish-haskell -c .stylish-haskell.yaml -i
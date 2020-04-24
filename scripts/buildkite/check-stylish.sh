#!/usr/bin/env bash

set -euo pipefail

stylish-haskell -i `git ls-files -- 'ouroboros-consensus*/*.hs' | grep -v Setup.hs`

git diff --exit-code

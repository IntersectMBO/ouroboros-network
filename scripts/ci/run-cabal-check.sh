#!/usr/bin/env bash

set -eo pipefail

FD="$(which fdfind 2>/dev/null || which fd 2>/dev/null)"

for x in $($FD -e cabal); do
  (
    d=$(dirname $x)
    echo "== $d =="
    pushd $d
    cabal check
    popd
  )
done

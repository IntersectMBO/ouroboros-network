#!/usr/bin/env bash

for x in $(fd -e cabal); do
  (
    d=$(dirname $x)
    echo "== $d =="
    pushd $d
    cabal check
    popd
  )
done

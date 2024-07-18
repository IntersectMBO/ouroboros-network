#!/usr/bin/env bash

set -euo pipefail

function usage {
  echo "Usage $(basename "$0") [-ch]"
  echo "Check files with 'stylish-haskell'; by default check all files."
  echo
  echo "        -u                        only check files uncommitted"
  echo "        -c                        only check files committed in HEAD"
  echo "        -h                        this help message"
  exit
}

export LC_ALL=C.UTF-8

STYLISH_HASKELL_ARGS="-c .stylish-haskell-network.yaml -i"

optstring=":uch"
while getopts ${optstring} arg; do
  case ${arg} in
    h)
      usage;
      exit 0
      ;;
    c)
      PATHS=$(git show --pretty='' --name-only HEAD)
      for path in $PATHS; do
        if [ "${path##*.}" == "hs" ]; then
          if grep -qE '^#' $path; then
            echo "$path contains CPP.  Skipping."
          else
            echo $path
            stylish-haskell $STYLISH_HASKELL_ARGS $path
          fi
        fi
      done
      exit 0
      ;;
    u)
      PATHS=$(git diff --name-only HEAD)
      for path in $PATHS; do
        if [ "${path##*.}" == "hs" ]; then
          if grep -qE '^#' $path; then
            echo "$path contains CPP.  Skipping."
          else
            echo $path
            stylish-haskell $STYLISH_HASKELL_ARGS $path
          fi
        fi
      done
      exit 0
      ;;
    ?)
      echo "Invalid argument ${arg}"
      exit 1
      ;;
  esac
done

# TODO CPP pragmas in export lists are not supported by stylish-haskell
FD_OPTS="-e hs --ignore-file ./scripts/ci/check-stylish-ignore -X stylish-haskell $STYLISH_HASKELL_ARGS"

fd . './quickcheck-monoids'          $FD_OPTS
fd . './network-mux'                 $FD_OPTS
fd . './ouroboros-network-api'       $FD_OPTS
fd . './ouroboros-network-framework' $FD_OPTS
fd . './ouroboros-network-mock'      $FD_OPTS
fd . './ouroboros-network-protocols' $FD_OPTS
fd . './ouroboros-network'           $FD_OPTS
fd . './cardano-client'              $FD_OPTS

#!/usr/bin/env bash

set -euo pipefail

function usage {
  echo "Usage $(basename "$0") [-ch]"
  echo "Check files with 'stylish-haskell'; by default check all files."
  echo
  echo "        -u                        only check files uncommitted"
  echo "        -c                        only check files committed in HEAD"
  echo "        -h                        this help message"
  echo "        -g                        don't show the diff with git"
  exit
}

export LC_ALL=C.UTF-8

STYLISH_HASKELL_ARGS="-c .stylish-haskell-network.yaml -i"
USE_GIT=1

optstring=":uchg"
while getopts ${optstring} arg; do
  case ${arg} in
    h)
      usage;
      exit 0
      ;;
    g)
      USE_GIT=0
      ;;
    c)
      PATHS=$(git show --pretty='' --name-only HEAD)
      for path in $PATHS; do
        echo $path
        fd -e hs --ignore-file ./scripts/ci/check-stylish-ignore --full-path $path -X stylish-haskell $STYLISH_HASKELL_ARGS
      done
      if [ $USE_GIT == 1 ]; then
        git --no-pager diff --exit-code
      fi
      exit 0
      ;;
    u)
      PATHS=$(git diff --name-only HEAD)
      for path in $PATHS; do
        if [ "${path##*.}" == "hs" ]; then
          echo $path
          fd -e hs --ignore-file ./scripts/ci/check-stylish-ignore --full-path $path -X stylish-haskell $STYLISH_HASKELL_ARGS
        fi
      done
      if [ $USE_GIT == 1 ]; then
        git --no-pager diff --exit-code
      fi
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

if [ $USE_GIT == 1 ]; then
git --no-pager diff --exit-code
fi

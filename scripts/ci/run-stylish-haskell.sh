#!/usr/bin/env bash

set -euo pipefail

function usage {
  echo "Usage $(basename "$0") [-ch]"
  echo "Check files with 'stylish-haskell'; by default check all files."
  echo
  echo "        -u                        only check uncommitted files"
  echo "        -c                        only check committed files in HEAD"
  echo "        -h                        this help message"
  echo "        -g                        don't show the diff with git"
  exit
}

export LC_ALL=C.UTF-8

FD="$(which fdfind 2>/dev/null || which fd 2>/dev/null)"

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
        $FD -e hs --ignore-file ./scripts/ci/check-stylish-ignore --full-path $path -X stylish-haskell $STYLISH_HASKELL_ARGS
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
          $FD -e hs --ignore-file ./scripts/ci/check-stylish-ignore --full-path $path -X stylish-haskell $STYLISH_HASKELL_ARGS
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

$FD . './network-mux'                 $FD_OPTS
$FD . './ouroboros-network'           $FD_OPTS
$FD . './cardano-diffusion'           $FD_OPTS

if [ $USE_GIT == 1 ]; then
git --no-pager diff --exit-code
fi

#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

ptype=$1
case ${ptype} in
        protocol | software ) ;;
        * ) echo "Usage:  $0 ( protocol | software ) [PROPOSAL-OPTION..]" >&2; exit 1;; esac
shift

set -x
cabal new-run demo-playground -- \
      --system-start "$now" \
      propose-$ptype \
      --topology demo-playground/simple-topology.json \
      --node-id 0 \
      $@

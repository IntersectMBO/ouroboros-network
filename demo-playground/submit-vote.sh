#!/bin/sh

now=`date "+%Y-%m-%d 00:00:00"`

NODE=$1
UPID=$2
VOTE=$3
case ${VOTE} in
        accept | reject ) ;;
        * ) echo "Usage:  $0 NODEID UPID ( accept | reject )" >&2; exit 1;; esac

set -x
cabal new-run demo-playground -- \
      --system-start "${now}" \
      vote \
      --topology demo-playground/simple-topology.json \
      --node-id ${NODE} \
      --${VOTE} \
      --proposal-id "\"${UPID}\""

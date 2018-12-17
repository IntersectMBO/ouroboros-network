#!/usr/bin/env bash

# Turbohack to clean up the resources.
trap ctrl_c INT

function ctrl_c() {
  echo "Cleaning up named pipes..."
  sh -c "./demo-playground/cleanup-demo.sh"
}

now=`date "+%Y-%m-%d 00:00:00"`

if [ $# -ne 2 ]
then
  echo "Usage: $0 [--bft | --praos] <nodeid>"
  exit
fi

cabal new-run demo-playground -- \
    --system-start "$now" --slot-duration 2 $1 \
    node -t demo-playground/simple-topology.json -n $2 \

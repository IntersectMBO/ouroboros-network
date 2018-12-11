#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

cabal new-run demo-playground -- submit \
    -t demo-playground/simple-topology.json -n $1 \
    --address $2 --amount $3 --system-start "$now" --slot-duration 2


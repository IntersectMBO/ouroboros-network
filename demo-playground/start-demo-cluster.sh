#!/usr/bin/env bash

. $(dirname $0)/common-config.sh

set -xe

cabal new-build demo-playground

atexit() {
        pkill demo-playground
}
trap atexit EXIT

start-node 0 --real-pbft
start-node 1 --real-pbft
start-node 2 --real-pbft

echo "Press Ctrl-C or Enter to terminate."
read

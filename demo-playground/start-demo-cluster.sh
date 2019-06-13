#!/usr/bin/env bash

. $(dirname $0)/common-config.sh

protocol=${1:-"--real-pbft"}
test -n "$1" && shift

minsev=${1:-info}
test -n "$1" && shift
echo "Minimum log severity is: ${minsev}"

set -xe

cabal new-build demo-playground

atexit() {
        pkill demo-playground
}
trap atexit EXIT

start-node 0 ${protocol} --minsev-${minsev} "$@"
start-node 1 ${protocol} --minsev-${minsev} "$@"
start-node 2 ${protocol} --minsev-${minsev} "$@"

echo "Press Ctrl-C or Enter to terminate."
read

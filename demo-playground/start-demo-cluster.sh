#!/usr/bin/env bash

. $(dirname $0)/common-config.sh

protocol=${1:-"--real-pbft"}
test -n "$1" && shift

set -xe

cabal new-build demo-playground

atexit() {
        pkill demo-playground
}
trap atexit EXIT

start-node 0 ${protocol} "$@"
start-node 1 ${protocol} "$@"
start-node 2 ${protocol} "$@"

echo "Press Ctrl-C or Enter to terminate."
read

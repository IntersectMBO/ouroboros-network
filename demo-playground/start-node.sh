#!/usr/bin/env bash

. $(dirname $0)/common-config.sh

set -x
start-node 0 "$@"

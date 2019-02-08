#!/bin/sh
set -x

nixopts="--cores 0 -j4 --no-build-output"
nix-shell ${nixopts} "$@"

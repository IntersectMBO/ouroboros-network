#!/usr/bin/env bash
nix build -f `dirname $0`/.. nightly-checks

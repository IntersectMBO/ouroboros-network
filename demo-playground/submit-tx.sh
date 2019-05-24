#!/usr/bin/env bash

. $(dirname $0)/common-config.sh

set -x
cabal new-run demo-playground -- \
      --system-start "$now" \
      --slot-duration ${slot_duration} \
      submit \
      --topology "${topology}" \
      $@

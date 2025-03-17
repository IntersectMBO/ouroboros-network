#!/bin/sh
cd "$(dirname "$0")"
cabal run kes-agent:exe:kes-agent -- run \
    --cold-verification-key ./cold.vkey \
    --genesis-file ../kes-agent/fixtures/mainnet-shelley-genesis.json

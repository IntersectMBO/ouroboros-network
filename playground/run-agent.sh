#!/bin/sh
cd "$(dirname "$0")"
case "$1" in
    "1")
            cabal run kes-agent:exe:kes-agent -- run \
                --cold-verification-key ./cold.vkey \
                --genesis-file ../kes-agent/fixtures/mainnet-shelley-genesis.json \
            -s /tmp/kes-agent-service-1.socket \
            -c /tmp/kes-agent-control.socket \
            -b /tmp/kes-agent-service.socket \
            -l notice
        ;;

    "2")
            cabal run kes-agent:exe:kes-agent -- run \
                --cold-verification-key ./cold.vkey \
                --genesis-file ../kes-agent/fixtures/mainnet-shelley-genesis.json \
            -s /tmp/kes-agent-service.socket \
            -c /tmp/kes-agent-control-2.socket \
            -b /tmp/kes-agent-service-1.socket \
            -l notice
        ;;

    "")
        cabal run kes-agent:exe:kes-agent -- run \
            --cold-verification-key ./cold.vkey \
            --genesis-file ../kes-agent/fixtures/mainnet-shelley-genesis.json \
            -l notice
        ;;
esac

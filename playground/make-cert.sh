#!/bin/sh
cd `dirname $0`
cardano-cli node issue-op-cert \
    --kes-verification-key-file kes.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter opcert.counter \
    --kes-period $(./get-current-period.sh ../kes-agent/fixtures/mainnet-shelley-genesis.json) \
    --out-file opcert.cert

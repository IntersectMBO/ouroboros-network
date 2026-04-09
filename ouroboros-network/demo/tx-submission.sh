#!/bin/env bash

INBOUND_ADDR=10.0.0.2
INBOUND_PORT=4000
OUTBOUND_ADDR=10.0.0.1
OUTBOUND_PORT=4001
TXS_FILE="/tmp/txs.cbor"

# network shaping parameters
THROUPUT="10mbit"
DELAY="10ms"

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/tx-submission-demo.XXXXXX)
rm -f ./tx-submission-tmp-dir
ln -s "$TMP_DIR" ./tx-submission-tmp-dir
echo "Using temporary directory: $TMP_DIR"

cabal build exe:demo-tx-submission
CMD=$(cabal list-bin exe:demo-tx-submission)

INBOUND_CMD="${CMD} inbound --addr ${INBOUND_ADDR} --port ${INBOUND_PORT}"
OUTBOUND_CMD="${CMD} outbound --addr ${INBOUND_ADDR} --port ${INBOUND_PORT} --bind-addr ${OUTBOUND_ADDR} --bind-port ${OUTBOUND_PORT} --input ${TXS_FILE} -n 20"

./network-mux/demo/ns-setup.sh "${THROUPUT}" "${DELAY}" "${TMP_DIR}" "${OUTBOUND_CMD}" "${INBOUND_CMD}"

#!/bin/env bash

addr() {
  echo "10.0.0.$1"
}

# server's configuration
SERVER_ADDR=$(addr 2)
SERVER_PORT=9001
REQUEST_SIZE=10 # 10B
MUX_SDU=12288
LEIOS_BLOCK_SIZE=$((($MUX_SDU - 5) * 1000))
PRAOS_BLOCK_SIZE=$((($MUX_SDU - 5) * 10))

# client's configuration
NUM_LEIOS_REQUESTS=20
NUM_PRAOS_REQUESTS=1000 # $(($NUM_LEIOS_REQUESTS * $LEIOS_BLOCK_SIZE / $PRAOS_BLOCK_SIZE))

# network shaping parameters
THROUPUT="100mbit"
DELAY="10ms"

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/mux-leios-demo.XXXXXX)
rm -f ./mux-leios-tmp-dir
ln -s "$TMP_DIR" ./mux-leios-tmp-dir
echo "Using temporary directory: $TMP_DIR"

RTS_CLIENT_OPTIONS="-N2 -l-asgu -ol${TMP_DIR}/client.eventlog"
RTS_SERVER_OPTIONS="-N2 -l-asgu -ol${TMP_DIR}/server.eventlog"

echo "REQUEST_SIZE: ${REQUEST_SIZE}B" >> $TMP_DIR/config
echo "PRAOS_BLOCK_SIZE: $(bc <<< "scale=2; $PRAOS_BLOCK_SIZE / 1000")KB" >> $TMP_DIR/config
echo "LEIOS_BLOCK_SIZE: $(bc <<< "scale=2; $LEIOS_BLOCK_SIZE / 1000000")MB" >> $TMP_DIR/config

cabal build exe:mux-leios-demo
CMD=$(cabal list-bin exe:mux-leios-demo)

# client is executed in `ns1`
CLIENT_CMD="${CMD} client-burst ${SERVER_ADDR} ${SERVER_PORT} +RTS ${RTS_CLIENT_OPTIONS} -RTS"
# server is executed in `ns2`
SERVER_CMD="${CMD} server-burst ${SERVER_ADDR} ${SERVER_PORT} ${NUM_PRAOS_REQUESTS} ${PRAOS_BLOCK_SIZE} ${NUM_LEIOS_REQUESTS} ${LEIOS_BLOCK_SIZE} +RTS ${RTS_SERVER_OPTIONS} -RTS"

./network-mux/demo/ns-setup.sh "$THROUPUT" "$DELAY" "$TMP_DIR" "$CLIENT_CMD" "$SERVER_CMD"

#!/bin/sh

NODE=${1:-0}
DEST=${2:-a}
AMOUNT=${3:-1000}

$(dirname $0)/submit-tx.sh \
             --node-id ${NODE} \
             --address ${DEST} \
             --amount  ${AMOUNT}

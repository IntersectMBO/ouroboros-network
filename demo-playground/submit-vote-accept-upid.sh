#!/bin/sh

NODE=${1:-0}
## XXX: invalid upid for testing
UPID=${2:-0001020304050607080910111213141516171819202122232425262728293031}

$(dirname $0)/submit-vote.sh ${NODE} ${UPID} accept

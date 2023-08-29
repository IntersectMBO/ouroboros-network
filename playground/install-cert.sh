#!/bin/sh
cd `dirname $0`
cabal run kes-agent-control -- install-key --opcert-file=opcert.cert

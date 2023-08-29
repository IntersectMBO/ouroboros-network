#!/bin/sh
cd `dirname $0`
cabal run kes-agent-control -- gen-staged-key --kes-verification-key-file kes.vkey

#!/bin/sh
cd `dirname $0`
cardano-cli node key-gen --cold-verification-key-file cold.vkey --cold-signing-key-file=cold.skey --operational-certificate-issue-counter-file=opcert.counter

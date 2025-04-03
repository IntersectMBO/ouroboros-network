#!/bin/sh
cd "$(dirname "$0")"
cabal run kes-agent:exe:kes-service-client-demo

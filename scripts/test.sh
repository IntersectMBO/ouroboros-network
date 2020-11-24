#!/bin/bash
# Run all the tests using `cabal`.  This is useful for runing tests on Windows.

# Any arguments are passed to `cabal run` command.

cabal run ${@} Win32-network:test && \
cabal run ${@} io-sim:test && \
cabal run ${@} network-mux:test && \
cabal run ${@} ntp-client:test && \
cabal run ${@} typed-protocols-examples:test && \
cabal run ${@} ouroboros-network-framework:test && \
cabal run ${@} ouroboros-network:test
# consensus tests are disabled due to #1082
# cabal run ${@} test-consensus && \
# cabal run ${@} test-storage
# cddl tests are disabled - one needs the cddl tool
# cabal run ${@} cddl

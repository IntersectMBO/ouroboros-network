#!/bin/bash
# Run all the tests using `cabal`.  This is useful for runing tests on Windows.

# Any arguments are passed to `cabal run` command.

cabal run ${@} test-Win32-network && \
cabal run ${@} test-sim && \
cabal run ${@} test-network-mux && \
cabal run ${@} ntp-client-test && \
cabal run ${@} test-typed-protocols-cbor && \
cabal run ${@} ouroboros-network-framework-tests && \
cabal run ${@} test-network
# consensus tests are disabled due to #1082
# cabal run ${@} test-consensus && \
# cabal run ${@} test-storage
# cddl tests are disabled - one needs the cddl tool
# cabal run ${@} cddl

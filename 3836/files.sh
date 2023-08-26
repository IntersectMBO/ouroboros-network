#!/usr/bin/env bash

set -e

# There are two tiers of files we're interested in.
# Tier 1 contains (non-testing) files within:
# - `ouroboros-network`
# - `ouroboros-network-api`
# - `ouroboros-network-framework`
# - `ouroboros-network-protocols`
# Tier 2 contains files from:
# - cardano-client
# - cardano-ping
# - monoidal-synchronisation
# - network-mux
# - ntp-client
# - ouroboros-network-mock
# - ouroboros-network-testing

function nontest () {
    # Find Haskell source files that aren't tests.
    #
    # Argument 1: an array of Haskell package directories

    REPOS=$1
    FILES=($(find ${REPOS[@]} -name '*.hs' | grep -v '.*/test.*/'))
    echo ${FILES[@]}
}

function tier1 () {
    # Tier 1 files
    REPOS=(
        "ouroboros-network"
        "ouroboros-network-api"
        "ouroboros-network-protocols"
        "ouroboros-network-framework"
    )
    FILES=(`nontest $REPOS`)
    echo ${FILES[@]}
}

function tier2 () {
    # Tier 2 files
    REPOS=(
        "cardano-client"
        "cardano-ping"
        "monoidal-synchronisation"
        "network-mux"
        "ntp-client"
        "ouroboros-network-mock"
        "ouroboros-network-testing"
    )
    FILES=(`nontest $REPOS`)
    echo ${FILES[@]}
}

function errors () {
    FILES=$1
    local IFS=$'\n'
    # Exclude line comments, block comments (sorta), and strings
    ERRORS=(`grep -n -w "error" ${FILES[@]} | grep -v -e "--.*error" -e "{-.*error" -e "\"[^\"]*error"`)
    for ERROR in ${ERRORS[@]}; do
        echo $ERROR
    done
}

pushd .. > /dev/null

TIER=$1
if [[ $TIER == 1 ]]; then
    FILES=(`tier1`)
elif [[ $TIER == 2 ]]; then
    FILES=(`tier2`)
else
    echo "no tier: $TIER"
fi

errors $FILES | grep --color=auto error

popd > /dev/null

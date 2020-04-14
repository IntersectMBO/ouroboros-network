#!/bin/bash
# Build haddock documentation and an index for all projects in
# `ouroboros-network` repository.
#
# usage:
# ./haddocks.sh directory [true|false]
#
# $1 - where to put the generated pages, this directory contents will be wiped
#      out (so don't pass `/` or `./` - the latter will delete your 'dist-newstyle')
#      (the default is './haddocks')
# $2 - weahter to re-build haddocjs with `cabal haddock` command or a component name
#      (the default is true)

set -euo pipefail

OUTPUT_DIR=${1:-"./haddocks"}
REGENERATE=${2:-"true"}

GHC_VERSION=$(ghc --numeric-version)

HADDOCK_OPTS=(
    --disable-optimization
    --haddock-all
    --haddock-html
    --haddock-hyperlink-source
    --haddock-option "--quickjump"
    --haddock-option "--show-all"
    --haddock-option "--use-unicode"
    --haddock-option "--use-index=\"../doc-index.html\""
    --haddock-option "--use-contents=\"../index.html\""
  )

# build documentation of all modules
if [ ${REGENERATE} == "true" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" all
elif [ ${REGENERATE} != "false" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" ${REGENERATE}
fi

# copy the new docs
for dir in $(ls "dist-newstyle/build/x86_64-linux/ghc-${GHC_VERSION}"); do
  package=$(echo "${dir}" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')
  cp -r "dist-newstyle/build/x86_64-linux/ghc-${GHC_VERSION}/${dir}/noopt/doc/html/${package}" ${OUTPUT_DIR}
done

# --read-interface options
interface_options () {
  for package in $(ls "${OUTPUT_DIR}"); do
    echo "--read-interface=${package},${OUTPUT_DIR}/${package}/${package}.haddock"
  done
}

haddock -o ${OUTPUT_DIR} \
  --title "ouroboros-network" \
  --package-name "Ouroboros-Network & Ouroboros-Consensus" \
  --gen-index \
  --gen-contents \
  --prolog ./scripts/prolog \
  $(interface_options)

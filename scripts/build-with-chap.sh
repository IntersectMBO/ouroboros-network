#!/usr/bin/env bash
# Build latest versions available from chap
#
# This script should be run after running ./script/release-to-chap.sh to check
# that what's being released to CHaP builds.

set -eo pipefail

# fail if `gh` is not installed
which gh

CHAP_DIR=${CARDANO_HASKELL_PACKAGES_DIR:-"/tmp/chap"}

if [[ -n "$(git status --untracked-files=no --porcelain)" ]];then
  echo "error: not a clean directory"
  exit 1
fi

cabal_files=$(fd -ae 'cabal')
for cf in $cabal_files; do
  name=$(cat $cf | grep '^name:' | awk '{ print $2 }')
  version=$(ls -1 $CHAP_DIR/_sources/$name | sort | tail -1)
  tag="$name-$version"
  echo "$tag ($(git rev-parse $tag))"
  git restore --source="$name-$version" -- $name
  revdir="$CHAP_DIR/_sources/$name/$version/revisions"
  if [[ -d $revdir ]]; then
    rev=$(ls $revdir | sort | tail -1)
    echo "copy revision $revdir/$rev to $name/$name.cabal"
    cp $revdir/$rev "$name/$name.cabal"
  fi
done

cabal build all
git reset --hard HEAD

pushd $CHAP_DIR
git symbolic-ref --short HEAD
if [[ $(git symbolic-ref --short HEAD) =~ ^network\/release- ]] then
  gh pr comment --body "* [x] checked with \`build-with-chap.sh\` in \`ouroboros-network\`"
fi

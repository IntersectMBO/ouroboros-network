#!/usr/bin/env bash
# Build latest versions available from chap
#
# This script should be run after running ./script/release-to-chap.sh to check
# that what's being released to CHaP builds.

set -eo pipefail

# fail if `gh` is not installed
which gh
# fail if `yq` is not installed
which yq # https://github.com/mikefarah/yq#install

CHAP_DIR=${CARDANO_HASKELL_PACKAGES_DIR:-"/tmp/chap"}

if [[ -n "$(git status --untracked-files=no --porcelain)" ]];then
  echo "error: not a clean directory"
  exit 1
fi

cabal_files=$(fd -ae 'cabal')
for cf in $cabal_files; do
  name=$(cat $cf | grep '^name:' | awk '{ print $2 }')
  version=$(ls -1 $CHAP_DIR/_sources/$name | sort -V | tail -1)
  rev=$(yq .github.rev $CHAP_DIR/_sources/$name/$version/meta.toml)
  git restore --source=$rev -- $name
  tb=0
  revdir="$CHAP_DIR/_sources/$name/$version/revisions"
  if [[ -d $revdir ]]; then
    rev=$(ls $revdir | sort -V | tail -1)
    echo "copy revision $revdir/$rev to $name/$name.cabal"
    cp $revdir/$rev "$name/$name.cabal"
  fi
done

cabal build all
git reset --hard HEAD

# check that all revs are on `master` or `release\/*` branches.
for cf in $cabal_files; do
  name=$(cat $cf | grep '^name:' | awk '{ print $2 }')
  version=$(ls -1 $CHAP_DIR/_sources/$name | sort -V | tail -1)
  rev=$(yq .github.rev $CHAP_DIR/_sources/$name/$version/meta.toml)
  ! { git branch -l --contains $rev --format="%(refname:short)" | grep -e '^\(master\|release//\)'; }
  if [[ $? == 0 ]]; then
    echo "$name: revision $rev is not on the master or a 'release/*' branch."
    exit 1
  fi
done

pushd $CHAP_DIR
git symbolic-ref --short HEAD
if [[ $(git symbolic-ref --short HEAD) =~ ^network\/release- ]] then
  gh pr comment --body "* [x] checked with \`build-with-chap.sh\` in \`ouroboros-network\`"
fi

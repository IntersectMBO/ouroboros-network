#!/usr/bin/env bash
#
# Create a branch in of `cardano-haskell-packages` which releases all
# unreleased versions of packages in `ouroboros-network`.
#
# Create tags, which must be pushed manually.

set -eo pipefail

REPO_URL="https://github.com/input-output-hk/ouroboros-network"

CHAP_DIR=${CARDANO_HASKELL_PACKAGES_DIR:-"/tmp/chap"}
CHAP_URL="https://github.com/input-output-hk/cardano-haskell-packages"
CYAN='[36m'
NC='[0m'

function trace() {
  echo "${CYAN}${1}${NC}"
}

branch=$(git rev-parse --abbrev-ref HEAD)
if [[ !($branch =~ ^(master|release/.*)$) ]]; then
  echo "error: one must release from master or a release/* branch"
  exit 1
fi

gitsha=$(git rev-parse HEAD)
gitdir=$(git rev-parse --show-toplevel)

cabal_files=$(fd -ae 'cabal')

if [[ !(-d $CHAP_DIR) ]]; then
  trace "clone CHaP to $CHAP_DIR"
  git clone $CHAP_URL $CHAP_DIR
  pushd $CHAP_DIR > /dev/null
else
  pushd $CHAP_DIR > /dev/null
  git switch main
  git pull
fi
git switch -c network/release-$(date -I)

for cf in $cabal_files; do
  name=$(cat $cf | grep '^name:' | awk '{ print $2 }')
  version=$(cat $cf | grep '^version:' | awk '{ print $2 }')
  dir="$CHAP_DIR/_sources/$name/$version"
  if [[ !(-d $dir) ]];then
    trace "publishing $name-$version"
    ./scripts/add-from-github.sh $REPO_URL $gitsha $name
    git --git-dir "$gitdir/.git" tag "$name-$version" $gitsha
  fi
done

git log --oneline origin/main..HEAD

popd > /dev/null
trace "created tags:"
git tag --points-at=HEAD
trace "please run ./scripts/build-with-chap.sh"
trace "once published, please push tags with:"
echo "git push origin \$(git tag --points-to=HEAD)"

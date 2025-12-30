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

function usage {
  echo "Build against cardano-haskell-packages branch"
  echo "-h help message "
  echo "-t build a release from a custom branch rather than main or release/* branches"
  echo "Set CARDANO_HASKELL_PACKAGES_DIR env variable to point to CHaP directory, if not set '/tmp/chap' will be used."
}

optstring="ht"
TEST=0
while getopts ${optstring} arg; do
  case ${arg} in
    h)
      usage
      exit 0
      ;;
    t)
      TEST=1
      ;;
    ?)
      echo "Invalid option '-${arg}'."
      exit 2
  esac
done

if [[ -n "$(git status --untracked-files=no --porcelain)" ]];then
  echo "error: not a clean directory"
  exit 1
fi

cabal_files=$(fd -ae 'cabal')
for cf in $cabal_files; do
  name=$(cat $cf | grep '^name:' | awk '{ print $2 }')
  if [[ -d "$CHAP_DIR/_sources/$name" ]]; then
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
  else
    echo "WARNING: $name not in cardano-haskell-packages"
  fi
done

cabal build all
git reset --hard HEAD

# check that all revs are on `main` or `release\/*` branches.
if [[ $TEST == 0 ]]; then
  for cf in $cabal_files; do
    name=$(cat $cf | grep '^name:' | awk '{ print $2 }')
    if [[ -d "$CHAP_DIR/_sources/$name" ]]; then
      version=$(ls -1 $CHAP_DIR/_sources/$name | sort -V | tail -1)
      rev=$(yq .github.rev $CHAP_DIR/_sources/$name/$version/meta.toml)
      ! { git branch -l --contains $rev --format="%(refname:short)" | grep -e '^\(main$\|release/\)'; }
      if [[ $? == 0 ]]; then
        echo "$name: revision $rev is not on the main or a 'release/*' branch."
        exit 1
      fi
    fi
  done
fi

pushd $CHAP_DIR
if [[ $TEST == 0 && $(git symbolic-ref --short HEAD) =~ ^network\/release- ]] then
  gh pr comment --body "* [x] checked with \`build-with-chap.sh\` in \`ouroboros-network\`"
fi

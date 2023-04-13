#!/usr/bin/env bash

# Push a release to CHaP.
#
# Tags must exist in `ouroboros-network` repository. It will push the latest
# version mentioned in the changelogs, pointing at the tag for that version.
git pull

function pkgs_in_bundle {
  sed '/Changelog entries/q' $1/CHANGELOG.md | grep "\- \`ouroboros-" | cut -d\` -f2 | sort | uniq
}

function last_version {
  ch_version=$(grep "<a id=" $1/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
  if [[ -z $ch_version ]]; then
    # this will only happen while there are no changelog entries on said bundles.
    # once there are entries this is dead code.
    if [[ $1 == "ouroboros-consensus-test" ]]; then
      echo "0.3.1.0"
    elif [[ $1 == "ouroboros-consensus-cardano-test" ]]; then
      echo "0.4.0.0"
    fi
  else
    echo $ch_version
  fi
}

consensus_packages=$(pkgs_in_bundle ouroboros-consensus)
consensus_last_version=$(last_version ouroboros-consensus)
consensus_test_packages=$(pkgs_in_bundle ouroboros-consensus-test)
consensus_test_last_version=$(last_version ouroboros-consensus-test)
cardano_packages=$(pkgs_in_bundle ouroboros-consensus-cardano)
cardano_last_version=$(last_version ouroboros-consensus-cardano)
cardano_test_packages=$(pkgs_in_bundle ouroboros-consensus-cardano-test)
cardano_test_last_version=$(last_version ouroboros-consensus-cardano-test)

if [[ ! $(git tag -l "release-consensus-$consensus_last_version") || ! $(git tag -l "release-consensus-test-$consensus_test_last_version") || ! $(git tag -l "release-cardano-$cardano_last_version") || ! $(git tag -l "release-cardano-test-$cardano_test_last_version") ]]; then
    echo "There are no tags pushed for the versions! Checkout the commit after bors merged the release PR and run ./scripts/consensus-tag-releases.sh"
    exit 1
fi

if [[ ! -d chap ]]; then
  git clone git@github.com:input-output-hk/cardano-haskell-packages chap
else
  ( cd chap
    git checkout main
    git pull
  )
fi

( cd chap
  # delete branch if it existed
  if [[ $(git branch -v | grep release-consensus) ]]; then
    git branch --delete release-consensus
  fi
  if [[ $(git branch --remote -v | grep release-consensus) ]]; then
    git push origin --delete release-consensus
  fi
  git checkout -b release-consensus

  if [[ ! -d _sources/ouroboros-consensus/$consensus_last_version ]]; then
      for f in $consensus_packages; do
          ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-network release-consensus-$consensus_last_version $f
      done
  fi

  if [[ ! -d _sources/ouroboros-consensus-test/$consensus_test_last_version ]]; then
      for f in $consensus_test_packages; do
          ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-network release-consensus-test-$consensus_test_last_version $f
      done
  fi

  if [[ ! -d _sources/ouroboros-consensus-cardano/$cardano_last_version ]]; then
      for f in $cardano_packages; do
          ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-network release-cardano-$cardano_last_version $f
      done
  fi

  if [[ ! -d _sources/ouroboros-consensus-cardano-test/$cardano_test_last_version ]]; then
      for f in $cardano_test_packages; do
          ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-network release-cardano-test-$cardano_test_last_version $f
      done
  fi

  echo "I have created a branch named \"release-consensus\" here: $(pwd)"
  echo "Push it to the remote and open a PR."
  echo ""
  echo "  git push --set-upstream origin release-consensus"
)

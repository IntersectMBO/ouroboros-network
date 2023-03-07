#!/usr/bin/env bash

# Push a release to CHaP.
#
# Tags must exist in `ouroboros-network` repository. It will push the latest
# version mentioned in the changelogs, pointing at the tag for that version.
git pull

cardano_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus-cardano*" -or -name "ouroboros-consensus-shelley*" -or -name "ouroboros-consensus-byron*")
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

consensus_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus*" -not -name "*byron*" -not -name "*shelley*" -not -name "*cardano*")
consensus_last_version=$(grep "<a id=" ouroboros-consensus/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

if [[ ! $(git tag -l "release-consensus-$consensus_last_version") || ! $(git tag -l "release-cardano-$cardano_last_version") ]]; then
    echo "There are no tags pushed for the versions! Checkout the commit after bors merged the release PR and run ./scripts/consensus-push-tags.sh"
    exit 1
fi

git clone git@github.com:input-output-hk/cardano-haskell-packages chap

( cd chap
  if [[ ! -d _sources/ouroboros-consensus/$consensus_last_version && ! -d _sources/ouroboros-consensus-cardano/$cardano_last_version ]]; then
      branch="release-consensus-$consensus_last_version-cardano-$cardano_last_version"
  elif [[ ! -d _sources/ouroboros-consensus/$consensus_last_version ]]; then
      branch="release-consensus-$consensus_last_version"
  elif [[ ! -d _sources/ouroboros-consensus/$consensus_last_version ]]; then
      branch="release-cardano-$cardano_last_version"
  else
      exit 1
  fi

  git checkout -b $branch

  if [[ ! -d _sources/ouroboros-consensus/$consensus_last_version ]]; then
      for f in $consensus_packages; do
          ./scripts/add-from-github https://github.com/input-output-hk/ouroboros-network release-consensus-$consensus_last_version $(echo $f | cut -d. -f2)
      done
  fi

  if [[ ! -d _sources/ouroboros-consensus-cardano/$cardano_last_version ]]; then
      for f in $cardano_packages; do
          ./scripts/add-from-github https://github.com/input-output-hk/ouroboros-network release-consensus-cardano-$cardano_last_version $(echo $f | cut -d. -f2)
      done
  fi

  git push --set-upstream origin $branch
)

rm -r chap

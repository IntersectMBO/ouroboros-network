#!/usr/bin/env bash

# Updates the version for ouroboros-consensus bundles depending on the entries
# in the changelog

increment_version() {
  local delimiter=.
  local array=($(echo "$1" | tr $delimiter '\n'))
  array[$2]=$((array[$2]+1))
  echo $(local IFS=$delimiter ; echo "${array[*]}")
}

cardano_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus-cardano*" -or -name "ouroboros-consensus-shelley*" -or -name "ouroboros-consensus-byron*")
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

consensus_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus*" -not -name "*byron*" -not -name "*shelley*" -not -name "*cardano*")
consensus_last_version=$(grep "<a id=" ouroboros-consensus/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

consensus_changelog=$(cat ouroboros-consensus/changelog.d/*.md | python3 ./scripts/strip-md-comments.py)
cardano_changelog=$(cat ouroboros-consensus-cardano/changelog.d/*.md | python3 ./scripts/strip-md-comments.py)

if [[ $(echo "$consensus_changelog" | grep "### Breaking") ]]; then
    consensus_new_version=$(increment_version $consensus_last_version 1)
elif [[ $(echo "$consensus_changelog" | grep "### Non-Breaking") ]]; then
    consensus_new_version=$(increment_version $consensus_last_version 2)
elif [[ $(echo "$consensus_changelog" | grep "### Patch") ]]; then
    consensus_new_version=$(increment_version $consensus_last_version 3)
fi

if [[ $(echo "$cardano_changelog" | grep "### Breaking") ]]; then
    cardano_new_version=$(increment_version $cardano_last_version 1)
elif [[ $(echo "$cardano_changelog" | grep "### Non-Breaking") ]]; then
    cardano_new_version=$(increment_version $cardano_last_version 2)
elif [[ $(echo "$cardano_changelog" | grep "### Patch") ]]; then
    cardano_new_version=$(increment_version $cardano_last_version 3)
fi

if [[ -n $consensus_new_version && -n $cardano_new_version ]]; then
    branch="release-consensus-$consensus_new_version-cardano-$cardano_new_version"
elif [[ -n $consensus_new_version ]]; then
    branch="release-consensus-$consensus_new_version"
elif [[ -n $cardano_new_version ]]; then
    branch="release-cardano-$cardano_new_version"
else
    exit 1
fi

git checkout -b $branch

if [[ -n $consensus_new_version ]]; then
    for f in $consensus_packages; do
        sed -i "/^version:/ s/$consensus_last_version/$consensus_new_version/g" $f/$(echo $f | cut -d'/' -f2).cabal
        sed -i "/ouroboros-consensus.*\w*==/ s/$consensus_last_version/$consensus_new_version/g" $f/$(echo $f | cut -d'/' -f2).cabal
    done

    ( cd ouroboros-consensus
      scriv collect
    )

    git add -A
    git commit -m "Release consensus-$consensus_new_version"
fi

if [[ -n $cardano_new_version ]]; then
    for f in $cardano_packages; do
        sed -i "/^version:/ s/$cardano_last_version/$cardano_new_version/g" $f/$(echo $f | cut -d'/' -f2).cabal
        sed -i "/ouroboros-consensus.*\w*==/ s/$cardano_last_version/$cardano_new_version/g" $f/$(echo $f | cut -d'/' -f2).cabal
        if [[ -n $consensus_new_version ]]; then
            sed -i "/ouroboros-consensus.*\w*\^>=/ s/$(echo $consensus_last_version | cut -d'.' -f1-2)/$(echo $consensus_new_version | cut -d'.' -f1-2)/g" $f/$(echo $f | cut -d'/' -f2).cabal
        fi
    done

    ( cd ouroboros-consensus-cardano
      scriv collect
    )

    git add -A
    git commit -m "Release cardano-$cardano_new_version"
fi

./scripts/ci/check-consensus-changelog.sh

git push --set-upstream origin $branch

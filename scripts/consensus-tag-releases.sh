#!/usr/bin/env bash

# Creates the right tags and pushes them to `origin`
#
# To be run on `master` just after a release was done

if [[ ! $(git rev-parse --abbrev-ref HEAD) == "master" ]]; then
    echo "This must be run on master"
    exit 1
fi

git pull

consensus_last_version=$(grep "<a id=" ouroboros-consensus/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
consensus_test_last_version=$(grep "<a id=" ouroboros-consensus-test/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
cardano_test_last_version=$(grep "<a id=" ouroboros-consensus-cardano-test/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

if [[ -z $consensus_last_version || -z $consensus_test_last_version || -z $cardano_last_version || -z $cardano_test_last_version ]]; then
    echo "Some package has no version at all in the changelog. This is a critical error!"
fi

./scripts/ci/check-consensus-release.sh || (echo "Inconsistent versioning!" && exit 1)

tags=()

if [[ ! $(git tag -l "release-consensus-$consensus_last_version") ]]; then
    t="release-consensus-$consensus_last_version"
    git tag $t HEAD
    tags+=($t)
fi

if [[ ! $(git tag -l "release-consensus-test-$consensus_test_last_version") ]]; then
    t="release-consensus-test-$consensus_test_last_version"
    git tag $t HEAD
    tags+=($t)
fi

if [[ ! $(git tag -l "release-consensus-cardano-$cardano_last_version") ]]; then
    t="release-consensus-cardano-$cardano_last_version"
    git tag $t HEAD
    tags+=($t)
fi

if [[ ! $(git tag -l "release-consensus-cardano-$cardano_last_version") ]]; then
    t="release-consensus-cardano-test-$cardano_test_last_version"
    git tag $t HEAD
    tags+=($t)
fi

printf "Tagged the release. Please push the following tags to origin:\n%s" "$tags"

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
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

if [[ $(git branch --remove | grep "release-consensus-$consensus_last_version") || $(git branch --remote | grep "release.*cardano-$cardano_last_version") ]]; then
    echo "Release branches were not deleted on the repository. Did you merge with bors?"
    exit 1
fi

./scripts/ci/check-consensus-changelog.sh || (echo "Inconsistent versioning!" && exit 1)

if [[ ! $(git tag -l "release-consensus-$consensus_last_version") ]]; then
    git tag release-consensus-$consensus_last_version HEAD
    # this will only succeed if bors deleted the branch because it reuses the
    # same name! (this is intentional)
    git push origin release-consensus-$consensus_last_version
fi

if [[ ! $(git tag -l "release-consensus-cardano-$cardano_last_version") ]]; then
    git tag release-consensus-cardano-$cardano_last_version HEAD
    # this will only succeed if bors deleted the branch because it reuses the
    # same name! (this is intentional)
    git push origin release-consensus-cardano-$cardano_last_version
fi

#!/usr/bin/env bash

# For each one of the two bundles, check that:
# (1) they all have the same version
# (2) if the version was changed from the last one on the changelog then there
#     must be no remaining changelog files
# otherwise exits with exit code 1

# If $CI is set, then use `git show` as GHA does a sparse checkout. Otherwise,
# do a normal cat of the cabal file

cardano_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus-cardano*" -or -name "ouroboros-consensus-shelley*" -or -name "ouroboros-consensus-byron*")
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
cardano_new_versions=$(if [[ -n "${CI}" ]]; then
                           for f in $cardano_packages; do
                               git show $(ls $f/*.cabal) | grep "+version" | rev | cut -d' ' -f1 | rev
                           done
                       else for f in $cardano_packages; do
                               cat $f/*.cabal | grep "^version" | rev | cut -d' ' -f1 | rev
                            done
                       fi)

consensus_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus*" -not -name "*byron*" -not -name "*shelley*" -not -name "*cardano*")
consensus_last_version=$(grep "<a id=" ouroboros-consensus/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
consensus_new_versions=$(if [[ -n "${CI}" ]]; then
                           for f in $consensus_packages; do
                               git show $(ls $f/*.cabal) | grep "+version" | rev | cut -d' ' -f1 | rev
                           done
                       else for f in $consensus_packages; do
                               cat $f/*.cabal | grep "^version" | rev | cut -d' ' -f1 | rev
                            done
                       fi)


if [ $(echo "$consensus_new_versions" | sort | uniq | wc -l) != 1 ]; then  # See (1) above
    echo "Inconsistent versioning of the ouroboros-consensus bundle, more than one version number: $consensus_new_versions"
    exit 1
else
    if [ $(echo "$consensus_new_versions" | sort | uniq) == "$consensus_last_version" ]; then
        echo "ouroboros-consensus version not updated, currently at $consensus_last_version"
    else
        if [ $(ls -l ouroboros-consensus/changelog.d | wc -l) != 2 ]; then # See (2) above
            echo "Tried to release for ouroboros-consensus but there are remaining changelog files:"
            ls -l ouroboros-consensus/changelog.d
            exit 1
        else
            echo "ouroboros-consensus version succesfully updated"
        fi
    fi
fi

if [ $(echo "$cardano_new_versions" | sort | uniq | wc -l) != 1 ]; then  # See (1) above
    echo "Inconsistent versioning of the ouroboros-cardano bundle, more than one version number: $cardano_new_versions"
    exit 1
else
    if [ $(echo "$cardano_new_versions" | sort | uniq) == "${cardano_last_version:-0.1.0.0}" ]; then
        echo "ouroboros-cardano version not updated, currently at $cardano_last_version"
    else
        if [ $(ls -l ouroboros-consensus-cardano/changelog.d | wc -l) != 2 ]; then # See (2) above
            echo "Tried to release for ouroboros-cardano but there are remaining changelog files."
            ls -l ouroboros-consensus-cardano/changelog.d
            exit 1
        else
            echo "ouroboros-cardano version succesfully updated"
        fi
    fi
fi

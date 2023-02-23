#!/usr/bin/env bash

# For each one of the two bundles, check that:
# (1) if the version in the cabal file was changed in any of the packages wrt the latest version described in the changelog
# (2) then it was changed on all of them
# (3) and to the same value
# (4) and also there are no remaining changelog entries
# otherwise exits with exit code 1

cardano_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus-cardano*" -or -name "ouroboros-consensus-shelley*" -or -name "ouroboros-consensus-byron*")
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
cardano_versions=$(for f in $cardano_packages; do
                       git show $(ls $f/*.cabal) | grep "+version" | rev | cut -d' ' -f1 | rev
                   done)

consensus_packages=$(find . -maxdepth 1 -type d -name "ouroboros-consensus*" -not -name "*byron*" -not -name "*shelley*" -not -name "*cardano*")
consensus_last_version=$(grep "<a id=" ouroboros-consensus/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
consensus_versions=$(for f in $consensus_packages; do
                         git show $(ls $f/*.cabal) | grep "+version" | rev | cut -d' ' -f1 | rev
                     done)

if [ $(echo "$consensus_versions" | tr ' ' '\n' | sort | uniq) == "$consensus_last_version" ]; then # See (1) above
    echo "ouroboros-consensus version not updated"
else
    if [ $(echo "$consensus_versions" | wc -l) != $(echo "$consensus_packages" | wc -l) ]; then  # See (2) above
        echo "Some packages in ouroboros-consensus bundle are being updated and others not."
        exit 1
    else
        if [ $(echo "$consensus_versions" | sort | uniq | wc -l) != 1 ]; then  # See (3) above
            echo "Inconsistent versioning of the ouroboros-consensus bundle."
            exit 1
        else
            if [ $(ls -l ouroboros-consensus/changelog.d | wc -l) != 2 ]; then # See (4) above
                echo "Tried to release for ouroboros-consensus but there are remaining changelog files."
                exit 1
            else
                echo "ouroboros-consensus version succesfully updated"
            fi
        fi
    fi
fi


# Cardano didn't have a first version when this was released, that's why there is that default value
if [ $(echo "$cardano_versions" | tr ' ' '\n' | sort | uniq) == "${cardano_last_version:-0.1.0.0}" ]; then # See (1) above
    echo "ouroboros-consensus-cardano version not updated"
else
    if [ $(echo "$cardano_versions" | wc -l) != $(echo "$cardano_packages" | wc -l) ]; then # See (2) above
        echo "Some packages in ouroboros-consensus-cardano bundle are being updated and others not."
        exit 1
    else
        if [ $(echo "$cardano_versions" | sort | uniq | wc -l) != 1 ]; then # See (3) above
            echo "Inconsistent versioning of the ouroboros-consensus-cardano bundle."
            exit 1
        else
            if [ $(ls -l ouroboros-consensus-cardano/changelog.d | wc -l) != 2 ]; then # See (4) above
                echo "Tried to release for ouroboros-consensus-cardano but there are remaining changelog files."
                exit 1
            else
                echo "ouroboros-consensus-cardano version succesfully updated"
            fi
        fi
    fi
fi

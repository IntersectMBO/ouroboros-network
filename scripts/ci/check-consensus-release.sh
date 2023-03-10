#!/usr/bin/env bash

# For each one of the two bundles, check that:
# (1) they all have the same version
# (2) if the version was changed from the last one on the changelog then there
#     must be no remaining changelog files
# otherwise exits with exit code 1

function pkgs_in_bundle {
  sed '/Changelog entries/q' $1/CHANGELOG.md | grep "\- \`ouroboros-" | cut -d\` -f2 | sort | uniq
}

function get_last_version {
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

function cabal_files_versions {
    for f in $1; do
        cat $f/$f.cabal | grep "^version" | rev | cut -d' ' -f1 | rev
    done
}

function last_commit_updated_versions {
    for f in $1; do
        git show $f/$f.cabal | grep "^+version" | rev | cut -d' ' -f1 | rev
    done
}

function check {
    main=$1

    echo "Checking consistency of $main"
    versions=$(cabal_files_versions $(pkgs_in_bundle $main))
    last_version=$(get_last_version $main)
    this_commit_updated_versions=$(last_commit_updated_versions $(pkgs_in_bundle $main))

    if [ $(echo "$versions" | sort | uniq | wc -l) != 1 ]; then
        echo "ERROR: Inconsistent versioning, more than one version number mentioned in the cabal files: $versions"
        exit 1
    elif [ $(echo "$versions" | sort | uniq ) != $last_version ]; then
        echo "ERROR: Last version in the changelog ($last_version) is not the same as in the bundle $(echo "$versions" | sort | uniq )"
        exit 1
    elif [[ -n "$this_commit_updated_versions" && $(ls -l $main/changelog.d | wc -l) != 2 ]]; then
        echo "ERROR: Last commit updated the version but there are remaining changelog fragments"
        exit 1
    else
        printf "OK: %s\n\n" $last_version
    fi

}

check ouroboros-consensus
check ouroboros-consensus-test
check ouroboros-consensus-cardano
check ouroboros-consensus-cardano-test

#!/usr/bin/env bash

# Updates the version for ouroboros-consensus bundles depending on the entries
# in the changelog

function increment_version {
  local delimiter=.
  local array=($(echo "$1" | tr $delimiter '\n'))
  array[$2]=$((array[$2]+1))
  for i in $(seq $(($2 + 1)) 3); do
    array[$i]=0
  done
  echo $(local IFS=$delimiter ; echo "${array[*]}")
}

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

function compute_new_version {
  ch=$(cat ./$1/changelog.d/*.md | python3 ./scripts/strip-md-comments.py)
  if [[ $(echo "$ch" | grep "### Breaking") ]]; then
      increment_version $2  1
  elif [[ $(echo "$ch" | grep "### Non-Breaking") ]]; then
      increment_version $2  2
  elif [[ $(echo "$ch" | grep "### Patch") ]]; then
      increment_version $2  3
  fi
}

if [[ $(ls ouroboros-consensus/changelog.d ouroboros-consensus-test/changelog.d ouroboros-consensus-cardano/changelog.d ouroboros-consensus-cardano-test/changelog.d | wc -l) == 11 ]]; then
    echo "No changelog fragments. No need to do a new release"
    exit 1
fi

consensus_packages=$(pkgs_in_bundle ouroboros-consensus)
consensus_last_version=$(last_version ouroboros-consensus)
consensus_test_packages=$(pkgs_in_bundle ouroboros-consensus-test)
consensus_test_last_version=$(last_version ouroboros-consensus-test)
cardano_packages=$(pkgs_in_bundle ouroboros-consensus-cardano)
cardano_last_version=$(last_version ouroboros-consensus-cardano)
cardano_test_packages=$(pkgs_in_bundle ouroboros-consensus-cardano-test)
cardano_test_last_version=$(last_version ouroboros-consensus-cardano-test)

echo "Preparing a release for:"
if [[ $(ls ouroboros-consensus/changelog.d | wc -l) != 1 ]]; then
    consensus_new_version=$(compute_new_version ouroboros-consensus $consensus_last_version)
    echo "- consensus      $consensus_last_version -> $consensus_new_version"
fi

if [[ $(ls ouroboros-consensus-test/changelog.d | wc -l) != 1 ]]; then
    consensus_test_new_version=$(compute_new_version ouroboros-consensus-test $consensus_test_last_version)
    echo "- consensus-test $consensus_test_last_version -> $consensus_test_new_version"
fi

if [[ $(ls ouroboros-consensus-cardano/changelog.d | wc -l) != 1 ]]; then
    cardano_new_version=$(compute_new_version ouroboros-consensus-cardano $cardano_last_version)
    echo "- cardano        $cardano_last_version -> $cardano_new_version"
fi

if [[ $(ls ouroboros-consensus-cardano-test/changelog.d | wc -l) != 1 ]]; then
    cardano_test_new_version=$(compute_new_version ouroboros-consensus-cardano-test $cardano_test_last_version)
    echo "- cardano-test   $cardano_test_last_version -> $cardano_test_new_version"
fi

function release_num {
  if [[ -n $2 ]]; then
      echo "$1-$2/"
  else
      echo ""
  fi
}

printf "\n"

################################################################################
## Create git branch
################################################################################

if [[ -z $cardano_new_version && -z $consensus_new_version && -z $consensus_test_new_version && -z $cardano_test_new_version ]]; then
    echo "Nothing to update"
    exit 1
fi

branch="rel/"
branch+="$(release_num "co" $consensus_new_version)"
branch+="$(release_num "cot" $consensus_test_new_version)"
branch+="$(release_num "ca" $cardano_new_version)"
branch+="$(release_num "cat" $cardano_test_new_version)"
printf "Creating branch %s\n\n" ${branch%?}
# remove last slash
git checkout -b ${branch%?} >/dev/null 2>&1

################################################################################
## Update cabal files, update changelogs and create commits
################################################################################

if [[ -n $consensus_new_version ]]; then
    echo "Updating consensus bundle"
    for f in $consensus_packages; do
        echo "- $f"
        # update version in cabal files
        sed -E -i "/^version:/ s/$consensus_last_version/$consensus_new_version/g" $f/$f.cabal
        # update fixed dep bounds for the packages in this bundle
        regex=$(echo "$consensus_packages" | tr '\n' '|')
        sed -E -i "/${regex%?}/ s/$consensus_last_version/$consensus_new_version/g" $f/$f.cabal
    done

    echo "- Updating changelog"

    ( cd ouroboros-consensus
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release consensus-$consensus_new_version" >/dev/null 2>&1
fi

function replace_caret_up_to {
  packages=$1
  old_ver=$2
  new_ver=$3
  up_to=$4
  cabal_file=$5
  if [[ -n $new_ver ]]; then
      # update caret dep bounds for packages in ouroboros-consensus
      regex=$(echo "$packages" | tr '\n' '|')
      sed -E -i "/${regex%?}/ s/$(echo $old_ver | cut -d'.' -f1-$up_to)/$(echo $new_ver | cut -d'.' -f1-$up_to)/g" $cabal_file
  fi
}

if [[ -n $consensus_test_new_version ]]; then
    echo "Updating consensus-test bundle"
    for f in $consensus_test_packages; do
        echo "- $f"
        # update version in cabal files
        sed -E -i "/^version:/ s/$consensus_test_last_version/$consensus_test_new_version/g" $f/$f.cabal
        # update fixed dep bounds for the packages in this bundle
        regex=$(echo "$consensus_test_packages" | tr '\n' '|')
        sed -E -i "/${regex%?}/ s/$consensus_test_last_version/$consensus_test_new_version/g" $f/$f.cabal

        replace_caret_up_to \
            "$consensus_packages"  \
            "$consensus_last_version" \
            "$consensus_new_version" \
            3                      \
            "$f/$f.cabal"
    done

    echo "- Updating changelog"

    ( cd ouroboros-consensus-test
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release consensus-test-$consensus_test_new_version" >/dev/null 2>&1
else
    if [[ -n $consensus_new_version ]]; then
        echo "WARNING: Ouroboros-consensus released a new version but Ouroboros-consensus-test didn't"
        echo "Are you adding new functionality that you forgot to test?"
    fi
fi

if [[ -n $cardano_new_version ]]; then
    echo "Updating cardano bundle"
    for f in $cardano_packages; do
        echo "- $f"
        # update version in cabal files
        sed -E -i "/^version:/ s/$cardano_last_version/$cardano_new_version/g" $f/$f.cabal
        # update fixed dep bounds for the packages in this bundle
        regex=$(echo "$cardano_packages" | tr '\n' '|')
        sed -E -i "/${regex%?}/ s/$cardano_last_version/$cardano_new_version/g" $f/$f.cabal

        replace_caret_up_to \
            "$consensus_packages"  \
            "$consensus_last_version" \
            "$consensus_new_version" \
            2                      \
            "$f/$f.cabal"

        replace_caret_up_to \
            "$consensus_test_packages"  \
            "$consensus_test_last_version" \
            "$consensus_test_new_version" \
            2                           \
            "$f/$f.cabal"
    done

    echo "- Updating changelog"
    ( cd ouroboros-consensus-cardano
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release cardano-$cardano_new_version" >/dev/null 2>&1
fi

if [[ -n $cardano_test_new_version ]]; then
    echo "Updating cardano-test bundle"
    for f in $cardano_test_packages; do
        echo "- $f"
        # update version in cabal files
        sed -E -i "/^version:/ s/$cardano_test_last_version/$cardano_test_new_version/g" $f/$f.cabal
        # update fixed dep bounds for the packages in this bundle
        regex=$(echo "$cardano_test_packages" | tr '\n' '|')
        sed -E -i "/${regex%?}/ s/$cardano_test_last_version/$cardano_test_new_version/g" $f/$f.cabal

        replace_caret_up_to \
            "$cardano_packages"  \
            "$cardano_last_version" \
            "$cardano_new_version" \
            3                    \
            "$f/$f.cabal"

        replace_caret_up_to \
            "$consensus_packages"  \
            "$consensus_last_version" \
            "$consensus_new_version" \
            2                      \
            "$f/$f.cabal"

        replace_caret_up_to \
            "$consensus_test_packages"  \
            "$consensus_test_last_version" \
            "$consensus_test_new_version" \
            2                           \
            "$f/$f.cabal"
    done

    echo "- Updating changelog"
    ( cd ouroboros-consensus-cardano-test
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release cardano-test-$cardano_test_new_version" >/dev/null 2>&1
else
    if [[ -n $cardano_new_version ]]; then
        echo "WARNING: Ouroboros-consensus-cardano released a new version but Ouroboros-consensus-cardano-test didn't"
        echo "Are you adding new functionality that you forgot to test?"
    fi
fi

echo "Checking that the versions are consistent with the contents of the changelog directories"
./scripts/ci/check-consensus-release.sh

echo "Checking that CHaP would be able to build these packages (this will take a while!)"
./scripts/check-consensus-builds-with-chap.sh

if [[ -z $? ]]; then
  echo "Succesfully created release. You should now inspect the current branch (${branch%?}) and if everything looks right, push it to GitHub, open a PR and get it merged!"
else
  echo "Your release seem to depend on changes that are not visible in CHaP/Hackage"
  echo "Maybe you added a function to a separate package or are using a package that is not in those indices?"
  echo "I created the branch ${branch%?} but it is probably wrong, so you should delete it, check the error above and re-run this script. I will leave the branch just in case."
fi

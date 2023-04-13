#!/usr/bin/env bash

set -o pipefail

function last_version {
  grep "<a id=" $1/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1
}

function last_tag {
  git tag -l | grep -e "release-$1-[0-9]" | sort | tail -n1 | rev | cut -d'-' -f1 | rev
}

function pkgs_in_bundle {
  sed '/Changelog entries/q' $1/CHANGELOG.md | grep "\- \`ouroboros-" | cut -d\` -f2 | sort | uniq
}

consensus_packages=$(pkgs_in_bundle ouroboros-consensus)
consensus_last_version=$(last_version ouroboros-consensus)
consensus_last_tag=$(last_tag consensus)

consensus_test_packages=$(pkgs_in_bundle ouroboros-consensus-test)
consensus_test_last_version=$(last_version ouroboros-consensus-test)
consensus_test_last_tag=$(last_tag ouroboros-consensus-test)

cardano_packages=$(pkgs_in_bundle ouroboros-consensus-cardano)
cardano_last_version=$(last_version ouroboros-consensus-cardano)
cardano_last_tag=$(last_tag cardano)

cardano_test_packages=$(pkgs_in_bundle ouroboros-consensus-cardano-test)
cardano_test_last_version=$(last_version ouroboros-consensus-cardano-test)
cardano_test_last_tag=$(last_tag cardano-test)

exclude=(cardano-ping
         ouroboros-network-testing
         monoidal-synchronisation
         network-mux
         ouroboros-network
         ouroboros-network-api
         ouroboros-network-mock
         ouroboros-network-protocols
         ouroboros-network-framework
         ouroboros-network-testing
         ntp-client
         cardano-client
        )

while IFS= read -r line
do
  if [[ $(echo "$line" | grep "\./") ]]; then
  pkg=$(echo "$line" | cut -d'/' -f2)
    if [[ $(echo "${exclude[*]}" | grep -w $pkg) ]]; then
      if [[ $(echo "$line" | cut -d':' -f1) == packages ]]; then
        echo "packages:"
      fi
    elif [[ $(echo "${consensus_packages[*]} ${consensus_test_packages[*]} ${cardano_packages[*]} ${cardano_test_packages}" | grep -w $pkg) ]]; then
      echo "$line"
    else
      not_found=$pkg
    fi

  else
    echo "$line"
  fi
done < cabal.project > cabal.project.consensus

if [[ -n $not_found ]]; then
  echo "Package $not_found is not known as a Consensus or Network package"
  rm cabal.project.consensus
  exit 1
fi

echo "I will now reset the packages to the last known version in CHaP"

if [[ $consensus_last_version == $consensus_last_tag ]]; then
  for pkg in $consensus_packages; do
    echo "Resetting $pkg to last published version $consensus_last_version"
    git restore --source=release-consensus-$consensus_last_version --staged --worktree -- $pkg
  done
fi

if [[ $consensus_test_last_version == $consensus_test_last_tag ]]; then
  for pkg in $consensus_test_packages; do
    echo "Resetting $pkg to last published version $consensus_test_last_version"
    git restore --source=release-consensus-test-$consensus_test_last_version --staged --worktree -- $pkg
  done
fi

if [[ $cardano_last_version == $cardano_last_tag ]]; then
  for pkg in $cardano_packages; do
    echo "Resetting $pkg to last published version $cardano_last_version"
    git restore --source=release-cardano-$cardano_last_version --staged --worktree -- $pkg
  done
fi

if [[ $cardano_test_last_version == $cardano_test_last_tag ]]; then
  for pkg in $cardano_test_packages; do
    echo "Resetting $pkg to last published version $cardano_test_last_version"
   git restore --source=release-cardano-test-$cardano_test_last_version --staged --worktree -- $pkg
  done
fi

cabal build --project-file cabal.project.consensus --builddir dist-chap all 2>&1 | ts "> "

# reset the git tree
git reset --hard HEAD

rm cabal.project.consensus

#!/usr/bin/env bash

set -eo pipefail

function check_project () {
  project=$1
  n=$()
  if [[ -n $(git diff --name-only master..HEAD -- $project) ]];then
    if [[ -z $(git diff --name-only master..HEAD -- $project/CHANGELOG.md) ]]; then
      echo "$project was modified but its CHANGELOG was not updated"
      exit 1
    fi
  fi
}

for cbl in $(fd -e 'cabal'); do
  check_project $(dirname $cbl)
done

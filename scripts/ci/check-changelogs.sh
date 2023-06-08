#!/usr/bin/env bash

FD="$(which fdfind 2>/dev/null || which fd 2>/dev/null)"

set -eo pipefail

function check_project () {
  project=$1
  n=$()
  if [[ -n $(git diff --name-only origin/master..HEAD -- $project) ]];then
    if [[ -z $(git diff --name-only origin/master..HEAD -- $project/CHANGELOG.md) ]]; then
      echo "$project was modified but its CHANGELOG was not updated"
      exit 1
    fi
  fi
}

for cbl in $($FD -e 'cabal'); do
  check_project $(dirname $cbl)
done

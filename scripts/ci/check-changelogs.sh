#!/usr/bin/env bash

FD="$(which fdfind 2>/dev/null || which fd 2>/dev/null)"

set -eo pipefail

RESULT=0

MERGE_BASE=$(git merge-base main HEAD)

function check_project () {
  project=$1
  n=$()
  if [[ -n $(git diff --name-only $MERGE_BASE HEAD -- $project) ]];then
    if [[ -z $(git diff --name-only $MERGE_BASE HEAD -- $project/changelog.d/*.md) ]]; then
      echo "$project was modified but its CHANGELOG was not updated"
      RESULT=1
    fi
  fi
}

for cbl in $($FD -e 'cabal'); do
  check_project $(dirname $cbl)
done

exit $RESULT

#!/usr/bin/env bash

set -euo pipefail

if [[ $# -gt 0 ]]; then
  case "$1" in
    --changes)
      files=$(git diff --diff-filter=MA --name-only origin/master HEAD -- '*.hs')
      if [[ -n "$files" ]]; then
        # Run fourmolu on changes compared to `master`.
        fourmolu -m inplace $(echo "$files" | grep -v Setup.hs)
      fi
      ;;
    *)
      echo "Invalid option: $1" >&2
      exit 1
      ;;
  esac
else
  fourmolu -m inplace $(git ls-files -- '*.hs' | grep -v Setup.hs)
fi

git diff --exit-code

#!/usr/bin/env bash

set -e

case "$(uname -s)" in
  Darwin) GREP=ggrep  ;;
  *)      GREP=grep   ;;
esac


RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

mkdir -p tmp

rm -rf tmp/dep-repo-result

# Convert source-repository-package stanzas to lines 'location: $url tag: $hash'
cat cabal.project \
    | "$GREP" -v -E -e '^ *--' \
    | "$GREP" -E -w -e '^source-repository-package' -e '^ *location:' -e '^ *tag:' \
    | tr '\n\r' ' ' \
    | sed 's/source-repository-package/\n/g' \
    > tmp/repositories.txt

basedir="$(pwd)"

# Check each repository
cat tmp/repositories.txt | while read -r line; do
  # re-read the line into an array of words
  read -r -a words <<< "$line"

  # skip whitespace-only lines
  [ "${#words[@]}" -eq 0 ] && continue

  # parse the line as "location: $url tag: $hash"
  [ "${#words[@]}" -eq 4 ] && [ "${words[0]}" = "location:" ] && [ "${words[2]}" = "tag:" ] \
      || { echo "Could not parse as '^ *location: +\$url +tag: +\$hash *$': $line"
	   false
         }
  url="${words[1]}"
  hash="${words[3]}"

  rm -f tmp/tmp-dep-repo-result
  rm -rf tmp/dep-repo
  echo "Checking $url"

  # clone, but don't download any files (ie no trees, so no blobs either)
  git clone --filter=tree:0 --no-checkout "$url" tmp/dep-repo
  ( cd tmp/dep-repo

    { git branch -r --contains "$hash"
      git tag --contains "$hash"
    } \
        | "$GREP" -q -E -w \
               -e '^ *origin/master' \
               -e '^ *origin/develop' \
               -e '^ *origin/main' \
               -e '^ *origin/[^ ]*release' \
               -e '*v[0-9]+' \
        || { echo "Commit $hash from $url is not on the main branch or any release branches" \
                 | tee -a "$basedir/tmp/dep-repo-result"
             false
           }
  )

  rm -rf tmp/dep-repo
done

if [ -s tmp/dep-repo-result ]; then
  printf "${RED}Commits not on the main branch or any release branches detected in dependencies${NC}\n"
  printf "${YELLOW}"
  cat tmp/dep-repo-result
  printf "${NC}"
  exit 1
fi

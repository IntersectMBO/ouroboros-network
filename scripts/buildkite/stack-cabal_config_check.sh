#!/bin/sh
set -euo pipefail

# This script checks that the `stack.yaml` and `cabal.project` files have
# consistent git hashes for the packages they depend on. We use
# `cardano-repo-tool`'s `update-cabal-project` command which modifies
# `cabal.project` to be consistent with `stack.yaml`s versions. If the
# diff is non-empty, we know they're out of sync.

# Check that functions are defined.
HELP_TEXT="cardano-repo-tool not found. Please add it to your PATH, either\n\n\
[1] installing it from \
https://github.com/input-output-hk/cardano-repo-tool\n\
[2] (usually for CI) using nix-shell from iohk-ops to bring it in scope: \
https://github.com/input-output-hk/iohk-nix/blob/\
91febccdb615c365dbbda03a9adcaffbafa57846/docs/buildkite-scripts.md"
type cardano-repo-tool &>/dev/null || { echo "${HELP_TEXT}"; exit 1; }
HELP_TEXT="git not found. Please add it to your PATH"
type git               &>/dev/null || { echo "${HELP_TEXT}"; exit 1; }

# Update `cabal.project` to be consistent.
cardano-repo-tool update-cabal-project

# This command exits 1 if the diff is non-empty, and 0 if the
# diff is empty.
HELP_TEXT="\`cabal.project\` and \`stack.yaml\` files are not consistent. If \
\`stack.yaml\` is correct and \`cabal.project\` is out of date, then running \
\`cardano-repo-tool update-cabal-project\` locally will modify \
\`cabal.project\` to be consistent. But if \`cabal.project\` is correct and \
\`stack.yaml\` is out of date, running the aforementioned command will just \
overwrite \`cabal.project\` with the outdated version from \`stack.yaml\`. \
Thus, in this case one must manually copy the git hash from \`cabal.project\` \
to \`stack.yaml\`."
git diff --exit-code || { echo "ERROR:\n${HELP_TEXT}"; exit 1; }

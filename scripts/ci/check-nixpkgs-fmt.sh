
#!/usr/bin/env bash

set -euo pipefail

nixpkgs-fmt `git ls-files -- '*.nix' ':!:nix/sources.nix'`

git diff --exit-code


#!/usr/bin/env bash

set -euo pipefail

nixpkgs-fmt `git ls-files -- '*.nix'`

git diff --exit-code

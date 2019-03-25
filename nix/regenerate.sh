#!/usr/bin/env bash

set -euo pipefail


exec $(nix-build `dirname $0`/iohk-common.nix -A nix-tools.regeneratePackages --no-out-link --option substituters "https://hydra.iohk.io https://cache.nixos.org" --option trusted-substituters "" --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=")

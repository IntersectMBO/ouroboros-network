#!/usr/bin/env bash

set -euo pipefail

# First, try to find the 'fd' command
FD="$(which fdfind 2>/dev/null || which fd 2>/dev/null)"

$FD -e nix -X nixpkgs-fmt

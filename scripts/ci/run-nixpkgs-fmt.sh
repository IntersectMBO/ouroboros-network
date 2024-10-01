#!/usr/bin/env bash

set -euo pipefail

fd -e nix -X nixpkgs-fmt

#!/bin/sh

# Usage: ./nix-shell-wrapper.sh <sub-project>

exec nix-shell shell.nix --run "nix-shell default.nix -A nix-tools._raw.$@.components.all.env"
{ commonLib }:
let
  pkgs = commonLib.pkgs;
  cfg = builtins.fromJSON (builtins.readFile ./old-cardano-sl-src.json);
in pkgs.fetchgit cfg

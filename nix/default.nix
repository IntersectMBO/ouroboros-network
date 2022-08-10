{ system ? builtins.currentSystem, crossSystem ? null, config ? { }
, sourcesOverride ? { } }:
let
  sources = import ./sources.nix { pkgs = import nixpkgs { }; }
    // sourcesOverride;
  iohkNixMain = import sources.iohk-nix { };
  haskellNix = import sources."haskell.nix" { inherit system sourcesOverride; };
  haskellNixArgs = haskellNix.nixpkgsArgs;
  nixpkgs = if (sources ? nixpkgs) then
    (builtins.trace ''
      Not using nixpkgs that haskell.nix is exposing.
              * This means that you've added entry to 'sources.json' via niv.
              * This is fine, but please be aware that you might be getting less cache hits.
              * Use 'niv drop nixpkgs' to use haskell.nix's nixpkgs''
      sources.nixpkgs)
  else
    (builtins.trace "Using haskell.nix's nixpkgs. Good. Sharing is caring"
      haskellNix.sources.nixpkgs-unstable);
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNixArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNixMain.overlays.haskell-nix-extra ++ iohkNixMain.overlays.crypto
    # iohkNix: nix utilities and niv:
    ++ iohkNixMain.overlays.iohkNix ++ iohkNixMain.overlays.utils
    # our own overlays:
    ++ [
      (pkgs: _:
        with pkgs; {

          # commonLib: mix pkgs.lib with iohk-nix utils and our own:
          commonLib = lib // iohkNix // import ./util.nix {
            inherit haskell-nix;
          }
          # also expose our sources and overlays
            // {
              inherit overlays sources;
            };
        })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
      # for LMDB cross compilation
      # remove once our nixpkgs pin contains https://github.com/NixOS/nixpkgs/pull/171686
      (self: super:
        super.lib.optionalAttrs super.stdenv.hostPlatform.isWindows {
          lmdb = super.lmdb.overrideAttrs (oldAttrs: {
            makeFlags = oldAttrs.makeFlags ++ [ "SOEXT=.dll" "BINEXT=.exe" ];
            buildInputs = [ super.windows.pthreads ];
            patches = [ ./lmdb-mingw.patch ];
          });
        })
    ] ++ [
      # This overlay adds a field localConfig to the pkgs that will be used
      # afterwards to retrieve the locally defined values for building the
      # environment, like ghcVersion.
      (self: super: {
        localConfig = (super.localConfig or { }) // import ./local-config.nix;
      })
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNixArgs.config // config;
  };

in pkgs

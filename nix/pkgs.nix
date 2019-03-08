{ pkgs ? import <nixpkgs> {}
, iohk-overlay ? {}
, iohk-module ? {}
, haskell
, ...
}:
let

  # our packages
  stack-pkgs = import ./.stack-pkgs.nix;

  # packages which will require TH and thus
  # will need -fexternal-interpreter treatment
  # when cross compiling.
  # list is derived from
  # `stack dot --external | grep "template-haskell"`
  th-packages = [
          "QuickCheck"
          "aeson"
          "bifunctors"
          "cardano-sl"
          "cardano-sl-binary"
          "cardano-sl-chain"
          "cardano-sl-core"
          "cardano-sl-util"
          "cardano-sl-util-test"
          "cmdargs"
          "deriving-compat"
          "ether"
          "exceptions"
          "file-embed"
          "file-embed-lzma"
          "free"
          "generics-sop"
          "half"
          "hedgehog"
          "invariant"
          "iohk-monitoring"
          "katip"
          "lens"
          "microlens-th"
          "neat-interpolation"
          "recursion-schemes"
          "reflection"
          "safecopy"
          "semigroupoids"
          "serokell-util"
          "swagger2"
          "tagged"
          "th-abstraction"
          "th-expand-syns"
          "th-lift"
          "th-lift-instances"
          "th-orphans"
          "th-reify-many"
          "th-utilities"
          "wai-app-static"
          "wreq"
          "yaml"
        ];

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.overlay haskell.hackage).compiler.nix-name;
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    pkg-def-overlays = [
      iohk-overlay.${compiler}
    ];
    modules = [
      (iohk-module { nixpkgs = pkgs;
                     inherit th-packages; })

      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.components.library.doExactConfig = true;
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }  

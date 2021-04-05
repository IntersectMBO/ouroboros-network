{ pkgs
}:
let
  hsPkgs = pkgs.haskell-nix.stackProject {
    compiler-nix-name = "ghc8104";
    modules = [ ];
    src = pkgs.fetchFromGitHub {
      owner = "jaspervdj";
      repo = "stylish-haskell";
      # 0.12.2.0 with custom feature that will be added to 0.12.3.0 release
      # see https://github.com/jaspervdj/stylish-haskell/commit/3d5348041e2efbcae95f1c62ace5ac0ffd9629d2
      rev = "3d5348041e2efbcae95f1c62ace5ac0ffd9629d2";
      sha256 = "1rms68fll6w1rzdg9ppjs9g246mbgxi5589vxb8r8pcn5v1ncfj2";
    };
  };
in
hsPkgs.stylish-haskell.components.exes.stylish-haskell

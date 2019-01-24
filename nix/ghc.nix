{ nixpkgs }:
let
  /* Building on 8.6.* with QuickCheck 2.12 requires some special package overrides */
  compiler = nixpkgs.haskell.packages.ghc863.override {
    overrides = self: super: {
      memory = self.callPackage ./memory-0.14.18.nix {};
      microlens-th = self.callPackage ./microlens-th-0.4.2.3.nix {};
      unliftio = self.callPackage ./unliftio-0.2.10.nix {};
      unliftio-core = self.callPackage ./unliftio-core-0.1.2.0.nix {};
      QuickCheck = self.callPackage ./QuickCheck-2.12.6.1.nix {};
      base-compat = self.callPackage ./base-compat-0.10.5.nix {};
      base-compat-batteries = self.callPackage ./base-compat-batteries-0.10.5.nix {};
      optparse-applicative = self.callPackage ./optparse-applicative-0.14.3.0.nix {};
      quickcheck-instances = self.callPackage ./quickcheck-instances-0.3.19.nix {};
      test-framework-quickcheck2 = self.callPackage ./test-framework-quickcheck2-0.3.0.5.nix {};
      quickcheck-state-machine = self.callPackage ./quickcheck-state-machine-0.6.0.nix {};
      psqueues = self.callPackage ./psqueues-0.2.7.1.nix {};
      ChasingBottoms = self.callPackage ./ChasingBottoms-1.3.1.5.nix {};
      contravariant = self.callPackage ./contravariant-1.5.nix {};
      /* Tests seem to fail for this one */
      doctest = nixpkgs.haskell.lib.dontCheck (self.callPackage ./doctest-0.16.0.1.nix {});
      polyparse = self.callPackage ./polyparse-1.12.1.nix {};
      haskell-src-exts = self.callPackage ./haskell-src-exts-1.20.3.nix {};
      haskell-src-meta = self.callPackage ./haskell-src-meta-0.8.0.3.nix {};
      hspec = self.callPackage ./hspec-2.6.0.nix {};
      hspec-core = self.callPackage ./hspec-core-2.6.0.nix {};
      hspec-discover = self.callPackage ./hspec-discover-2.6.0.nix {};
      hspec-meta = self.callPackage ./hspec-meta-2.6.0.nix {};
      tasty-hspec = self.callPackage ./tasty-hspec-1.1.5.1.nix {};
      unordered-containers = self.callPackage ./unordered-containers-0.2.10.0.nix {};
      ekg = self.callPackage ./ekg-0.4.0.15.nix {};
      ekg-core = self.callPackage ./ekg-core-0.1.1.6.nix {};
      ekg-json = self.callPackage ./ekg-json-0.1.0.6.nix {};
      ekg-statsd = self.callPackage ./ekg-statsd-HEAD.nix {};
      safe-exceptions = self.callPackage ./safe-exceptions-0.1.7.0.nix {};
      lens = self.callPackage ./lens-4.17.nix {};
      semigroupoids = self.callPackage ./semigroupoids-5.3.1.nix {};
      free = self.callPackage ./free-5.1.nix {};
      /* Still doesn't admit QuickCheck >= 2.12 so we disable tests */
      aeson = nixpkgs.haskell.lib.dontCheck (self.callPackage ./aeson-1.4.2.0.nix {});
      aeson-compat = self.callPackage ./aeson-compat-0.3.9.nix {};
      aeson-options-HEAD = self.callPackage ./aeson-options-HEAD.nix {};
      io-streams = self.callPackage ./io-streams-1.5.0.1.nix {};
      io-streams-haproxy = self.callPackage ./io-streams-haproxy-1.0.0.2.nix {};
      base-orphans = self.callPackage ./base-orphans-0.8.nix {};
      foldl = self.callPackage ./foldl-1.4.5.nix {};
      katip = self.callPackage ./katip-0.7.0.0.nix {};
      snap = self.callPackage ./snap-1.1.1.0.nix {};
      snap-core = self.callPackage ./snap-core-1.0.3.2.nix {};
      snap-server = self.callPackage ./snap-server-1.1.0.0.nix {};
      fingertree = self.callPackage ./fingertree-0.1.4.2.nix {};
      cborg = self.callPackage ./cborg-0.2.1.0.nix {};
      feed = self.callPackage ./feed-1.0.1.0.nix {};
      /* One test case fails. Hopefully fix will come soon. */
      iohk-monitoring = nixpkgs.haskell.lib.dontCheck (self.callPackage ./iohk-monitoring-HEAD.nix {});
    };
  };
in
  compiler

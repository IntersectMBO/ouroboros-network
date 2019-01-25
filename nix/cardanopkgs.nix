/* Package overrides for cardano-sl the library */

{ nixpkgs, cardanoroot }: self: super: {

  rocksdb-haskell-ng = self.callPackage ./rocksdb-haskell-ng-0.0.0.nix {};
  cardano-crypto = self.callPackage ./cardano-crypto-1.2.0.nix {};
  plutus-prototype = self.callPackage ./plutus-prototype-0.1.0.0.nix {};
  cardano-report-server = self.callPackage ./cardano-report-server-0.5.10.nix {};
  # canonical-json which is containers >= 0.6 cmopatible is not on hackage.
  canonical-json = self.callPackage ./canonical-json-HEAD.nix {};
  Glob = nixpkgs.haskell.lib.dontCheck (self.callPackage ./Glob-0.10.0.nix {});
  # Can't test with our bleeding-edge QuickCheck choice.
  Diff = nixpkgs.haskell.lib.dontCheck super.Diff;
  aeson-options = self.callPackage ./aeson-options-HEAD.nix {};
  network-transport = self.callPackage ./network-transport-HEAD.nix {};
  network-transport-inmemory = self.callPackage ./network-transport-inmemory-HEAD.nix {};
  network-transport-tcp = self.callPackage ./network-transport-tcp-HEAD.nix {};
  generics-sop = self.callPackage ./generics-sop-0.4.0.1.nix {};
  sop-core = self.callPackage ./sop-core-0.4.0.0.nix {};
  cereal = self.callPackage ./cereal-0.5.8.0.nix {};
  ekg-statsd = self.callPackage ./ekg-statsd-0.2.4.0.nix {};
  entropy = self.callPackage ./entropy-0.4.1.4.nix {};
  cipher-aes128 = self.callPackage ./cipher-aes128-0.7.0.4.nix {};
  http-media = self.callPackage ./http-media-0.7.1.3.nix {};
  http-api-data = self.callPackage ./http-api-data-0.4.nix {};
  http-types = self.callPackage ./http-types-0.12.2.nix {};
  insert-ordered-containers = self.callPackage ./insert-ordered-containers-0.2.1.0.nix {};
  lrucache = self.callPackage ./lrucache-1.2.0.1.nix {};
  # name collision between haskell package and system package must be manually resolved.
  # Also incompatible with our tasty version, so we turn off tests.
  lzma = nixpkgs.haskell.lib.dontCheck (self.callPackage ./lzma-0.0.0.3.nix { lzma = nixpkgs.lzma; });
  pretty-show = self.callPackage ./pretty-show-1.8.2.nix {};
  safecopy = self.callPackage ./safecopy-0.9.4.3.nix {};
  singleton-bool = self.callPackage ./singleton-bool-0.1.4.nix {};
  stringsearch = self.callPackage ./stringsearch-0.3.6.6.nix {};
  tasty-expected-failure = self.callPackage ./tasty-expected-failure-0.11.1.1.nix {};
  RSA = self.callPackage ./RSA-2.3.1.nix {};
  lifted-async = self.callPackage ./lifted-async-0.10.0.3.nix {};
  th-expand-syns = self.callPackage ./th-expand-syns-0.4.4.0.nix {};
  file-embed-lzma = self.callPackage ./file-embed-lzma-0.nix {};
  concurrent-output = self.callPackage ./concurrent-output-1.10.9.nix {};
  # we need a custom hedgehog
  hedgehog = self.callPackage ./hedgehog-HEAD.nix {};
  tasty-hedgehog = self.callPackage ./tasty-hedgehog-0.2.0.0.nix {};
  uri-bytestring = nixpkgs.haskell.lib.dontCheck (self.callPackage ./uri-bytestring-0.3.2.1.nix {});
  # Incompatibility with our hedgehog choice.
  o-clock = nixpkgs.haskell.lib.dontCheck (self.callPackage ./o-clock-1.0.0.1.nix {});
  swagger2 = self.callPackage ./swagger2-2.3.1.nix {};
  these = self.callPackage ./these-0.7.5.nix {};
  transformers-lift = self.callPackage ./transformers-lift-0.2.0.1.nix {};
  ether = self.callPackage ./ether-HEAD.nix {};
  attoparsec-iso8601 = self.callPackage ./attoparsec-iso8601-1.0.1.0.nix {};
  HTF = nixpkgs.haskell.lib.dontCheck (self.callPackage ./HTF-0.13.2.5.nix {});
  servant = self.callPackage ./servant-0.15.nix {};
  servant-blaze = self.callPackage ./servant-blaze-0.8.nix {};
  servant-client = self.callPackage ./servant-client-0.15.nix {};
  servant-client-core = self.callPackage ./servant-client-core-0.15.nix {};
  servant-quickcheck = self.callPackage ./servant-quickcheck-0.0.7.3.nix {};
  servant-server = self.callPackage ./servant-server-0.15.nix {};
  tdigest = self.callPackage ./tdigest-0.2.1.nix {};
  universum = nixpkgs.haskell.lib.dontCheck (self.callPackage ./universum-iohk.nix {});
  ekg-wai = self.callPackage ./ekg-wai-0.1.0.3.nix {};
  # Warp tests fail... uh oh
  warp = nixpkgs.haskell.lib.dontCheck (self.callPackage ./warp-3.2.26.nix {});
  network = nixpkgs.haskell.lib.dontCheck (self.callPackage ./network-2.8.0.0.nix {});
  resourcet = self.callPackage ./resourcet-1.2.2.nix {};
  wai-extra = self.callPackage ./wai-extra-3.0.24.3.nix {};
  kademlia = nixpkgs.haskell.lib.dontCheck (self.callPackage ./kademlia-HEAD.nix {});
  openssl-streams = self.callPackage ./openssl-streams-1.2.1.3.nix {};
  megaparsec = self.callPackage ./megaparsec-7.0.4.nix {};
  neat-interpolation = self.callPackage ./neat-interpolation-0.3.2.4.nix {};
  fmt = self.callPackage ./fmt-0.6.1.1.nix {};
  serokell-util = self.callPackage ./serokell-util-iohk.nix {};
  log-warper = self.callPackage ./log-warper-HEAD.nix {};
  servant-swagger = self.callPackage ./servant-swagger-1.1.7.nix {};
  servant-swagger-ui = self.callPackage ./servant-swagger-ui-0.3.2.3.19.3.nix {};
  servant-swagger-ui-core = self.callPackage ./servant-swagger-ui-core-0.3.2.nix {};
  servant-swagger-ui-redoc = self.callPackage ./servant-swagger-ui-redoc-0.3.2.1.22.2.nix {};
  recursion-schemes = self.callPackage ./recursion-schemes-5.1.nix {};

  cardano-sl-util = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-util" (cardanoroot + /util) {});
  cardano-sl-util-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-util-test" (cardanoroot + /util/test) {});
  cardano-sl-binary = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-binary" (cardanoroot + /binary) {});
  cardano-sl-binary-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-binary-test" (cardanoroot + /binary/test) {});
  cardano-sl-core = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-core" (cardanoroot + /core) {});
  cardano-sl-core-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-core-test" (cardanoroot + /core/test) {});
  cardano-sl-chain = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-chain" (cardanoroot + /chain) {});
  cardano-sl-chain-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-chain-test" (cardanoroot + /chain/test) {});
  cardano-sl-crypto = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-crypto" (cardanoroot + /crypto) {});
  cardano-sl-crypto-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-crypto-test" (cardanoroot + /crypto/test) {});
  cardano-sl-db = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-db" (cardanoroot + /db) {});
  cardano-sl-db-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-db-test" (cardanoroot + /db/test) {});
  cardano-sl-networking = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-networking" (cardanoroot + /networking) {});
  cardano-sl-infra = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-infra" (cardanoroot + /infra) {});
  cardano-sl-infra-test = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl-infra-test" (cardanoroot + /infra/test) {});
  cardano-sl = nixpkgs.haskell.lib.dontCheck (self.callCabal2nix "cardano-sl" (cardanoroot + /lib) {});
}

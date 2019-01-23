{ dontCheck
}:
let
in
  self: super: {
    # Custom packages not in hackage, needed for cardano-sl.
    rocksdb-haskell-ng = self.callPackage ./rocksdb-haskell-ng-0.0.0.nix {};
    cardano-report-server = self.callPackage ./cardano-report-server-0.5.10.nix {};
    cardano-crypto = self.callPackage ./cardano-crypto-1.2.0.nix {};
    plutus-prototype = self.callPackage ./plutus-prototype-0.1.0.0.nix {};

    # New quickcheck has _massive_ fallout.
    # Many packages don't allow for >= 2.12; even more do allow it but fail due
    # to type changes.
    # Alas, we use the new features, so need it.
    QuickCheck = self.callPackage ./QuickCheck-2.12.6.1.nix {};
    quickcheck-instances = self.callPackage ./quickcheck-instances-0.3.19.nix {};
    HTF = dontCheck (self.callPackage ./HTF-0.13.2.5.nix {});
    test-framework-quickcheck2 = self.callPackage ./test-framework-quickcheck2-0.3.0.5.nix {};
    hspec = self.callPackage ./hspec-2.6.0.nix {};
    hspec-core = self.callPackage ./hspec-core-2.6.0.nix {};
    hspec-discover = self.callPackage ./hspec-discover-2.6.0.nix {};
    hspec-meta= self.callPackage ./hspec-meta-2.6.0.nix {};
    tasty-hspec = self.callPackage ./tasty-hspec-1.1.5.1.nix {};
    # Also QuickCheck  version incompatibilty.
    # Latest version can use QuickCheck 2.12, but its test-suite depends upon
    # test-framework-quickcheck, which is deprecated and incompatible.
    ChasingBottoms = dontCheck (self.callPackage ./ChasingBottoms-1.3.1.5.nix {});
    # 0.14.2.0 incompatible with QuickCheck 2.12
    optparse-applicative = self.callPackage ./optparse-applicative-0.14.3.0.nix {};
    # Its test suite is incompatible with quickcheck 2.12.
    blaze-builder = dontCheck super.blaze-builder;
    blaze-markup = dontCheck super.blaze-markup;
    cereal = dontCheck super.cereal;
    Diff = dontCheck super.Diff;
    psqueues = dontCheck super.psqueues;
    # Something needs aeson >= 1.4
    # That has a big fallout.
    aeson = dontCheck (self.callPackage ./aeson-1.4.2.0.nix {});
    aeson-compat = self.callPackage ./aeson-compat-0.3.9.nix {};
    # serokell package; aeson bound is <1.4.
    aeson-options = self.callPackage ./aeson-options-HEAD.nix {};
    base-compat = self.callPackage ./base-compat-0.10.5.nix {};
    base-compat-batteries = self.callPackage ./base-compat-batteries-0.10.5.nix {};
    contravariant = self.callPackage ./contravariant-1.5.nix {};
    either = dontCheck super.either;
    http-media = self.callPackage ./http-media-0.7.1.3.nix {};
    o-clock = self.callPackage ./o-clock-1.0.0.1.nix {};
    insert-ordered-containers = dontCheck super.insert-ordered-containers;
    swagger2 = self.callPackage ./swagger2-2.3.1.nix {};
    these = self.callPackage ./these-0.7.5.nix {};
    fmt = self.callPackage ./fmt-0.6.1.1.nix {};
    # Must bump aeson upper bound, but not use megaparsec as they do in mainline.
    serokell-util = self.callPackage ./serokell-util-iohk.nix {};
    # Why are we still using this?
    log-warper = self.callPackage ./log-warper-HEAD.nix {};

    servant = self.callPackage ./servant-0.15.nix {};
    servant-server = self.callPackage ./servant-server-0.15.nix {};
    servant-client = self.callPackage ./servant-client-0.15.nix {};
    servant-client-core = self.callPackage ./servant-client-core-0.15.nix {};
    servant-blaze = self.callPackage ./servant-blaze-0.8.nix {};
    servant-swagger = self.callPackage ./servant-swagger-1.1.7.nix {};
    servant-swagger-ui = self.callPackage ./servant-swagger-ui-0.3.2.3.19.3.nix {};
    servant-swagger-ui-core = self.callPackage ./servant-swagger-ui-core-0.3.2.nix {};
    servant-swagger-ui-redoc = self.callPackage ./servant-swagger-ui-redoc-0.3.2.1.22.2.nix {};
    # servant-swagger apparently needs the latest lens package
    lens = self.callPackage ./lens-4.17.nix {};
    servant-quickcheck = self.callPackage ./servant-quickcheck-0.0.7.3.nix {};
    entropy = self.callPackage ./entropy-0.4.1.4.nix {};
    semigroupoids = self.callPackage ./semigroupoids-5.3.1.nix {};
    base-orphans = self.callPackage ./base-orphans-0.8.nix {};
    http-api-data = self.callPackage ./http-api-data-0.4.nix {};
    http-types = self.callPackage ./http-types-0.12.2.nix {};
    tagged = self.callPackage ./tagged-0.8.6.nix {};
    attoparsec-iso8601 = self.callPackage ./attoparsec-iso8601-1.0.1.0.nix {};
    # They released it with failing tests, maybe?
    warp = dontCheck (self.callPackage ./warp-3.2.25.nix {});
    wai-extra = self.callPackage ./wai-extra-3.0.24.3.nix {};
    resourcet = self.callPackage ./resourcet-1.2.2.nix {};
    network = dontCheck (self.callPackage ./network-2.8.0.0.nix {});
    # test suite fails due to some automake/autoreconf issue.
    # HsNetworkConfig.h is not generated there, but it _is_ for the
    # library itself.
    io-streams = self.callPackage ./io-streams-1.5.0.1.nix {};
    io-streams-haproxy = self.callPackage ./io-streams-haproxy-1.0.0.2.nix {};
    openssl-streams = self.callPackage ./openssl-streams-1.2.1.3.nix {};
    snap = self.callPackage ./snap-1.1.1.0.nix {};
    snap-core = self.callPackage ./snap-core-1.0.3.2.nix {};
    snap-server = self.callPackage ./snap-server-1.1.0.0.nix {};
    time-locale-compat = self.callPackage ./time-locale-compat-0.1.1.5.nix {};
    # We still need this D:
    # Custom branch bumps its network bound.
    # mainline froozen/kademlia has no commits since 2015! Perfect software?
    # Tests also fail... luckily we don't _really_ use it...
    # But getting it out of the cardano-sl tree would be at least an hour of
    # surgery.
    kademlia = dontCheck (self.callPackage ./kademlia-HEAD.nix {});

    generics-sop = self.callPackage ./generics-sop-0.4.0.1.nix {};
    sop-core = self.callPackage ./sop-core-0.4.0.0.nix {};
    free = self.callPackage ./free-5.1.nix {};

    # stm API changes also have fallout.
    stm = self.callPackage ./stm-2.5.0.0.nix {};
    hedgehog = self.callPackage ./hedgehog-HEAD.nix {};
    concurrent-output = self.callPackage ./concurrent-output-1.10.9.nix {};
    unliftio = self.callPackage ./unliftio-0.2.10.nix {};
    unliftio-core = self.callPackage ./unliftio-core-0.1.2.0.nix {};
    katip = self.callPackage ./katip-0.7.0.0.nix {};
    # Custom network-transport{-inmemory,-tcp} required for cardano-sl.

    network-transport = self.callPackage ./network-transport-HEAD.nix {};
    network-transport-tcp = dontCheck (self.callPackage ./network-transport-tcp-HEAD.nix {});
    network-transport-inmemory = dontCheck (self.callPackage ./network-transport-inmemory-HEAD.nix {});

    graphviz = self.callPackage ./graphviz-2999.20.0.3.nix {};
    fgl = self.callPackage ./fgl-5.7.0.1.nix {};
    polyparse = self.callPackage ./polyparse-1.12.1.nix {};
    singleton-bool = self.callPackage ./singleton-bool-0.1.4.nix {};
    microlens-th = self.callPackage ./microlens-th-0.4.2.3.nix {};
    th-expand-syns = self.callPackage ./th-expand-syns-0.4.4.0.nix {};
    doctest = self.callPackage ./doctest-0.16.0.1.nix {};
    Glob = self.callPackage ./Glob-0.10.0.nix {};
    tasty = self.callPackage ./tasty-1.2.nix {};
    tasty-expected-failure = self.callPackage ./tasty-expected-failure-0.11.1.1.nix {};
    tasty-ant-xml = self.callPackage ./tasty-ant-xml-1.1.5.nix {};
    tasty-hedgehog = self.callPackage ./tasty-hedgehog-0.2.0.0.nix {};
    memory = self.callPackage ./memory-0.14.18.nix {};
    safe-exceptions = self.callPackage ./safe-exceptions-0.1.7.0.nix {};
    unordered-containers = self.callPackage ./unordered-containers-0.2.10.0.nix {};
    charset = self.callPackage ./charset-0.3.7.1.nix {};
    ekg = self.callPackage ./ekg-0.4.0.15.nix {};
    ekg-core = self.callPackage ./ekg-core-0.1.1.6.nix {};
    ekg-json = self.callPackage ./ekg-json-0.1.0.6.nix {};
    ekg-statsd = self.callPackage ./ekg-statsd-HEAD.nix {};
    foldl = self.callPackage ./foldl-1.4.5.nix {};
    haskell-src-exts = self.callPackage ./haskell-src-exts-1.20.3.nix {};
    haskell-src-meta = self.callPackage ./haskell-src-meta-0.8.0.3.nix {};
    fingertree = self.callPackage ./fingertree-0.1.4.2.nix {};
    lifted-async = self.callPackage ./lifted-async-0.10.0.3.nix {};
    persistent = self.callPackage ./persistent-2.9.0.nix {};
    persistent-template = self.callPackage ./persistent-template-2.5.4.nix {};
    persistent-postgresql = self.callPackage ./persistent-postgresql-2.9.0.nix {};
    conduit = self.callPackage ./conduit-1.3.1.nix {};
    quickcheck-state-machine = self.callPackage ./quickcheck-state-machine-0.6.0.nix {};
    tree-diff = self.callPackage ./tree-diff-0.0.2.nix {};
    pretty-show = self.callPackage ./pretty-show-1.9.5.nix {};

    # for decodeStringCanonical
    cborg = self.callPackage ./cborg-0.2.1.0.nix {};

    # Not in nixpkgs apparently... And its test suite demands aseon == 1.4.*
    canonical-json = dontCheck (self.callPackage ./canonical-json-0.5.0.1.nix {});
    # ether is deprecated...
    ether = self.callPackage ./ether-HEAD.nix {};

    # hedge against the ever-changing interface of universum
    universum = self.callPackage ./universum-iohk.nix {};
  }

{
  extras = hackage:
    {
      packages = {
        "bimap" = (((hackage.bimap)."0.4.0").revisions).default;
        "binary" = (((hackage.binary)."0.8.7.0").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "ekg-prometheus-adapter" = (((hackage.ekg-prometheus-adapter)."0.1.0.4").revisions).default;
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.1").revisions).default;
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "splitmix" = (((hackage.splitmix)."0.0.2").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "Win32" = (((hackage.Win32)."2.5.4.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "graphviz" = (((hackage.graphviz)."2999.20.0.3").revisions)."cde383c356bc41136ed53cd27e0800f46dbd2185600dd0de18d66d5c49739d94";
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions)."3e4f8df0f6b5d415e3c8840dc75034a63e37f56f5f8cfa1035ded16345235ac4";
        } // {
        typed-transitions = ./typed-transitions.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-cbor = ./typed-protocols-cbor.nix;
        network-mux = ./network-mux.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-network-testing = ./ouroboros-network-testing.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        io-sim = ./io-sim.nix;
        io-sim-classes = ./io-sim-classes.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        contra-tracer = ./contra-tracer.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-shell = ./cardano-shell.nix;
        cardano-sl-x509 = ./cardano-sl-x509.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }
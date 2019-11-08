{
  extras = hackage:
    {
      packages = {
        "moo" = (((hackage.moo)."1.2").revisions).default;
        "gray-code" = (((hackage.gray-code)."0.3.1").revisions).default;
        "bimap" = (((hackage.bimap)."0.4.0").revisions).default;
        "binary" = (((hackage.binary)."0.8.7.0").revisions).default;
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions).default;
        "graphviz" = (((hackage.graphviz)."2999.20.0.3").revisions).default;
        "hedgehog-quickcheck" = (((hackage.hedgehog-quickcheck)."0.1.1").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions).default;
        "splitmix" = (((hackage.splitmix)."0.0.2").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "Unique" = (((hackage.Unique)."0.4.7.6").revisions).default;
        "Win32" = (((hackage.Win32)."2.6.2.0").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "cborg" = (((hackage.cborg)."0.2.2.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
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
        ntp-client = ./ntp-client.nix;
        Win32-network = ./Win32-network.nix;
        contra-tracer = ./contra-tracer.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        small-steps = ./small-steps.nix;
        cs-ledger = ./cs-ledger.nix;
        cs-blockchain = ./cs-blockchain.nix;
        goblins = ./goblins.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-crypto = ./cardano-crypto.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }
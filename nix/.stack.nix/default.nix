{
  extras = hackage:
    {
      packages = {
        "ekg" = (((hackage.ekg)."0.4.0.15").revisions).default;
        "libyaml" = (((hackage.libyaml)."0.1.0.0").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "ekg-json" = (((hackage.ekg-json)."0.1.0.6").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "half" = (((hackage.half)."0.2.2.3").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
        "graphviz" = (((hackage.graphviz)."2999.20.0.3").revisions)."cde383c356bc41136ed53cd27e0800f46dbd2185600dd0de18d66d5c49739d94";
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions)."3e4f8df0f6b5d415e3c8840dc75034a63e37f56f5f8cfa1035ded16345235ac4";
        } // {
        typed-transitions = ./typed-transitions.nix;
        typed-protocols = ./typed-protocols.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-network-testing = ./ouroboros-network-testing.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        io-sim = ./io-sim.nix;
        io-sim-classes = ./io-sim-classes.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        contra-tracer = ./contra-tracer.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        hedgehog = ./hedgehog.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.4";
      compiler.nix-name = "ghc864";
      };
  resolver = "lts-13.16";
  compiler = "ghc-8.6.4";
  }
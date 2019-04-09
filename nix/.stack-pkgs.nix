{
  overlay = hackage:
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
        "pretty-show" = (((hackage.pretty-show)."1.8.2").revisions).default;
        "graphviz" = (((hackage.graphviz)."2999.20.0.3").revisions)."cde383c356bc41136ed53cd27e0800f46dbd2185600dd0de18d66d5c49739d94";
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions)."3e4f8df0f6b5d415e3c8840dc75034a63e37f56f5f8cfa1035ded16345235ac4";
        } // {
        typed-transitions = ./.stack.nix/typed-transitions.nix;
        typed-protocols = ./.stack.nix/typed-protocols.nix;
        ouroboros-network = ./.stack.nix/ouroboros-network.nix;
        ouroboros-network-testing = ./.stack.nix/ouroboros-network-testing.nix;
        ouroboros-consensus = ./.stack.nix/ouroboros-consensus.nix;
        io-sim = ./.stack.nix/io-sim.nix;
        io-sim-classes = ./.stack.nix/io-sim-classes.nix;
        iohk-monitoring = ./.stack.nix/iohk-monitoring.nix;
        contra-tracer = ./.stack.nix/contra-tracer.nix;
        cardano-binary = ./.stack.nix/cardano-binary.nix;
        cardano-ledger = ./.stack.nix/cardano-ledger.nix;
        cardano-crypto-wrapper = ./.stack.nix/cardano-crypto-wrapper.nix;
        cardano-prelude = ./.stack.nix/cardano-prelude.nix;
        cborg = ./.stack.nix/cborg.nix;
        cardano-crypto = ./.stack.nix/cardano-crypto.nix;
        hedgehog = ./.stack.nix/hedgehog.nix;
        canonical-json = ./.stack.nix/canonical-json.nix;
        };
      compiler.version = "8.6.4";
      compiler.nix-name = "ghc864";
      };
  resolver = "lts-13.13";
  compiler = "ghc-8.6.4";
  }

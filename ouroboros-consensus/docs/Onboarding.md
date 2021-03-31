# Onboarding to the Consensus Team

The content in this file should help orient a new contributor to the Consensus
Layer: its fundamental purpose and its neighboring components.

Please open PRs to add particularly useful content in this file. Though we would
like the file to remain as approachable as possible, therefore small and
narrowly-scoped.

## Quick-start

### Build the project

> note: there might be value to move the content of this section to a README.md and
> only reference it from here

There are multiple ways to build this project. By far the most straight-forward
approach is to use Nix.

#### Pre-requirements

##### 1. Nix

You have a [Nix](https://nixos.org/download.html) installed on your system. You can
verify it by running:

```
$> nix-env --version
nix-env (Nix) 2.3.10
```

##### 2. cachix

> note that this step is not mandatory but highly recommended, without it you
> you will have to "compile the whole world" (including few GHC versions) before
> you will be able to build, test and run our project

* Install cachix

```
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

* Configure cachix

Open your `~/.config/nix/nix.conf` and add/modify following entries

```
substituters = https://cache.nixos.org/ https://ouroboros-network.cachix.org https://hydra.iohk.io/ https://iohk.cachix.org/
trusted-substituters = https://cache.nixos.org/ https://ouroboros-network.cachix.org https://hydra.iohk.io/ https://iohk.cachix.org/
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ouroboros-network.cachix.org-1:YnmfwRhnjzcIiyzuHX5tWfTD08PqFySl6ySldfgTUIY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
```

#### Build & test it

Enter nix shell from project's root folder

```
$> nix-shell -j4
```

Build all

```
[nix-shell] > cabal build all
```

and test all

```
[nix-shell] > cabal test all
```

### Congrats!

Congratulations! You are all set.
Now, [draw the rest of the owl](https://i.kym-cdn.com/photos/images/newsfeed/000/572/078/d6d.jpg).


## Very High-Level Motivation

At a very high level, a net of Cardano nodes should exhibit the following
concrete behaviors.

  * The net should diffuse transactions throughout the net as rapidly as
    possible.

  * The net should diffuse blocks throughout the net as rapidly as possible.

  * Whenever the Ouroboros protocol specifies that (a stake pool operating) a
    particular node should lead, that node should extend the best chain it has
    seen so far by minting a new block, which should contain as many
    transactions as possible.

  * The net should be very difficult to disrupt. For example, a successful
    denial of service attack should be prohibitively expensive to enact.

The primary data are _blocks_ and _transactions_. The possible contents of both
of those are primarily constrained by the ledger rules. The Consensus Layer and
Network Layer together implement a _filtering forwarding network_. Specifically,
the Consensus Layer determines which blocks propagate between neighboring nodes:
those on the _best_ chain. The IOHK researchers have established that the
Ouroboros protocol can be used to ensure that -- unless an adversary controls
more than half of the net's stake -- the honest nodes will all continually reach
_consensus_ regarding the selection of a single best chain and that that chain
grows over time.

The Consensus Layer defines the core Consensus components and logic, notably the
Ouroboros protocol.

## The Neighbors of Consensus

The Consensus Layer integrates the Consensus core with the Network Layer and the
ledger rules. We therefore work closely with the Network Team and the Ledger
Team.

The Network Layer is also defined in this repository. It manages the nodes'
connections to its neighbors. So it ultimately provides communication channels
to the Consensus Layer, while the Consensus Layer reports back to it if a
neighbor has misbehaved etc. The Network Layer also provides the library used to
define the Consensus Layer's _client_ and _server_ state machines that let each
connected pair of nodes exchange messages according to the various _mini
protocols_ (cf `typed-protocols` package).

The ledger rules are defined in
https://github.com/input-output-hk/cardano-ledger-specs. The Consensus Layer
uses the ledger to validate blocks and transactions and apply them in order to
maintain _the ledger state_. The Consensus Layer in turn needs the ledger state
in order to determine when a node is allowed to mint a block, ie the _leader
schedule_.

The primary use of the Consensus Layer is the
https://github.com/input-output-hk/cardano-node. This is what IOHK actually
deploys on the live Cardano mainnet. Such a node runs the full Ouroboros
protocol and mints new blocks. Secondary uses include
https://github.com/input-output-hk/cardano-wallet and
https://github.com/input-output-hk/cardano-db-sync, which connect to a proper
node and only follow and _trust_ its chain selection. For example, these uses
involve problem-specific queries of the latest ledger state.

## Design Documents

The following artifacts influence and/or describe the Consensus implementation.

  * From IOHK researchers:

      * Ouroboros BFT https://iohk.io/en/research/library/papers/ouroboros-bfta-simple-byzantine-fault-tolerant-consensus-protocol/

      * Ouroboros Praos https://iohk.io/en/research/library/papers/ouroboros-praosan-adaptively-securesemi-synchronous-proof-of-stake-protocol/

      * Ouroboros Genesis https://iohk.io/en/research/library/papers/ouroboros-genesiscomposable-proof-of-stake-blockchains-with-dynamic-availability/

      * Internal notes, on the IOHK Google Docs

  * The ledger specifications, cf https://github.com/input-output-hk/cardano-ledger-specs/blob/master/README.md, especially:

      * "Shelley design specification" -> "Design Specification for Delegation
        and Incentives in Cardano" (as of this line's latest commit)

      * "Shelley ledger formal specification" -> "A Formal Specification of the
        Cardano Ledger" (as of this line's latest commit)

      * "Byron chain specification" -> "Specification of the Blockchain Layer"
        (as of this line's latest commit)

      * "Byron ledger specification" -> "A Formal Specification of the Cardano
        Ledger (for the Byron release)" (as of this line's latest commit)

  * Documents and presentations linked in this repository's
    https://github.com/input-output-hk/ouroboros-network/README.md, especially:

      * "Introduction to the design of Data Diffusion and Networking of Cardano
        Shelley"

      * "The Cardano Consensus (and Storage Layer)" (aka "The Consensus Report")

  * Large comments, for example in these modules. This list is a continual work
    in progress -- if you find some comment to be particularly illuminating,
    please open a PR adding it here.

      * `Ouroboros.Consensus.Util.ResourceRegistry`
      * `Ouroboros.Consensus.HeaderValidation`
      * `Ouroboros.Consensus.Mempool.API`
      * `Ouroboros.Consensus.Forecast`
      * `Ouroboros.Consensus.HardFork.History.EraParams`
      * `Ouroboros.Consensus.HardFork.History.Qry`
      * `Ouroboros.Consensus.HardFork.History.Summary`
      * `Ouroboros.Consensus.Protocol.Abstract`
      * `Ouroboros.Consensus.Storage.ChainDB.API`
      * `Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel`
      * `Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator`
      * `Ouroboros.Network.AnchoredFragment`
      * `Ouroboros.Consensus.MiniProtocol.ChainSync.Client`
      * `Ouroboros.Network.BlockFetch.Decision`
      * `Network.TypedProtocol.Core`

  * CI-built Haddock, at https://input-output-hk.github.io/ouroboros-network/

  * IOHK media:

      * https://iohk.io/en/blog/posts/2017/11/03/writing-a-high-assurance-blockchain-implementation/

      * https://iohk.io/en/blog/posts/2018/06/04/semi-formal-development-the-cardano-wallet/

      * 2018 August wallet video https://www.youtube.com/watch?v=6VWCB0_uLLw

      * https://iohk.io/en/blog/posts/2020/05/07/combinator-makes-easy-work-of-shelley-hard-fork/

      * 2020 July hard fork combinator presentation at Cardano2020, on YouTube
        [part 1](https://www.youtube.com/watch?v=D8OTZULEsaI) and [part 2](
        https://www.youtube.com/watch?v=wNZq6VPLIXg)

      * https://iohk.io/en/blog/posts/2020/05/28/the-abstract-nature-of-the-consensus-layer/

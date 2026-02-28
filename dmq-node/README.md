# dmq-node

dmq-node is a Haskell implementation of a Decentralized Message Queue (DMQ) node, part of the ouroboros-network repository. This component enables efficient peer-to-peer message diffusion within the Cardano ecosystem, following the specifications defined in CIP-0137.

## Overview

The Decentralized Message Queue (DMQ) provides a topic-based message diffusion system that leverages the Cardano network layer. The `dmq-node` allows client peers to communicate efficiently by publishing and consuming messages that are diffused over a peer-to-peer network to other nodes.

## Purpose

The DMQ protocol addresses the need for many Cardano ecosystem protocols—including Mithril, Leios, and Peras—to diffuse messages from block producers to subscribers in a decentralized manner. Rather than building separate peer-to-peer networks from scratch, DMQ leverages Cardano's proven infrastructure to reduce development costs and enable fast bootstrapping of nodes.

The primary use case currently is Mithril, where the DMQ node handles signer-to-aggregator communication for multi-signature operations. This enables secure light wallet support and facilitates inter-node communications across the Cardano ecosystem.

## Architecture

### How It Works

The `dmq-node` operates as a standalone process that communicates with `cardano-node` via its local interface using the node-to-client protocol over a Unix socket. This architecture allows the DMQ node to run alongside the Cardano node with minimal impact on performance and no impact on security.

The node utilizes the Ouroboros-Network stack, adapted specifically for DMQ operations. Each stake pool operator (SPO) may run the DMQ node alongside their Cardano node, enabling the node to access KES (Key Evolving Signature) keys for message signing when acting as a message producer.

### Mini-Protocols

The DMQ implementation includes three complementary mini-protocols that handle different aspects of message diffusion:

- Message Submission Mini-Protocol (Node-to-Node): transfers messages between full nodes in the network using a pull-based strategy. It implements FIFO queue management and supports blocking/non-blocking request modes with resource protection.
- Local Message Submission Mini-Protocol (Node-to-Client): allows local clients to submit messages to their local network node via a request-response pattern.
- Local Message Notification Mini-Protocol (Node-to-Client): notifies local clients when new messages are received and supports blocking/non-blocking modes.

### Message Authentication

Messages are authenticated using cryptographic signatures. Each message includes a KES signature, an operational certificate, and a cold verification key for pool identification. Verification includes checking the operational certificate, verifying the KES signature, computing the pool ID and stake distribution membership, and preventing replay via message IDs.

## Building the Project

### Prerequisites

- GHC (Glasgow Haskell Compiler) compatible with this project
- Cabal (`cabal-install`) or Nix
- A running `cardano-node` for integration scenarios

### Building with Cabal

From the repository root:

```bash
cabal build dmq-node:exe:dmq-node
```

### Building with Nix

For reproducible builds:

```bash
nix build .#dmq-node

# Static binary for deployment
nix build .#dmq-node-static
```

### Entering the development shell

```bash
nix develop
```

## Running dmq-node

### Basic usage

Using Cabal:

```bash
cabal run dmq-node
```

Using a Nix-built binary:

```bash
./result/bin/dmq-node
```

### Command-line options

Run `--help` for the full list. Common options:

- `--version`
- `--config FILE` — specify configuration file
- `--socket-path PATH` — Cardano node Unix socket
- `--network-magic NUMBER` — network magic

### Network configuration

Common network magic values:

| Network | Network Magic |
|---------|---------------|
| Mainnet | 2912307721    |
| Preprod | 2147483649    |
| Preview | 2147483650    |

For Mithril-specific networks, consult Mithril docs.

### Connecting to Cardano node

Ensure a running `cardano-node` with an accessible socket and correct configuration so the DMQ node can validate message authenticity and participate in peer discovery.

## Configuration

The node typically uses a YAML configuration file. Important sections:

- Network configuration (socket path, network magic)
- Peer configuration (static peers, local roots)
- Logging configuration (levels, outputs)
- Resource limits (message size, queue depth, connection limits)

## Message expiration

Messages contain a Unix timestamp expiration. The node processes messages only if the local clock is before expiration; TTL is a per-topic protocol parameter.

## Security considerations

- Sybil attacks: mitigated via authenticated messages and operational certificates.
- Equivocation: receivers must detect conflicting messages; the network diffuses messages but does not validate content.
- Denial of Service: resource limits and peer throttling protect the node.

## Testing

### Running tests

```bash
cabal test all
```

### CDDL verification

```bash
cabal run dmq-node:dmq-cddl
```

## Troubleshooting

- Socket connection failed: ensure `cardano-node` is running and socket path is correct with proper permissions.
- Peer connection issues: configure static peers/local roots if automatic discovery is insufficient.
- Message verification failures: ensure KES keys and operational certificates are accessible and stake distribution is synced.

## Getting help

See CIP-0137, Ouroboros Network docs, and Mithril docs for protocol and integration details.

## Contributing

Contributions are welcome. Follow repository `CONTRIBUTING.md` and ensure CDDL conformance checks pass.

## License

Apache License 2.0 — see the repository `LICENSE` file.

---

If you'd like additional examples, sample configs, or deployment notes added here, let me know what to include.

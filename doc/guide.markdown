KES Agent User Guide
====================

Introduction
------------

On the Cardano blockchain, we use KES (Key Evolving Signature) to sign and
verify blocks, such that signing keys can be cycled regularly, without having
to publish new verification keys every time. KES achieves this by allowing us
to "evolve" sign keys, such that the evolved key is different from the original
one, and the original key cannot be inferred from the evolved key anymore, but
the original verification key remains valid for the evolved key.

The ultimate purpose of this setup is "forward security": once a key has been
evolved, the old key can be deleted, avoiding the possibility of a compromise
(a key that no longer exist cannot be leaked anymore) - but because the same
verification key is still valid, we can just use the evolved key, and it will
still validate against the verification key we've published.

However, actual forward security can only be achieved if the deletion of old
keys is reliable, that is, when we delete it, we must make sure that all traces
of it are wiped off the face of the Earth. Unfortunately, this is not something
modern mass storage media can achieve, unless we physically destroy the storage
device itself; and this means that we cannot store KES sign keys on disk, ever,
if we want meaningful forward security.

But this poses a new problem: `cardano-node` may have to restart itself, and
when it does, it loses any keys it may have stored in memory.

This is where the KES Agent system comes in: it stores KES keys in memory only,
using a persistent background process to keep the key available across Node
restarts.

Design Overview
---------------

At the core of the KES Agent system is the KES Agent itself, a long-running
background process that keeps a KES sign key in memory.

A Cardano Node can connect to the Agent to fetch the current key, and to
receive new keys as they are created. We call `cardano-node` the "Service
Client" in this context (because the KES Agent sends out KES sign keys as a
"service")

A CLI tool, `kes-agent-control`, can be used to interact with a running
`kes-agent` process. The control CLI is called "Control Client" (because it
connects to the KES Agent in order to "control" it). Through the Control
Client, we can command the KES Agent to generate new sign keys, export the
corresponding verification key, and we can upload an OpCert, which triggers the
KES Agent to start using it.

And finally, we need a CLI tool for signing KES keys and metadata, producing
*Operational Certificates* ("OpCerts"). `cardano-cli` already has functionality
for this built into it, however, because creating OpCerts requires access to a
"cold key", which must never ever be leaked, this needs to be done on a
separate air-gapped machine.

Summarizing, the following data is handled by the system; items that are
sensitive to leaks are in **boldface**.

- **KES Sign Key**: the "hot" sign key that is used in `cardano-node` to sign
  blocks. This key must never be stored on disk.
- KES Verification Key: the verification key matching the KES sign key.
- **Cold Sign Key**: a DSIGN key used to sign KES keys and metadata. This key
  needs to be stored on a disk, but that disk must be airgapped from the
  internet at all times, because an attacker in possession of the cold sign key
  can fabricate KES sign keys at will.
- Cold Verification Key: the verification key matching the cold sign key. This
  key is used to verify OpCerts.
- OpCert (Operational Certificate) - consists of:
    - KES Verification Key
    - Serial Number (ascending, and unique across all OpCerts signed with the
      same cold key; this is used for various consistency checks, and to avoid
      overwriting newer keys with older ones)
    - KES Period (the KES period, `(current_timestamp - genesis) /
      slots_per_kes_period`, from which the KES key's 0th evolution is valid)
    - Sigma (a DSIGN signature of the above 3 items)

Data flow works as follows:

1. Run `kes-agent-control` on a regular (but trusted) computer (the "control
   host"), using the `gen-staged-key` command. This will make the KES Agent generate
   a new KES Sign Key, keep it in its staging area, and write the corresponding
   KES Verification Key to a local file.
2. Copy the KES Verification Key to an air-gapped key signing host that
   holds the Cold Verification Key (using a secure removable storage medium -
   we cannot use a network connection, as that would defeat the air-gapping).
3. On the air-gapped machine, generate an OpCert.
4. Copy the OpCert back to the control host.
5. Run `kes-agent-control` to load the OpCert and push it to the KES Agent.
6. KES Agent verifies the OpCert against the staged sign key, moves them to the
   "active" slot, and pushes them to any connected Nodes.  KES Agent will also
   independently evolve the KES Key as time passes; this is important, because
   we won't get full forward security if we keep old evolutions of the key
   around anywhere, including the Agent.

This way, sensitive items are handled correctly:

- The **Cold Sign Key** never leaves the air-gapped signing host; it can be
  securely erased by physically destroying the signing host (or its mass
  storage devices)
- The **KES Sign Key** is only kept in secure memory, and transmitted over
  network connections without using intermediate data structures that could
  cause the key data to be written to swap space. Memory used for storing keys
  and seeds uses `mlock`, a kernel feature that prevents it from being swapped
  out, and employs a few other techniques to harden it against various attacks.

The following figure illustrates the data flow with two agents, one node, and a
cold server for generating OpCerts.

![Data Flow Overview](diagrams/overview.png)

Installation
------------

### Building From Source

#### Build Prerequisites

- The `git` version control system
- The `gcc` compiler, including C++ support
- Developer libraries for `libsodium`
- GHC, the Haskell compiler
- The Haskell build tool, Cabal

#### Building

1. Check out source code from github:
    git clone https://github.com/input-output-hk/kes-agent ./kes-agent
2. Build and install with cabal:
    cd kes-agent
    cabal install exe:kes-agent exe:kes-agent-control

### Installing KES Agent

KES Agent can run in two modes:

- "Service Mode", in which it acts as a systemd service. The process can be
  managed using the `start`, `stop`, `restart` and `status` subcommands; when
  started, it will double-fork, drop privileges, and send all log output to
  syslog.
  This is the recommended mode for a production setup.
- "Normal Mode", in which it runs as a regular process; the process starts
  immediately, does not fork or drop privileges, and writes log output to
  stdout. This mode is mainly useful for debugging and development purposes; it
  is not recommended for production use.

To run KES Agent as a daemon, the following steps are necessary:

1. Install the `kes-agent` binary into a suitable location, e.g.
   `/usr/local/bin`.
2. Create a kes-agent system user and group; the default name for both is
   `kes-agent`, and we suggest to keep this.
3. Add the control user (the interactive user who will manage KES keys for this
   Agent) to the kes-agent group.
4. Write systemd service to drive the `kes-agent` binary.
   (TODO: we should provide a service template)
5. Edit configuration
   (TODO)
6. Start the kes-agent service

### Installing KES Agent Control

Simply put the `kes-agent-control` binary somewhere on your `$PATH`.

(TODO: configuration)

### Setting Up An Air-gapped Signing Host

You need an "expendable" computer for this, such as a cheap single-board, that
never connects to the internet once set up. The only things that need to be
installed on this machine are:

- `cardano-cli`, for generating cold keys and OpCerts
- whatever software is needed to copy keys and OpCerts to and from a secure
  removable storage device

Now:

1. Generate a fresh Cold Key pair and a counter:
   ```sh
   cardano-cli node key-gen \
       --cold-verification-key-file cold.vkey \
       --cold-signing-key-file cold.skey \
       --operational-certificate-issue-counter-file opcert.counter
   ```
2. Copy the cold *verification* key to removable storage, and move it to
   the control host:
   ```sh
   mount /path/to/secure/device /mnt/secure-device
   cp cold.vkey /mnt/secure-device/
   umount /mnt/secure-device
   ```
   **DO NOT COPY THE SIGN KEY** - the sign key must never leave the signing host.

### Configuring `cardano-node` To Use The KES Agent

TODO

### Running `cardano-node`, `kes-agent-control` And/Or `kes-agent` On Separate Hosts

By default, `kes-agent` uses Unix domain sockets for communication with both
the control CLI and the Node. This has several advantages:

- By controlling the permissions of the files that represent the sockets, we
  get OS-level access control "for free".
- Unix domain sockets use kernel buffers, which will not be leaked to swap
  space or otherwise end up on persistent mass storage.
- Since the connection remains on the local host, it does not need to be
  encrypted.

To run these 3 processes on different machines, one may use OpenSSH domain
socket forwarding; this will use the existing authentication and encryption
mechanisms built into OpenSSH, and the processes on either side will be
blissfully unaware of the fact that there is a network connection between them.

(TODO: explicit instructions)

Usage
-----

To generate and push a new KES sign key:

### On The Control Host

- Find the number of slots per KES period; we can get this from the genesis
  file, e.g.:
  ```sh
  cat mainnet-shelley-genesis.json | grep KESPeriod
  > "slotsPerKESPeriod": 3600,
  ```
- Find the current tip of the blockchain:
  ```sh
  cardano-cli query tip --mainnet
  {
      "epoch": 259,
      "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
      "slot": 26633911,
      "block": 5580350
  }
  ```
  We want the "slot" value.
- Calculate the current KES period from this:
  ```sh
  expr 26633911 / 3600
  > 7398
  ```
- Generate a new key by using the kes-agent-control's `gen-staged-key` command:
  ```sh
  kes-agent-control gen-staged-key \
    --kes-verification-key-file kes.vkey
  ```
- Copy `kes.vkey` to your secure removable storage device

### On The Signing Host

- Copy `kes.vkey` from the secure removable storage device
- Generate an OpCert:
  ```sh
  cardano-cli node issue-op-cert \
    --kes-verification-key-file kes.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter opcert.counter \
    --kes-period 7398 \
    --out-file opcert.cert
  ```
- Copy `ocert.cert` to the secure removable storage device

### On The Control Host

- Copy `ocert.cert` from the secure removable storage device
- Use the kes-agent-control's `install-key` command to upload the opcert and
  activate the staged key:
  ```sh
  kes-agent-control install-key --opcert-file opcert.cert
  ```
- The control CLI will now upload the key.
- You can use the `info` command to verify that the key has been uploaded
  correctly:
  ```sh
  kes-agent-control info
  ```

Command Reference
-----------------

The full list of command line options for `kes-agent` and `kes-agent-control`
can be printed using the `--help` option.

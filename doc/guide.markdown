KES Agent User Guide
====================

Introduction
------------

Key Evolving Signature (KES) cryptography is a cryptographic signing scheme
where one verification key (VerKey) covers a series of signing keys (SignKey),
such that:

- Any signature created with any of the SignKeys can be verified with the same
  VerKey.
- Future SignKeys can be derived ("evolved") from past ones, but not the other
  way around, up to a maximum number of evolutions.

We use this in Cardano in order to achieve a degree of *forward security*:

- The original SignKey of each series of evolutions is verified with an OpCert,
  and installed into a Node.
- Every 36 hours (one "KES Period"), the Node evolves the SignKey, and deletes
  the old evolution.
- Once we reach the end of a key's series of evolutions, a new key and OpCert
  must be generated and installed.

Once a key is deleted, it can no longer be leaked, and an attacker cannot infer
the old keys from newer evolutions, which means that any signature made with a
key gains forward security within no more than 36 hours of signing, at which
point the sign key will be deleted. However, because the same verification key
covers all evolutions of the key, there is no need to distribute a new
verification key for each evolution, which makes the 36-hour periods viable in
practice.

There is one caveat though: in order for all this to work, we need to actually
delete those keys, and because reliably erasing data from modern mass storage
devices (such as harddisks or SSD's) is effectively not a thing, we need to
handle the keys such that they are never stored on disk. However, we also need
the Node process to be able to restart, for various reasons, and without
external storage, this means the key would be lost after a restart.

This is where the KES Agent comes in, an external process that retains a KES
key in memory, and exchanges it with a locally connected Node. Great care is
taken to make sure that keys are never stored on disk, and that the RAM they
are stored in is protected against swapping out to disk ("mlocked"), and when
sending keys over a network socket, we do it such that the keys are moved
directly between mlocked memory and the socket file descriptors, without using
any intermediate data structures for serialization/deserialization.

Design Overview
---------------

The KES Agent system consist of 3 components:

- The *Agent* itself, a standalone process that is responsible for:
    - Generating KES keys
    - Serving KES sign keys to a block-forging Cardano Node, and, optionally,
      other KES agent instances
    - Outputting KES verification keys, for the purpose of generating OpCerts.
  The KES agent accepts connections on two sockets:
    - A "service" socket, on which keys are sent out. Nodes and other KES
      agents will connect to this socket.
    - A "control" socket, on which commands are received. The Control Client
      will connect to this socket.
  The Agent will also automonously evolve keys, such that any keys it sends out
  on demand will match the current KES period on the ledger. Further, while the
  KES Agent can generate new KES keys, they have to be signed externally. Any
  newly generated keys will be held in a *staging area* inside the KES agent
  process until a matching OpCert is added, at which point the key is activated
  and sent out to any connected clients.
- The *Node* (cardano-node), which connects to an Agent's "service" socket, and
  receives keys upon first connecting and when a fresh key is pushed to the
  Agent. Once the Node has received a valid key, it will evolve it
  autonomously, so there is no need for the Agent to send out subsequent
  evolutions of the same key, unless the Node reconnects. The latter normally
  only happens when a Node process restarts.
- The *Control Client* (to be implemented), a utility that can be used to
  generate keys and OpCerts, and push them to an Agent via the "control"
  socket. This utility should run on a separate host, and needs access to the
  "cold" key in order to sign the "hot" keys managed by the Agent.

This package contains:

- The `kes-agent` binary (the Agent).
- The `kes-agent-control` binary (the Control Client, used to interact with
  running `kes-agent` processes).
- The `kes-agent` library that contains shared code for all of the above as well
  as client code to be used in `cardano-node`.
- A test suite.
- The `kes-service-client-demo` binary, which implements the service client
  protocol and can be used as a mock `cardano-node` for testing and
  demonstration purposes.
- The `kes-agent-protodocs` binary, a tool that will output documentation of
  the KES Agent protocols as used in the current versions of the library.
  **This part is currently disabled due to dependency issues.**

We also need a CLI tool for signing KES keys and metadata, producing
*Operational Certificates* ("OpCerts"). `cardano-cli` already has functionality
for this built into it, so we are not providing one from the `kes-agent`
package.

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

### Using a tarball

1. Download a suitable installer tarball for your OS and architecture.
2. Unpack
3. Run the included `./install.sh` script as root

This will install `kes-agent` as a systemd service. To actually use it, the
following steps are required:

1. Copy your cold key file (`cold.vkey`) into `/etc/kes-agent/cold.vkey`
2. Edit `/etc/kes-agent/agent.toml` as desired
3. Reload the `kes-agent` configuration (`systemctl reload kes-agent`).

### Docker

A Docker image is available (TODO: document image name). The image includes a
default configuration, but, for obvious reasons, no cold verification key. To
provide one, mount it into the container, like so:

    docker run --mount type=bind,source=/path/to/your/cold.vkey,target=/etc/kes-agent/cold.vkey cardano/kes-agent:0.1

### Building From Source

#### Build Prerequisites

- The `git` version control system
- The `gcc` compiler, including C++ support
- Developer libraries for `libsodium`
- Developer libraries for `secp256k1` (https://github.com/bitcoin-core/secp256k1)
- Developer libraries for `libblst` (https://github.com/supranational/blst).
  Note that installation requires some undocumented manual steps:
  - Copying the headers into an appropriate system-wide location
    (`/usr/local/include/`).
  - Copying the library `libblst.a` into an appropriate system-wide location
    (`/usr/local/lib/`).
  - Creating a `pkgconf` entry so that the build tooling can find the library.
  - Running `ldconf` to register the library with the system-wide linker.
- GHC, the Haskell compiler
- The Haskell build tool, Cabal

#### Building KES Agent

1. Check out source code from github:
    ```sh
    git clone https://github.com/input-output-hk/kes-agent ./kes-agent
    ```
2. Build and install with cabal:
    ```sh
    cd kes-agent
    cabal update
    cabal install exe:kes-agent exe:kes-agent-control
    ```

#### Installing KES Agent

KES Agent can run in two modes:

- "Service Mode", in which it acts as a Unix service ("daemon"). The process
  can be managed using the `start`, `stop`, `restart` and `status` subcommands;
  when started, it will double-fork and drop privileges, and it will send all
  log output to syslog.
  This is the recommended mode for a production setup.
- "Normal Mode", in which it runs as a regular process; the process starts
  immediately, does not fork or drop privileges, and writes log output to
  stdout. This mode is mainly useful for debugging and development purposes; it
  is not recommended for production use.

To run KES Agent as a daemon using systemd, the following steps are necessary:

1. Install the `kes-agent` binary into a suitable location, e.g.
   `/usr/local/bin`.
2. Create a kes-agent system user and group; the default name for both is
   `kes-agent`, and we suggest to keep this.
3. Add all users that need to connect to
   the KES Agent (whether as a Node or via the `kes-agent-control` tool) to the
   `kes-agent` group.
4. Obtain a verification key for your Cold Key and copy it into a suitable
   location (e.g. `/etc/kes-agent/cold.vkey`). KES Agent needs this file in
   order to verify OpCerts.
5. Make a systemd service to drive the `kes-agent` binary. An example service
   file can be found in `./systemd/etc/systemd/system/kes-agent.service`.
6. Create an environment file for the systemd service to configure it. An
   example file can be found in `./systemd/etc/kes-agent/kes-agent.env`.
7. Make systemd reload the service configuration (`systemctl daemon-reload`).
8. Start the kes-agent service (`systemctl start kes-agent`).

An example installation script that performs the above steps is provided in
`etc/systemd/install.sh`.

#### Installing KES Agent Control

Simply put the `kes-agent-control` binary somewhere on your `$PATH`.

To tell `kes-agent-control` where to find the KES agent, you can use the
following methods:

1. Set up the kes-agent to listen for control connections on the default path,
   `/tmp/kes-agent-control.socket`.
2. Pass the control socket path as a command-line argument to
   `kes-agent-control`, using the `--control-address` or `-c` option.
3. Pass the control socket path through the environment, by setting the
   `KES_AGENT_CONTROL_PATH` environment variable to the desired path.

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

A simple command for setting up a tunnel from a host running `cardano-node` to
a host running `kes-agent` might look like this:

```sh
ssh kes-agent-host -L /tmp/kes-agent-service-remote.socket:/tmp/kes-agent-service.socket
```

However, in practice, you would want to add a few options to background the
`ssh` process, skip opening a terminal, and automatically delete the local
socket file if it already exists, like so:

```sh
ssh kes-agent-host \
    -L /tmp/kes-agent-service-remote.socket:/tmp/kes-agent-service.socket \
    -nNT \
    -o StreamLocalBindUnlink=yes \
    & disown
```

Further, if you want the ssh connection to be automatically resumed whenever it
gets disconnected, try `autossh`:

```sh
autossh -M0 \
    kes-agent-host \
    -L /tmp/kes-agent-service-remote.socket:/tmp/kes-agent-service.socket \
    -nNT \
    -o StreamLocalBindUnlink=yes \
    & disown
```

The options explained in detail:

- `-L /tmp/kes-agent-service-remote.socket:/tmp/kes-agent-service.socket`: this
  sets up the domain socket forwarding.
- `-n`: prevent reading from `stdin`.
- `-N`: do not execute a remote command.
- `-T`: disable pseudo-terminal allocation
- `-o StreamLocalBindUnlink=yes`: automatically delete (stale) local sockets if
  they clash with the newly created local sockets. Forwarded local socket files
  will remain on the local filesystem after the forwarding ssh process
  terminates, and subsequent attempts at binding the same address will fail;
  this option chances that, so that the existing file will instead be deleted.

You may also want to wrap this up in a systemd service; note however that you
must set up authentication in such a way that it never requires any user
interaction (e.g., asking for a password or passphrase).

Similar forwardings will be needed for "bootstrapping" connections and control
client connections.

You can also set up the tunnel from the other side, using the `-R` option;
note, however, that there is no equivalent option to `StreamLocalBindUnlink`
that can clean up stale sockets on the remote, so you will have to take care of
that separately.

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

Recommended Setups
------------------

**Legend:**

- `---S-->`: service protocol connection (one-way)
- `<==S==>`: service protocol connection (two-way; this actually uses two
  separate network tunnels)
- `---C-->`: control protocol connection

In all setups, an air-gapped "cold key host" is assumed, which is used for
signing operational certificates based on verification keys extracted from a
running KES agent using the `kes-agent-control` CLI tool. This cold key host
should be located somewhere within convenient reach of the machine running the
`kes-agent-control` tool, but it should remain physically disconnected from any
networks at all times in order to protect the cold key.

Further, it is assumed that the SPO has some sort of local machine available
from where they control their cardano node; this machine is labelled the
"control host" in these example setups.

## Single-Agent

This is a simple setup that requires no extra hardware, since the only KES
Agent process runs on the same machine as the block-forging node.

    ------------ node host ------------
     +---------+      +--------------+
     | Agent A |<--S--| cardano-node |
     +---------+      +--------------+
          ^
          |
    ------|--- control host -----------
          C
          |
     +-------------------+
     | kes-agent-control |
     | (CLI)             |
     +-------------------+

### Properties:

- Key persists through network outages: YES
- Key persists through restart of cardano-node process: YES
- Key persists through restart of KES agent process: NO
- Key persists through reboots/hibernation of the node host: NO
- Recovery from cardano-node restart: automatic
- Recovery from agent process restart: manual via cold key host
- Recovery from node host reboot/hibernation: manual via cold key host
- Key updates possible during network outages: NO

### Recommended hardening:

- Set up SSH tunneling from the control host to the node host for the control
  socket only.
- Run the KES Agent under an locked-down user account that cannot log into a
  shell and can only access the files it needs (configuration files and local
  domain sockets for control and service connections).
- Keep the control host behind a firewall.

## Backup Agent On Control Host

This setup is suitable for SPOs who use a control host that runs most of the
time; it does not require any additional hardware beyond the node host and the
control host, and offers a basic degree of redundancy and self-healing. Even if
the control host does not offer high availability, this setup supports
scheduled restarts of the node or the host it runs on, as long as the KES
agent on the control server remains active during the restart.

        ------------ node-host ------------
         +---------+      +--------------+
     +==>| Agent A |<--S--| cardano-node |
     |   +---------+      +--------------+
     |        
     S  --------- control host -----------------
     |   +---------+      +-------------------+
     +==>| Agent B |<--C--| kes-agent-control |
         +---------+      | (CLI)             |
                          +-------------------+

### Properties:

- Key persists through network outages: YES
- Key persists through restart of cardano-node process: YES
- Key persists through restart of KES agent process A: YES (as long as KES
  agent B remains available)
- Key persists through restart of KES agent process B: YES (as long as KES
  agent A stays up)
- Key persists through reboots/hibernation of the node host: YES (as long as
  KES agent B stays up)
- Key persists through reboots/hibernation of the control host: YES (as long as
  KES agent A stays up)
- Recovery from cardano-node restart: automatic
- Recovery from agent process restart: automatic
- Recovery from node host reboot/hibernation: automatic
- Recovery from control host reboot/hibernation: automatic
- Recovery from simultaneous restart of both agent processes: manual via cold
  key host
- Recovery from simultaneous reboot of both hosts: manual via cold
  key host
- Key updates possible during network outages: YES (updates will propagate once
  network connectivity is restored)

### Recommended hardening:

- Set up SSH tunneling from the control host to the node host for the service
  sockets only (one in each direction).
- Disable the control socket on Agent A.
- Run the KES agent A under an locked-down user account that cannot log into a
  shell and can only access the files it needs (configuration files and local
  domain sockets for control and service connections).
- Keep the control host behind a firewall.

## Basic 3-Agent Setup

This setup is suitable for SPOs who desire more redundancy for more reliable
persistence / automatic recovery, and are willing to provision an extra host
for that.

           ------------- node host -------------
             +---------+      +--------------+
      +=====>| Agent A |<--S--| cardano-node |
      |  +==>|         |      +--------------+
      |  |   +---------+
      |  |
      |  S ------------- agent host -------------
      |  |   +---------+
      S* +==>| Agent B |
      |  +==>|         |
      |  |   +---------+
      |  |        
      |  S ---------- control host -----------------
      |  |   +---------+      +-------------------+
      |  +==>| Agent C |<--C--| kes-agent-control |
      +=====>|         |      | (CLI)             |
             +---------+      +-------------------+

It is advisable to provision the agent host separately from the node host to
minimize the risk of agents A and B becoming unavailable simultaneously.

The connection between agents A and C, marked with an asterisk, is optional; it
allows agents A and C to stay in sync during times when agent B is down.

### Properties:

- Key persists through network outages: YES
- Key persists through restart of cardano-node process: YES
- Key persists through restart of KES agent process: YES (as long as at least
  one other KES agent process stays up)
- Key persists through reboots/hibernation of any host: YES (as long as at
  least one other KES agent process stays up)
- Recovery from cardano-node restart: automatic
- Recovery from agent process restart: automatic
- Recovery from node host reboot/hibernation: automatic
- Recovery from control host reboot/hibernation: automatic
- Recovery from simultaneous restart of up to 2 agent processes: automatic
- Recovery from simultaneous reboot of up to 2 hosts: automatic
- Recovery from simultaneous restart of all KES agents: manual via cold key
  host
- Recovery from simultaneous reboot/hibernation of all hosts: manual via cold
  key host
- Key updates possible during network outages: YES (updates will propagate once
  network connectivity is restored)


### Recommended hardening:

- Set up SSH tunneling from the control host to the agent and node hosts for
  the service sockets only (one in each direction), and from the agent host to
  the node host. Alternatively, if you want to avoid SSH connections into the
  node host, do not make the optional connection between agents A and C, and
  set up SSH tunnels for the connection between agents A and B from the node
  host. This way, only outgoing SSH connections need to be allowed from the
  node host (however, the agent host must accept incoming SSH connections).
- Disable the control socket on Agents A and B.
- Run KES agent A and B under locked-down user accounts that cannot log into a
  shell and can only access the files they need (configuration files and local
  domain sockets for control and service connections).
- Keep the control host behind a firewall.

More Elaborate Setups
---------------------

The Basic 3-Agent Setup can be extended to arbitrarily many KES Agent hosts to
achieve the desired degree of redundancy and availability. Many topologies are
possible; the below diagrams show some basic options (only KES agents are
shown for clarity).

### Bilateral Linear

    +---+     +---+     +---+     +---+     +---+
    | A |<===>| B |<===>| C |<===>| D |<===>| E |
    +---+     +---+     +---+     +---+     +---+

Every agent connects to one or two peers.

Propagation will be disrupted whenever one agent goes down, and will be
restored once that agent comes back up.

### Ring

    +---+     +---+     +---+
    | A |<===>| B |<===>| C |
    +---+     +---+     +---+
      ^                   ^
      |                   |
      v                   v
    +---+               +---+
    | H |               | D |
    +---+               +---+
      ^                   ^
      |                   |
      v                   v
    +---+     +---+     +---+
    | G |<===>| F |<===>| E |
    +---+     +---+     +---+

Every agent connects to two peers.

Propagation will be disrupted whenever two agents go down, and will be restored
once one of them comes back up.

### Web

    +---+     +---+
    | A |<===>| B |
    +---+<   >+---+
      ^   \ /   ^
      |    X    |
      v   / \   v
    +---+<   >+---+
    | C |<===>| D |
    +---+     +---+

Every agent connects to every other agent.

Propagation is never disrupted between any two active agents.

Using As A Library
------------------

The `kes-agent` package also contains the `kes-agent` library, which provides
functionality for the agent itself, the control client, and any service
clients.

To implement KES agent connectivity in your own software, look at the modules
in kes-agent/src/Cardano/KESAgent/Processes/:

- kes-agent/src/Cardano/KESAgent/Processes/Agent.hs provides agent
  functionality. You will not need this unless you want to make your own KES
  client.
- kes-agent/src/Cardano/KESAgent/Processes/ControlClient.hs provides control
  client functionality (query the state of an agent process, requesting a new
  KES key to be generated, outputting the staged KES verification key,
  uploading OpCerts to activate the staged key, dropping the current key). You
  will need this if you want to make a custom frontend for controlling KES
  agents.
- kes-agent/src/Cardano/KESAgent/Processes/Agent.hs provides service client
  functionality (receiving KES sign keys). You will need this if you want your
  application to connect to a KES agent.

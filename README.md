# KES Agent

## Introduction

Key Evolving Signature (KES) cryptography is a cryptographic signing scheme
designed for forward security. In Cardano, we use this to sign blocks,
"evolving" keys every 36 hours.

This forward security however requires that after a key has been evolved, all
copies of the old key must be securely erased. This implies that KES keys must
never be stored on disk. The KES Agent, then, exists so that keys aren't lost
in the event of a restart.

For further information, see [the Guide](docs/guide.markdown).

## OS Compatibility

KES Agent does not currently work on Windows.

It will compile and run, but the test suite has been disabled because it fails
/ deadlocks, and it will almost certainly not work correctly.

## Building & Installing

Quick guide:

Clone git repository:

```sh
git clone https://github.com/input-output-hk/kes-agent/ ./kes-agent
cd kes-agent
```

Build & install:

```sh
cabal install exe:kes-agent exe:kes-agent-control
```

Running tests:

```sh
cabal test all
```

Running KES agent as a regular process:

```sh
kes-agent run \
    -s /path/to/service.socket -c /path/to/control.socket \
    --cold-verification-key /path/to/cold.vkey \
    --genesis-file /path/to/genesis.json
```

Querying the KES agent to verify that it works:

```sh
KES_AGENT_CONTROL_PATH=/path/to/control.socket kes-agent-control info
```

For further information, see [the Guide](docs/guide.markdown).

## License & Copyright

Copyright INTERSECT 2024-2025.

Licensed under the Apache License, Version 2.0 (the "License"); see the
enclosed NOTICE and LICENSE files.

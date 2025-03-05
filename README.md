# KES Agent

## Introduction

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

## Design & Architecture

The KES Agent system consist of 3 components:

- The *Agent* itself, a standalone process that accepts connections on two
  sockets: a "service" socket, on which keys are sent out, and a "control"
  socket, on which commands are received. The Agent will also automonously
  evolve keys, such that any keys it sends out on demand will match the current
  KES period on the ledger. Further, the KES Agent can generate new KES keys;
  they have to be signed externally, and until they are, the KES Agent will
  store them in a staging area.
- The *Node* (cardano-node), which connects to an Agent's "service" socket, and
  receives keys upon first connecting and when a fresh key is pushed to the
  Agent. Once the Node has received a valid key, it will evolve it
  autonomously, so there is no need for the Agent to send out subsequent
  evolutions of the same key, unless the Node reconnects (which may mean that
  it has just restarted).
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

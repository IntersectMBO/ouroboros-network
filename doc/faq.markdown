# KES Agent FAQ

## KES Fundamentals

### Why do we need KES?

In Cardano, we need a digital signature algorithm that provided forward
security, that is, it must be impossible to "rewrite history" by tampering with
past chain data, *even when a signing key is leaked.*

A simple solution to this is to regularly create new signing keys, and delete
old ones as soon as a new one becomes valid. This way, only the currently valid
key can ever be leaked, but because it isn't valid for signing blocks in the
past, the forward security property we are after is assured - at least for any
chain data before the validity period of the current key.

We can pick a duration for the validity that gives us the security properties
we want, but there is one downside: every time we generate a new key, we have
to distribute a matching certificate, otherwise other participants cannot
verify our signatures. This is inconvenient: if, as in Cardano, we want forward
security within no more than 2 days (thus guaranteeing that chain data older
than 2 days cannot be tampered with), then we need to generate new keys and
distribute new certificates at that rate. (The actual duration of such a
"KES period" in Cardano at the time of writing is 36 hours.)

This is where "Key Evolving Signatures" come in. KES is a clever algorithm that
allows us to generate an entire series of keys that has the following
properties:

1. From each key in the series, all subsequent keys can be derived ("evolved").
2. Preceding keys *cannot* be derived from subsequent keys (e.g., key #8 can be
   derived from key #7, but not the other way around).
3. A verification key derived from any of the keys can be used to verify
   signatures made with this key and all subsequent keys.
4. The sequence of evolutions is finite, but we can pick a size when generating
   the initial key.

So now instead of generating an entire new key every 36 hours, we generate an
initial key, derive a verification key from it, and then *evolve* the secret
key every 36 hours, deleting the previous evolution. Since keys can only be
evolved forward, this is effectively just as good as generating brand new keys,
but because the verification key works for all evolutions, we don't need to
create and distribute a new certificate.

### Why do we need a KES *Agent*?

In order for KES (or any other digital signature, for that matter) to provide
meaningful forward security, secret keys must be securely erased (i.e., deleted
in such a way that it is physically impossible to recover them) the moment they
become invalid.

Unfortunately, securely erasing data from modern mass storage devices generally
requires physically destroying the device - this was true with magnetic storage
devices such as harddisks, and it is even more true with SSDs. Hence, we want
to make it such that KES keys are never stored on disk, nor become subject to
being swapped out.

At the same time, we need keys to remain available when a block forging node
process restarts or reloads.

We solve that by providing a "KES Agent". This is a separate process that runs
outside of the main node process, and keeps a KES key in RAM, carefully
designed to never allow key data to be swapped out, and making it available to
the node through a Unix domain socket.

### Do I need a KES agent if I'm not running a block-forging node?

No.

KES is only used in block forging, so if you're not running a block-forging
node, you don't need to worry about KES or KES agents.

## Design Considerations

### What happens when a node process restarts?

When a node process configured to receive its KES keys from a KES agent
restarts, it will connect to the agent again after restarting, and the agent
will serve the current evolution of the KES key, after which the node can start
forging blocks.

### What happens when the entire server reboots?

In this case, a KES agent running on the same server will terminate, and the
key will be lost.

This can be solved in two ways:

- By manual intervention, creating a brand new key and installing it into the
  KES agent after rebooting.
- By running another KES agent on a separate machine, and setting up both KES
  agents to talk to each other over Unix domain sockets forwarded over SSH. In
  this scenario, whenever either KES agent reboots, it will drop its key, but
  after rebooting, it will connect to the other agent and receive a key from
  it. This can work in both directions, creating a self-healing system; it can
  also be extended to arbitrarily many agents to further increase availability.

### Why not just use encrypted storage?

Because that only shifts the problem around.

If you store a KES key on an encrypted volume, it still cannot be erased,
unless you erase the encryption key for the encrypted volume. And because
erasure must happen after every evolution, this means you would have to erase
and re-encrypt the entire encrypted volume every time the KES key evolves, and
you would have to find a way of securely erasing the encryption key for that
volume.

### What about Trusted Computing (TPM etc.)?

These devices are excellent for storing small long-lived secrets, but they have
two issues:

1. They are not reliably available on all the platforms we need to support.
2. KES keys are fairly large, and cannot typically be stored inside the TPM
   itself. Rather, the TPM just stores a set of "master" keys (the Endorsement
   Key, EK, and a Storage Root Key, SRK); the actual secrets to be protected
   are stored outside the TPM, and decrypted through the TPM on demand.

The second point means that we're left with the same problem again: the TPM
doesn't actually store the (encrypted) keys themselves, it just stores a
(long-lived) key that can be used to decrypt them, so in order to achieve
forward security, we would have to erase those master keys at the same rate as
we're evolving our KES keys. Again, we'd only be shifting the problem, and in
this case, we would add the complexity of resetting the TPM itself.

### Can we not use mass storage devices that support secure erasing?

Such devices exist, but according to our research, they fall into two
categories:

- Home-brew DIY solutions that require a soldering iron and a fair bit of
  hardware tinkering skills.
- "Enterprise grade" solutions that cannot be ordered off the shelf, but
  require contacting the vendor and negotiating a price on a per case basis.

Neither of these are realistically feasible for all SPOs. They may be feasible
for some, and in those cases, they could be an alternative to running a KES
agent, but for the majority of users, we need something easier (and cheaper).

### Why not do the signing on the KES agent? Wouldn't that be better, because it means the key never needs to leave the agent at all?

We have considered this design, but there are a few major downsides to it:

1. It would mean that a block forging node can only sign while it is actively
   connected to a KES agent. If, for any reason, the KES agent is temporarily
   unavailable, the node cannot forge any blocks. In the current design, the
   node holds the key itself, so it can keep forging blocks until it reaches
   the end of the series of evolutions of the current KES key; it only needs a
   working KES agent connections immediately after restarting, and to receive a
   completely new KES key when the user generates one.
2. The advantage of the KES key never leaving the agent only holds up as long
   as you run only one agent process - however, this provides insufficient
   availability, because there is no self-healing. The moment you add a second
   KES agent process to achieve that self-healing, keys will have to move
   around anyway, so we might as well keep a copy in the node.
3. It would slow down the block forging itself. Right now, block forging uses
   a key that is readily available to the node process, so the overhead of
   accessing it for signing is negligible. With a socket connection in between,
   potentially forwarded over a network connection, there would be significant
   overhead.

### What happens when a VPS/Cloud server that runs a KES agent gets moved?

This usually involves suspending the host to disk, copying the image to another
machine, and waking it up there.

Unfortunately, suspend-to-disk is the one scenario for which the mlocking
guarantees that we rely on to keep the keys away from mass storage devices
doesn't hold: suspend-to-disk means that all RAM, including mlocked RAM, is
written out to a swap file.

We are aware of this, and will address it in the future, but there are
workarounds that can already be applied now:

- **Disable suspend-to-disk (and ideally also swap).** This will also make it
  impossible to move the virtual machine to another physical server, but may be
  a viable way of preventing accidental suspend-to-disk on a server that runs
  "on metal" or simply isn't expected to be moved.
- **Configure the system to terminate the KES agent process when hibernating.**
  This should be fairly straightforward with most init systems, including
  systemd - just make sure the process is terminated *before* the hibernation
  starts.

Note that whatever solution you use, suspend-to-disk will lose the key stored
in the KES agent, so unless you are running your KES agent on a machine that
will never be suspended to disk, a self-healing setup is highly recommended.
You should also take precautions to avoid suspending/moving all KES agent
machines simultaneously.

A viable setup that combines both approaches could be something like this:

- A block forging node and one KES agent runs on one (virtual) machine,
  configured to terminate the KES agent before suspend-to-disk.
- Another KES agent runs on a separate (virtual) machine, also configured to
  terminate the KES agent before suspend-to-disk.
- A *third* KES agent runs on metal on a machine you control, like an
  inexpensive single-board server on your office or home network. This machine
  would be configured to not 
- All 3 KES agents are configured to connect to each other.

## Usage

### What is a "Cold Key host", and why do I need one?

In order to use a KES key, you need an "operational certificate" ("OpCert"),
which consists of:

- A verification key matching the KES key
- Metadata defining the times at which the key is valid
- A digital signature to prove that the key was generated by you

For that last bit, you need to produce a digital signature using a "Cold Key";
that cold key, unlike the KES key itself, cannot easily be replaced, so you
need to guard it as strongly as you possibly can.

However, unlike the KES key, it never needs to leave the computer on which you
create your OpCerts, and that computers does need to be connected to any kind
of network, as long as you have a way of moving verification keys onto it, and
OpCerts off of it.

The recommended setup is to keep your Cold Key on a dedicated computer (this
can be something small and cheap, like an old laptop, a single-board computer,
etc.), and keep that computer "air gapped" (i.e., physically disconnected from
any and all networks). That computer is your "Cold Key host", and it should be
kept in a location where it is maximally protected against compromises.

To generate a new KES key and making it usable, then, you would follow these
steps:

1. Command your KES agent to generate a new KES key and output the matching
   verification key.
2. Copy the verification key to your Cold Key host (e.g. using a trustworthy
   removable storage device).
3. On the Cold Key host, generate an OpCert for that verification key.
4. Copy the OpCert to the KES agent host (e.g. using the same trustworthy
   removable storage device)
5. Upload the OpCert to the KES agent; the KES agent will verify it, and if
   verification succeeds, activate it and send it out to any connected clients
   (nodes and other agents).

This process is described in more detail in the [Guide](./guide.markdown).

### How can I run multiple KES agents on different computers?

Set up SSH forwarding (detailed instructions tbd) to forward the "service"
sockets between machines, using OS permissions to control who can and cannot
connect to those sockets.

Then configure each KES agent to connect to all other KES agents as "peers" via
those forwarded sockets.

### Can I run KES agent on Windows?

Unfortunately, no.

While current Windows versions support Unix domain sockets, forwarding these
over SSH is problematic, so the only possible setup would be to run a block
forging node and a connected KES agent side-by-side on the same machine.

There are also some other issues with running the KES agent code on Windows,
which we haven't fully investigated yet.

As far as we know, none of the block forging nodes currently run on Windows, so
for the time being, we will not be supporting KES Agent on Windows.

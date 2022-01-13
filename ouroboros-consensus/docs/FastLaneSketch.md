# Overview

In Praos, the honest chain grows most efficiently when the winning block minted
during one active slot is able to diffuse to the elected leaders of the next
active slot early enough that they can mint an extension of it.

Without pipelining, blocks diffuse in rigid manner as follows.

  * A peer notifies that they have a new block by sending a header to our
    ChainSync client for that peer.

  * If the header is valid, the ChainSync client writes it to a shared variable.

  * Our global BlockFetch decision logic monitors such shared variables written
    to by all ChainSync clients and will instruct the BlockFetch client for a
    peer whose corresponding ChainSync client has received that header to
    download the block. (TODO Will it ever tell multiple BlockFetch clients to
    fetch the same block?)

  * That BlockFetch client will receive the block from the peer and add it to
    the ChainDB's queue of blocks-to-add.

  * A background thread of the ChainDB processes that queue; a typical new
    block will be written to the VolDB.

  * Whenever that background thread processes a block-to-add, it performs chain
    selection on the entirety of the VolDB (including "future blocks", but
    that's a story for another time).

  * If the block we just added is valid, the best we've ever seen etc, then the
    ChainDB will switch its selected chain to the chain that ends in that new
    block. If the block is invalid, we should disconnect from the peers who
    sent its header via ChainSync and any that do so later: ChainSync conveys
    the server's selection, and these peers have told us they selected an
    invalid block.

  * Our ChainSync servers will relay this switch of our selection to our peers,
    and then this set of steps repeats for them.

Thus, without pipelining, we must pay for (the relatively expensive) block
validation at each hop along the critical path between the current and next
elected leaders. BlockFetch only downloads blocks for headers that ChainSync
has already received, and ChainSync only sends headers from the ChainDB
selection, and the ChainDB only selects blocks that it successfully validated.

The goal of Block Diffusion Pipelining via Delayed Validation is to adjust the
above so that we can -- under carefully chosen narrow circumstances -- diffuse
blocks without validating them at each step. They'll only be validated once
along the critical path between consecutive slot leaders, at the end of it. The
narrow circumstances are essentially when we're forwarding the new best block
we've ever seen to each peer that could easily select it.

### ChainSync pointers

* messages https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/Protocol/ChainSync/Type.hs
* the client https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/MiniProtocol/ChainSync/Client.hs
* the server is primarily a thin wrapper around the ChainDB Follower abstraction
    * https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/MiniProtocol/ChainSync/Server.hs
    * https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/Storage/ChainDB/Impl/Follower.hs

### BlockFetch pointers

* messages https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/Protocol/BlockFetch/Type.hs
* the client https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/BlockFetch/Client.hs
* the server https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/MiniProtocol/BlockFetch/Server.hs
* the decision logic https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/BlockFetch.hs
    * the node's sole logic thread is forked here at startup https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/NodeKernel.hs#L158-L159
    * the decision logic is ultimately implemented across multiple modules, many of which contain elaborate comments; these are the main ones
        * https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/BlockFetch/Decision.hs
        * https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/BlockFetch/ClientState.hs
        * https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-network/src/Ouroboros/Network/BlockFetch/State.hs
    * I've mostly just left out the "fetch client registry", I think. I suggest understanding that part later.

### ChainDB pointers

* API https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/Storage/ChainDB/API.hs
* the blocks-to-add queue https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/Storage/ChainDB/Impl/Types.hs#L244
* how BlockFetch adds blocks to the queue https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/NodeKernel.hs#L333-L336
* relevant background thread https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/Storage/ChainDB/Impl/Background.hs#L523-L536
* chain selection logic https://github.com/input-output-hk/ouroboros-network/blob/b0d95f33314e0562b6eea55455f1847efb85f8a4/ouroboros-consensus/src/Ouroboros/Consensus/Storage/ChainDB/Impl/ChainSel.hs

# Initial implementation plan

I think I see a lightweight means of extending the above system to "rush"
through the latest block and to do so only when it would matter.

* Extend the ChainDB API with a part that notifies subscribed threads whenever
  a new most-desirable-block-ever-seen is added.
* Define a new mini protocol named FastLane as a pared down ChainSync.
    * It has two messages: `MsgRequestNextFast`, `MsgFast`
    * The intended semantics is that the server sends `MsgFast hdr` to the
      client as soon as the server downloads a new most-desirable block.
    * Notably, the server sends `MsgFast` _before_ attempting to validate the
      new block.
* Expand the input to the block fetch decision logic so that it includes the
  latest header received via FastLane.
* Crucially, peers are to be forgiven for sending us an invalid block this way.

That's the big picture. Some more details follow.

* Unlike ChainSync, The FastLane client ignores the header if it doesn't
  directly extend the node's selected chain.
    * I'm tempted to make it more liberal, but that would involve extra
      complexity, along the line of checking whether all the blocks between the
      new header and its intersection with our selection are already in the
      VolDB.
    * Notably, if pipelining has been working, then the simpler check should
      suffice.
* Like ChainSync, if the header is valid, then the FastLane client makes its
  available to the BlockFetch decision logic.
    * The FastLane client disconnects from the peer if the header extends the
      node's selection chain but is invalid.
* The FastLane server does not reply to `MsgRequestNextFast` until it has a
  _new_ most-desirable-block-ever-seen.
    * The `MsgFast` timeout is comparable to the ChainSync timeout after
      receiving `MsgAwaitReply`.
    * Every `MsgFast` should be strictly preferable to the previous `MsgFast`
      that peer sent (and from that peer's latest ChainSync, but I haven't
      convinced myself that's worth checking).
* Like ChainSync, FastLane is pull-based, so we can't get overwhelmed.
* Like ChainSync, FastLane is also pipelined, so that we can hide some latency.
* If a peer offered a (valid) header for an invalid block via ChainSync we
  disconnect. If the peer only offered instead that header via FastLane, then
  we don't disconnect.

# Doubts

* Should FastLane just send the block? Is the extra RTT worthwhile? If it just
  sends the block, then I worry it's a DoS vector for them to consume our
  bandwidth.

* Am I overlooking some fundamental difference when I consider FastLane as just
  a variant of ChainSync? IE is it more than just something that makes headers
  available to the BlockFetch decision logic?

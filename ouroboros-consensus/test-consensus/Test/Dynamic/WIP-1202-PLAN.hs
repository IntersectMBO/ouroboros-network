{- |

Assumed by the test infrastructure:

  * A delegation map is an injective function from genesis ("cold") keys to
    operational ("hot") keys, and no operational key is a genesis key.

  * The chain's ledger includes a delegation map.

  * The genesis chain's ledger delegation map is total, mapping every genesis
    key present in that genesis configuration.

  * A node's static configuration determines its operational key. A node
    operator can only "change" a node's operational key by restarting or
    replacing that node.

  * An honest node cannot forge a block if its operational key is not in the
    range of its current ledger's delegation map.

Guaranteed by the test infrastructure:

  * The test infrastructure maintains the *actual* delegation map.
    
    EdV: This is the only part of this plan that I feel needs some clarification. 
    What does the "actual" delegation map mean, in the context of a blockchain with 
    potential forks? Actual according to who? How does this relate to the
    delegation map in the chain?

  * The genesis chain's ledger delegation map equals the initial actual
    delegation map.

  * Each ledger delegation map is either synchronized with the actual
    delegation map or else out-of-date.
    
    EdV: Not sure I understand "out of date". This and the point I commented on 
    above suggest that history is linear, and nodes may merely be "behind" on
    updates. But what if they disagree?

  * When the test infrastucture changes the actual delegation map, it also
    promptly generates the necessary delegation certificate transactions to
    re-synchronize the ledger delegation maps with the new actual delegation
    map.

  * Invariant: No two node instances simultaneously have the same operational
    key.

  * Invariant: For all genesis keys @gk@, no two node instances simultaneously
    have a current ledger that maps @gk@ to their own operational key.

Mechanism:

We ensure that last invariant by always shutting down the relevant node
instance before altering the actual delegation map.

* Suppose we are changing the actual delegate map so that it maps @gk@ to @ok2@
  instead of @ok1@.

* We would first shut down the node currently configured to use @ok1@, if any.

* We would then update the actual delegate map.

* We would then either restart the node or replace it with a new one; either
  way, the resulting node is (newly) configured to use @ok2@.

* We would generate the @gk := ok2@ delegate certificate transaction in the new
  node's memory pool. (TxSub will propagate this to all downstream nodes, so it
  can get into the ledger. The new (honest) node can't lead until that
  happens.)

Concern:

* What if the transaction expires before being propagated or else before being
  included in a block?

* Similar but slightly different: What if the transaction does get included in
  a block, but then we switch off that chain?

It seems like the new node may need to persistently re-generate that
transaction until it finds it in its immutable DB.

EdV: So these are not test concerns, these are concerns inherent in delegating.
Indeed, a wallet node would do something similar to this (monitor the chain
to see if its transactions have been included, and resubmit after a while). 
I'm not sure that we should include this ability in the node right now; let's
perhaps open a ticket for that particular subproblem. 

EdV: One thing that is useful to realize is that a node _can_ safely start
producing blocks as soon as a certificate has been included on the chain; 
it doesn't need to wait. After all, other nodes that synchronize that same
chain will see the certificate before the new blocks, and so the certificate
will come first; if they are on a different chain, they won't see either.

EdV: Note however that the spec limits when delegation certificates can take
effect, precisely to address some chain stability problems. See the Byron
ledger specification. 

-}

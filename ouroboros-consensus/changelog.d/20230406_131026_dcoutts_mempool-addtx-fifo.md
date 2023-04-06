<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->
<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->

### Breaking

- Remove function `tryAddTxs` from the mempool API. The implementation (Shelly Era)
  of this function relied on the fairness of 'service-in-random-order', and 
  endeavoured to maximally fill the mempool. Since the Babbage Era there is an 
  increased variation in representational size of transactions for a given cost 
  of processing. This means that, under certain conditions, representationally
  large transactions could be stalled in progress between mempools.
  This function was replaced by `addTx`.
- Add a `addTx` function to the mempool API. This function tries to add a single
  transaction and blocks if the mempool can not accept the given transaction. 
  This means that entry to a mempool is now a (per-node) FIFO. This also ensure
  that transactions will always progress, irrespective of size.
  The refactoring introduces two FIFO queues. Remote clients have to queue in both
  of them, whereas local clients only have to queue in the local clients' queue. 
  This gives local clients a higher precedence to get into their local mempool under
  heavy load situations.



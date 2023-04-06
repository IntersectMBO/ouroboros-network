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

- Remove function `tryAddTxs` from the mempool API. The implementation of this
  function did not guarantee fairness, and therefore transactions could starve.
  This function was replaced by `addTx`.
- Add a `addTx` function to the mempool API. This function tries to add a single
  transaction and blocks if the mempool if full. The mempool is now fair, which
  means that no transaction will be starved, provided that transactions are
  eventually removed from the mempool. Fairness is achieved by introducing two
  FIFO queues. Remote clients have to queue in both of them, whereas local
  clients only have to queue in the local clients' queue. This gives local
  clients priority over remote ones.



# The Oddchain

An exercise on integrating
[`ouroboros-consensus`](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus/)
with custom blocks and ledger rules.

The odd chain is a blockchain consisting only of odd numbers. Even numbers are
rejected.

## Defining the block type

We start defining a block. When we try to define the instance of `UpdateLedger`
we'll find out we need a `HasHeader` instance for `OddBlock`.

`HasHeader` is defined in `Ouroboros.Network.Block`.

## Defining the ledger update functions

Update ledger class defined at `Ouroboros.Consensus.Ledger.Abstract`.

I.e. `UpdateLedger`.

## Hooking ledger to a consensus protocol

See `Ouroboros.Consensus.Protocol.Abstract`

TODO.

## Defining `ProtocolLedgerView`

See `Ouroboros.Consensus.Ledger.Abstract`.

TODO.

## Testing the integration

See `Test.ThreadNet.BFT`.


### Defining a `RunNode` instance

## Questions

### Does `blk` stands for block?
  In `class UpdateLedger`.

### Why requiring `HasHeader` twice?
  - HasHeader blk
  - HasHeader (Header blk)

### Why can't `applyChainTick` throw errors?

  I still don't get it, even after reading the comment:

  ```haskell
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  ```
